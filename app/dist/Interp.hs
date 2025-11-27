{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interp where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import qualified Data.Map as M
import Data.Data (Data, Typeable)

import Location
import Prim
import Literal
import Type (Type)
import Expr
    ( ExprVar,
      BindingDecl(..),
      TopLevelDecl(BindingTopLevel),
      Expr(..), Alternative(..), locExprSubst )
import EnvMem
import Elaboration

import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import BasicLibDist


-- 실행 상태
data Config = Config
  { cfgEnv   :: Env
  , cfgMem   :: Mem
  }
  deriving (Show, Generic, Binary)

initConfig :: Config
initConfig = Config 
    { cfgEnv = M.empty
    , cfgMem = initMem
    }

data RemoteMessage
  = RequestVar Addr ProcessId
  | RequestApp Value Value ProcessId
  | RequestAbs Env [(ExprVar, Maybe Type, Location)] Expr ProcessId
  deriving (Show, Generic, Binary, Typeable)

data ReturnMessage
  = ReturnVal Value
  | ReturnEnv Env
  deriving (Show, Generic, Binary, Typeable)

-- Top-level 실행
runTop :: [TopLevelDecl] -> Process Value
runTop tops = do
  let tops' = desugarProgram tops
  run (lowerProgram tops')

lowerProgram :: [TopLevelDecl] -> Expr
lowerProgram tops =
  let topBinds = [ b | BindingTopLevel b <- tops ]
      body     = Var "main"
  in  nestLets topBinds body

nestLets :: [BindingDecl] -> Expr -> Expr
nestLets binds body =
  foldr (\b acc -> Let [b] acc) body binds


--
run :: Expr -> Process Value
run e = do
  let cfg0 = initConfig
  cfg1 <- installBasicLib cfg0
  (v, cfgFinal) <- evalExpr cfg1 e
  pure v


-- Expr을 현재 Config에서 평가해서 (Value, 새 Config)를 리턴
evalExpr :: Config -> Expr -> Process (Value, Config)
evalExpr cfg (Var x) = do
  self <- getSelfPid
  case M.lookup x (cfgEnv cfg) of
    Just (addr, pid) 
      | pid == self -> do
              liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Var (Local): " ++ x 
              let v = readMem addr (cfgMem cfg)
              pure (v, cfg)
      | otherwise -> do
              liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Var (Remote): " ++ x 
              send pid (RequestVar addr self)
              waitForReturn cfg
    Nothing -> error $ "[Var] variable not found: " ++ x 

evalExpr cfg (LocAbs ls body) =
  error "TODO"

evalExpr cfg (TypeAbs as body) =
  error "TODO"

evalExpr cfg (Abs param body) = do   -- 의미론은 단일 인자
  self <- getSelfPid
  case param of
    ((_,_,loc):_) -> do
      -- 함수 지정된 위치 확인 1) 로컬 변수 -> 액터 id 가져오기
      let locName = locNameOf loc

      -- 0. locName == "$empty" 이면 무조건 로컬 함수로
      if locName == "$empty"
        then do
              let cloVal = VClosure self (cfgEnv cfg) param body
              liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Abs (Local): " ++ show cloVal
              pure (cloVal, cfg)
        else
          -- 1. locName 이 현재 위치 변수인 경우: env에서 찾기
          case M.lookup locName (cfgEnv cfg) of

            -- (1-1) locName이 현재 액터 env 에 있고, 로컬 바인딩
            Just (addr, pid)
              | pid == self -> do
                  let v = readMem addr (cfgMem cfg)
                      VActorId pid' = v
                  if pid' == self 
                  then do -- 로컬 액터의 함수
                          let cloVal = VClosure self (cfgEnv cfg) param body
                          liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Abs (Local): " ++ show cloVal
                          pure (cloVal, cfg)
                  else do -- 원격 액터의 함수
                          send pid' (RequestAbs (cfgEnv cfg) param body self)
                          waitForReturn cfg

            -- (1-2) locName 이 다른 위치에 있는 경우: 먼저 actor id 값을 원격에서 받아오기
              | otherwise -> do
                  send pid (RequestVar addr self)
                  (v, cfg') <- waitForReturn cfg
                  let VActorId pid' = v
                  if pid' == self
                    then do -- 로컬 액터의 함수
                          let cloVal = VClosure self (cfgEnv cfg') param body
                          liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Abs (Local): " ++ show cloVal
                          pure (cloVal, cfg')
                    else do -- 원격 액터의 함수
                          send pid' (RequestAbs (cfgEnv cfg') param body self)
                          waitForReturn cfg'
              
            Nothing -> error $ "[Abs] location variable not found: " ++ locName
    [] -> error "[Abs] no parameter"

evalExpr cfg (Let [] body) = 
  evalExpr cfg body

evalExpr cfg (Let (Binding _ x _ exp : rest) body) = do
  self <- getSelfPid
  -- 첫번째 바인딩 평가 후 환경에 추가
  (v, cfg1) <- evalExpr cfg exp
  liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Let : " ++ show x ++ " = " ++ show v
  let (addr, mem1) = allocMem v (cfgMem cfg1)
      env1 = M.insert x (addr, self) (cfgEnv cfg1)
      cfg2 = cfg1{ cfgEnv = env1, cfgMem = mem1 }
  continue cfg2
  -- 나머지 바인딩 처리
  where
    continue cfg2 =
      case rest of
        [] -> evalExpr cfg2 body
        _  -> evalExpr cfg2 (Let rest body)

evalExpr cfg (Case scrut _ alts) = do
  (vScrut, cfg1) <- evalExpr cfg scrut
  evalCaseAlts vScrut cfg1 alts

evalExpr cfg (App f _ arg _) = do
  (rator, cfg1) <- evalExpr cfg f    -- rator 평가
  (rand, cfg2) <- evalExpr cfg1 arg  -- rand 평가
  self <- getSelfPid
  case rator of
    VClosure pid cloEnv param body
      | pid == self -> do
          -- 로컬 함수 호출
          case param of
            ((x,_,_):_) -> do
              liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "App (Local): " ++ show x ++ " = " ++ show rand
              let (addr, mem') = allocMem rand (cfgMem cfg2)
                  env' = M.insert x (addr, self) cloEnv
                  cfg3 = Config { cfgEnv = env', cfgMem = mem' }
              (v,cfg3') <- evalExpr cfg3 body
              pure (v, cfg{cfgMem = cfgMem cfg3'})
            [] -> error "[App] no parameters in closure"
      
      | otherwise -> do
          -- 원격 함수 호출 요청
          liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "App (Request): " ++ show f ++ " "++ show rand
          send pid (RequestApp rator rand self)
          waitForReturn cfg2

    _ -> error "[App] rator is not a closure"

             
evalExpr cfg (TypeApp exp _ _) =
  error "TODO"

evalExpr cfg (LocApp exp _ _) =
  error "TODO"

evalExpr cfg (Tuple exps) = do
  (vs, cfg') <- evalExprList cfg exps
  pure (VTuple vs, cfg')

evalExpr cfg (Prim op _ _ exps) = do
  (vs, cfg1) <- evalExprList cfg exps
  (v, mem') <- calc op vs (cfgMem cfg1)
  pure (v, cfg1{ cfgMem = mem' })

evalExpr cfg (Lit l) =
  pure (VLit l, cfg)

evalExpr cfg (Constr s _ _ exps _) = do
  (vs, cfg') <- evalExprList cfg exps
  pure (VConstr s vs, cfg')

evalExpr cfg (Spawn Nothing) = do
  pid <- spawnLocal $ do 
                let cfg0 = Config
                        { cfgEnv = cfgEnv cfg
                        , cfgMem = initMem
                        }
                cfgLib <- installBasicLib cfg0
                runReadyServiceLoop cfgLib
                forever $ liftIO $ threadDelay maxBound
  pure (VActorId pid, cfg)

evalExpr cfg (Spawn (Just e))= do
  current <- getSelfPid
  pid <- spawnLocal $ do
                self <- getSelfPid 
                -- 새로운 mem, 기존 env사용
                let cfg0 = Config
                        { cfgEnv = cfgEnv cfg
                        , cfgMem = initMem
                        }
                cfgLib <- installBasicLib cfg0
                (v, cfg1) <- evalExpr cfgLib e

                send current (ReturnEnv (cfgEnv cfg1))
                liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "Spawn : " ++ "send extended env" ++ show (cfgEnv cfg1)
                runReadyServiceLoop cfg1
                forever $ liftIO $ threadDelay maxBound

  -- 확장된 env 가져와서 현재 프로세스의 env 업데이트 
  ReturnEnv env' <- expect :: Process ReturnMessage
  let cfg' = cfg { cfgEnv = env' }
  pure (VActorId pid, cfg')

evalExpr cfg (Exports binds) = do
  (_, cfg') <- evalExports cfg binds
  pure (VLit UnitLit, cfg')


--
evalExprList :: Config -> [Expr] -> Process ([Value], Config)
evalExprList cfg [] = pure ([], cfg)
evalExprList cfg (e:es) = do
  (v, cfg1)  <- evalExpr cfg e
  (vs, cfg2) <- evalExprList cfg1 es
  pure (v:vs, cfg2)

evalExports :: Config -> [BindingDecl] -> Process ([Value], Config)
evalExports cfg [] = pure ([VLit UnitLit], cfg)
evalExports cfg (Binding _ x _ exp : rest) = do
  self <- getSelfPid
  -- 첫번째 바인딩 평가 후 환경에 추가
  (v, cfg1) <- evalExpr cfg exp
  let (addr, mem1) = allocMem v (cfgMem cfg1)
      env1 = M.insert x (addr, self) (cfgEnv cfg1)
      cfg2 = cfg1{ cfgEnv = env1, cfgMem = mem1 }
  liftIO $ putStrLn $ "["  ++ show self ++ "] " ++ "evalExports : " ++ show x ++ " = " ++ show v
  -- 나머지 바인딩 처리
  evalExports cfg2 rest


-- 원격 메시지 처리 보조 함수
runReadyServiceLoop :: Config -> Process ()
runReadyServiceLoop cfg = do
  cfg' <- runReadyService cfg
  runReadyServiceLoop cfg'

runReadyService :: Config -> Process Config
runReadyService cfg = do
  msg <- expect :: Process RemoteMessage
  handleRemoteMessage cfg msg

handleRemoteMessage :: Config -> RemoteMessage -> Process Config
handleRemoteMessage cfg msg = do
  current <- getSelfPid
  case msg of
    RequestVar addr sender -> do
        let v = readMem addr (cfgMem cfg)
        send sender (ReturnVal v)
        liftIO $ putStrLn $ "["  ++ show current ++ "] " ++ "ReturnVal : " ++ show v
        pure cfg

    RequestAbs env params body sender -> do
        let cloVal = VClosure current env params body
        send sender (ReturnVal cloVal)
        liftIO $ putStrLn $ "["  ++ show current ++ "] " ++ "ReturnVal : " ++ show cloVal
        pure cfg

    RequestApp f arg sender -> do
        let VClosure pid cloEnv param body = f
        case param of
          ((x,_,_):_) -> do
            let (addr, mem') = allocMem arg (cfgMem cfg)
                cloEnv' = M.insert x (addr, current) cloEnv
                cfg1 = Config{ cfgEnv = cloEnv', cfgMem = mem' }
            (v,cfg2) <- evalExpr cfg1 body
            send sender (ReturnVal v)
            liftIO $ putStrLn $ "["  ++ show current ++ "] " ++ "ReturnVal : " ++ show v
            pure cfg { cfgMem = cfgMem cfg2 }
          [] -> error "[App] no parameter in closure"

waitForReturn :: Config -> Process (Value, Config)
waitForReturn cfg = do
  current <- getSelfPid 
  receiveWait 
    [ match $ \(msg :: ReturnMessage) -> case msg of
        ReturnVal v -> do
          liftIO $ putStrLn $ "[" ++ show current ++ "] waitForReturn: got ReturnVal"
          pure (v, cfg)
        _ -> waitForReturn cfg
    
    , match $ \(msg :: RemoteMessage) -> do
        liftIO $ putStrLn $ "[" ++ show current ++ "] waitForReturn: handle " ++ show msg
        cfg1 <- handleRemoteMessage cfg msg
        waitForReturn cfg1
    ]


-- 기본 라이브러리 추가
installBasicLib :: Config -> Process Config
installBasicLib cfg0 = do
  (_, cfgLib) <- evalExports cfg0 basicLibBindings
  pure cfgLib


-- case 보조 함수
    -- v : scrutinee 평가 결과
    -- cfg : scrutinee 평가 이후의 Config
    -- [Alternative] : case 분기들
evalCaseAlts :: Value -> Config -> [Alternative] -> Process (Value, Config)
evalCaseAlts _ cfg [] =
  error "[Case] non-exhaustive patterns"

evalCaseAlts v cfg (alt:alts) =
  case matchAlternative v alt of
    Nothing ->
      -- 이 분기는 안 맞으면 다음 분기로
      evalCaseAlts v cfg alts

    Just bindings -> do
      self <- getSelfPid
      -- 패턴에서 나온 (변수, 값)들을 env/mem에 바인딩
      let (env', mem') = extendEnvWithBindings (cfgEnv cfg) (cfgMem cfg) self bindings
          cfg'         = cfg { cfgEnv = env', cfgMem = mem' }
          body         = altBody alt
      evalExpr cfg' body

-- scrutinee 값 v 와 Alternative 하나를 패턴 매칭
-- 성공하면 (패턴 변수, 값) 리스트를 돌려줌
matchAlternative :: Value -> Alternative -> Maybe [(ExprVar, Value)]
matchAlternative v (Alternative cname xs _body) =
  case v of
    VConstr s vs
      | s == cname && length vs == length xs ->
          Just (zip xs vs)
      | otherwise ->
          Nothing

    VLit (BoolLit b)
      | null xs
      , (b && cname == "True") || (not b && cname == "False") ->
          Just []
      | otherwise -> 
        Nothing

    _ ->
      Nothing

matchAlternative v (TupleAlternative xs _body) =
  case v of
    VTuple vs
      | length vs == length xs ->
          Just (zip xs vs)
      | otherwise ->
          Nothing
    _ ->
      Nothing

-- Alternative에서 body Expr 꺼내기
altBody :: Alternative -> Expr
altBody (Alternative _ _ e)      = e
altBody (TupleAlternative _ e)   = e

-- (x1,v1), (x2,v2), ... 를 env/mem에 순서대로 바인딩
extendEnvWithBindings :: Env -> Mem -> ProcessId -> [(ExprVar, Value)] -> (Env, Mem)
extendEnvWithBindings env mem _self [] = (env, mem)
extendEnvWithBindings env mem self ((x,v):bs) =
  let (addr, mem1) = allocMem v mem
      env1         = M.insert x (addr, self) env
  in extendEnvWithBindings env1 mem1 self bs