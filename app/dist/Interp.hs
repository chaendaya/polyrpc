{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Interp where

import qualified Data.Map as M
import Data.Data (Data, Typeable)

import Location
import Prim
import Literal
import Type (Type)
import Expr hiding (Env)

import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process

type Env = M.Map String Value

data Value
  = VLit    Literal
  | VTuple  [Value]
  | VConstr String [Value]
  | VClosure Env [(String, Maybe Type, Location)] Expr  -- λ 클로저
  | VTAbs   [String] Expr Env
  | VLAbs   [String] Expr Env
  | VActorId ProcessId
  deriving (Show, Typeable, Data)


-- 메모리
data Mem = Mem { _new :: Integer, _map :: M.Map Integer Value }
type Addr = Integer

initMem :: Mem
initMem = Mem { _new = 1, _map = M.empty }

allocMem :: Value -> Mem -> (Addr, Mem)
allocMem v mem =
  let a = _new mem in (a, mem { _new = a+1, _map = M.insert a v (_map mem) })

readMem :: Addr -> Mem -> Value
readMem a mem = maybe (error $ "[readMem] not found: " ++ show a) id (M.lookup a (_map mem))

writeMem :: Addr -> Value -> Mem -> Mem
writeMem a v mem = mem { _map = M.insert a v (_map mem) }


-- 프로세스 테이블 (spawn/export)
type ProcTable = M.Map ProcessId Env

-- 런타임 표현식: Expr 또는 Value
data RExpr = E Expr | V Value
  deriving (Show)

-- 함수형 컨텍스트
type EvalContext = RExpr -> RExpr
type Stack = [EvalContext]

data Config = Config
  { cfgEnv   :: Env
  , cfgCur   :: RExpr
  , cfgStack :: Stack
  , cfgMem   :: Mem
  , cfgPT    :: ProcTable
  , cfgNextP :: Integer
  }

initConfig :: Expr -> Config
initConfig e = Config M.empty (E e) [] initMem M.empty 1

applyEvCxt :: [EvalContext] -> RExpr -> RExpr
applyEvCxt []     r = r
applyEvCxt (k:ks) r = applyEvCxt ks (k r)

toFun :: [EvalContext] -> EvalContext
toFun []     = id
toFun (k:ks) = toFun ks . k

runTop :: [TopLevelDecl] -> Process Value
runTop tops = run (lowerProgram tops)

lowerProgram :: [TopLevelDecl] -> Expr
lowerProgram tops =
  let topBinds = [ b | BindingTopLevel b <- tops ]
      body     = Var "main"
  in  nestLets topBinds body

nestLets :: [BindingDecl] -> Expr -> Expr
nestLets binds body =
  foldr (\b acc -> Let [b] acc) body binds

run :: Expr -> Process Value
run e = loop (initConfig e)
  where
    loop cfg = case cfg of
      Config _ (V v) [] _ _ _ -> return v
      _ -> step cfg >>= loop

step :: Config -> Process Config
step (Config env (V v) (k:ks) mem pt np) =
  evalValue env v ks k mem pt np
step (Config env (V v) [] mem pt np) =
  pure (Config env (V v) [] mem pt np)
step (Config env (E e) ks mem pt np) =
  evalExpr env e ks mem pt np

evalExpr  :: Env -> Expr -> Stack -> Mem -> ProcTable -> Integer -> Process Config
-- 변수
evalExpr env (Var x) ks mem pt np =
  case M.lookup x env of
    Just v  -> pure (Config env (V v) ks mem pt np)
    Nothing -> error ("[Var] unbound: " ++ x)

-- 리터럴
evalExpr env (Lit l) ks mem pt np =
  pure (Config env (V (VLit l)) ks mem pt np)

-- 튜플
evalExpr env (Tuple es) ks mem pt np =
    error "TODO"

-- 데이터 생성자
evalExpr env (Constr c _ _ es _argTys) ks mem pt np =
    error "TODO"

-- λ / 타입 / 위치 추상
evalExpr env (Abs params body) ks mem pt np =
  pure (Config env (V (VClosure env params body)) ks mem pt np)

evalExpr env (TypeAbs as body) ks mem pt np =
  pure (Config env (V (VTAbs as body env)) ks mem pt np)

evalExpr env (LocAbs ls body) ks mem pt np =
  pure (Config env (V (VLAbs ls body env)) ks mem pt np)

-- let x = e1 in e2
-- evalExpr env (Let [Binding _ x _ e1] e2) ks mem pt np =
--   let k :: Env -> String -> Expr -> EvalContext
--       k saved x' body (V v) = E (substOne x' v body saved)  -- 간단 치환 or env 확장
--       k _ _ _ r             = r
--   in pure (Config env (E e1) ( (k env x e2) : ks) mem pt np)
evalExpr env (Let (Binding _ x _ e1 : bs) body) ks mem pt np =
  let k :: Env -> String -> [BindingDecl] -> Expr -> EvalContext
      k saved x' rest body' (V v) =
        -- 치환 기반 (간단)
        E (Let rest (substOne x' v body' saved))
      k _ _ _ _ r = r
  in pure (Config env (E e1) (k env x bs body : ks) mem pt np)


-- case e of ...
evalExpr env (Case scrut _ alts) ks mem pt np =
    error "TODO"

-- 함수 적용 f arg
evalExpr env (App f _ arg _) ks mem pt np =
  let k :: Expr -> Env -> EvalContext
      k argE saved (V vf) = E argE  -- 인자 평가로 진입
      k _    _     r      = r
  in pure (Config env (E f) (k arg env : ks) mem pt np)

-- 타입/위치 적용: 런타임엔 본질적으로 no-op (본문으로 진행)
evalExpr env (TypeApp e _ _) ks mem pt np = pure (Config env (E e) ks mem pt np)
evalExpr env (LocApp  e _ _) ks mem pt np = pure (Config env (E e) ks mem pt np)

-- Prim: 왼→오 인자 평가 후 계산
evalExpr env (Prim op locs tys es) ks mem pt np =
    error "TODO"

-- Spawn / Exports
evalExpr env (Spawn Nothing) ks mem pt np = do
  pid <- spawnLocal $ do 
                self <- getSelfPid
                liftIO $ putStrLn $ "Spawned process with PID: " ++ show self
  pure (Config env (V (VActorId pid)) ks mem pt (np+1))

evalExpr env (Spawn (Just e)) ks mem pt np =
    error "TODO"

evalExpr env (Exports binds) ks mem pt np = do
  (env', mem') <- evalBindings env binds mem
  pure (Config env' (V (VLit UnitLit)) ks mem' pt np)

-- 기타
evalExpr _ e _ _ _ _ =
  error ("[evalExpr] unhandled: " ++ show e)

-- ===== Value 단계 =====
evalValue :: Env -> Value -> Stack -> EvalContext -> Mem -> ProcTable -> Integer -> Process Config

evalValue _ (VClosure cloEnv ((x,_,_):rest) body) ks k mem pt np =
  pure (Config cloEnv (V (VClosure cloEnv ((x,Nothing,LocVar "$dummy"):rest) body)) ks mem pt np)

evalValue env v ks k mem pt np =
  let r' = k (V v)
  in pure (Config env r' ks mem pt np)


evalBindings :: Env -> [BindingDecl] -> Mem -> Process (Env, Mem)
evalBindings env [] mem = pure (env, mem)
evalBindings env (Binding _ x _ rhs : bs) mem = do
  v <- run rhs
  let env' = M.insert x v env
  evalBindings env' bs mem

-- Prim
primCalc :: PrimOp -> [Location] -> [Type] -> [Value] -> Mem -> Process (Value, Mem)
primCalc PrimRefCreateOp _ _ [v] mem = let (a, mem') = allocMem v mem in pure (VAddr a, mem')
primCalc PrimRefReadOp   _ _ [VAddr a] mem = pure (readMem a mem, mem)
primCalc PrimRefWriteOp  _ _ [VAddr a, v] mem = pure (VLit UnitLit, writeMem a v mem)
primCalc PrimPrintOp     _ _ [VLit (StrLit s)] mem = do
  liftIO (putStr s)
  pure (VLit UnitLit, mem)
primCalc _ _ _ _ _ = error "[primCalc] fill more cases"

unsafePrim :: PrimOp -> [Location] -> [Type] -> [Value] -> Value
unsafePrim AddPrimOp _ _ [VLit (IntLit x), VLit (IntLit y)] = VLit (IntLit (x+y))
unsafePrim _ _ _ vs = error ("[unsafePrim] unsupported: " ++ show vs)

substOne :: String -> Value -> Expr -> Env -> Expr
substOne x v body _savedEnv =
  body