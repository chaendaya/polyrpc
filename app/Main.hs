{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import NodeRegistry
import SystemMessage
import qualified PolyPipeline as Poly

import System.IO
import System.Environment (getArgs)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, forM_, when)
import Control.Concurrent.STM

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

--
import CommonParserUtil

import TokenInterface
import Token
import Lexer
import Terminal
import Parser
import qualified ParserBidi as PB
import qualified ParserBidiLinks as PBLinks
import Type
import Expr
import BasicLib
import qualified CSType as TT
import qualified CSExpr as TE
import TypeCheck
import TypeInfLinks
import Monomorphization
import Report
import Compile
import Simpl
import Verify
import Execute
import Interp


import Text.JSON.Generic
import Text.JSON.Pretty
import Text.PrettyPrint
-- For aeson
--import qualified Data.ByteString.Lazy.Char8 as B
--import Data.Aeson.Encode.Pretty
import Data.Maybe
import System.IO
import System.Environment (getArgs, withArgs)

import Data.Text.Prettyprint.Doc.Util (putDocW)

-- Syntax completion
import EmacsServer
import SyntaxCompletion(computeCand)
import SyntaxCompletionSpec(spec)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("main":addrStr:fileName:_)        -> runMainNode addrStr fileName
    -- (role:addrStr:mainAddrStr:fileName:_)   -> runNodeByRole role addrStr mainAddrStr fileName
    _ -> putStrLn $ unlines
      [ "Usage:"
      , "  polyrpc main <ip:port> <file>"
      , "  polyrpc <role> <ip:port> <main-ip:port> <file>"
      ]

runMainNode :: String -> FilePath -> IO ()
runMainNode addrStr fileName = do
  -- create TCP transport and Main node
  let (host, portStr) = break (== ':') addrStr
  Right transport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  -- -- 메인과 연결된 프로세스들 레지스트리 생성 (STM TVar)
  -- rolesRegistry <- newRoleRegistry 

  -- mvar <- newEmptyMVar              -- just for prompt

  -- -- Background listener : 노드 레지스트리 프로세스
  -- _ <- forkIO $ runProcess node $ do
  --   mNid <- getSelfNode
  --   liftIO $ putMVar mvar mNid      -- just for prompt

  --   -- "nodeRegistry"라는 이름으로 현재 프로세스 등록
  --   self <- getSelfPid
  --   register "nodeRegistry" self
  --   liftIO $ putStrLn $ "[Main@" ++ show mNid ++ "] Node registry process started."

  --   -- 무한 루프 : 
  --   --    1. 노드 등록 (stack run actors-exe <role> ...)
  --   --    2. 노드 다운 모니터링
  --   forever $ do
  --     receiveWait
  --       [ match $ \(msg :: NodeMessage) -> 
  --           case msg of
  --             -- role을 지정하여 새 노드 등록 요청 처리
  --             RegisterRole role requesterPid -> do
  --               _ <- monitor requesterPid
  --               liftIO $ atomically $ registerRole rolesRegistry role requesterPid
  --               allPids <- liftIO $ atomically $ getAllPids rolesRegistry
  --               forM_ allPids $ \pid ->
  --                 when (pid /= requesterPid) $ send pid (List_Val [(Str_Val "CONNECT"), List_Val [(Str_Val role), (Actor_Val requesterPid)]])
  --               liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Registered role: " ++ show requesterPid
  --         ,
  --         -- 다운된 노드 감지 시 레지스트리에서 제거
  --         match $ \(ProcessMonitorNotification _ deadPid _) -> do
  --           liftIO $ atomically $ removeProcess rolesRegistry deadPid
  --           liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Role down: " ++ show deadPid ++ " removed"
  --       ]

  -- mNid <- takeMVar mvar

  -- Run the interpreter
  runProcess node $ do
    pid <- getSelfPid
    -- register "mainInterp" pid   -- main 액터의 pid 이름
    -- liftIO $ atomically $ registerRole rolesRegistry "main" pid
    liftIO $ putStrLn $ "dist polyrpc main started at " ++ show pid

    liftIO $ putStrLn $ "[Reading] " ++ fileName
    text <- liftIO $ readFile fileName

    liftIO $ putStrLn "[Parsing-Surface syntax]"
    -- exprSeqAst <- parsing PB.parserSpec terminalList
    exprSeqAst <- liftIO $ parsing 
                              False 
                              PBLinks.parserSpec 
                              ((), 1, 1, text) 
                              (aLexer lexerSpec) 
                              (fromToken (endOfToken lexerSpec))
  
    let toplevelDecls = fromASTTopLevelDeclSeq exprSeqAst
    liftIO $ print toplevelDecls

    res <- runTop toplevelDecls
    liftIO $ print res

    -- liftIO $ putStrLn "[Bidirectional type checking]"
    -- (gti, elab_toplevelDecls1, elab_builtinDatatypes, lib_toplevelDecls)
    --   <- liftIO $ typeInf False toplevelDecls basicLib

    -- let elab_toplevelDecls = lib_toplevelDecls ++ elab_builtinDatatypes ++ elab_toplevelDecls1

    -- mapM_ (\(name, decls) -> do
    --           liftIO $ putStrLn $ "[" ++ name ++ "]"
    --           liftIO $ print decls
    --           liftIO $ putStrLn ""
    --       )
    --   [ ("Lib",   lib_toplevelDecls)
    --   , ("Built-in", elab_builtinDatatypes)
    --   , ("User", elab_toplevelDecls1)
    --   ]

  {-
    putStrLn "[Type checking]"
    (gti, elab_toplevelDecls) <- typeCheck toplevelDecls basicLib
    verbose (_flag_debug_typecheck cmd) $ putStrLn "Dumping..."
    verbose (_flag_debug_typecheck cmd) $ putStrLn $ show $ elab_toplevelDecls

    print_rpc cmd file elab_toplevelDecls
  -}

    -- (s_gti, s_toplevelDecls, s_basicLib) <- liftIO $ return (gti, elab_toplevelDecls, basicLib)

    -- liftIO $ putStrLn "[Compiling]"
    -- (t_gti, funStore, t_expr) <- liftIO $ compile s_gti s_toplevelDecls s_basicLib
    -- liftIO $ print t_gti
    -- liftIO $ putStrLn ""
    -- liftIO $ print funStore
    -- liftIO $ putStrLn ""
    -- liftIO $ print t_expr
    -- liftIO $ putStrLn ""

--  Poly.runPolyPipeline [fileName]

