{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EnvMem where

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
      Expr(..), locExprSubst )

import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process
import Control.Monad (forever)


-- 환경
type Env = M.Map ExprVar (Addr, ProcessId)

-- 값
data Value
  = VLit    Literal
  | VTuple  [Value]
  | VConstr String [Value]
  | VClosure ProcessId Env [(ExprVar, Maybe Type, Location)] Expr
  | VTAbs   [String] Expr
  | VLAbs   [String] Expr
  | VAddr   ProcessId Integer
  | VActorId ProcessId
  deriving (Show, Typeable, Data, Generic, Binary)

-- 메모리
data Mem = Mem { _new :: Integer, _map :: M.Map Integer Value }
  deriving (Show, Generic, Binary)

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
