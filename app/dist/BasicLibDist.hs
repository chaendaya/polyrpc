module BasicLibDist where

import qualified Data.Map as M
import Control.Monad (foldM)
import Control.Distributed.Process

import Expr
import Type
import Location
import Prim
import Literal
import EnvMem


dummyTy :: Type
dummyTy = TypeVarType "_dummy"

-- read : Unit -> String
-- read x = primRead x
readBinding :: BindingDecl
readBinding =
  Binding False "read" dummyTy $
    Abs [("x", Nothing, LocVar "$empty")] $
      Prim PrimReadOp [] [] [Var "x"]

-- print : String -> Unit
-- print x = primPrint x
printBinding :: BindingDecl
printBinding =
  Binding False "print" dummyTy $
    Abs [("x", Nothing, LocVar "$empty")] $
      Prim PrimPrintOp [] [] [Var "x"]

-- intToString : Int -> String
-- intToString x = primIntToString x
intToStringBinding :: BindingDecl
intToStringBinding =
  Binding False "intToString" dummyTy $
    Abs [("x", Nothing, LocVar "$empty")] $
      Prim PrimIntToStringOp [] [] [Var "x"]

-- concat : String -> String -> String
-- concat x y = primConcat x y
concatBinding :: BindingDecl
concatBinding =
  Binding False "concat" dummyTy $
    Abs [("x", Nothing, LocVar "$empty")] $
      Abs [("y", Nothing, LocVar "$empty")] $
        Prim PrimConcatOp [] [] [Var "x", Var "y"]

-- ref : a -> Ref a
-- ref x = primRefCreate x
refBinding :: BindingDecl
refBinding =
  Binding False "ref" dummyTy $
    Abs [("x", Nothing, LocVar "$empty")] $
      Prim PrimRefCreateOp [] [] [Var "x"]

-- (!) : Ref a -> a
-- (!) addr = primRefRead addr
bangBinding :: BindingDecl
bangBinding =
  Binding False "!" dummyTy $
    Abs [("addr", Nothing, LocVar "$empty")] $
      Prim PrimRefReadOp [] [] [Var "addr"]

-- (:=) : Ref a -> a -> Unit
-- (:=) addr v = primRefWrite addr v
assignBinding :: BindingDecl
assignBinding =
  Binding False ":=" dummyTy $
    Abs [("addr", Nothing, LocVar "$empty")] $
      Abs [("v",    Nothing, LocVar "$empty")] $
        Prim PrimRefWriteOp [] [] [Var "addr", Var "v"]


basicLibBindings :: [BindingDecl]
basicLibBindings =
  [ readBinding
  , printBinding
  , intToStringBinding
  , concatBinding
  , refBinding
  , bangBinding
  , assignBinding
  ]

-- prim util
calc :: PrimOp -> [Value] -> Mem -> Process (Value, Mem)
calc op vs mem =
  case op of
    -- 기본 라이브러리 / Ref
    PrimReadOp ->
      case vs of
        [VLit UnitLit] -> do
          line <- liftIO getLine
          pure (VLit (StrLit line), mem)
        _ ->
          error $ "[PrimReadOp] expected one Unit argument, got: " ++ show vs

    PrimPrintOp ->
      case vs of
        [VLit (StrLit s)] -> do
          liftIO $ putStr s
          pure (VLit UnitLit, mem)
        _ ->
          error $ "[PrimPrintOp] expected one String argument, got: " ++ show vs

    PrimIntToStringOp ->
      case vs of
        [VLit (IntLit i)] ->
          pure (VLit (StrLit (show i)), mem)
        _ ->
          error $ "[PrimIntToStringOp] expected one Int argument, got: " ++ show vs

    PrimConcatOp ->
      case vs of
        [VLit (StrLit s1), VLit (StrLit s2)] ->
          pure (VLit (StrLit (s1 ++ s2)), mem)
        _ ->
          error $ "[PrimConcatOp] expected two String arguments, got: " ++ show vs

    PrimRefCreateOp -> unsupported
    PrimRefReadOp   -> unsupported
    PrimRefWriteOp  -> unsupported

    _ -> let lits = map getLit vs
             lit  = calc' op lits
         in pure (VLit lit, mem)
  where
    unsupported = error $ "[Prim] basic library operation not supported yet: " ++ show op


getLit :: Value -> Literal
getLit (VLit lit) = lit
getLit v          = error $ "[Prim] expected literal value, got: " ++ show v

calc' :: PrimOp -> [Literal] -> Literal
-- 논리 연산
calc' NotPrimOp [BoolLit b]               = BoolLit (not b)
calc' OrPrimOp  [BoolLit x, BoolLit y]    = BoolLit (x || y)
calc' AndPrimOp [BoolLit x, BoolLit y]    = BoolLit (x && y)

-- == : EqPrimOp
calc' EqPrimOp [IntLit x,  IntLit y]      = BoolLit (x == y)
calc' EqPrimOp [BoolLit x, BoolLit y]     = BoolLit (x == y)
calc' EqPrimOp [StrLit x,  StrLit y]      = BoolLit (x == y)

-- != : NeqPrimOp
calc' NeqPrimOp [IntLit x,  IntLit y]     = BoolLit (x /= y)
calc' NeqPrimOp [BoolLit x, BoolLit y]    = BoolLit (x /= y)
calc' NeqPrimOp [StrLit x,  StrLit y]     = BoolLit (x /= y)

-- Int/Bool/String 확정된 EqPrimOp, NeqPrimOp
calc' EqIntPrimOp    [IntLit x,  IntLit y]   = BoolLit (x == y)
calc' EqBoolPrimOp   [BoolLit x, BoolLit y]  = BoolLit (x == y)
calc' EqStringPrimOp [StrLit x,  StrLit y]   = BoolLit (x == y)

calc' NeqIntPrimOp    [IntLit x,  IntLit y]  = BoolLit (x /= y)
calc' NeqBoolPrimOp   [BoolLit x, BoolLit y] = BoolLit (x /= y)
calc' NeqStringPrimOp [StrLit x,  StrLit y]  = BoolLit (x /= y)

-- <, <=, >, >=
calc' LtPrimOp [IntLit x, IntLit y] = BoolLit (x <  y)
calc' LePrimOp [IntLit x, IntLit y] = BoolLit (x <= y)
calc' GtPrimOp [IntLit x, IntLit y] = BoolLit (x >  y)
calc' GePrimOp [IntLit x, IntLit y] = BoolLit (x >= y)

-- 산술 연산
calc' AddPrimOp [IntLit x, IntLit y] = IntLit (x + y)
calc' SubPrimOp [IntLit x, IntLit y] = IntLit (x - y)
calc' MulPrimOp [IntLit x, IntLit y] = IntLit (x * y)
calc' DivPrimOp [IntLit x, IntLit y] = IntLit (x `div` y)
calc' NegPrimOp [IntLit x]           = IntLit (-x)

calc' op lits =
  error $ "[Prim] unexpected operands for " ++ show op ++ " : " ++ show lits