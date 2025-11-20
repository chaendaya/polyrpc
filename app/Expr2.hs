module Expr2 where
import Expr (BindingDecl)



data Expr =
      Var Identifier        -- 변수
    | Lit Literal           -- 리터럴
    | Let [BindingDecl] Expr    -- let 바인딩
    | Prim PrimOp [Expr]

    | Abs Identifier Expr   -- 람다 함수
    | App Expr Expr         -- 함수 적용

    | Tuple [Expr]
    | Const Identifier [Expr]
    | Case Expr [Alternative]

    | Spawn (Maybe Expr)    -- 액터 생성
    deriving (Show, Eq)

data PrimOp
  = AddPrimOp
  | SubPrimOp
  | MulPrimOp
  | DivPrimOp
  | EqIntPrimOp
  | LtPrimOp
  | LePrimOp
  | GtPrimOp
  | GePrimOp
  deriving (Show, Eq)

type Identifier = String

data Value
  = VLit    Literal
  | VTuple  [Value]
  | VConstr String [Value]
  | VClosure Env [(String, Maybe Type, Location)] Expr  -- λ 클로저
  | VTAbs   [String] Expr Env
  | VLAbs   [String] Expr Env
  | VActorId ProcessId
  deriving (Show, Typeable, Data)