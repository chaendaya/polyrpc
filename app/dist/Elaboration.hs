module Elaboration where

import Expr

import qualified Data.Set as S

collectConstrNames :: [TopLevelDecl] -> S.Set String
collectConstrNames tops =
  foldr goTop S.empty tops
  where
    goTop (DataTypeTopLevel (DataType _ _ _ tycons)) acc =
      foldr (\(TypeCon name _) s -> S.insert name s) acc tycons
    goTop _ acc = acc

-- App 체인 평탄화: f a b c  ->  (f, [a,b,c])
collectApps :: Expr -> (Expr, [Expr])
collectApps (App f _ arg _) =
  let (h, args) = collectApps f
  in (h, args ++ [arg])
collectApps e = (e, [])

desugarExpr :: S.Set String -> Expr -> Expr
desugarExpr cons e =
  case e of
    Var x -> Var x

    Abs ps body ->
      Abs ps (desugarExpr cons body)

    Let bs body ->
      Let (map (desugarBinding cons) bs) (desugarExpr cons body)

    Tuple es ->
      Tuple (map (desugarExpr cons) es)

    Case scrut mt alts ->
      Case (desugarExpr cons scrut) mt
           (map (desugarAlt cons) alts)

    Constr c ls tys es ml ->
      Constr c ls tys (map (desugarExpr cons) es) ml

    Spawn me ->
      Spawn (fmap (desugarExpr cons) me)

    Exports bs ->
      Exports (map (desugarBinding cons) bs)

    TypeAbs as body ->
      TypeAbs as (desugarExpr cons body)

    LocAbs ls body ->
      LocAbs ls (desugarExpr cons body)

    TypeApp e' mt tys ->
      TypeApp (desugarExpr cons e') mt tys

    LocApp e' mt locs ->
      LocApp (desugarExpr cons e') mt locs

    Prim op ls tys es ->
      Prim op ls tys (map (desugarExpr cons) es)

    Lit l ->
      Lit l

    -- App 체인 처리
    app@(App _ _ _ _) ->
      let (h, args) = collectApps app
          h'        = desugarExpr cons h
          args'     = map (desugarExpr cons) args
      in case h' of
           -- 데이터 생성자라면 Constr로 교체
           Var c | c `S.member` cons ->
             Constr c [] [] args' []

           -- 아니면 그냥 App으로 복구
           _ ->
             foldl (\acc a -> App acc Nothing a Nothing) h' args'


desugarBinding :: S.Set String -> BindingDecl -> BindingDecl
desugarBinding cons (Binding btop x ty expr) =
  Binding btop x ty (desugarExpr cons expr)

desugarAlt :: S.Set String -> Alternative -> Alternative
desugarAlt cons (Alternative cname vars body) =
  Alternative cname vars (desugarExpr cons body)
desugarAlt cons (TupleAlternative vars body) =
  TupleAlternative vars (desugarExpr cons body)


desugarProgram :: [TopLevelDecl] -> [TopLevelDecl]
desugarProgram tops =
  let cons = collectConstrNames tops
  in map (desugarTop cons) tops

desugarTop :: S.Set String -> TopLevelDecl -> TopLevelDecl
desugarTop cons (BindingTopLevel b) =
  BindingTopLevel (desugarBinding cons b)
desugarTop _ t@(DataTypeTopLevel _) =
  t  -- data 선언은 그대로 놔둠
