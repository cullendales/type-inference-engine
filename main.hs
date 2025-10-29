import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import System.Environment 
import System.IO


-- definitions and types mentioned to be used in program
type VarId = String

data Expr = CInt Int
          | CBool Bool
          | Var VarId
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | ITE Expr Expr Expr
          | Abs VarId Expr
          | App Expr Expr
          | LetIn VarId Expr Expr
          deriving (Eq, Ord, Read, Show)

data Type = TInt
          | TBool
          | TError
          | TVar Int
          | TArr Type Type
          deriving (Eq, Ord, Read, Show)

data Constraint = CEq Type Type
                | CError
                deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

type RelabelState a = State (Map.Map Int Int) a -- forgot this initially

type Env = Map.Map VarId Type
type InferState a = State Int a

-- step 2 the getFreshTVar function  returning a monadic value
getFreshTVar :: InferState Type
getFreshTVar = do n <- get
                  put (n + 1)
                  return (TVar n)

-- step 3: long one to return monadic value using CT rules

infer :: Env -> Expr -> InferState (Type, ConstraintSet)
infer g (CInt _)  = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)
infer g (Var x) = case Map.lookup x g of
                    Just y -> return (y, Set.empty)
                    Nothing -> return (TError, Set.singleton CError)

infer g (Abs x e) = do y <- getFreshTVar
                       (t, c) <- infer (Map.insert x y g) e
                       return (TArr y t, c)

-- maybe could use different style that would be more efficient but works
infer g (Plus e1 e2) = do (t1, c1) <- infer g e1
                          (t2, c2) <- infer g e2
                          let c = Set.union (Set.union c1 c2) (Set.fromList [CEq t1 TInt, CEq t2 TInt])
                          return (TInt, c)

infer g (Minus e1 e2) = do (t1, c1) <- infer g e1
                           (t2, c2) <- infer g e2
                           let c = Set.union (Set.union c1 c2) (Set.fromList [CEq t1 TInt, CEq t2 TInt])
                           return (TInt, c)

infer g (Equal e1 e2) = do (t1, c1) <- infer g e1
                           (t2, c2) <- infer g e2
                           let c = Set.union (Set.union c1 c2) (Set.singleton (CEq t1 t2))
                           return (TBool, c)


infer g (ITE e1 e2 e3) = do (t1, c1) <- infer g e1
                            (t2, c2) <- infer g e2
                            (t3, c3) <- infer g e3
                            let c = Set.union (Set.union c1 (Set.union c2 c3)) (Set.fromList [CEq t1 TBool, CEq t2 t3])
                            return (t2, c)

infer g (App e1 e2) = do x1 <- getFreshTVar
                         x2 <- getFreshTVar
                         (t1, c1) <- infer g e1
                         (t2, c2) <- infer g e2
                         let c = Set.union (Set.union c1 c2) (Set.fromList [CEq t1 (TArr x1 x2), CEq t2 x1])
                         return (x2, c)

infer g (LetIn v e1 e2) = do x <- getFreshTVar
                             (t1, c1) <- infer (Map.insert v x g) e1
                             (t2, c2) <- infer (Map.insert v x g) e2
                             let c = Set.union (Set.union c1 c2) (Set.singleton (CEq x t1))
                             return (t2, c)


-- step 4 takes a FUN expression and produces constraint based typing output
inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr x = evalState (infer Map.empty x) 0

-- step 5
toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

--step6
type Substitution = Map.Map Type Type
applySub :: Substitution -> Type -> Type
applySub s (TVar x) = Map.findWithDefault (TVar x) (TVar x) s
applySub s (TArr t1 t2) = TArr (applySub s t1) (applySub s t2)
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub _ TError = TError

--step7 2 substitutions σ1, σ2 as input and produces σ1 ◦ σ2 as output.
applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList s c = map applySubToCstr c
  where applySubToCstr (CEq t1 t2) = CEq (applySub s t1) (applySub s t2)
        applySubToCstr CError = CError

--step 8
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = Map.union (Map.map (applySub s1) s2) s1

--step 9 returns all type variables in a FUN type
tvars :: Type -> Set.Set Type
tvars (TVar x) = Set.singleton (TVar x)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)
tvars _ = Set.empty

--step 10 unify function: check slides for it
unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify (CError : _) = Nothing
unify (CEq s t : c)
   | s == t = unify c
   | otherwise = case (s, t) of
      (TVar x, _) -> if Set.member (TVar x) (tvars t) then Nothing
                     else do
                        let v = Map.singleton (TVar x) t
                        y <- unify (applySubToCstrList v c)
                        return (composeSub y v)
      (_, TVar x) -> if Set.member (TVar x) (tvars s) then Nothing
                     else do
                        let v = Map.singleton (TVar x) s
                        y <- unify (applySubToCstrList v c)
                        return (composeSub y v)
      (TArr s1 s2, TArr t1 t2) -> unify (CEq s1 t1 : CEq s2 t2 : c)
      _ -> Nothing

--step11
typing :: Expr -> Maybe Type
typing x = let (t, c) = inferExpr x in
           case unify (toCstrList c) of
              Just s  -> Just (applySub s t)
              Nothing -> Nothing

--step12: use relabel from doc
typeInfer :: Expr -> String
typeInfer x = case (typing x) of
    Just t -> show (relabel t)
    Nothing -> "Type Error"


-- the relabel function we are supplied with
relabel :: Type -> Type

relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt = return TInt
    go TBool = return TBool
    go TError = return TError
    go (TVar x) = do m <- get
                     case Map.lookup x m of
                      Just v -> return (TVar v)
                      Nothing -> do let n = 1 + Map.size m
                                    put (Map.insert x n m)
                                    return (TVar n)
    go (TArr t1 t2) = do t1' <- go t1
                         t2' <- go t2
                         return (TArr t1' t2')



--main
main :: IO ()
main = do args <- getArgs
          let filename = head args
          content <- readFile filename
          let ls = lines content
          mapM_ (putStrLn . typeInfer . read) ls





