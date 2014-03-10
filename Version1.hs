--
-- Using actual names requires tricky management of variable names
-- for capture-avoiding substitution. If we knew all of the dangerous
-- vars before hand, substitution is cleaner. When type checking
-- we know what vars are in scope because they're given by the context
-- so capture-avoiding substitution should take advantage of that
--

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

type Name = String

-- Scopes bind single variables, independent of mode of use
-- Scope n b is equivalent to x.b in Harper's notation
data Scope = Scope Name Exp
  deriving (Eq,Show)

data Exp = Var Name
         | Prop
         | Arr Exp Scope   -- (x : A) -> B  as  Arr A (Scope x B)  analogous to Pi(A,x.B)
         | Lam Exp Scope   -- \(x : A) -> b  as  Lam A (Scope x B)  analogous to \(A,x.B)
         | App Exp Exp
  deriving (Eq,Show)

-- we can make a successor name by adding a prime
next :: Name -> Name
next x = x ++ "'"


-- given some in-scope vars, we can make a name fresh
-- by repeatedly taking the next name until we get
-- a name that's not in scope
freshen :: [Name] -> Name -> Name
freshen inscope n = if n `elem` inscope
                    then freshen inscope (next n)
                    else n

-- we substitute into a scope by freshening and
-- substituting into the fresh body
substScope :: [Name] -> Exp -> Name -> Scope -> Scope
substScope inscope expr n (Scope name body)
  | n == name   = Scope name body
  | otherwise   = let fresh_name = freshen inscope name
                      fresh_body = subst (name:inscope) (Var fresh_name) name body
                  in Scope fresh_name (subst (fresh_name:inscope) expr n fresh_body)

subst :: [Name] -> Exp -> Name -> Exp -> Exp
subst _    expr n (Var n')
  | n == n'                  = expr
  | otherwise                = Var n'
subst _       _    _ Prop        = Prop
subst inscope expr n (Pi t s)   = Pi (subst inscope expr n t) (substScope inscope expr n s)
subst inscope expr n (Lam t s)  = Lam (subst inscope expr n t) (substScope inscope expr n s)
subst inscope expr n (App e e') = App (subst inscope expr n e) (subst inscope expr n e')
