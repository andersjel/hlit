module Lit.Splice where

data Splice

instance Functor Splice where
    fmap = undefined

instance Applicative Splice where
    pure  = undefined
    (<*>) = undefined

type TypeID = ()
type Expr = ()
type Errors = ()
type Options = ()

splice :: TypeID -> Expr -> Splice a

runSplice :: Options -> Splice a -> IO (Either Errors a)