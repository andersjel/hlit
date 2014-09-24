{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Both haskell-src-exts and pandoc have quite voluminous types. We use
-- Data.Generics to traverse them. Here are some assorted helpers.
module Lit.Generic where

import           Control.Applicative
import           Data.Data           (Data)
import qualified Data.Data           as Data
import           Data.Foldable       (asum)
import           Data.Maybe          (fromMaybe, fromJust)
import           Data.Traversable
import           Data.Typeable       (cast, gcast)
import qualified Data.Typeable       as Typeable
import           Text.Pandoc.Builder (Block, Inline, Pandoc)

-- | A generic applicative transformation that maps over the immediate
-- subterms
gmapA
    :: (Applicative f, Data a)
    => (forall b. Data b => b -> f b)
    -> a -> f a
gmapA f = Data.gfoldl (\x y -> x <*> f y) pure

-- Get an immediate subterm of matching type
getG :: (Data.Data a, Typeable.Typeable b) => a -> b
getG = fromJust . Data.gmapQl (<|>) Nothing cast

setG :: (Data.Data a, Typeable.Typeable b) => b -> a -> a
setG v = Data.gmapT f
  where
    f x = fromMaybe x $ cast v

concatTraverse :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatTraverse h x = concat <$> traverse h x

walk
    :: forall f. Applicative f
    => (Inline -> Maybe (f [Inline]))
    -> (Block -> Maybe (f [Block]))
    -> Pandoc
    -> f Pandoc
walk f1 f2 = gmapA f
  where
    g :: Data a => (a -> Maybe (f [a])) -> a -> f [a]
    g h x = fromMaybe ((:[]) <$> gmapA f x) $ h x
    f :: Data a => a -> f a
    f x = fromMaybe (gmapA f x) $ asum
        [ gcast . concatTraverse (g f1) =<< cast x
        , gcast . concatTraverse (g f2) =<< cast x
        ]
