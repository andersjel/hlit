{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lit.Walk
    ( walk
    ) where

import           Control.Applicative
import           Data.Data           (Data, gfoldl)
import           Data.Foldable       (asum)
import           Data.Maybe          (fromMaybe)
import           Data.Traversable
import           Data.Typeable       (cast, gcast)
import           Text.Pandoc.Builder (Block (..), Inline (..), Pandoc (..))

-- | A generic applicative transformation that maps over the immediate
-- subterms
gmapA
    :: (Applicative f, Data a)
    => (forall b. Data b => b -> f b)
    -> a -> f a
gmapA f = gfoldl (\x y -> x <*> f y) pure

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
