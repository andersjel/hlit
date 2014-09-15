{-# OPTIONS_HADDOCK hide #-}

module Text.Lit.Internal
    ( render_
    , renderBlock_
    , BlockType
    , InlineType ) where

import qualified Text.Lit.Render     as R
import           Text.Lit.Report     (Report)
import           Data.Foldable       (toList)
import qualified Text.Pandoc.Builder as B

type InlineType = Report [B.Inline]
render_ :: R.Render a => a -> InlineType
render_ = fmap toList . R.render

type BlockType = Report [B.Block]
renderBlock_ :: R.RenderBlock a => a -> BlockType
renderBlock_ = fmap toList . R.renderBlock
