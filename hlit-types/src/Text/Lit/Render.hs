{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.Lit.Render
    ( Render (..)
    , RenderBlock (..)
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid
import           Text.Lit.RenderFloat   (renderFloat)
import           Text.Lit.Report
import qualified Text.Pandoc.Builder    as Pandoc

-- | Instances of this typeclass can occur inline in @hlit@ documents.
-- For instance, in the document
--
-- > Pi is equal to: `@ pi :: Float`.
-- 
-- the `render` method would be called to convert @pi@ to pandoc `Pandoc.Inlines`.
class Render a where
    
    -- | Convert to pandoc types within the `Report` monad. See "Text.Pandoc.Builder"
    -- if you need to write instances.
    render :: a -> Report Pandoc.Inlines
    
    -- | Special case for lists. This makes it possible to render a 
    -- `String` different from other lists. Compare `Text.Show.showList` 
    -- from "base". The default implementation in terms of `render` should
    -- normally suffice.
    renderAsList :: [a] -> Report Pandoc.Inlines
    renderAsList xs = pure "[" <=> f xs <=> pure "]"
      where f []  = pure mempty
            f [x] = render x
            f (x:xs') = render x <=> pure ", " <=> f xs'
            (<=>) = liftA2 (<>)

class RenderBlock a where
    renderBlock :: a -> Report Pandoc.Blocks

instance Render Pandoc.Inlines where
    render = return

instance Render (Report Pandoc.Inlines) where
    render = id

instance Render Pandoc.Inline where
    render = return . Pandoc.singleton

instance Render a => Render (IO a) where
    render x = render =<< liftIO x

instance Render a => Render [a] where
    render = renderAsList

instance Render () where
    render = const $ return mempty

instance Render Char where
    render = renderAsList . pure
    renderAsList = return . Pandoc.text

instance Render Int where
    render = render . show

instance Render Integer where
    render = render . show

instance Render Bool where
    render = render . show

instance Render Float where
    render = renderFloat

instance Render Double where
    render = renderFloat

instance Render a => Render (Maybe a) where
    render (Just x) = render x
    render Nothing = return $ Pandoc.emph "Nothing"

instance (Render a, Render b) => Render (Either a b) where
    render (Right x) = render x
    render (Left x) = Pandoc.emph <$> render x

instance RenderBlock () where
    renderBlock = const $ return mempty

instance RenderBlock (Report ()) where
    renderBlock a = a *> return mempty

instance RenderBlock Pandoc.Blocks where
    renderBlock = return

instance RenderBlock (Report Pandoc.Blocks) where
    renderBlock = id

instance RenderBlock Pandoc.Block where
    renderBlock = return . Pandoc.singleton

instance RenderBlock a => RenderBlock (IO a) where
    renderBlock x = renderBlock =<< liftIO x

instance RenderBlock String where
    renderBlock = return . Pandoc.codeBlock
