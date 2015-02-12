{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Text.Lit.C3js (chart) where

import qualified Text.Pandoc.Builder as P
import qualified Data.Aeson as A
import Data.Aeson (toJSON, encode)
import Data.Typeable (Typeable)
import Text.Lit.Report (ConfigVar, fromTag, modify, get, Report)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Strict (insert)

data ChartCountTag = ChartCountTag deriving Typeable
chartCount :: ConfigVar Int
chartCount = fromTag ChartCountTag 0

chart :: A.ToJSON a => a -> Report P.Block
chart ch = do
  count <- get chartCount
  modify chartCount (+1)
  let (A.Object obj) = toJSON ch
      chartName = "_chart_" ++ show count
      obj' = insert "bindto" (toJSON $ "#" ++ chartName) obj
      element = "<div id=\"" ++ chartName ++ "\"></div>"
      json = unpack $ encode obj'
      script = "<script>c3.generate(" ++ json ++ ");</script>"
  return $ P.RawBlock (P.Format "html") $ element ++ script
