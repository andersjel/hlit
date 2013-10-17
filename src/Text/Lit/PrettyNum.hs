module Text.Lit.PrettyNum
    ( prettyNum
    , Style (..)
    , PrettyNumContext (..)
    ) where

import Data.Default
import Text.Pandoc.Builder as Pandoc

data Style = Style
    { showTrailingZeros :: !Bool
    , showExtraDigits :: !Bool
    }

fuzzy = Style False True
standard = Style True False

instance Default Style where
    def = fuzzy

data PrettyNumContext = PrettyNumContext
    { precision :: !Int
    , large :: !Int
    , small :: !Int
    }

instance Default PrettyNumContext where
    def = PrettyNumContext 
        { precision = 5
        , large = 9
        , small = 3
        }

prettyNum :: RealFrac a => PrettyNumContext -> a -> Pandoc.Inlines
prettyNum context num = undefined