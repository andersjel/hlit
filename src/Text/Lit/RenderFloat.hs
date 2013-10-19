module Text.Lit.RenderFloat
    ( renderFloat
    , FloatStyle (..)
    , fuzzy
    , standard
    , FloatContext (..)
    , FloatPresentation (..)
    , presentFloat
    ) where

import Data.Default
import Text.Pandoc.Builder as Pandoc
import qualified Numeric

data FloatStyle = FloatStyle
    { showTrailingZeros :: !Bool
    , showExtraDigits :: !Bool
    }
  deriving Show

fuzzy :: FloatStyle
fuzzy = FloatStyle False True
standard :: FloatStyle
standard = FloatStyle True False

instance Default FloatStyle where
    def = fuzzy

data FloatContext = FloatContext
    { sigFigs :: !Int
    , large :: !Int
    , small :: !Int
    , style :: !FloatStyle
    }
  deriving Show

instance Default FloatContext where
    def = FloatContext 
        { sigFigs = 5
        , large = 8
        , small = 3
        , style = def
        }

-- | Given /sig/ and the output of 'Numeric.floatToDigits 10 (abs x)' 
--   for some x, round of to /sig/ digits.
--   Includes trailing zeros. Disregards the sign of the number.
roundOff 
    :: Int          -- ^ How many significant digits  
    -> ([Int], Int) -- ^ 'Numeric.floatToDigits 10 (abs x)'
    -> ([Int], Int) -- ^ (digits big-endian, potentially modified exponent)
roundOff sig (digits, expon) =
    let
        digits' = take (sig + 1) $ digits ++ repeat 0
        roundedDown = init digits'
        (roundedUp, carry) = foldr f ([], True) $ init digits'
          where f 9 (ds, True) = (0 : ds, True)
                f d (ds, True) = (d + 1 : ds, False)
                f d (ds, x) = (d : ds, x)
    in case () of
        _ | last digits' < 5 -> (roundedDown, expon)
        _ | carry            -> (1 : init roundedUp, expon + 1)
        _                    -> (roundedUp, expon)

data FloatPresentation = FloatPresentation
    { hasMinusSign      :: Bool
    , digitsBeforePoint :: [Int]
    , showPoint         :: Bool
    , digitsAfterPoint  :: [Int]
    , exponent_         :: Int
    }
  deriving Show

presentFloat :: RealFloat a => FloatContext -> a -> FloatPresentation
presentFloat context num = 
    let
        r@(_, expon) = Numeric.floatToDigits 10 $ abs num
        sig = sigFigs context
        sigN = 
          if showExtraDigits . style $ context
            then max sig expon
            else sig
        dropZeroes = 
          if showTrailingZeros . style $ context
            then id
            else reverse . dropWhile (== 0) . reverse
        asExp = FloatPresentation 
            (num < 0) [d] True (dropZeroes ds) (ex - 1)
          where (d:ds, ex) = roundOff sig r
        (digitsN, exponN) = roundOff sigN r
        asN = FloatPresentation (num < 0) pre (sigN >= exponN) post 0
          where pre = take exponN $ digitsN ++ repeat 0
                post = dropZeroes $ drop exponN digitsN
    in case () of
        _ | expon > large context               -> asExp
        _ | expon < small context               -> asExp
        _ | exponN > large context              -> asExp
        -- A number like 1.20e4 cannot be written as 1200 because 
        -- it is then unclear that the the number has three significant
        -- figures. The next guard covers this case.
        _ | exponN > sigN && last digitsN == 0  -> asExp
        _                                       -> asN

renderFloat :: RealFloat a => FloatContext -> a -> Pandoc.Inlines
renderFloat context num = undefined