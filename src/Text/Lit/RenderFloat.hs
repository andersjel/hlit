{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Lit.RenderFloat
    ( renderFloat
    , FloatStyle (..)
    , FloatContext (..)

#ifdef COMPILE_TESTS
    , tests
#endif
    ) where

import           Data.Default
import           Data.Monoid
import qualified Numeric
import qualified Text.Pandoc.Builder                  as Pandoc

#ifdef COMPILE_TESTS
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck                      as Q
#endif

data FloatStyle = Standard | Fuzzy
  deriving Show

data FloatContext = FloatContext
    { sigFigs :: !Int
    , large   :: !Int
    , small   :: !Int
    , style   :: !FloatStyle
    }
  deriving Show

isFuzzy :: FloatContext -> Bool
isFuzzy FloatContext{style=Fuzzy} = True
isFuzzy _ = False

instance Default FloatContext where
    def = FloatContext
        { sigFigs = 5
        , large = 8
        , small = -3
        , style = Fuzzy
        }

-- | FloatD is a number of the form +/- 0.xxx*10^y
data FloatD = FloatD
    Bool  -- ^ True if this number is negative
    [Int] -- ^ The digits of the number
    Int   -- ^ The exponent

-- | A thin wrapper around Numeric.floatToDigits
toFloatD :: RealFloat a => a -> FloatD
toFloatD x = FloatD (x < 0) ds ex
    where (ds, ex) = Numeric.floatToDigits 10 (abs x)

-- | Round off to /sig/ digits. Includes trailing zeros.
roundD :: Int -> FloatD -> FloatD
roundD sig (FloatD neg digits expon) =
    let digits' = take (sig + 1) $ digits ++ repeat 0
        roundedDown = init digits'
        (roundedUp, expon') =
            if carry then (1:init ds', expon + 1) else (ds', expon)
          where (ds', carry) = foldr f ([], True) $ init digits'
                f 9 (ds, True) = (0 : ds, True)
                f d (ds, True) = (d + 1 : ds, False)
                f d (ds, x) = (d : ds, x)
    in if last digits' < 5
        then FloatD neg roundedDown expon
        else FloatD neg roundedUp expon'

data FloatPresentation = FloatPresentation
    Bool  -- ^ has minus sign
    [Int] -- ^ digits before decimal point
    Bool  -- ^ show decimal point
    [Int] -- ^ digits after decimal point
    Int   -- ^ exponent
  deriving Show

toExp :: FloatD -> FloatPresentation
toExp (FloatD neg (d:ds) ex) =
    FloatPresentation neg [d] True ds (ex - 1)
toExp _ = error "Malformed floatD."

-- | Show a float without exponential notation.
--   This function returns nothing if it is unclear
--   how many significant figures the resulting presentation
--   has. E.g. 1.30*10^3 cannot be rendered as '1300' because
--   that would imply only two significant figures, and it
--   cannot be rendered as '1300.' because that would imply
--   four signigicant figures.
toNorm :: FloatD -> Maybe FloatPresentation
toNorm (FloatD neg ds ex) =
    let pre = if ex < 1 then [0] else take ex (ds ++ repeat 0)
        post = if ex < 0
            then replicate (abs ex) 0 ++ ds
            else drop ex ds
        point = length ds >= ex
    in if not point && last ds == 0
        then Nothing
        else Just $ FloatPresentation neg pre point post 0

dropZeroes :: FloatPresentation -> FloatPresentation
dropZeroes (FloatPresentation neg pre point post ex) =
    FloatPresentation neg pre point (f post) ex
  where
    f = reverse . dropWhile (==0) . reverse

presentFloat :: RealFloat a => FloatContext -> a -> FloatPresentation
presentFloat context num =
    let r@(FloatD _ _ expon) = toFloatD num
        sig = sigFigs context
        sigN = if isFuzzy context then max sig expon else sig
        asExp = toExp $ roundD sig r
        rN@(FloatD _ _ exponN) = roundD sigN r
        asNorm = toNorm rN
        presentation = case asNorm of
            _ | expon > large context  -> asExp
            _ | expon < small context  -> asExp
            _ | exponN > large context -> asExp
            Just x                     -> x
            Nothing                    -> asExp
    in if isFuzzy context
        then dropZeroes presentation
        else presentation

renderFloat :: RealFloat a => FloatContext -> a -> Pandoc.Inlines
renderFloat context num =
    let FloatPresentation neg pre point post expon =
            presentFloat context num
    in  if neg then "-" else mempty
        <> Pandoc.str (concatMap show pre)
        <> if point then "." else mempty
        <> Pandoc.str (concatMap show post)
        <> if expon /= 0
            then "\215\&10" <> (Pandoc.superscript . Pandoc.str . show) expon
            else mempty

#ifdef COMPILE_TESTS

tests :: Test
tests = testGroup "RenderFloat"
    [ testProperty "prop_sigfig" prop_sigfig
    ]

prop_sigfig :: Double -> Q.Property
prop_sigfig x = x /= 0 Q.==>
    Q.forAll (Q.choose (1, 20)) $ \sig ->
        let context = def{sigFigs=sig, style=Standard}
            FloatPresentation _ pre point post _ =
                presentFloat context x
            sig' = if point
                then length . dropWhile (==0) $ pre ++ post
                else length . dropWhile (==0) . reverse $ pre
        in sig == sig'

#endif
