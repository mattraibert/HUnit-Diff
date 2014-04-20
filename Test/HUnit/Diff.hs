{- | Very basic support for diffing with HUnit.

Limitations:

  * Prints the whole value, not just the difference with a few lines of
    context.

  * Relies on the similarity of pretty-printed 'show' results which
    sorta-kinda works much of the time but may sometimes highlight
    differences too eagerly.

  * Always colors the differences for ANSI terminals, regardless of output
    target.

Despite these limitations, I find it more useful than HUnit's defaults.

-}
module Test.HUnit.Diff ((@?==), (@==?)) where

import Data.Functor        ( (<$>) )

import Data.Algorithm.Diff ( getDiff, Diff(Both, First, Second) )
import System.Console.ANSI ( Color(Green, Red, White), ColorIntensity(Dull)
                           , ConsoleLayer(Foreground), SGR(SetColor, Reset)
                           , setSGRCode )
import Test.HUnit          ( Assertion, assertBool, (@?=), (@=?) )
import Text.Groom          ( groom )

-- | Like '@?=' but producing a colored diff on failure.
(@?==) :: (Eq a, Show a) => a -> a -> Assertion
x @?== y =
    assertBool ('\n':msg) (x == y)
  where
    msg       = unlines $ fmt <$> getDiff (lines . groom $ x)
                                          (lines . groom $ y)
    fmt (Both s _) = color White $ ' ' : s
    fmt (First s)  = color Green $ '+' : s
    fmt (Second s) = color Red   $ '-' : s
    color c s = setSGRCode [SetColor Foreground Dull c]
                ++ s ++
                setSGRCode [Reset]

-- | Like '@=?' but producing a colored diff on failure.
(@==?) :: (Eq a, Show a) => a -> a -> Assertion
y @==? x = x @?== y
