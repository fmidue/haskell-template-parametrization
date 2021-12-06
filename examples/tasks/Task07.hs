configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-matches
- unused-pattern-binds
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use camelCase
- Use drop
- Use elem
- Use even
- Use foldl
- Use foldr
- Use fst
- Use head
- Use id
- Use init
# - Use isJust
# - Use isNothing
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use maximum
- Use minimum
- Use null
- Use odd
- Use otherwise
- Use pictures
- Use product
- Use replicate
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
- codeworld
- teaching
- monomorphic
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- unused-local-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'fixity: "infixr 0 &"'
- 'hint: {lhs: "3.14", rhs: pi}'
- 'hint: {lhs: "6.28", rhs: 2 * pi, name: Use pi}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant /=
- Redundant ==
- Redundant if
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use &&
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use brighter
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use darker
- Use dilated
- Use dilatedPoint
- Use duller
- Use find
- Use floor
- Use fromMaybe
- Use guards
- Use if
- Use infix
- Use lefts
- Use lighter
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use newtype instead of data
- Use notElem
- Use or
- Use pi
- Use repeat
- Use rights
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use ||
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
import CodeWorld hiding (blank)
import Prelude hiding (($), (!!), null, head, tail, last, init, take, drop, splitAt)
import Data.Text (pack)

-- Complete the following skeleton for a loading screen animation, by
-- implementing the 'circles' and 'partialCircle' functions.
--
-- After you implemented the two functions, the result should look
-- similar to:
-- https://code.world/run.html?mode=haskell&dhash=DSjC7gTCi_bE2Yy7cwjG31g
--
-- The 'partialCircle' should result in a rotating circle with two
-- gaps at opposing sides. It takes a five-tuple and the animation
-- time as parameters. The five-tuple consists of the radius and
-- thickness of the circle, the size of the gaps in the circle (with
-- no particular unit, a bigger gap value just means a bigger gap),
-- the speed of the rotation (negative speeds invert the direction of
-- rotation), and the color of the circle.
--
-- The 'circles' function takes a list of parameters for partial
-- circles and should combine them into one animation.
-- (Hint: A list comprehension is useful here.)
--
-- Optional: For a little challenge, try modifing your program to
-- produce an animation like:
-- https://code.world/run.html?mode=haskell&dhash=DIDA_jQC__jKNt9NWimiMkA
-- or even:
-- https://code.world/run.html?mode=haskell&dhash=DPWdRCwl_iNJcP3NOwceFxg
-- (If you do so, please provide both the basic and the modified
-- version in your submission.)
-- Hint: The difficulty of this modification heavily depends on how
-- you realized movement in the basic version. There is a way to do it
-- such that only very little adjustment is needed for the advanced
-- version.

main :: IO ()
main = animationOf loading

loading :: Double -> Picture
loading t =
  styledLettering Plain SansSerif (pack "LOADING")
  & circles
    [(3,0.2,1.5,2,black)
    ,(4,0.15,3,-0.5,red)
    ,(5,0.1,4.5,0.25,black)
    ] t

circles :: [(Double,Double,Double,Double,Color)] -> Double -> Picture
circles = undefined

partialCircle :: (Double,Double,Double,Double,Color) -> Double -> Picture
partialCircle = undefined
----------
module Test (test) where
import qualified Main
import CodeWorld (black)
import Test.HUnit ((~:), Test)

import TestHelper (isDefined, isDeeplyDefined)

test :: [ Test ]
test =
  [ "circles =/= undefined?" ~: isDeeplyDefined (Main.circles [] 1.0)
  , "partialCircle =/= undefined?" ~: isDefined (Main.partialCircle (3,0.2,1.5,2,black))
  ]
