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
- Use fst
- Use guards
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
# - Use null
- Use odd
- Use otherwise
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
allowAdding: false
allowModifying: true
allowRemoving: false
configHlintGroups:
- codeworld
- monomorphic
- teaching
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
- Use foldl
- Use foldr
- Use fromMaybe
# - Use if
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
- Use pictures
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
import CodeWorld
import Prelude hiding (($))
import Data.Text

-- Some animation functions are given, and you are not allowed to
-- change them:

animation1 :: Double -> Picture
animation1 t
  | t < 2     = colored red (circle t)
  | otherwise = blank

animation2 :: Double -> Picture
animation2 t
  | t < 3     = rectangle t (t + 2)
  | otherwise = blank

animation3 :: Double -> Picture
animation3 t
  | t < 2     = solidCircle (t / 2)
  | otherwise = blank

-- The following program overlays these animations. Unfortunately,
-- that is not exactly what we want. We want the animations to be
-- "played" one after another, and only afterwards do we want the end
-- notice to be shown. Moreover, we want to scale 'animation2' by a
-- size factor of 2, and we want 'animation3' to be played at half
-- speed.
--
-- The outcome should look as follows:
--
-- https://code.world/run.html?mode=haskell&dhash=DWiaEAgKRHlF5Cg4mKQCv_w
--
-- But remember: You are not allowed to change anything in the
-- definitions of 'animation1', 'animation2' and 'animation3'
-- themselves. You should use them unchanged. And if someone else were
-- to change them, your code in 'scene' should still work with them
-- (assuming that said someone else has not changed the individual
-- animations' durations).
--
-- Of course you *are* allowed to change the conceptual structure and
-- the content of the 'scene' function. But, as always desirable, do
-- make sure that your code causes no non-exhaustiveness warnings.
--
-- There is no need for adding new functions, only change `scene`.

scene :: Double -> Picture
scene t
  = animation1 t
  & animation2 t
  & animation3 t
  & lettering (pack "The End")

main :: IO ()
main = animationOf scene
----------
module Test (test) where
import Test.HUnit ((~:), Test)

import qualified Main
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Main.scene 1.0)
  ]
