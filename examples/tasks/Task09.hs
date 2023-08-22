enableWhitespaceWatermarking = return "True"
moduleName = return "Task09"
link = return "https://code.world/run.html?mode=haskell&dhash=DWiaEAgKRHlF5Cg4mKQCv_w"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Redundant bracket
- Use camelCase
- Use even
- Use guards
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate

allowAdding: false
allowModifying: true
allowRemoving: false

#{commonConfigHlintGroups}
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

#{commonConfigHlintRules}
- 'fixity: "infixr 0 &"'
- 'hint: {lhs: "3.14", rhs: pi}'
- 'hint: {lhs: "6.28", rhs: 2 * pi, name: Use pi}'

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant if
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
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use ||

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where

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
-- #{link}
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
import qualified #{moduleName}

import Test.HUnit ((~:), Test)
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (#{moduleName}.scene 1.0)
  ]
