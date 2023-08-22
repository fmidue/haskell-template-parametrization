enableWhitespaceWatermarking = return "True"
moduleName = return "Task06"
correct = return "https://code.world/run.html?mode=haskell&dhash=DnjQ1h6dp34fkusV07_t77w"
incorrect = return "https://code.world/run.html?mode=haskell&dhash=DC3VbjP5p3C_RFIJc_9i60w"
fancy = return "https://code.world/run.html?mode=haskell&dhash=DLbdDNwLsQlsV_8yC4Ztd5A"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Redundant bracket
- Use camelCase
- Use even
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate

allowAdding: true
allowModifying: true # because of allowing to change the moon-Picture
allowRemoving: false

#{commonConfigHlintGroups}
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
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

-- Produce a never ending animation of the day-and-night cycle, as in:
-- #{correct}
--
-- You have some artistic freedom, but we want a non-full moon that
-- holds steadily upright, as in the above animation, and *not* as in:
-- #{incorrect}
--
-- You may (or may not) also want to prettify the moon itself, by
-- using some other CodeWorld primitives to draw it rather than with
-- our simple calls to 'curve'.
--
-- And maybe during night time you want to show some stars in the sky?
-- As in:
-- #{fancy}
--
-- Do not try to "cheat" by letting sun or moon continue to move under
-- the ground but hiding them behind an artificially placed rectangle
-- or anything similar.

scene :: Double -> Picture
scene t = grass & sunOrMoon t

grass :: Picture
grass = blank

sunOrMoon :: Double -> Picture
sunOrMoon t = blank

sun :: Picture
sun = colored yellow (solidCircle 1)

moon :: Picture
moon = curve [(0,-1),(-0.9,0),(0,1)] & curve [(0,-1),(-0.4,0),(0,1)]

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
