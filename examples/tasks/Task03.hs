enableWhitespaceWatermarking = return "True"
moduleName = return "Task03"
otherTask = return "Task01"
link = return "https://code.world/run.html?mode=haskell&dhash=DYBPZOMntsydDgOvvqQNsPQ"
wikipedia = return "https://en.wikipedia.org/wiki/Unit_circle"
geogebra = return "https://www.geogebra.org/m/Jgt2n9ah"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
- Redundant bracket
- Use even
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintRules}
- 'fixity: "infixr 0 &"'
- 'hint: {lhs: "3.14", rhs: pi}'
- 'hint: {lhs: "6.28", rhs: 2 * pi, name: Use pi}'

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
# - Eta reduce
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
- Use camelCase
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
import Data.Text (pack)

-- Draw a rising, moving, then setting sun, as in the sample animation
-- #{link}
--
-- You need not have the exact same positions of the sun at the
-- beginning and end as in our sample animation. For example, the sun
-- can start completely hidden behind the grass line, and can
-- completely disappear behind the grass line at the end. But after
-- having set (Sonnenuntergang) it should stay where it is or
-- completely wink out of existence, not continue to move under the
-- ground or other strange behavior.
--
-- Do not try to "cheat" by letting the sun continue to move under the
-- ground but hiding it behind a white rectangle or anything similar.
--
-- You can work with elementary trigonometry for this task. If you
-- need to refresh your knowledge of trigonometric functions and their
-- connection to circular motion, you may want to have a look at
-- #{wikipedia} as well as
-- #{geogebra}
--
-- Of course, you can also work with CodeWorld's 'rotated' function
-- instead.
--
-- In any case, note that angles in CodeWorld are measured in radians,
-- not in degrees.
--
-- Hint: Note that 'scene' is now a function from Double to Picture as
--       opposed to just a Picture in #{otherTask}. This
--       additional parameter, here named t, is the number of seconds
--       elapsed since the animation started. As additional help, the
--       current value for t is displayed by the given template
--       (confirming that the program keeps running).

scene :: Double -> Picture
scene t = undefined

-- Do not change the stuff below here!
sceneWithTime :: Double -> Picture
sceneWithTime t = countTime t & scene t

main :: IO ()
main = animationOf sceneWithTime

countTime :: Double -> Picture
countTime t = dilated 0.5 (translated 15 (-6) (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (#{moduleName}.scene 1.0)
  ]
