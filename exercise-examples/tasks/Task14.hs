enableWhitespaceWatermarking = return "True"
moduleName = return "Task14"
otherTask = return "Task04"
link = return "https://code.world/run.html?mode=haskell&dhash=DnTKIZwvQ9Z27nECizOShjg"
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
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use ||

allowAdding: true
allowModifying: false
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
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
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

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where

import CodeWorld
import Data.Text (pack)

-- Extend your tree program from #{otherTask}: Draw a never-ending animation
-- of multiple differently sized trees swaying in the wind.
-- Additionally, the color of the leaves should change over time like
-- it does on actual trees over the course of a year. Use the month
-- function below to also indicate the current month.
--
-- For example, the result could look as follows:
-- #{link}
--
-- Try to make your solution as modular and reusable as possible.
--
-- Hint: You might want to consider using the transparent color
-- defined below.
--
-- Optional: Add additional seasonal elements to your animation.

main :: IO ()
main = animationOf trees

trees :: Double -> Picture
trees = undefined

-- helpers

transparent :: Color
transparent = RGBA 0 0 0 0

month :: Int -> Picture
month 1 = lettering (pack "January")
month 2 = lettering (pack "February")
month 3 = lettering (pack "March")
month 4 = lettering (pack "April")
month 5 = lettering (pack "May")
month 6 = lettering (pack "June")
month 7 = lettering (pack "July")
month 8 = lettering (pack "August")
month 9 = lettering (pack "September")
month 10 = lettering (pack "October")
month 11 = lettering (pack "November")
month 12 = lettering (pack "December")
month _ = lettering (pack "???")
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "trees =/= undefined?" ~: isDeeplyDefined (#{moduleName}.trees 1.0)
  ]
