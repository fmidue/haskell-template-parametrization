enableWhitespaceWatermarking = return "True"
moduleName = return "Task16"
otherTask = return "Task13"
link = return "https://code.world/run.html?mode=haskell&dhash=DynGTDdNu3OAk0EklMWky6Q"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
# - incomplete-uni-patterns # might reveal list patterns
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
import Prelude hiding (($), (!!), head, tail, last, init, take, drop, splitAt, truncate, round, ceiling, floor, fromInteger)

-- Look at the following selection of game levels:
--
--   #{link}
--
-- Choose one and write a program that draws it on the screen, using the
-- technique from #{otherTask}.
--
-- Note that, to get a better look, you can zoom into the picture at
-- the above link, as well as move the viewed part around.

scene :: Picture
scene = undefined

main :: IO ()
main = drawingOf scene
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined #{moduleName}.scene
  ]
