enableWhitespaceWatermarking = return "True"
moduleName = return "Task02"
link = return "https://code.world/run.html?mode=haskell&dhash=DDZ_AqUvWQm3FricOIoNx7A"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
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
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant bracket
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

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where

import CodeWorld
import Prelude hiding (($))

-- Draw a picture of a tree similar to:
--
--   #{link}
--
-- Your tree should at least consist of a trunk, two branches and some
-- leaves/crown.
--
-- You can look up how to produce and transform relevant shapes in the
-- CodeWorld documentation and in the examples from the lecture.

main :: IO ()
main = drawingOf scene

scene :: Picture
scene = undefined
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined #{moduleName}.scene
  ]
