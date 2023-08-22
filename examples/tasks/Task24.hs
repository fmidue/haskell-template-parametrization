enableWhitespaceWatermarking = return "True"
moduleName = return "Task24"
otherTask = return "Task13"
maybeDocs = return "https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Maybe.html"
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
# - Eta reduce
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
# - Use null
- Use odd
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
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use or
- Use pi
- Use pictures
- Use repeat
- Use replicate
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
import Prelude hiding (($), (!!), (>>=), (=<<), (<*>), head, tail, last, init, take, drop, splitAt, truncate, round, ceiling, floor, fromInteger)
import Data.Maybe

-- Recall the visualization of game levels from #{otherTask}. This time we
-- want to do essentially the same, but use algebraic data types and
-- avoid list comprehensions.

-- A level is now a function from integer pairs to 'Maybe Tile', where
-- 'Nothing' corresponds to no tile being present at that coordinate
-- position (similarly to how we used 0 before).
--
-- In what follows, we use:

data Tile = Block | Water | Pearl | Air

-- as well as the following type synonym for levels, to make type
-- signatures more informative:

type Level = (Integer, Integer) -> Maybe Tile

-- For the compiler there is now no difference whether you write
-- 'Level' or '(Integer, Integer) -> Maybe Tile' somewhere, but it
-- lets us express intended concepts more clearly. In particular, the
-- type of a concrete level can now alternatively be given as
--
--   level :: (Integer, Integer) -> Maybe Tile
--
-- or simply as follows:

level :: Level
level = undefined

-- Replace 'undefined' above by an actual level in the new, Tile-based
-- encoding. You can either use one of the levels from earlier tasks,
-- or still surprise us with a completly new level of your own making.

-- As before, specific pictures for the different tiles are given:

block, water, pearl, air :: Picture

block = colored (light grey) (solidRectangle 1 1)

water = colored blue (solidRectangle 1 1)

pearl = colored purple (solidCircle 0.3) & air

air = colored (translucent blue) (solidRectangle 1 1)

-- The aTile-function has to be adapted to work on the new data type.

aTile :: Tile -> Picture
aTile = undefined

-- Now in order to draw a level in the coordinate range [-10..10] on
-- both dimensions, we want you to implement the (higher-order)
-- function 'visualize', under the following constraints:
--
-- Do not use list comprehensions or recursion. Range expressions
-- like [a .. b] are still allowed.
--
-- (Do also not use do-notation in the list monad, if you happen to
-- know that concept.)

visualize :: Level -> Picture
visualize lev = undefined

-- Hint: see #{maybeDocs}
-- for some functions dealing with 'Maybe' values.

main :: IO ()
main = drawingOf (visualize level)
----------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Test (test) where
import qualified #{moduleName}
import CodeWorld (Picture)
import Test.HUnit ((~:), Test)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "Is visualize fully defined?" ~: isDeeplyDefined (#{moduleName}.visualize (const Nothing))
  , "aTile =/= undefined?" ~: isDeeplyDefined (#{moduleName}.aTile #{moduleName}.Block)
  , "level =/= undefined?" ~: isDeeplyDefined (#{moduleName}.level (0,0))
  ]

deriving instance Generic #{moduleName}.Tile
deriving instance NFData #{moduleName}.Tile
