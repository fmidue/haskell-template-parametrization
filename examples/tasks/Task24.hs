configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
# - incomplete-uni-patterns # might reveal list patterns
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
# - Eta reduce
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
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
- Use if
- Use init
# - Use isJust
# - Use isNothing
- Use last
- Use left fold instead of right fold
- Use list literal pattern
# - Use null
- Use odd
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Use ||
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
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
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
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
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
import Prelude hiding (($), (!!), (>>=), (=<<), (<*>), head, tail, take, drop, splitAt, truncate, round, ceiling, floor, fromInteger)
import Data.Maybe

-- Recall the visualization of game levels from Task13. This time we
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

-- Hint: see https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
-- for some functions dealing with 'Maybe' values.

main :: IO ()
main = drawingOf (visualize level)
----------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Test (test) where
import qualified Main
import CodeWorld (Picture)
import Test.HUnit ((~:), Test)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "Is visualize fully defined?" ~: isDeeplyDefined (Main.visualize (const Nothing))
  , "aTile =/= undefined?" ~: isDeeplyDefined (Main.aTile Main.Block)
  , "level =/= undefined?" ~: isDeeplyDefined (Main.level (0,0))
  ]

deriving instance Generic Main.Tile
deriving instance NFData Main.Tile
