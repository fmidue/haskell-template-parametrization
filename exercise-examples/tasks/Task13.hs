enableWhitespaceWatermarking = return "True"
moduleName = return "Task13"
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

-- Suppose we want to implement a jump & run game. First off, we
-- should care for level design. In a conceptually tiled world, a
-- level can be thought of as a function assigning to each (x,y)
-- position of the (assumed infinite) screen some number representing
-- what occupies that tile space.
--
-- For example (note that this takes a pair as input, not two
-- individual coordinate values):

level :: (Integer, Integer) -> Integer
level (x, y)
  | abs x > 6 || abs y > 5             = 0  -- outside of the level
  | abs x == 6 || abs y == 5           = 1  -- for a block
  | y < 1 && x >= y && abs (x - 2) > 2 = 1
  | y < -1                             = 2  -- for water
  | abs y > 2 && abs (x + y - 1) > 5   = 3  -- for a pearl
  | x < -4 && y < 2                    = 3
  | otherwise                          = 4  -- for air

-- We want to produce an actual screen drawing from such a purely
-- mathematical level description. Fortunately, our graphics designers
-- have already done some work:

block, water, pearl, air :: Picture

block = colored (light grey) (solidRectangle 1 1)

water = colored blue (solidRectangle 1 1)

pearl = colored purple (solidCircle 0.3) & air

air = colored (translucent blue) (solidRectangle 1 1)

-- But the remaining work rests with us. The first step is to turn
-- number codes 1, 2, ... used above into the corresponding
-- pictures. That should not be too hard, right? But make sure that
-- your implementation of the following function is total, i.e., gives
-- no non-exhaustiveness warning:

aTile :: Integer -> Picture
aTile = undefined

-- Then, we can use that function to produce the overall drawing of
-- the level. Our visible screen has x-coordinates from -10 to 10, and
-- likewise for y. And we should really call the 'aTile' function for
-- all (x,y) combinations in that range, not just some smaller part of
-- the screen, because somebody on the team might still change the
-- 'level' function to cover more of the visible screen with
-- interesting stuff, and then we do not want to produce only a part
-- of the drawing.
--
-- So we would have to produce 21 * 21 = 441 individual calls to cover
-- all (x,y) combinations. Surely you can do this more succinctly?
--
-- If you are thinking of using recursion: don't! We have arts people
-- on our game development team and don't want to blow their mind.

scene :: Picture
scene = undefined

-- Extra: Design your own level by providing a different 'level'
--        function of type (Integer, Integer) -> Integer.

main :: IO ()
main = drawingOf scene
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDefined, isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined #{moduleName}.scene
  , "aTile works on Integers?" ~: isDeeplyDefined (#{moduleName}.aTile (1 :: Integer))
  , "level is still Integer-typed?" ~: isDefined (#{moduleName}.level (0 :: Integer, 0 :: Integer) >= (0 :: Integer))
  ]
