enableWhitespaceWatermarking = return "True"
moduleName = return "Task21"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
# - missing-signatures # would often show confusing type
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use odd
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use find
- Use floor
- Use foldl
- Use foldr
- Use fromMaybe
- Use infix
# - Use isJust
# - Use isNothing
- Use lefts
- Use list comprehension
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use notElem
# - Use null
- Use or
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
import Prelude hiding (($))

-- Recall that we encode a game level as a function of type
-- (Integer,Integer) -> Integer
--
-- Write a function 'isLessFilledThan' that compares two levels over
-- coordinates ranging from -10 to 10, and determines if the first
-- level is equal to the second level but with possibly some pearls
-- already collected by a player.
--
-- That is, the function returns True exactly if for all coordinate
-- pairs the elements at that position are equal or the first level
-- has an air tile (integer code 4) and the second level has a pearl
-- (integer code 3) there.
--
-- Note that we want reflexivity (x `isLessFilledThan` x) to hold.
--
-- Mathematically speaking, we want to implement a partial order
-- (that sorts levels with the same structure according to their
-- location-aware pearl richness).
--
-- Implement such a function and give it an appropriate type
-- signature.

isLessFilledThan = undefined
---------
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Test (test) where
import qualified #{moduleName}
import TestHelper (qcWithTimeout,qcWithTimeoutAndArgs)
import Test.HUnit (Test, (@=?), (@?), (~:))
import Data.Function (on)
import Test.QuickCheck

test :: [[Test]]
test = [
  [ "Comparing air against pearl"
    ~: True @=? (#{moduleName}.isLessFilledThan `on` value) airLevel pearlLevel
  , "Comparing pearl against air"
    ~: False @=? (#{moduleName}.isLessFilledThan `on` value) pearlLevel airLevel
  , "Comparing air against block"
    ~: False @=? (#{moduleName}.isLessFilledThan `on` value) airLevel blockLevel
  , "Comparing block against air"
    ~: False @=? (#{moduleName}.isLessFilledThan `on` value) blockLevel airLevel
  , "Testing with matching levels:"
    ~: qcHelp (similarLevel (-1) 1) (uncurry (#{moduleName}.isLessFilledThan `on` value))
  , "Testing with different levels:"
    ~: qcHelp (differentLevel (-1) 1) (not . uncurry (#{moduleName}.isLessFilledThan `on` value))
  , "Testing with matching levels:"
    ~: qcHelp (similarLevel 9 10) (uncurry (#{moduleName}.isLessFilledThan `on` value))
  , "Testing with different levels:"
    ~: qcHelp (differentLevel (-5) (-4)) (not . uncurry (#{moduleName}.isLessFilledThan `on` value))
  , "Testing reflexivity (i.e. x `isLessFilledThan` x):"
    ~: qcWithTimeoutAndArgs 50000 testArgs $ forAllShrink @Level @Bool (randomLevel (-1) 1) shrinkLevel (\x -> value x `#{moduleName}.isLessFilledThan` value x)
  ]]

qcHelp :: Gen LevelPair -> ((Level,Level) -> Bool) -> IO ()
qcHelp gen p = qcWithTimeoutAndArgs 50000 testArgs $ forAllShrink gen shrinkLevelPair (p . unpair)

testArgs :: Args
testArgs = stdArgs{maxShrinks = 1, maxSuccess = 200}

airLevel, pearlLevel, blockLevel :: Level
airLevel = Level 0 0 (\case { (0,0) -> 4; _ -> 0} )
pearlLevel = Level 0 0 (\case { (0,0) -> 3; _ -> 0} )
blockLevel = Level 0 0 (\case { (0,0) -> 1; _ -> 0} )

data Level
  = Level Int Int ((Integer, Integer) -> Integer)
  | DiffLevel (Integer,Integer) (Integer -> Integer) Level

value :: Level -> (Integer,Integer) -> Integer
value (Level _ _ f) x = f x
value (DiffLevel p f lvl) x = if p == x then f (value lvl x) else value lvl x

bounds :: Level -> (Int,Int)
bounds (Level l h _) = (l,h)
bounds (DiffLevel _ _ lvl) = bounds lvl

instance Show Level where
  show lvl = "Index-range: " ++ show l ++ " to " ++ show h ++ "\n" ++ unlines [ show (x,y) ++ " -> " ++ show (value lvl (x,y)) | x <- range, y <- range ]
    where range = [fromIntegral l .. fromIntegral h]
          (l,h) =  bounds lvl

newtype LevelPair = LPair { unpair :: (Level,Level) }

instance Show LevelPair where
  show (LPair (l1,l2)) = "Level 1:\n" ++ show l1 ++ "Level 2:\n" ++ show l2

shrinkLevel :: Level -> [Level]
shrinkLevel lvl@(Level l h _) = [shrinkToRange l' h' lvl | (l',h') <- range ]
  where range | l == 0 && h == 0 = []
              | 0 `elem` [l..h] = (0,0) : baseRange
              | otherwise = baseRange
        baseRange = [ (l+1,h) | l+1 <= h ] ++ [ (l,h-1) | h-1 >= l]
shrinkLevel lvl@(DiffLevel (x,y) _ _) = if (l,h) == bounds lvl then [] else [shrinkToRange l h lvl]
  where l = fromInteger $ min x y
        h = fromInteger $ max x y

shrinkToRange :: Int -> Int -> Level -> Level
shrinkToRange l h (Level _ _ f)=
  let f' (x,y) | fromIntegral x `elem` [l..h] && fromIntegral y `elem` [l..h] = f (x,y)
               | otherwise = 0
  in Level l h f'
shrinkToRange l h (DiffLevel p f lvl) = DiffLevel p f $ shrinkToRange l h lvl

-- tailored to usage of DiffLevel (only ever used as 2nd part of LevelPair)
shrinkLevelPair :: LevelPair -> [LevelPair]
shrinkLevelPair (LPair (l1,l2@DiffLevel{})) = [ LPair (shrinkToRange l h l1 ,l2') | l2' <- shrinkLevel l2, let (l,h) = bounds l2']
shrinkLevelPair (LPair (l1,l2)) = [ LPair (l1',shrinkToRange l h l2) | l1'@(Level l h _) <- shrinkLevel l1]

randomLevel :: Int -> Int -> Gen Level
randomLevel low high = fst . unpair <$> relatedLevel low high undefined

similarLevel :: Int -> Int -> Gen LevelPair
similarLevel low high = relatedLevel low high (\x y -> if x == 4 then y else x)

differentLevel :: Int -> Int -> Gen LevelPair
differentLevel low high = do
  let len = high - low + 1
  values1 <- vectorOf (len ^ 2) $ choose (1, 4)
  x <- fromIntegral <$> choose (low,high)
  y <- fromIntegral <$> choose (low,high)
  let level1 = fromList low high values1
      level2 = DiffLevel (x,y) ((`mod` 5).(+2)) level1
  return $ LPair (level1,level2)

relatedLevel :: Int -> Int -> (Integer -> Integer -> Integer) -> Gen LevelPair
relatedLevel low high f = do
  let len = high - low + 1
  values1 <- vectorOf (len ^ 2) $ choose (1, 4)
  values2 <- vectorOf (len ^ 2) $ elements [3, 4]
  let values3 = zipWith f values1 values2
  return $ LPair (fromList low high values1, fromList low high values3)

fromList :: Int -> Int -> [Integer] -> Level
fromList low high values =
  let len = high - low + 1
      func x y
        | x `elem` [low .. high] && y `elem` [low .. high] = values !! ((x - low) * len + (y - low))
        | otherwise                                        = 0
  in  Level low high (\(x, y) -> func (fromIntegral x) (fromIntegral y))
