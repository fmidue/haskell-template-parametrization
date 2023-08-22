enableWhitespaceWatermarking = return "True"
moduleName = return "Task20"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- missing-signatures
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

allowAdding: false
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
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
# - Use concatMap # would violate the template
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
{-# LANGUAGE TupleSections #-}
module #{moduleName} where
import Prelude hiding (($))
import Test.QuickCheck

{- A lecture slide claims that every list comprehension can be
 - expressed via map, filter, and concat instead. Let us try to
 - exemplify this.
 -
 - Here is a concrete list comprehension:
 -}

original :: [ (Integer, Integer) ]
original = [ (x,y) | x <- [-50..50], y <- [x..50], abs (x + y) > 35 ]

{- Your task is to implement the same without using list
 - comprehensions (though range expressions are still allowed).
 -
 - A rough idea is already given here. Complete that definition
 - by replacing the occurrences of 'undefined' appropriately.
 -
 - Do not use any additional top-level or local (let, where)
 - definitions.
 -}

alternative :: [ (Integer, Integer) ]
alternative = concat (map (\x -> map undefined (filter undefined [undefined .. 50])) [undefined .. 50])

{- By executing 'main' below, you can partially test your solution
 - before uploading it.
 -}

main :: IO ()
main = do putStrLn "Checking prefixes:"
          quickCheck
           (
            forAllShrink (growingElements [1 .. length original]) shrink
              (\n -> take n original == take n alternative)
           )
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test = [ " original == alternative" ~:
         (#{moduleName}.original == #{moduleName}.alternative) @?= True
       ]
