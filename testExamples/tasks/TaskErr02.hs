rdmSelection = withCurrentSeed (shuffle [1,0,1,0,1,0,1,0,1])
bug1 = return $ ["let line = getLine'", "line <- getLine'"]!!(#{rdmSelection}!!0)
bug2 = return $ ["", "num' <- "]!!(#{rdmSelection}!!1)
bug3 = return $ ["num", "num'"]!!(#{rdmSelection}!!1)
bug4 = return $ ["line", "(read line :: Int)"]!!(#{rdmSelection}!!2)
bug5 = return $ ["num", "show num"]!!(#{rdmSelection}!!3)
bug6 = return $ ["", "   "]!!(#{rdmSelection}!!4)
bug7 = return $ ["(c ++ l)", "(c:l)"]!!(#{rdmSelection}!!5)
bug8 = return $ ["x:xs", "(x:xs)"]!!(#{rdmSelection}!!6)
bug9 = return $ ["", "\nisNum [] = True"]!!(#{rdmSelection}!!7)
bug9 = if #{rdmSelection}!!7 == 1 then return "\nisNum [] = True" else withCurrentSeed (elements ["", "\nisNum [] = False"])
bug10 = if #{rdmSelection}!!8 == 1 then return "&& isNum xs" else withSeed (elements ["", "|| isNum xs"]) (#{seed} + 1)
-------
configGhcErrors:
- deprecation
- empty-enumerations
- identities
# - incomplete-patterns # might reveal list patterns
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
- Eta reduce
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
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use otherwise
- Use product
- Use replicate
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
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- unused-local-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
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
- Use lefts
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use newtype instead of data
- Use notElem
- Use or
- Use repeat
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
-------
module Main where
import Control.Monad

main :: IO ()
main = addInput 0

addInput :: Int -> IO ()
addInput num = do
  #{bug1}
  if line /= "end" then do
    #{bug2}if isNum line 
              then return (num + #{bug4}) 
              else do putStrLn "Input is not a number!"
                      return num
    addInput #{bug3}
  else putStrLn $ "Result: " ++ #{bug5}

getLine' :: IO String
getLine' = do c <- getChar
           #{bug6}if c == '\n'
                  then return ""
                  else do l <- getLine'
                          return #{bug7}

isNum :: String -> Bool#{bug9}
isNum #{bug8} = elem x "1234567890" #{bug10}

-- Correct the Tasks above!
