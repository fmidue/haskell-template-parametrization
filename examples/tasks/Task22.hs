enableWhitespaceWatermarking = return "True"
moduleName = return "Task22"
correct = return "https://code.world/run.html?mode=haskell&dhash=D8vallegOLFOSRlLLYSvTkg"
incorrect = return "https://code.world/run.html?mode=haskell&dhash=Dex05jwjVg9D5ooT7tOybQg"
withScaling = return "https://code.world/run.html?mode=haskell&dhash=DokcKG92ZCcopCeGgOxbEBA"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- missing-signatures
# - name-shadowing # in the interest of easy abstracting over 'primes' and 'assortedColors' in g, allow shadowing
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
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- unused-local-binds

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
# - Use isJust
# - Use isNothing
- Use lefts
- Use lighter
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
import Prelude hiding (($), (!!))

{- Brace yourself, since this is going to take a bit of
 - explaining. The thing is, we have a new customer, and that customer
 - has some strange wishes. But such is life as a software engineer.
 -
 - So this customer wants a certain test chart design, like this:
 - #{correct}
 -
 - The customer's specification mentions prime numbers. Fortunately,
 - we already have a definition of the infinite list of all prime
 - numbers readily at hand, from a previous project of our company:
 -}

-- To convince yourself that this definition is working, you might
-- want to copy it into a different file, load that file into ghci,
-- and run something like 'take 100 primes'.
primes :: [Integer]
primes = sieve [2..]
  where sieve cs =
          let p = head cs
          in [ p ] ++ sieve [ c | c <- tail cs, c `mod` p /= 0 ]

{- What the customer wants to see on screen is: many 5-by-5 squares
 - with different colors, each square offset from the coordinate
 - system's origin by a prime number in the x-direction. How many such
 - squares there should be, and thus how much of the increasing
 - sequence of prime numbers to use, will be given as a parameter. For
 - example, the specific test chart linked to above uses the first 4
 - prime numbers. Due to the relative spacing of prime numbers,
 - specifically since they will often be closer than 5 to each other,
 - some squares will overlap. In that case, the customer wants the
 - squares created for smaller prime numbers to appear behind the ones
 - for larger prime numbers. That is, working with the first 4 prime
 - numbers again, the image should really be the one linked to above,
 - *not* the following one:
 - #{incorrect}
 -
 - Where do we get enough different colors from?
 -
 - Well, fortunately CodeWorld provides exactly what we need, an
 - infinite list 'assortedColors :: [Color]' that we can use.
 -
 - So is that it? Not quite. Since, if we create a larger number of
 - squares this way, the overall image will quickly become larger
 - than the 20x20 coordinate plane of CodeWorld, the customer wants to
 - set an additional parameter for scaling the complete image up or
 - down (mostly down, with factors smaller than 1.0). So the first
 - image linked to above would be the outcome of 'scene 4 1.0', while
 - the following image would be the outcome of 'scene 10 0.3':
 - #{withScaling}
 -}

scene :: Int -> Double -> Picture
scene number factor = undefined

main :: IO ()
main = drawingOf (coordinatePlane & scene 10 0.3)

{- Strange wishes by the customer indeed. But hey, the customer is
 - always king. And this customer is paying well.
 -
 - But wait. Unfortunately, there is not only customer-king, but also
 - technical-boss at our company. And this boss of yours is always
 - forward thinking and re-use oriented. "Maybe we will be able to use
 - part of the functionality here for another project later on", the
 - boss said. "So make things modular and as general as possible."
 -
 - Specifically, the instructions were that you should not simply
 - write a monolithic implementation of the 'scene' function above,
 - but actually decompose it into two functions, say 'f' and 'g', to
 - be used as follows for replacing the 'undefined' above:
 -
 -                      f ... (g ...)
 -
 - where 'f' takes some parameter(s) as well as a call to 'g', which
 - itself also takes some parameter(s). And the aim is that 'g' should
 - be *as polymorphic as possible*. So probably you should stuff
 - everything that has to do only with general list manipulation into
 - 'g' and everything that has to do more specifically with pictures
 - and colors etc. into 'f'.
 -
 - It is an essential part of your task to find a reasonable division
 - of work between 'f' and 'g' such that 'g' has a lot of polymorphism
 - (type variables in its type signature).
 -
 - Writing down the type signatures for your functions is mandatory.
 -
 - It would also be good to come up with some meaningful function
 - names for 'f' and 'g'. (The boss was not of much help in that
 - regard.)
 -
 - Also, do not use the !! operator or re-implementations of it.
 -}
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (#{moduleName}.scene 10 0.3)
  ]
