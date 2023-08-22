enableWhitespaceWatermarking = return "True"
moduleName = return "Task07"
link = return "https://code.world/run.html?mode=haskell&dhash=DSjC7gTCi_bE2Yy7cwjG31g"
fancy1 = return "https://code.world/run.html?mode=haskell&dhash=DIDA_jQC__jKNt9NWimiMkA"
fancy2 = return "https://code.world/run.html?mode=haskell&dhash=DPWdRCwl_iNJcP3NOwceFxg"
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
- Redundant bracket
- Use camelCase
- Use even
- Use foldl
- Use foldr
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
- Use null
- Use odd
- Use pictures
- Use replicate

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
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant if
- Use &&
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
- Use fromMaybe
- Use guards
- Use if
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
- Use repeat
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use ||

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where

import CodeWorld hiding (blank)
import Prelude hiding (($), (!!), null, head, tail, last, init, take, drop, splitAt)
import Data.Text (pack)

-- Complete the following skeleton for a loading screen animation, by
-- implementing the 'circles' and 'partialCircle' functions.
--
-- After you implemented the two functions, the result should look
-- similar to:
-- #{link}
--
-- The 'partialCircle' should result in a rotating circle with two
-- gaps at opposing sides. It takes a five-tuple and the animation
-- time as parameters. The five-tuple consists of the radius and
-- thickness of the circle, the size of the gaps in the circle (with
-- no particular unit, a bigger gap value just means a bigger gap),
-- the speed of the rotation (negative speeds invert the direction of
-- rotation), and the color of the circle.
--
-- The 'circles' function takes a list of parameters for partial
-- circles and should combine them into one animation.
-- (Hint: A list comprehension is useful here.)
--
-- Optional: For a little challenge, try modifing your program to
-- produce an animation like:
-- #{fancy1}
-- or even:
-- #{fancy2}
-- (If you do so, please provide both the basic and the modified
-- version in your submission.)
-- Hint: The difficulty of this modification heavily depends on how
-- you realized movement in the basic version. There is a way to do it
-- such that only very little adjustment is needed for the advanced
-- version.

main :: IO ()
main = animationOf loading

loading :: Double -> Picture
loading t =
  styledLettering Plain SansSerif (pack "LOADING")
  & circles
    [(3,0.2,1.5,2,black)
    ,(4,0.15,3,-0.5,red)
    ,(5,0.1,4.5,0.25,black)
    ] t

circles :: [(Double,Double,Double,Double,Color)] -> Double -> Picture
circles = undefined

partialCircle :: (Double,Double,Double,Double,Color) -> Double -> Picture
partialCircle = undefined
----------
module Test (test) where
import qualified #{moduleName}
import CodeWorld (black)
import Test.HUnit ((~:), Test)

import TestHelper (isDefined, isDeeplyDefined)

test :: [ Test ]
test =
  [ "circles =/= undefined?" ~: isDeeplyDefined (#{moduleName}.circles [] 1.0)
  , "partialCircle =/= undefined?" ~: isDefined (#{moduleName}.partialCircle (3,0.2,1.5,2,black))
  ]
