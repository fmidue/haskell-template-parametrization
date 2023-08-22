enableWhitespaceWatermarking = return "True"
moduleName = return "Task04"
otherTask = return "Task02"
link = return "https://code.world/run.html?mode=haskell&dhash=Do2vyh7eJauHPLmL9QI2AHQ"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
- Redundant bracket
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
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
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
# - Eta reduce
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
- Use guards
- Use if
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
- Use ||

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where

import CodeWorld
import Prelude hiding (($))
import Data.Text (pack)

-- Extend your tree from #{otherTask} with a short shaking animation you
-- would see when the leaves/crown get hit by some wind. Only the
-- leaves and branches should move and the motion should stop after a
-- few seconds (but the program keep running). Make sure that there
-- are no apparent jumps in your animation.
--
-- It could look something like this:
--
--   #{link}
--
-- Hint: Note that 'tree' is now a function from Double to Picture as
--       opposed to just a Picture in #{otherTask}. This
--       additional parameter, here named t, is the number of seconds
--       elapsed since the animation started. As additional help, the
--       current value for t is displayed by the given template
--       (confirming that the program keeps running).

tree :: Double -> Picture
tree t = undefined

-- Do not change the stuff below here!

scene :: Double -> Picture
scene t =
  countTime t &
  tree t

main :: IO ()
main = animationOf scene

countTime :: Double -> Picture
countTime t = dilated 0.5 (translated 5 0 (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "tree =/= undefined?" ~: isDeeplyDefined (#{moduleName}.tree 1.0)
  ]
