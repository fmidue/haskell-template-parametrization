enableWhitespaceWatermarking = return "True"
moduleName = return "Task08"
link = return "https://code.world/run.html?mode=haskell&dhash=DsF4psfXjd1h15OlncXT00w"
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
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate

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
- Use foldl
- Use foldr
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

-- The given program implements an animation of a circle that grows
-- over time. Modify it so that the circle switches to shrinking after
-- a certain amount of time and eventually stops changing at all,
-- keeping its size constant from then on.
--
-- The exact sizes the balloon reaches and the rates at which it grows
-- and shrinks are up to you, just try to not let it grow bigger than
-- the screen.
--
-- Additionally, give the circle a different color during each stage
-- (one color while growing, one color while shrinking, one color in
-- the stable state).
--
-- Overall, the result could look as follows:
--
--   #{link}
--
-- Make sure that the animation is smooth as in this example (no jumps).
--
-- And do make sure that your code causes no non-exhaustiveness warnings.

balloon :: Double -> Picture
balloon t = solidCircle (0.5 * t)

main :: IO ()
main = animationOf balloon
----------
module Test (test) where
import qualified #{moduleName}

import Test.HUnit ((~:), Test)
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "balloon =/= undefined?" ~: isDeeplyDefined (#{moduleName}.balloon 1.0)
  ]
