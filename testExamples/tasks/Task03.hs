ungen_zoom = elements [0.4, 0.5, 0.6]
ungen_x = chooseInt (12,18)
ungen_y = chooseInt (-4,-8)
-----
configGhcErrors:
- deprecation
- empty-enumerations
- identities
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use drop
- Use elem
- Use even
- Use fst
- Use head
- Use id
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
- teaching
- monomorphic
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds
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
# - Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant /=
- Redundant ==
- Redundant if
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
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
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use ||
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
import Prelude hiding (($))
import Data.Text (pack)

-- Draw a rising, moving, then setting sun, as in the sample animation
-- https://code.world/run.html?mode=haskell&dhash=DYBPZOMntsydDgOvvqQNsPQ
--
-- You need not have the exact same positions of the sun at the
-- beginning and end as in our sample animation. For example, the sun
-- can start completely hidden behind the grass line, and can
-- completely disappear behind the grass line at the end. But after
-- having set (Sonnenuntergang) it should stay where it is or
-- completely wink out of existence, not continue to move under the
-- ground or other strange behavior.
--
-- Do not try to "cheat" by letting the sun continue to move under the
-- ground but hiding it behind a white rectangle or anything similar.
--
-- You can work with elementary trigonometry for this task. If you
-- need to refresh your knowledge of trigonometric functions and their
-- connection to circular motion, you may want to have a look at
-- https://en.wikipedia.org/wiki/Unit_circle as well as
-- https://www.geogebra.org/m/Jgt2n9ah
--
-- Of course, you can also work with CodeWorld's 'rotated' function
-- instead.
--
-- In any case, note that angles in CodeWorld are measured in radians,
-- not in degrees.
--
-- Hint: Note that 'scene' is now a function from Double to Picture as
--       opposed to just a Picture in last week's task. This
--       additional parameter, here named t, is the number of seconds
--       elapsed since the animation started. As additional help, the
--       current value for t is displayed by the given template
--       (confirming that the program keeps running).

scene :: Double -> Picture
scene t = undefined

-- Do not change the stuff below here!
sceneWithTime :: Double -> Picture
sceneWithTime t = countTime t & scene t

main :: IO ()
main = animationOf sceneWithTime

countTime :: Double -> Picture
countTime t = dilated #{ungen_zoom} (translated #{ungen_x} (#{ungen_y}) (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))
----------
module Test (test) where
import qualified Main
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Main.scene 1.0)
  ]
