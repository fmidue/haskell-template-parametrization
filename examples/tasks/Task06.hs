configGhcErrors:
- deprecation
- empty-enumerations
- identities
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
- Use camelCase
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
allowModifying: true # because of allowing to change the moon-Picture
allowRemoving: false
configHlintGroups:
- codeworld
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
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
- Eta reduce
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

-- Produce a never ending animation of the day-and-night cycle, as in:
-- https://code.world/run.html?mode=haskell&dhash=DnjQ1h6dp34fkusV07_t77w
--
-- You have some artistic freedom, but we want a non-full moon that
-- holds steadily upright, as in the above animation, and *not* as in:
-- https://code.world/run.html?mode=haskell&dhash=DC3VbjP5p3C_RFIJc_9i60w
--
-- You may (or may not) also want to prettify the moon itself, by
-- using some other CodeWorld primitives to draw it rather than with
-- our simple calls to 'curve'.
--
-- And maybe during night time you want to show some stars in the sky?
-- As in:
-- https://code.world/run.html?mode=haskell&dhash=DLbdDNwLsQlsV_8yC4Ztd5A
--
-- Do not try to "cheat" by letting sun or moon continue to move under
-- the ground but hiding them behind an artificially placed rectangle
-- or anything similar.

scene :: Double -> Picture
scene t = grass & sunOrMoon t

grass :: Picture
grass = blank

sunOrMoon :: Double -> Picture
sunOrMoon t = blank

sun :: Picture
sun = colored yellow (solidCircle 1)

moon :: Picture
moon = curve [(0,-1),(-0.9,0),(0,1)] & curve [(0,-1),(-0.4,0),(0,1)]

main :: IO ()
main = animationOf scene
----------
module Test (test) where
import Test.HUnit ((~:), Test)

import qualified Main
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Main.scene 1.0)
  ]
