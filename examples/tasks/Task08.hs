configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
- incomplete-uni-patterns
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
allowModifying: true
allowRemoving: false
configHlintGroups:
- codeworld
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
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
--   https://code.world/run.html?mode=haskell&dhash=DsF4psfXjd1h15OlncXT00w
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
import Test.HUnit ((~:), Test)

import qualified Main
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "balloon =/= undefined?" ~: isDeeplyDefined (Main.balloon 1.0)
  ]
