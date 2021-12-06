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
- codeworld
- teaching
- monomorphic
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
import Data.Text

-- Extend your tree program from Task04: Draw a never-ending animation
-- of multiple differently sized trees swaying in the wind.
-- Additionally, the color of the leaves should change over time like
-- it does on actual trees over the course of a year. Use the month
-- function below to also indicate the current month.
--
-- For example, the result could look as follows:
-- https://code.world/run.html?mode=haskell&dhash=DnTKIZwvQ9Z27nECizOShjg
--
-- Try to make your solution as modular and reusable as possible.
--
-- Hint: You might want to consider using the transparent color
-- defined below.
--
-- Optional: Add additional seasonal elements to your animation.

main :: IO ()
main = animationOf trees

trees :: Double -> Picture
trees = undefined

-- helpers

transparent :: Color
transparent = RGBA 0 0 0 0

month :: Int -> Picture
month 1 = lettering (pack "January")
month 2 = lettering (pack "February")
month 3 = lettering (pack "March")
month 4 = lettering (pack "April")
month 5 = lettering (pack "May")
month 6 = lettering (pack "June")
month 7 = lettering (pack "July")
month 8 = lettering (pack "August")
month 9 = lettering (pack "September")
month 10 = lettering (pack "October")
month 11 = lettering (pack "November")
month 12 = lettering (pack "December")
month _ = lettering (pack "???")
----------
module Test (test) where
import qualified Main
import Test.HUnit ((~:), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "trees =/= undefined?" ~: isDeeplyDefined (Main.trees 1.0)
  ]
