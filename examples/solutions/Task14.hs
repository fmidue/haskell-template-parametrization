module #{moduleName} where

import CodeWorld
import Data.Text (pack)

-- Extend your tree program from #{otherTask}: Draw a never-ending animation
-- of multiple differently sized trees swaying in the wind.
-- Additionally, the color of the leaves should change over time like
-- it does on actual trees over the course of a year. Use the month
-- function below to also indicate the current month.
--
-- For example, the result could look as follows:
-- #{link}
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
trees t =
  tree t &
  dilated 0.6 (translated (-3) 2.5 (tree t)) &
  dilated 0.6 (translated 4 2 (tree t)) &
  translated 0 (-2) (month (1 + floor t `mod` 12))

tree :: Double -> Picture
tree t =
  translated 0 2 (
    colored brown (
      solidRectangle 0.5 4 &
      translated 0 1.75 (rotated (pi/4 - offset) (translated 0 1 (solidRectangle 0.25 2))) &
      translated 0 1.75 (rotated (-(pi/4 + offset)) (translated 0 1 (solidRectangle 0.25 2)))
      ) &
    translated (0 + offset) 3 (leafs t)
    )
  where offset = 0.2 * sin t

leafColor :: Double -> Color
leafColor t
  | 1 + floor t `mod` 12 `elem` [3..7] = green
  | 1 + floor t `mod` 12 `elem` [8..10] = orange
  | otherwise            = transparent

leafs :: Double -> Picture
leafs t = colored (leafColor t) (solidCircle 2)

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
