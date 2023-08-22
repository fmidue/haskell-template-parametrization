module #{moduleName} where

import CodeWorld
import Prelude hiding (($), (!!), head, tail, last, init, take, drop, splitAt, truncate, round, ceiling, floor, fromInteger)

-- Look at the following selection of game levels:
--
--   #{link}
--
-- Choose one and write a program that draws it on the screen, using the
-- technique from #{otherTask}.
--
-- Note that, to get a better look, you can zoom into the picture at
-- the above link, as well as move the viewed part around.

level :: (Integer, Integer) -> Integer
level (x, y)
        | abs x > 5 || abs y > 5             = 0  -- outside of the level
        | abs x == 5 || abs y == 5           = 1  -- for a block
        | x <= 0 && y == 1                   = 1
        | x == 0 && y == 2                   = 1
        | x >= 3 && (y <= -1 && y > -3)      = 1
        | x <= 0 && y == 2                   = 2  -- for water
        | x == -4 && y == 4                  = 3  -- for a pearl
        | x >= 3 && y >= 3                   = 3
        | x == 4 && y == -4                  = 3
        | otherwise                          = 4  -- for air

block, water, pearl, air :: Picture

block = colored (light grey) (solidRectangle 1 1)

water = colored blue (solidRectangle 1 1)

pearl = colored purple (solidCircle 0.3) & air

air = colored (translucent blue) (solidRectangle 1 1)

aTile :: Integer -> Picture
aTile 1 = block
aTile 2 = water
aTile 3 = pearl
aTile 4 = air
aTile _ = blank

scene :: Picture
scene = pictures [ translated (fromIntegral x) (fromIntegral y) (aTile (level (x, y))) | x <- [-10..10], y <- [-10..10] ]

main :: IO ()
main = drawingOf scene
