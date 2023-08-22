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
circles cs t = pictures [ partialCircle c t | c <- cs ]

partialCircle :: (Double,Double,Double,Double,Color) -> Double -> Picture
partialCircle (r,tn,g,s,c) t = colored white (rotated (s*t) gap) & colored c (thickCircle tn r)
  where gap = solidRectangle g (2*(tn+r))

-- advanced version
partialCircle' :: (Double,Double,Double,[Double],Color) -> Double -> Picture
partialCircle' (r,tn,g,sps,c) t = gaps & colored c (thickCircle tn r)
  where gaps = colored white (pictures [rotated (s*t) gap | s <- sps ])
        gap = solidRectangle g (2*(tn+r))

circles' :: [(Double,Double,Double,[Double],Color)] -> Double -> Picture
circles' cs t = pictures [ partialCircle' c t | c <- cs ]
