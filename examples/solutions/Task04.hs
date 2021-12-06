import CodeWorld
import Prelude hiding (($))
import Data.Text (pack)

-- Extend your tree from last week with a short shaking animation you
-- would see when the leaves/crown get hit by some wind. Only the
-- leaves and branches should move and the motion should stop after a
-- few seconds (but the program keep running). Make sure that there
-- are no apparent jumps in your animation.
--
-- It could look something like this:
--
--   https://code.world/run.html?mode=haskell&dhash=Do2vyh7eJauHPLmL9QI2AHQ
--
-- Hint: Note that 'tree' is now a function from Double to Picture as
--       opposed to just a Picture in last week's task. This
--       additional parameter, here named t, is the number of seconds
--       elapsed since the animation started. As additional help, the
--       current value for t is displayed by the given template
--       (confirming that the program keeps running).

tree :: Double -> Picture
tree t =
  translated 0 2 (
    colored brown (
      solidRectangle 0.5 4 &
      translated 0 1.75 (rotated (pi/4 - offset) (translated 0 1 (solidRectangle 0.25 2))) &
      translated 0 1.75 (rotated (-(pi/4 + offset)) (translated 0 1 (solidRectangle 0.25 2)))
      ) &
    translated offset 3 (colored green (solidCircle 2))
    )
  where offset = if t < (3/2 * pi) then 0.3 * sin (2 * t) else 0

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
