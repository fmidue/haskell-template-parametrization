import CodeWorld
import Prelude hiding (($))

-- Draw a yellow solid circle and a green solid rectangle below it -
-- not behind it!
--
-- As guidance, the result should look roughly like this:
-- https://code.world/run.html?mode=haskell&dhash=DEpssmhzFT22fqxwW2yXJVQ
--
-- Consider the examples from the lecture and also look up how to
-- produce and transform relevant shapes in the CodeWorld
-- documentation.

scene :: Picture
scene = translated 0 6 (colored yellow (solidCircle 1)) & colored green (solidRectangle 20 2)

main :: IO ()
main = drawingOf scene
