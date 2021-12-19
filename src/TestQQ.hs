{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TestQQ (testQQ) where

import qualified Data.Map as M
import Task ( task, combineToString )

testQQ :: IO ()
testQQ = do
    let x = [task|test = return "TEST!"
test2 = return "TEST! #{test}"
test4 = return "TEST! #{test2}"
test3 = return "TEST! #{test4}"
-----
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
-- documentation. #{test} #{test3}

scene :: Picture
scene = translated 0 6 (colored yellow (solidCircle 1)) & colored green (solidRectangle 20 2)

main :: IO ()
main = drawingOf scene
|]
    res <- combineToString x M.empty
    print res
    print x
