module #{moduleName} where
import Prelude hiding ((!!), head, take, drop, length)

{- Implement a function 'dropEven' in Haskell which, given a list,
 - removes every second element:
 -}

dropEven :: [a] -> [a]
dropEven (x:_:zs) = x : dropEven zs
dropEven xs = xs
