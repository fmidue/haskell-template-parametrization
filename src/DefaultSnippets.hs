{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : DefaultSnippets

Contains code snippets that can be added to a task with "Task.addRawVar".
-}

module DefaultSnippets (withSeed, withCurrentSeed) where
import Snippet ( snippet, Snippet )

-- | Contains a function to generate data with a generator and a seed
withSeed :: Snippet
withSeed = [snippet|import Data.List (isPrefixOf, isSuffixOf)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)

withSeed :: Show a => Gen a -> Int -> IO String
withSeed content seed = return $ if isPrefixOf "\"" str && isSuffixOf "\"" str then init (tail str) else str
  where str = show (unGen ( content ) (mkQCGen seed) 5)|]

-- | Contains a function to generate data with a generator and the set seed
withCurrentSeed :: Snippet
withCurrentSeed = [snippet|import Data.List (isPrefixOf, isSuffixOf)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)

withCurrentSeed :: Show a => Gen a -> IO String
withCurrentSeed content = return $ if isPrefixOf "\"" str && isSuffixOf "\"" str then init (tail str) else str
  where str = show (unGen ( content ) (mkQCGen #{seed}) 5)
|]
