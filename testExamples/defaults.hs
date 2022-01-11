seed = return "Wintersemester"
watermark {
module Snippet (watermark) where

import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)

watermark :: IO String
watermark = return $ unGen ( elements ["course", "lecture"] ) (mkQCGen #{seed}) 0
}
