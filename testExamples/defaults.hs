seed = return "Wintersemester"
watermark {
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)

watermark :: IO String
watermark = return $ unGen ( elements ["course", "lecture"] ) (mkQCGen #{seed}) 0
}
