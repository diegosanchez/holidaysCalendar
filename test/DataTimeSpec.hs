module DataTimeSpec (spec) where

import Test.Tasty.Hspec

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate



spec :: Spec
spec = do
  describe "Calendar " $ do
    it "returns day of week" $ do
        toWeekDate (fromGregorian 2018 10 16) `shouldBe` (2018,42,2)

    it "returns day of week and day of week is 7" $ do
        toWeekDate (fromGregorian 2018 10 21) `shouldBe` (2018,42,7)
