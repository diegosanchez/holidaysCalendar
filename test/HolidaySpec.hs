module HolidaySpec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Control.Exception (evaluate)

import Data.Time.Calendar

import Holidays

spec :: Spec
spec = do
  describe "Holiday Calendar" $ do
    it "returns true whether puntual holiday is specified" $ do
        isHoliday (Puntual 2018 10 16) (fromGregorian 2018 10 16) `shouldBe` True
    it "returns false whether there not holidays at all" $ do
        anyHoliday [] (fromGregorian 2018 10 16) `shouldBe` False
    it "returns false whether there not holidays at all #2" $ do
        anyHoliday [] (fromGregorian 2018 10 16) `shouldBe` False
    it "returns true whether day of week is Sunday" $ do
        isHoliday (Sunday) (fromGregorian 2018 10 21) `shouldBe` True
    it "returns false whether day of week is not Sunday" $ do
        isHoliday (Sunday) (fromGregorian 2018 10 20) `shouldBe` False
    it "returns true for Chrismas regardless year" $ do
        isHoliday (RegardlessYear { month = 12, day = 25 }) (fromGregorian 2018 12 25) `shouldBe` True
    it "returns true for Chrismas regardless year" $ do
        isHoliday (RegardlessYear { month = 12, day = 25 }) (fromGregorian 2018 12 25) `shouldBe` True
    it "returns false before '83" $ do
        isHoliday (Since { year = 1983, month = 4, day = 2 }) (fromGregorian 1903 4 2) `shouldBe` True
    it "returns true since '83" $ do
        isHoliday (Since { year = 1983, month = 4, day = 2 }) (fromGregorian 1983 4 2) `shouldBe` True
    it "returns false because there aren't holidays at all" $ do 
        let calendar = []
        anyHoliday calendar (fromGregorian 2018 10 22) `shouldBe` False
    it "returns true because there is sunday on calendar" $ do 
        let calendar = [Sunday]
        anyHoliday calendar (fromGregorian 2018 10 21) `shouldBe` True
    it "returns false because there are Since and RegardlessYear holidays but not Sunday" $ do 
        let calendar = [ Since { year = 1983, month = 4, day = 2 } , RegardlessYear { month = 12, day = 25 } ]
        anyHoliday calendar (fromGregorian 2018 10 21) `shouldBe` False
    it "returns true since '83 even with list of holidays" $ do
        anyHoliday calendar (fromGregorian 1983 4 2) `shouldBe` True
        where calendar = [ Since { year = 1983, month = 4, day = 2 }, RegardlessYear { month = 12, day = 25 } ]
