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
        isHoliday (None) (fromGregorian 2018 10 16) `shouldBe` False
    it "returns false whether there not holidays at all #2" $ do
        isHoliday (None) (fromGregorian 2018 10 16) `shouldBe` False
    it "returns true whether day of week is Sunday" $ do
        isHoliday (Sunday) (fromGregorian 2018 10 21) `shouldBe` True
    it "returns false whether day of week is not Sunday" $ do
        isHoliday (Sunday) (fromGregorian 2018 10 20) `shouldBe` False
