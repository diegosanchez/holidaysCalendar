module Holidays where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data Holiday = 
    Puntual Integer Int Int | 
    Sunday | 
    RegardlessYear { month :: Int, day :: Int } |
    Since { year :: Integer, month :: Int, day :: Int }
    deriving (Show)
                    

isHoliday:: Holiday -> Day -> Bool

isHoliday (Puntual year month day) date = date == fromGregorian year month day

isHoliday Sunday date = weekDay == 7
    where (_, _, weekDay) = toWeekDate date 

isHoliday RegardlessYear{month = m, day = d} date = m == monthOfDate && d == dayOfDate 
    where
        (_, monthOfDate, dayOfDate) = toGregorian date

isHoliday Since{year = y, month = m, day = d} date = 
    y >= yearOfDate && isHoliday RegardlessYear { month = monthOfDate, day = dayOfDate} date 
    where
        (yearOfDate, monthOfDate, dayOfDate) = toGregorian date

isHoliday None _ = False

isDateHoliday :: Day -> Holiday -> Bool
isDateHoliday d h = isHoliday h d

anyHoliday:: [Holiday] -> Day -> Bool
anyHoliday l date = any (isDateHoliday date) l
