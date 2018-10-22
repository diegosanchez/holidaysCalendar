module Holidays where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data Holiday = Puntual Integer Int Int | Sunday | None 

isHoliday:: Holiday -> Day -> Bool
isHoliday (Puntual year month day) date = date == fromGregorian year month day
isHoliday Sunday date = weekDay == 7
    where (_, _, weekDay) = toWeekDate date 
isHoliday None _ = False
