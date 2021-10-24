{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import GHC.Natural

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show)

nextDay :: Day -> Day
nextDay = \case
 Monday    -> Tuesday
 Tuesday   -> Wednesday
 Wednesday -> Thursday
 Thursday  -> Friday
 Friday    -> Saturday
 Saturday  -> Sunday
 Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays count day =
  let
    helper :: Natural -> Day -> Day
    countMod7 = mod count 7
    helper 0 curDay   = curDay
    helper acc curDay = helper (acc - 1) $ nextDay curDay
   in helper countMod7 day

isWeekend :: Day -> Bool
isWeekend day
  | day == Saturday = True
  | day == Sunday   = True
  | otherwise = False

daysToParty :: Day -> Natural
daysToParty day = helper day 0
  where
    helper :: Day -> Natural -> Natural
    helper Friday acc = acc
    helper curDay acc = helper (nextDay curDay) (acc + 1)
