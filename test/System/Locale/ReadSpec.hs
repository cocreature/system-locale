module System.Locale.ReadSpec
  ( spec
  ) where

import System.Locale.Read
import Test.Hspec

spec :: Spec
spec =
  do describe "getLocale" $
       do it "should return the en_US.UTF-8 locale" $
            getLocale (Just "en_US.UTF-8") `shouldReturn`
            TimeLocale
              [("Sunday","Sun")
              ,("Monday","Mon")
              ,("Tuesday","Tue")
              ,("Wednesday","Wed")
              ,("Thursday","Thu")
              ,("Friday","Fri")
              ,("Saturday","Sat")]
              [("January","Jan")
              ,("February","Feb")
              ,("March","Mar")
              ,("April","Apr")
              ,("May","May")
              ,("June","Jun")
              ,("July","Jul")
              ,("August","Aug")
              ,("September","Sep")
              ,("October","Oct")
              ,("November","Nov")
              ,("December","Dec")]
              ("AM","PM")
              "%a %d %b %Y %r %Z"
              "%m/%d/%Y"
              "%r"
              "%I:%M:%S %p"
              []
          it "should return the de_DE.UTF-8 locale" $
            getLocale (Just "de_DE.UTF-8") `shouldReturn`
            TimeLocale
              [("Sonntag","So")
              ,("Montag","Mo")
              ,("Dienstag","Di")
              ,("Mittwoch","Mi")
              ,("Donnerstag","Do")
              ,("Freitag","Fr")
              ,("Samstag","Sa")]
              [("Januar","Jan")
              ,("Februar","Feb")
              ,("M\228rz","M\228r")
              ,("April","Apr")
              ,("Mai","Mai")
              ,("Juni","Jun")
              ,("Juli","Jul")
              ,("August","Aug")
              ,("September","Sep")
              ,("Oktober","Okt")
              ,("November","Nov")
              ,("Dezember","Dez")]
              ("","")
              "%a %d %b %Y %T %Z"
              "%d.%m.%Y"
              "%T"
              ""
              []
