#!/usr/bin/env stack
{- stack script --nix --resolver lts-15.12
  --nix-packages pcre
  --no-nix-pure
  --package conduit
  --package containers
  --package csv-conduit
  --package non-empty-text
  --package optparse-generic
  --package pcre-heavy
  --package safe
  --package text
  --package time
  --package transformers
  --package turtle
  --package unexceptionalio-trans
  --extra-dep unexceptionalio-0.5.1
  --ghc-options -Wall
-}

-- Use --verbose above for better error messages for library build failures

{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts  #-} -- for pcre-heavy

module Main where

import           Prelude hiding (error)

import           Conduit
import           Control.Monad (forM)
import           Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import           Data.Bifunctor
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit
import           Data.Maybe
import qualified Data.Map.Strict as DM
import qualified Data.NonEmptyText as DTNE
import           Data.List (isSubsequenceOf)
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.IO as DTIO
import           Data.Time.Clock
import           Data.Time.Calendar
import           Options.Generic
import           Safe (lastMay)
import qualified Text.Read as TR
import           Text.Regex.PCRE.Heavy
import           Turtle
import           UnexceptionalIO.Trans (UIO, fromIO, run)

active_cac_entries :: Text
active_cac_entries = "ACTIVE_CAC_ENTRIES"

active_cac_projects :: Text
active_cac_projects = "ACTIVE_CAC_PROJECTS"

billPfx :: Text
billPfx = "Billed"

projPfx :: Text
projPfx = "project"

notProjPfx :: Text
notProjPfx = "unbilled"

unbillPfx :: Text
unbillPfx = "ReportNotBilled"

acctIndent :: Text
acctIndent = "    "

notReportEntry :: Text
notReportEntry = " not:acct:Report"

data ReportOpts = ReportOpts {start :: Text, end :: Text}
  deriving (Generic, Show)
instance ParseRecord ReportOpts

type EitherIO a = ExceptT String UIO a

main :: IO ()
main = (either putStrLn pure) =<< ((run . runExceptT) script)

eToStr :: (Show e, Bifunctor p) => p e a -> p String a
eToStr pea = first show pea

ulift :: IO a -> EitherIO a
ulift ioa = except =<< (eToStr <$> (runExceptT $ fromIO ioa))

printLn :: String -> EitherIO ()
printLn = ulift . putStrLn

script :: EitherIO ()
script = do
  dateNow <- getDateNow
  opts :: ReportOpts <- ulift $ getRecord "hledger time report"
  repRange@(fromDay, untilDay) <- except $ dateRange dateNow (start opts) (end opts)
  journalFileMay <- ulift $ need "LEDGER_FILE"
  journalFile <- pure $ case journalFileMay of
    Just jf -> DT.unpack jf
    Nothing -> "report.journal"
  entryKeys <- getEnvList active_cac_entries
  projs <- getEnvList active_cac_projects
  _ <- except $ entryNameCheck entryKeys
  let entryKeySet = DS.fromList entryKeys
  let projSet = DS.fromList projs
  let unbilledKeySet = entryKeySet `DS.difference` projSet
  _ <- except $ assertOrErr (projSet `DS.isProperSubsetOf` entryKeySet)
    "Project set is not a subset of entry set"
  estDescrMap <- DM.fromList <$>
    ((zip entryKeys) <$> (sequence $ (projectReport repRange) <$> entryKeys))
  let estMap = DM.map fst estDescrMap
  let descrMap = DM.map snd estDescrMap
  projBals :: [Double] <- sequence $ (projectBalance untilDay) <$> entryKeys
  let balMap = DM.fromList $ zip entryKeys projBals
  let billMap = DM.map floor $ DM.restrictKeys balMap projSet
  let unbillMap = DM.restrictKeys balMap unbilledKeySet
  let reportValMap = DM.union (DM.map fromIntegral billMap) unbillMap
  _ <- except $ sequence $ projs <&> (\ek -> do
    valueCheck ek estMap billMap
    )
  linesOut :: [Text] <- pure $ catMaybes $ entryKeys <&> (\ek -> do
    hours <- DM.lookup ek reportValMap
    descr <- DM.lookup ek descrMap
    if hours > 0 then Just $
         (dQ ek) <> ","
      <> (dQ $ tshow hours) <> ","
      <> (dQ descr)
    else Nothing
    )
  _ <- ulift $ forM linesOut DTIO.putStrLn
  billEntriesMay <- except $ sequence $
    (billEntry fromDay reportValMap projSet descrMap) <$> entryKeys
  billEntries <- pure $ catMaybes $ billEntriesMay
  ulift $ DTIO.appendFile journalFile $ "\n\n; Adding billing entry on "
    <> dateToTxt dateNow <> "\n\n"
  _ <- ulift $ forM billEntries (\entry -> DTIO.appendFile journalFile entry)
  pure ()
  where
    valueCheck :: Text -> DM.Map Text Double -> DM.Map Text Int -> Either String ()
    valueCheck eKey m1 m2 = do
      est <- getEst
      bill <- getBill
      assertOrErr (bill < 0 || est <= bill) (msg est bill)
      where
        msg est bill = "weekly hours estimate " <> show est
          <> " is not <= to bill " <> show bill <> " for " <> DT.unpack eKey
        getEst = case DM.lookup eKey m1 of
          Just b -> Right $ floor b
          Nothing -> Left $ "couldn't lookup est for "
            <> DT.unpack eKey <> " in valueNotGE: " <> show m1
        getBill = case DM.lookup eKey m2 of
          Just b -> Right b
          Nothing -> Left $ "couldn't lookup bill for "
            <> DT.unpack eKey <> " in valueNotGE: " <> show m2

absolveUnits :: f () -> ()
absolveUnits _ = ()

-- This doesn't need to be IO, but am using putStrLn for debugging
entryNameCheck :: [Text] -> Either String ()
entryNameCheck eNames = absolveUnits <$> (sequence $ checkPair <$> namePairs)
  where
    lStr = DT.unpack . DT.toLower
    namePairs = [(lStr i, lStr j) | i <- eNames, j <- eNames]
    errmsg ns = "!! " <> fst ns <>  " is a substring of " <> snd ns
    checkPair p = assertOrErr ((fst p == snd p) || (not $ isSubsequenceOf (fst p) (snd p))) (errmsg p)

debugCheck :: [Text] -> [(String, String)]
debugCheck eNames = namePairs
  where
    lStr = DT.unpack . DT.toLower
    namePairs = [(lStr i,lStr j) | i <- eNames, j <- eNames, i /= j]

projectReport :: (Text, Text) -> Text -> EitherIO (Double, Text)
projectReport (fromDay, untilDay) projKey = do
  -- putStrLn $ "<< " <> DT.unpack projKey <> " >>" -- DEBUG
  let regCmd = "hledger print -b " <> fromDay <> " -e " <> untilDay <> notReportEntry
        <> " | hledger register -f- " <> projKey <> " -O csv"
  (_, regOut) <- ulift $ shellStrict regCmd empty
  let regLines = DT.lines regOut
  weeklyEst <- except $ getWeeklyEst regLines
  projNotes :: [Text] <- ulift $ runConduitRes $ sourceLazy (DTL.fromStrict regOut)
    .| intoCSV defCSVSettings .| regProcessor .| sinkList
  let projNotesProcessed = DT.dropWhileEnd (=='.') <$> DT.strip <$> projNotes
  let projNotesUniq = removeTags <$> (DS.toList . DS.fromList) projNotesProcessed
  let projReport =
        let projRep = (DT.intercalate ". " projNotesUniq) in
        if DT.length projRep == 0 then "" else projRep <> "."
  pure (weeklyEst, stripExtraPunct projReport)

projectBalance :: Text -> Text -> EitherIO Double
projectBalance untilDay projKey = do
  balCmd <- pure $
       "hledger print -e " <> untilDay
    <> " | hledger -f- b -N --depth 2 --format \"%(total)\" " <> projKey
  (_, balOut) <- ulift $ shellStrict balCmd empty
  let balLines = DT.strip <$> DT.lines balOut
  let subProjBals = readBal <$> balLines
  projBal <- except $ (eitherReduce sum) subProjBals
  -- putStrLn $ "Balance for " <> DT.unpack projKey  -- DEBUG
  --   <> " is " <> show projBal                     -- DEBUG
  when (projBal < 0) $ printLn $ (DT.unpack projKey)
    <> " has negative balance " <> show projBal
  pure projBal
  where
    readBal :: Text -> Either String Double
    readBal balTxt = case readMay $ stripHour balTxt of
      Just bal -> Right bal
      Nothing -> Left $ "Couldn't read balance from: " <> DT.unpack balTxt
        <> " for project " <> DT.unpack projKey

removeTags :: Text -> Text
removeTags descr = gsub [re|(\s*[A-Za-z0-9_-]+:.*?(,|$))|] ("" :: Text) descr

stripExtraPunct :: Text -> Text
stripExtraPunct descr = gsub [re|(\.+)|] ("." :: Text) descr

-- Processes each description
regProcessor :: Monad m => ConduitT (MapRow Text) Text m ()
regProcessor = mapC (DM.lookup $ DT.pack "description") .| CL.catMaybes
  .| mapC DTNE.fromText .| CL.catMaybes .| mapC DTNE.toText

-- TODO: use this as a sanity check
getWeeklyEst :: [Text] -> Either String Double
getWeeklyEst (_:[]) = Right 0.0
getWeeklyEst (_:lns) = case lastMay lns of
  Just line -> case join $ lastMay <$> DT.split (==',') <$> lastMay lns of
    Just txt ->
      let numTxt = stripHour $ DT.dropAround (=='"') txt in
        case readMay numTxt of
          Just dv -> Right dv
          Nothing -> Left $ "numTxt '" <> DT.unpack numTxt
            <> "' not a Double while in getWeeklyEst on line:"
            <> DT.unpack line
    Nothing -> Left $ "empty string detected for weekly estimate"
  Nothing -> Right 0.0
getWeeklyEst [] = Right 0.0

billEntry :: Text -> DM.Map Text Double -> DS.Set Text -> DM.Map Text Text -> Text
  -> Either String (Maybe Text)
billEntry repDay bMap projSet dMap eKey = do
  bAmnt <- bAmntEi
  billLine <- pure $ acctIndent <> consumePfx <> (DT.replicate defSpaces " ")
    <> tshow bAmnt <> " h"
  projLine <- pure $ acctIndent <> projPfx'' <> (DT.replicate projSpaces " ") <> "-"
    <> tshow bAmnt <> " h"
  pure $ if bAmnt > 0 then Just $
    "\n" <> (DT.intercalate "\n" [dateLine, billLine, projLine]) <> "\n"
    else Nothing
  where
    bAmntEi = case DM.lookup eKey bMap of
      Just bAmt -> Right bAmt
      Nothing -> Left $ "No bill amount for: " <> DT.unpack eKey <> " in billEntry"
    defSpaces = 25
    descr = DM.findWithDefault "" eKey dMap
    dateLine = repDay <> (if DT.length descr > 0 then " " <> descr else "")
    (consumePfx, consumeLen) = getRecInfo
    projPfx' = case DS.member eKey projSet of
      True -> projPfx
      False -> notProjPfx
    projPfx'' = projPfx' <> ":" <> eKey
    projSpaces = max 1 $ consumeLen - (DT.length projPfx'') - 1
    getRecInfo :: (Text, Int)
    getRecInfo = case DS.member eKey projSet of
      True -> (billPfx, defSpaces + (DT.length billPfx))
      False -> (unbillPfx, defSpaces + (DT.length unbillPfx))

stripHour :: Text -> Text
stripHour txt = DT.takeWhile (/=' ') txt

readMay :: Read a => Text -> Maybe a
readMay = TR.readMaybe . DT.unpack

tshow :: Show a => a -> Text
tshow = DT.pack . show

dQ :: Text -> Text
dQ txt = "\"" <> txt <> "\""

assertOrErr :: Bool -> String -> Either String ()
assertOrErr cond msg = if cond then Right () else Left msg

safeIx :: Text -> Int -> Maybe Char
safeIx txt ix =
  if ix < DT.length txt then Just $ DT.index txt ix
  else Nothing

getDateNow :: EitherIO (Integer, Int, Int) -- :: (year,month,day)
getDateNow = ulift $ getCurrentTime >>= return . toGregorian . utctDay

dateToTxt :: (Integer, Int, Int) -> Text
dateToTxt (year,month,day) = 
  tshow year <> "/" <> tshow month <> "/" <> tshow day

dateYear :: (Integer, Int, Int) -> Integer
dateYear (year,_,_) = year

today :: (Integer, Int, Int) -> Text
today (year,month,day) = DT.intercalate "/"
  [tshow year, tshow month, tshow day]

dateRange :: (Integer, Int, Int) -> Text -> Text -> Either String (Text, Text)
dateRange dte fromDay untilDay = do
  fromTxt <- processDate fromDay
  untilTxt <- processDate untilDay
  Right (fromTxt, untilTxt)
  where
    processDate :: Text -> Either String Text
    processDate dTxt = dTxt'
      where
        dTxtLen = partCount dTxt
        dTxt' = if dTxtLen == 2 then Right $ prependYear dTxt
          else if dTxtLen == 3 then Right dTxt
          else Left $ "invalid date format: " <> DT.unpack dTxt
    partCount txt = length $ filter (\dt -> DT.length dt > 0) $ DT.split (=='/') txt
    prependYear :: Text -> Text
    prependYear moDay = (tshow $ dateYear dte) <> "/" <> moDay

getEnvList :: Text -> EitherIO [Text]
getEnvList varName = do
  varMay <- ulift $ need varName
  except $ case varMay of
    Just eks -> Right $ filter (\t -> DT.length t > 0) $ DT.split (==' ') eks
    Nothing -> Left $ DT.unpack varName <> " not set or exported"

eitherReduce :: Traversable t => (t a -> a) -> t (Either String a) -> Either String a
eitherReduce ffun es = (second ffun) . sequence $ es