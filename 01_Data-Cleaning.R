#### RUM Cleaning Code ####
## Charlotte Aston ##
## 24/09/2021 ##
## Code to clean up RUM variables from survey data ##

#### Packages #####
library(tidyverse)
library(dplyr)
library(lubridate)
library(chron)
library(lunar)
library(sp)
library(raster)
library(ggplot2)
library(leaflet)
library(rgeos)
library(rgdal)
library(sf)
library(ggsn)
library(nngeo)
library(spatstat)

#### #DATA + DIRECTORIES ####
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later
data_dir <- paste(working.dir, "Raw Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")
clean_dir <- paste(working.dir, "Clean Data", sep="/")


#### Functions ####
source(paste(f.dir, "cleaningFunc.R", sep = '/'))

#### Read in Data ####
# After downloading the data from arcCollector, you end up three individual .csv docs for each survey layer; meta, use, avidity. Join the three sets of data, linking them by their GlobalID (primary key) and GUID (foreign key). 

# meta (GlobalID) -> use GUID
# use GlobalID -> avidity GUID

# Before joining dataset make sure none of the linking keys contain NAs. NAs may appear if someone has deleted only one part of the survey eg. deleted the avidity survey but not the related use and meta. 
# This is likely the case for dummys. If they are dummys they should be filtered out on each individual csv before joining. If there are completed surveys that have an NAs in a linking key, use logical detective skills to 
# manually find the associated key. A good starting point is using `anti_join` which can show you what surveys don't marry up, and then using CreationDate to find closest match and go from there. 

# 1. Read in data, including `na.strings = c("", " ")` to make all empty cells consistent

setwd(data_dir)
meta <- read.csv("meta_21.csv", na.strings = c("", " "))
use <- read.csv("use_21.csv", na.strings = c("", " "))
avidity <- read.csv("avidity_21.csv", na.strings = c("", " "))

#### Renaming and removing Dummy Surveys ####
# For each individual dataset: 
# 2. Give ID variables intuitive names
# 3. Manually add any missing linking IDs
# 4. Filter out dummy surveys
# 5. Filter out only data from Carnarvon from in between all the Ningaloo stuff

meta <- meta %>%
  rename(mObjID = OBJECTID,
         mGlobID = GlobalID,
         mGUID = Guid) %>%
  dplyr::filter_all(all_vars(!grepl('ummy', .))) %>% # getting rid of Dummys/dummys
  dplyr::filter_all(all_vars(!grepl("lah", .)))  %>% # getting rid of blah blahs
  dplyr::mutate(CreationDate = parse_date_time(CreationDate, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(tDate = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d")))) %>%
  dplyr::filter(tDate < "2018-08-08") %>% # filtering data to only have Carnarvon - will have to change this when you get new data
  dplyr::select(-c(tDate)) # made a temporary date col to do this as i already have code dealing with date later 

use <- use %>%
  dplyr::rename(uObjID = OBJECTID,
                uGlobID = GlobalID,
                uGUID = Guid)%>%
  dplyr::mutate(uGUID = ifelse(str_detect(CreationDate, "9/27/2020 7:08:03 AM"), "706f2244-30f9-48f5-9bf0-9c5ebf9d8766", uGUID)) %>%
  dplyr::filter_all(all_vars(!grepl('ummy', .))) %>% # getting rid of Dummys/dummys
  dplyr::filter_all(all_vars(!grepl("lah", .))) %>% # getting rid of blah blahs
  dplyr::mutate(CreationDate = parse_date_time(CreationDate, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(Date = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d")))) %>%
  dplyr::filter(Date < "2018-08-08") %>% # filtering data to only have Carnarvon - will have to change this when you get new data
  dplyr::select(-c(Date))


avidity <- avidity %>%
  rename(aObjID = OBJECTID,
         aGlobID = GlobalID,
         aGUID = Guid)%>%
  mutate(aGUID = ifelse(str_detect(CreationDate, "9/27/2020 7:53:16 AM"), "7008d806-f10e-49cc-b866-1e3ec6848b2e", aGUID)) %>%
  dplyr::filter_all(all_vars(!grepl('ummy', .))) %>% # getting rid of Dummys/dummys
  dplyr::filter_all(all_vars(!grepl("lah", .))) %>% # getting rid of blah blahs
  dplyr::mutate(CreationDate = parse_date_time(CreationDate, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(Date = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d")))) %>%
  dplyr::filter(Date < "2018-08-08") %>% # filtering data to only have Carnarvon - will have to change this when you get new data
  dplyr::select(-c(Date))

#### Join Data Together ####
# In a complete survey with 2 uses you would expect a 1:2:1 ratio of meta:use:avidity observations - when theses are joined you would expect this to become two observations with the meta and avidity duplicated. 

# However, looking at number of observations in dfs, there will not be the same number of obs in meta and avidity as as some meta surveys might be refusals so you would not expect there to be an associated avidity survey. Generally you would expect there to be more use obs than meta as one meta survey can have multiple uses, and you would expect less avidity obs than meta, due to the refusals. 

# 5. `full_join` meta and use -> MetaUse (can't use inner join as it will remove the refusals - which we need for response rate calculations)
# 6. `full_join` MetaUse and avidity
# 7. `bind_rows` to stack Jon's data onto newly merged data set

MetaUse <- full_join(meta, use, by = c("mGlobID" = "uGUID"), keep=T) 
SBFull <- full_join(MetaUse, avidity, by = c("uGlobID" = "aGUID"), keep=T) %>%
  rename(nDate = Date) %>% 
  drop_na(nDate) %>%  #Two rows that were all NA about from the object IDs
  select_if(~!all(is.na(.)))

as_tibble(SBFull) 

#### Selecting Relevant Variables ####

## Renaming and selecting variables from Harrison's survey
SB_H <- SBFull %>% 
  mutate(FieldTrip=1) %>% 
  dplyr::rename(Interviewer = "Interviewer_name",
                BR = "Boat_ramp_location", 
                Agreement = "Full_completion_or_partial_or_refusal",
                SurveyLong = "x.x",
                SurveyLat = "y.x",
                FishingType = "What_fishing_method_was_used",
                BaitLure = "What_type_of_bait_or_lure_was_used",
                exStart = "What_time_did_lines_enter_the_water",
                exStop = "What_time_did_lines_leave_the_water",
                MaxHook = "What_was_the_maximum_depth_of_hooks",
                KeptUndam = "How_many_fish_did_you_catch_including_those_kept_and_those_returned",
                nDP = "If_yes_how_many_fish_did_you_lose",
                Species = "What_species_of_fish_did_you_catch",
                UseLong = "x.y",
                UseLat = "y.y",
                exnTimes12m = "How_many_days_have_you_fished_from_a_boat_this_year",
                exYrs = "How_many_years_have_you_been_fishing_for",
                Postcode = "Home_postcode",
                BoatLength = "Boat_length",
                BoatID = "Boat_name_or_number",
                Comments = "Comments",
                PersonID = "mObjID")
SB_H <- SB_H %>% 
  dplyr::select(PersonID, nDate, Interviewer, BR, Agreement, SurveyLong, SurveyLat, FishingType, BaitLure, exStart, exStop, MaxHook, 
         KeptUndam, nDP, Species, UseLong, UseLat, exnTimes12m, exYrs, Postcode, BoatLength, BoatID, Comments)

## Renaming and selection variables from my surveys

## Putting the two together

SB_Full <- SB_H

#### Filling in Avidity #####
Ning <- Ning %>%
  dplyr::group_by(PersonID) %>%
  tidyr::fill(exnTimes12m, exYrs, nexYrs, Postcode, YrBorn, Sex, Party, Accom,
              Accom_other, BoatID, BoatType, BoatLength) %>%
  dplyr::ungroup()

#### Temporal Variables #####

# The date/time survey was conducted.
SB_Full <- SB_Full %>%
  dplyr::mutate(DateTime = ifelse(is.na(nDate), CreationDate.x, nDate)) %>%
  dplyr::mutate(DateTime = parse_date_time(DateTime, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(Date = as.Date(ifelse(!is.na(DateTime), as.character(as.Date(substr(DateTime, 1, 10), "%Y-%m-%d"))))) %>%
  dplyr::mutate(numYear = as.numeric(substr(Date, 1, 4))) %>%
  dplyr::mutate(facYear = as.factor(numYear)) %>% # Doing this after filter so 2018 is not a level
  dplyr::mutate(DateTime = with_tz(DateTime, "Australia/Perth")) %>%
  dplyr::mutate(Time = ifelse(!is.na(DateTime), as.character(substr(DateTime, 12, 16)), as.character(jTime))) %>%
  dplyr::relocate(Date, numYear, facYear, Time, .after = PersonID) %>%
  dplyr::mutate(Date = as.Date(Date))

## Recall date for both extractive and non-extractive activities need to be done at the beginning SBFull in order to make correct TripNum, where a trip is defined as each date associated with a PersonID. 
# Extractive
SB_Full <- SB_Full %>%
  dplyr::mutate(DateTime = parse_date_time(exRecall, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(exRecall = as.Date(substr(DateTime, 1, 10), "%Y-%m-%d")) %>%
  dplyr::select(-c(DateTime))

## Trip date
SB_Full <- SB_Full %>%
  dplyr::mutate(TripDate = as.Date(ifelse(!is.na(exRecall), as.character(as.Date(exRecall))))) %>%
  dplyr::mutate(TripDate = as.Date(ifelse(as.character(as.Date(Date)) == as.character(as.Date(TripDate)), NA, as.character(as.Date(TripDate))))) %>%
  dplyr::mutate(TripDate = as.Date(ifelse(is.na(TripDate), as.character(as.Date(Date)), as.character(as.Date(TripDate))))) %>%
  dplyr::relocate(TripDate, .after = Time) %>%
  dplyr::mutate(TripDate = as.Date(TripDate))

## Binary Recall Column
SB_Full <- SB_Full %>%
  dplyr::mutate(Recall = ifelse(TripDate==Date, 0, 1)) %>%
  dplyr::mutate(Recall = as.factor(Recall)) %>%
  dplyr::relocate(Recall, .after = TripDate)

## The start time of extractive and non-extractive activities. 
# Extractive
SB_Full <- SB_Full %>%
  mutate(exStart = ifelse(str_detect(exStart, "30 min,"), "13:10", exStart))%>%
  mutate(TimeOfDay = ifelse(str_detect(exStart, "pm"), "pm",
                            ifelse(str_detect(exStart, "am"), "am", NA)))%>%
  mutate(exStart = gsub("[.]|[:]|[\\]|[a-zA-Z]|[ ]", "", exStart))%>%
  mutate_at(vars(exStart), funs(as.numeric))%>%
  mutate(exStart = ifelse((str_detect(TimeOfDay, "pm") & (exStart<100)), exStart+12, exStart))%>%
  mutate(exStart=ifelse((str_extract(exStart, "\\d")>1&str_extract(exStart, "\\d")<=9), paste(0,exStart,sep=""), exStart))%>%
# Put in here any specific things you need to change
  mutate(exStart=gsub("^([0-9]{2})([0-9]+)$", "\\1:\\2", exStart))%>%
  mutate(exStart=ifelse(str_length(exStart)==4, str_c("0", exStart), exStart))%>%
  dplyr::select(-TimeOfDay)


## Stop time of extractive 
# Extractive
SB_Full <- SB_Full %>%
  mutate(exStop = ifelse(exWhyLeave_other %in% "While I interviewed them they were still there so I didn’t ask but I saw them leave at 1020", "10:20", exStop)) %>%
  mutate(TimeOfDay = ifelse(str_detect(exStop, "pm"), "pm",
                            ifelse(str_detect(exStop, "am"), "am", NA))) %>%
  mutate(exStop = ifelse(str_detect(exStop, "[S, s]till"), "StillThere", exStop)) %>%
  mutate(StillThere = ifelse(exStop %in% "StillThere", 1, 0)) %>%
  mutate(exStop = ifelse(exStop %in% "StillThere", NA, exStop)) %>%
# Put any specific things in you need to change
  mutate(exStop = gsub("[.]|[:]|[\\]|[a-zA-Z]|[ ]", "", exStop))%>%
  mutate_at(vars(exStop), funs(as.numeric))%>%
  mutate(exStop = ifelse((str_detect(TimeOfDay, "pm") & (exStop<100)), exStop+12, exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==3&str_extract(exStop, "^\\d")<=6,exStop+1200, exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==2, str_c(exStop, "00"), exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==3, str_c("0", exStop), exStop))%>%
  mutate(exStop=gsub("^([0-9]{2})([0-9]+)$", "\\1:\\2", exStop))%>%
  mutate(exStop=ifelse(str_length(exStop)==4, str_c("0", exStop), exStop))%>%
  relocate(StillThere, .after = nexStop) %>%
  dplyr::select(-TimeOfDay)


## Note: exMedian Time and number of hours doing activity needs to be done for non-extractive
SB_Full <- SB_Full %>%
  # Median Fishing Time
  dplyr::mutate(temp_exStart = parse_date_time(exStart, c("HM"))) %>%
  dplyr::mutate(temp_exStop = parse_date_time(exStop, c("HM"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(exMedianTime = median(c(temp_exStart, temp_exStop))) %>%
  dplyr::mutate(exMedianTime = substr(exMedianTime, 12, 16)) %>%
  dplyr::relocate(exMedianTime, .after = exStop) %>%
  dplyr::select(-c(temp_exStart, temp_exStop)) %>%
  dplyr::ungroup() %>%
  # Fishing Hours
  dplyr::mutate(temp_exStart = parse_date_time(exStart, c("HM"))) %>%
  dplyr::mutate(temp_exStop = parse_date_time(exStop, c("HM"))) %>%
  dplyr::mutate(DecFishingHr = difftime(temp_exStop, temp_exStart, units = "hours")) %>%
  dplyr::mutate(DecFishingHr = round(DecFishingHr, digits = 2)) %>%
  dplyr::mutate(DecFishingHr = gsub("[a-zA-Z]|[ ]", "", DecFishingHr)) %>%
  dplyr::mutate(FishingHr = ceiling(as.numeric(DecFishingHr))) %>%
  dplyr::select(-c(temp_exStart, temp_exStop)) %>%
  dplyr::relocate(DecFishingHr, FishingHr, .after = exMedianTime) %>%
  dplyr::mutate(DecFishingHr = as.numeric(DecFishingHr)) %>%
  # DecMedianTime
  dplyr::mutate(exDecMedianTime = hhmm2dec(exMedianTime)) %>% # CAN'T MAKE THIS WORK
  dplyr::mutate(exDecMedianTime = round(exDecMedianTime, digits = 2)) %>%
  dplyr::mutate(exDecMedianTime = as.numeric(exDecMedianTime)) %>%
  dplyr::relocate(exDecMedianTime, .after = exMedianTime)

## Month of trip
SB_Full <- SB_Full %>%
  mutate(Month = format(Date,"%B")) %>%
  relocate(Month, .after = Date)


#### Adding IDs to People and Trips ####
# TripNum - a trip is defined as a day, so TripNum is the number of date associated with each person. This should only be over 1 if the person has provided any recall information. 
# SiteNum - the number of site within one trip in chronological order. 

SB_Full2 <- SB_Full %>%
  dplyr::arrange(Date, Time, PersonID) %>%
  dplyr::mutate(ID = row_number()) %>%
  dplyr::arrange(PersonID, TripDate) %>%
  dplyr::mutate(PersonID = ifelse(is.na(PersonID), 0, PersonID)) %>%
  dplyr::mutate(PersonID = ifelse(PersonID == 0, seq(from = (max(PersonID)+1), to = nrow(SB_Full)), PersonID)) %>%
  dplyr::group_by(PersonID) %>%
  dplyr::arrange(StartTime) %>%
  dplyr::mutate(TripNum = as.numeric(as.factor(TripDate))) %>%
  dplyr::mutate(SiteNum = sequence(rle(TripNum)$lengths)) %>%
  dplyr::arrange(Date, Time, PersonID) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::mutate(PersonID = as.numeric(PersonID)) %>%
  dplyr::mutate(TripNum = as.numeric(TripNum)) %>%
  dplyr::mutate(SiteNum = as.numeric(SiteNum)) %>%
  dplyr::relocate(ID) %>%
  dplyr::relocate(TripNum, SiteNum, .after = PersonID) 
