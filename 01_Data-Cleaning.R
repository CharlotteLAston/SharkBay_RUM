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
setwd(working.dir)
source("cleaningFunc.R")

#### Read in Data ####
# After downloading the data from arcCollector, you end up three individual .csv docs for each survey layer; meta, use, avidity. Join the three sets of data, linking them by their GlobalID (primary key) and GUID (foreign key). 

# meta (GlobalID) -> use GUID
# use GlobalID -> avidity GUID

# Before joining dataset make sure none of the linking keys contain NAs. NAs may appear if someone has deleted only one part of the survey eg. deleted the avidity survey but not the related use and meta. 
# This is likely the case for dummys. If they are dummys they should be filtered out on each individual csv before joining. If there are completed surveys that have an NAs in a linking key, use logical detective skills to 
# manually find the associated key. A good starting point is using `anti_join` which can show you what surveys don't marry up, and then using CreationDate to find closest match and go from there. 

# 1. Read in data, including `na.strings = c("", " ")` to make all empty cells consistent
setwd(data_dir)
meta <- read.csv("202109-10_Meta_Ning-SB.csv", na.strings = c("", " "))
use <- read.csv("202109-10_Use_Ning-SB.csv", na.strings = c("", " "))
avidity <- read.csv("202109-10_Avidity_Ning-SB.csv", na.strings = c("", " "))

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
  dplyr::mutate(mDate = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d"))))

use <- use %>%
  dplyr::rename(uObjID = OBJECTID,
                uGlobID = GlobalID,
                uGUID = Guid)%>%
  dplyr::mutate(uGUID = ifelse(str_detect(CreationDate, "9/27/2020 7:08:03 AM"), "706f2244-30f9-48f5-9bf0-9c5ebf9d8766", uGUID)) %>%
  dplyr::filter_all(all_vars(!grepl('ummy', .))) %>% # getting rid of Dummys/dummys
  dplyr::filter_all(all_vars(!grepl("lah", .))) %>% # getting rid of blah blahs
  dplyr::mutate(CreationDate = parse_date_time(CreationDate, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(uDate = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d"))))

avidity <- avidity %>%
  rename(aObjID = OBJECTID,
         aGlobID = GlobalID,
         aGUID = Guid)%>%
  mutate(aGUID = ifelse(str_detect(CreationDate, "9/27/2020 7:53:16 AM"), "7008d806-f10e-49cc-b866-1e3ec6848b2e", aGUID)) %>%
  dplyr::filter_all(all_vars(!grepl('ummy', .))) %>% # getting rid of Dummys/dummys
  dplyr::filter_all(all_vars(!grepl("lah", .))) %>% # getting rid of blah blahs
  dplyr::mutate(CreationDate = parse_date_time(CreationDate, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(aDate = as.Date(as.character(as.Date(substr(CreationDate, 1, 10), "%Y-%m-%d"))))

#### Join Data Together ####
# In a complete survey with 2 uses you would expect a 1:2:1 ratio of meta:use:avidity observations - when theses are joined you would expect this to become two observations with the meta and avidity duplicated. 

# However, looking at number of observations in dfs, there will not be the same number of obs in meta and avidity as as some meta surveys might be refusals so you would not expect there to be an associated avidity survey. Generally you would expect there to be more use obs than meta as one meta survey can have multiple uses, and you would expect less avidity obs than meta, due to the refusals. 

# 5. `full_join` meta and use -> MetaUse (can't use inner join as it will remove the refusals - which we need for response rate calculations)
# 6. `full_join` MetaUse and avidity
# 7. `bind_rows` to stack Jon's data onto newly merged data set


MetaUse <- full_join(meta, use, by = c("mGlobID" = "uGUID"), keep=T) %>% 
  mutate(mObjID = ifelse(mDate < "2018-08-08", NA, mObjID))

Full <- full_join(MetaUse, avidity, by = c("uGlobID" = "aGUID"), keep=T) %>% 
  dplyr::select(-Date)

SBFull <- Full%>%
  rename(Date = "CreationDate.x") %>% 
  dplyr::filter(Date < "2018-08-08" | Date >"2021-09-27" & !str_detect(REQUIRED..Boat.ramp.transect.name, "Exmouth|Bundegi|Tantabiddi|CoralBay"))

#### Selecting Relevant Variables ####

## Renaming and selecting variables from Harrison's survey
SB_H <- SBFull %>% 
  filter(Date < "2018-08-08") %>% 
  mutate(FieldTrip=1) %>% 
  dplyr::rename(Interviewer = "Interviewer_name",
                BR = "Boat_ramp_location", 
                Agreement = "Full_completion_or_partial_or_refusal",
                BRLong = "x.x",
                BRLat = "y.x",
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
                FinalComments = "Comments",
                PersonID = "mObjID")
SB_H <- SB_H %>% 
  dplyr::select(PersonID, Date, FieldTrip, Interviewer, BR, Agreement, BRLong, BRLat, FishingType, BaitLure, exStart, exStop, MaxHook, 
         KeptUndam, nDP, Species, UseLong, UseLat, exnTimes12m, exYrs, Postcode, BoatLength, BoatID, FinalComments)


## Renaming and selectingvariables from my surveys
SB_C <- SBFull %>% 
  filter(Date > "2021-09-27") %>% 
  mutate(FieldTrip=2) %>% 
  dplyr::rename(Interviewer = "REQUIRED..Interviewer",
                BR = "REQUIRED..Boat.ramp.transect.name",
                Screen18 = "SCREEN..18.",
                PrevInter = "SCREEN..have.we.interviewed.you.in.the.last.2.weeks.", 
                Agreement = "REQUIRED..Agreement.to.Participate",
                nDaysInArea = "How.many.days.are.you.in.the.area.",
                nDaysInArea_other = "If.OTHER....days.in.area",
                BoatAccess = "Do.you.have.access.to.a.motorised.boat.while.you.re.here.",
                nBoatDays = "If.YES....days.you.intend.to.spend.on.boat.",
                nBoatDays_other = "If.OTHER....days.on.private.boat.",
                nShoreDays = "X..days.you.intend.to.be.engaged.in.shore.based.activities.",
                nShoreDays_other = "If.OTHER....days.on.shore.",
                nTimesLast12m = "X..separate.times.in.area.in.last.12.months.",
                nTimesLast12m_other = "If.OTHER....times.in.last.12.months",
                nTimes19 = "X..separate.times.in.area.in.2019.",
                nTimes19_other = "If.OTHER....times.in.area.in.2019",
                Covid = "Would.you.be.elsewhere.if.it.weren.t.for.travel.restrictions.",
                BRLong = "x.x",
                BRLat = "y.x",
                EXTRACTIVE = "EXTRACTIVE.SURVEY",
                exRecall = "If.RECALL..date",
                FishingType = "What.type.of.fishing.did.you.do.at.this.site.",
                FishingType_other = "If.OTHER..fishing.type",
                BaitLure = "Did.you.use.a.bait.or.lure.",
                exStart = "What.time.did.you..if.spearfishing...your.lines.get.in.the.water.",
                exStop = "What.time.did.you..if.spearfishing..your.lines.get.out.the.water..",
                MaxHook = "Max.depth.of.hook.at.this.site..m..",
                KeptUndam = "X..fish.you.kept.undamaged.at.this.site.",
                KeptUndam_other = "If.OTHER....kept.undamaged",
                RelUndam = "X..released.undamaged.at.this.site.",
                RelUndam_other = "If.OTHER....released.undamaged",
                nDP = "X..depredated",
                nDP_other = "If.OTHER....depredated",
                Species = "What.species.did.you.catch.", 
                exWhyLeave = "Why.did.you.leave.this.site.",
                exWhyLeave_other = "If.OTHER..why.did.you.leave.this.site.",
                NONEXTRACTIVE = "NON_EXTRACTIVE.SURVEY",
                nexRecall = "If.RECALL..date.1",
                Activity = "What.activity.did.you.do.",
                Activity_other = "If.OTHER..activity",
                nexStart = "What.time.did.you.start.the.activity.",
                nexStop = "What.time.did.you.end.the.activity.",
                DiveMaxDepth = "If.DIVING..max.depth.",
                WhyChoose = "Why.did.you.choose.this.site.",
                WhyChoose_other = "If.OTHER..why.this.site",
                nexWhyLeave = "Why.did.you.leave.this.site..1",
                nexWhyLeave_other = "If.OTHER..why.did.you.leave.this.site..1",
                UseLong = "x.y",
                UseLat = "y.y",
                exnTimes12m = "X..times.fished.in.last.12.months.",
                nexnTimes12m = "X..times.doing.NEX.last.12.months..",
                exYrs = "X..years.fishing.",
                nexYrs = "X..years.doing.NEX.", 
                DiveCert = "Are.you.a.diver..If.so..what.is.your.certification.",
                nDives = "If.DIVING....dives.",
                YrBorn = "What.year.were.you.born.",
                Postcode = "Home.postcode",
                Accom = "If.OTHER..where.are.you.staying",
                Sex = "Male.or.Female",
                Party = "Describe.party", 
                BoatLength = "Boat.length",
                BoatType = "Boat.Type",
                BoatID = "Boat.name.or.number",
                PersonID = "mObjID",
                SiteType = "Site.Type",
                UseComments = "Comments_use",
                FinalComments = "FINISH.SURVEY....Comments",
                nMale = "Number.of.Males",
                nFemale = "Number.of.Females",
                nBoys = "Number.of.boys",
                nGirls = "Number.of.girls"
                ) %>%
  
  dplyr::select(PersonID, Date, Interviewer, FieldTrip, BR, BRLat, BRLong, Site, Screen18,
                PrevInter, Agreement, nDaysInArea, nDaysInArea_other, BoatAccess,
                nBoatDays, nBoatDays_other, nShoreDays, nShoreDays_other, nTimesLast12m,
                nTimesLast12m_other, nTimes19, nTimes19_other, Covid,
                UseLat, UseLong, Activity, Activity_other, exRecall,
                nexRecall, FishingType, FishingType_other, BaitLure,
                exStart, exStop, nexStart, nexStop, MaxHook,
                KeptUndam,KeptUndam_other, RelUndam, RelUndam_other, KeptUndam,
                nDP, nDP_other, Species, exWhyLeave, exWhyLeave_other, nexWhyLeave,
                nexWhyLeave_other, DiveMaxDepth, WhyChoose, WhyChoose_other,exnTimes12m,
                nexnTimes12m, exYrs, nexYrs,
                nDives, DiveCert, Postcode, YrBorn, Sex, nMale, nFemale, nGirls, nBoys,
                Accom, BoatID, BoatType, BoatLength,
                EXTRACTIVE, NONEXTRACTIVE, FinalComments, UseComments)

## Putting the two together
# Have to change some of the types of the columns to make them fit together nicely
SB_H$KeptUndam <- as.integer(SB_H$KeptUndam)
SB_H$nDP <- as.integer(SB_H$nDP)
SB_H$BoatLength <- as.integer(SB_H$BoatLength)

SB_Dat <- bind_rows(SB_H, SB_C)
SB_Dat <- SB_Dat %>% 
  rename(Comments = "FinalComments")

#### Filling in Avidity #####
SB_Dat <- SB_Dat %>%
  dplyr::group_by(PersonID) %>%
  tidyr::fill(exnTimes12m, exYrs, nexYrs, Postcode, YrBorn, Sex, nMale, nFemale, nBoys, nGirls, Accom,
              Accom, BoatID, BoatType, BoatLength) %>%
  dplyr::ungroup()

#### Participant Agreement ####
SB_Dat <- SB_Dat %>%
  mutate(Agreement = ifelse(is.na(Agreement) & is.na(UseLat), "No", Agreement)) %>% 
  mutate(Agreement = ifelse(is.na(Agreement) & !is.na(UseLat), "Yes", Agreement)) %>% 
  mutate(Agreement = ifelse(Agreement == "Full completion", "Yes", Agreement)) %>% 
  mutate(Agreement = ifelse(Agreement == "Partial completion" & is.na(UseLat), "No", Agreement))

#### Temporal Variables #####

# The date/time survey was conducted.
SB_Dat <- SB_Dat %>%
  dplyr::mutate(DateTime = Date) %>%
  # dplyr::mutate(Date = as.Date(ifelse(!is.na(DateTime), as.character(as.Date(substr(DateTime, 1, 10), "%Y-%m-%d")),
  #                                     as.character(as.Date(jDate, "%d/%m/%Y"))), "%Y-%m-%d")) %>%
  dplyr::mutate(numYear = as.numeric(substr(Date, 1, 4))) %>%
  dplyr::mutate(facYear = as.factor(numYear)) %>% # Doing this after filter so 2018 is not a level
  dplyr::mutate(DateTime = with_tz(DateTime, "Australia/Perth")) %>%
  dplyr::mutate(Time = ifelse(!is.na(DateTime), as.character(substr(DateTime, 12, 16)), NA)) %>%
  dplyr::relocate(Date, numYear, facYear, Time, .after = PersonID) %>%
  dplyr::select(-c(DateTime)) %>%
  dplyr::mutate(Date = as.Date(Date))

## Recall date for both extractive and non-extractive activities need to be done at the beginning SBFull in order to make correct TripNum, where a trip is defined as each date associated with a PersonID. 
# Extractive
SB_Dat <- SB_Dat %>%
  dplyr::mutate(DateTime = parse_date_time(exRecall, c("mdY IMS p"))) %>% # making POSIX
  dplyr::mutate(exRecall = as.Date(substr(DateTime, 1, 10), "%Y-%m-%d")) %>%
  dplyr::select(-c(DateTime))

## Trip date
SB_Dat <- SB_Dat %>%
  dplyr::mutate(TripDate = as.Date(ifelse(!is.na(exRecall), as.character(as.Date(exRecall)), exRecall))) %>%
  dplyr::mutate(TripDate = as.Date(ifelse(as.character(as.Date(Date)) == as.character(as.Date(TripDate)), NA, as.character(as.Date(TripDate))))) %>%
  dplyr::mutate(TripDate = as.Date(ifelse(is.na(TripDate), as.character(as.Date(Date)), as.character(as.Date(TripDate))))) %>%
  dplyr::relocate(TripDate, .after = Time) %>%
  dplyr::mutate(TripDate = as.Date(TripDate))

## Binary Recall Column
SB_Dat <- SB_Dat %>%
  dplyr::mutate(Recall = ifelse(TripDate==Date, 0, 1)) %>%
  dplyr::mutate(Recall = as.factor(Recall)) %>%
  dplyr::relocate(Recall, .after = TripDate)

## The start time of extractive and non-extractive activities. 
# Extractive
SB_Dat <- SB_Dat %>%
  mutate(TimeOfDay = ifelse(str_detect(exStart, "pm"), "pm",
                            ifelse(str_detect(exStart, "am"), "am", NA)))%>%
  mutate(exStart = ifelse(str_detect(exStart, "11:45, 16:00"), "11:45", exStart))%>%
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
SB_Dat <- SB_Dat %>%
  mutate(TimeOfDay = ifelse(str_detect(exStop, "pm"), "pm",
                            ifelse(str_detect(exStop, "am"), "am", NA))) %>%
  mutate(exStop = ifelse(str_detect(exStop, "12:00 16:30"), "16:30", exStop))%>% #This is the interview with Wet Patch where they didn't catch anything where they went but went to a trench on the way there and the way back and caught two snapper, that's why the times are weird because they only spent like 15 mins there each time
# Put any specific things in you need to change
  mutate(exStop = gsub("[.]|[:]|[\\]|[a-zA-Z]|[ ]", "", exStop))%>%
  mutate_at(vars(exStop), funs(as.numeric))%>%
  mutate(exStop = ifelse((str_detect(TimeOfDay, "pm") & (exStop<100)), exStop+12, exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==3&str_extract(exStop, "^\\d")<=6,exStop+1200, exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==2, str_c(exStop, "00"), exStop))%>%
  mutate(exStop = ifelse(str_length(exStop)==3, str_c("0", exStop), exStop))%>%
  mutate(exStop=gsub("^([0-9]{2})([0-9]+)$", "\\1:\\2", exStop))%>%
  mutate(exStop=ifelse(str_length(exStop)==4, str_c("0", exStop), exStop))%>%
  dplyr::select(-TimeOfDay)

## Fixing start and stop times in the wrong columns
Times <- SB_Dat[c(91,210,273), c("exStart", "exStop")]
Times <- Times%>%
  relocate(exStart, .after=exStop)

SB_Dat[c(91,210,273), c("exStart", "exStop")] <- Times[1:3, 1:2]

## Start time where there are both extractive and non-extractive activities - probs not really needed 
SB_Dat <- SB_Dat %>%
  mutate(StartTime = ifelse(!is.na(exStart), exStart, nexStart)) %>%
  relocate(StartTime, .before = exStart)

## Note: exMedian Time and number of hours doing activity needs to be done for non-extractive
# There's one record where the median fishing time is 0 hours?
SB_Dat <- SB_Dat %>%
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
SB_Dat <- SB_Dat %>%
  mutate(Month = format(Date,"%B")) %>%
  relocate(Month, .after = Date)


#### Adding IDs to People and Trips ####
# TripNum - a trip is defined as a day, so TripNum is the number of date associated with each person. This should only be over 1 if the person has provided any recall information. 
# SiteNum - the number of site within one trip in chronological order. 

SB_Dat <- SB_Dat %>%
  dplyr::arrange(Date, Time, PersonID) %>%
  dplyr::mutate(ID = row_number()) %>%
  dplyr::arrange(PersonID, TripDate) %>%
  dplyr::mutate(PersonID = ifelse(is.na(PersonID), 0, PersonID)) %>%
  dplyr::mutate(PersonID = ifelse(PersonID == 0, seq(from = (max(PersonID)+1), to = nrow(SB_Dat)), PersonID)) %>%
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

# Cleaning BoatName and Number
SB_Dat <- SB_Dat %>%
  rename(BoatName = "BoatID") %>% 
  dplyr::mutate(BoatName = ifelse(str_detect(BoatName, "[H, h]ire|[R,r]ental|RENTAL"), "Rental", BoatName)) %>%
  mutate(BoatName = str_replace(BoatName, "Firestar 530", "")) %>%
  mutate(BoatName = str_replace(BoatName, "DO176 or", "")) %>% 
  mutate(BoatID = ifelse(str_detect(BoatName, "[a-zA-Z]{2}\\d{3}"), str_extract(BoatName,"(?<=^| )[a-zA-Z]{2}\\d{3}.*?(?=$| )"), NA)) %>% 
  mutate(BoatID = ifelse(str_detect(BoatName, "(?<=^| )[:digit:].*?(?=$| )"), str_extract(BoatName,"(?<=^| )[:digit:].*?(?=$| )"), BoatID)) %>%
  mutate(BoatID = ifelse(str_detect(BoatName, "Ozi"), "62923", BoatID)) %>% 
  mutate(BoatName = ifelse(str_detect(BoatName, "Ozi"), "Ozi I", BoatName)) %>% 
  mutate(BoatID = ifelse(str_detect(BoatName,"^(?!(?:[a-zA-Z]+|[0-9]+)$)[a-zA-Z0-9]+$") & is.na(BoatID), BoatName, BoatID)) %>% 
  mutate(BoatName = str_replace(BoatName, "(?<=^| )[a-zA-Z]{2}\\d{3}.*?(?=$| )", "")) %>% 
  mutate(BoatName = str_replace(BoatName, "(?<=^| )[:digit:].*?(?=$| )", "")) %>% 
  mutate(BoatName = str_replace(BoatName, "^(?!(?:[a-zA-Z]+|[0-9]+)$)[a-zA-Z0-9]+$", "")) %>% 
  mutate(BoatName = str_replace(BoatName, "\\,", ""))%>%
  mutate(BoatName = str_replace(BoatName, "^[:space:]", ""))%>%
  mutate(BoatName = str_replace(BoatName, "[:space:]$", ""))%>%
  mutate(BoatName = ifelse(BoatName != "Oryx II", str_to_title(BoatName), BoatName)) %>% 
  mutate(BoatName = ifelse(BoatName=="Delta", "Delta 057", BoatName)) %>% 
  mutate(BoatID = ifelse(BoatID=="057", NA, BoatID)) %>% 
  mutate(BoatID = ifelse(Comments %in% c("Same guy as 26050"), "26050", BoatID)) %>% 
  mutate(BoatID = toupper(BoatID)) %>%
  mutate(BoatID = ifelse(BoatID=="", NA, BoatID)) %>% 
  mutate(BoatName = ifelse(BoatName=="", NA, BoatName)) %>% 
  relocate(BoatID, .after=BoatName)

## Making sure that people with the same boat ID have the same person ID and filling in the Boat ID/Names where they might be missing 
Refusals <- SB_Dat %>%
  filter(Agreement != "Yes")

SB_Dat <- SB_Dat %>%
  filter(Agreement=="Yes") %>% # Had to filter out the refusals because they were messing things up for some reasons
  group_by(PersonID) %>%
  fill(BoatID, .direction=c("downup")) %>%
  fill(BoatName, .direction=c("downup")) %>%
  ungroup() %>%
  group_by(BoatName) %>% 
  fill(BoatID, .direction=c("downup")) %>% 
  ungroup() %>% 
  group_by(BoatID) %>% 
  fill(BoatName, .direction=c("downup")) %>% 
  ungroup() %>% 
  group_by(BoatName) %>%
  mutate(PersonID = ifelse(anyDuplicated(BoatName) & BoatName != "RENTAL" & !is.na(BoatName), min(PersonID), PersonID)) %>%
  ungroup() %>%
  group_by(BoatID) %>%
  mutate(PersonID = ifelse(anyDuplicated(BoatID) & BoatID != "RENTAL" & !is.na(BoatID), min(PersonID), PersonID)) %>%
  ungroup() %>% 
  rbind(., Refusals) #Put the refusals back into the full data set but they won't have any of the data cleaning 

## Field trip
SB_Dat <- SB_Dat %>%
  dplyr::mutate(FieldTrip = ifelse(Date < "2018-08-08", "1",
                                   FieldTrip)) %>%
  dplyr::mutate(FieldTrip = ifelse(Date > "2021-09-27", "2",
                                   FieldTrip)) %>%
  dplyr::mutate(FieldTrip = as.numeric(FieldTrip))

#### Information about the participants ####

## Remove Charters
SB_Dat <- SB_Dat %>% 
  filter(!grepl("Top gun|charter", Comments))

## Resident/ Non-Resident 
# Participants have been classed as residents if they are have any reason to have a deeper familiarity with the area than visitors, so includes semi residents that perhaps have a house up in Exmouth 
# that they visit multiple times a year. 
# Need to consider what to do about previous resident (eg. some people are not current resident but were resident back in 2019). 
# Also what to do with semi-residents

SB_Dat <- SB_Dat %>%
  dplyr::mutate(Resident = ifelse(str_detect(nDaysInArea, "Resident"), 1, 0)) %>%
  dplyr::mutate(Resident = ifelse(numYear < 2021, NA, Resident)) %>% # no info on whether Jon's are locals or not
  dplyr::mutate(Resident = as.factor(Resident)) %>%
  dplyr::relocate(Resident, .before = nDaysInArea)

## Number of boat days - need to sort out residents 
## Probably don't need to clean this now as we might not even be able to use the column as it wasn't in the ethics and probably isn't super relevant

# SB_Dat <- SB_Dat %>%
#   mutate(nBoatDays = ifelse(nBoatDays == "All", nDaysInArea, nBoatDays)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "30 per year"), 30, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "50"), 50, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "4 month"), 4*12, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "6 Per month"), 6*12, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "Couple weeks once"), 14, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "2 months in total of year"), 2*30, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "1month"), 30, nBoatDays_other)) %>%
#   mutate(nBoatDays_other = ifelse(str_detect(nBoatDays_other, "Month 5"), 12*5, nBoatDays_other)) %>%
#   rename(ResnBoatDaysYr = nBoatDays_other) %>% 
#   mutate(ResnBoatDaysYr = as.numeric(ResnBoatDaysYr)) %>%
#   relocate(ResnBoatDaysYr, .after = Resident)

## Number of days in area
SB_Dat <- SB_Dat %>% 
  mutate(nDaysInArea = ifelse(str_detect(nDaysInArea, "Other")|is.na(nDaysInArea), str_extract(nDaysInArea_other, "(?<=^)[:digit:].*?(?=$)"), nDaysInArea)) %>% 
  mutate(nDaysInArea = ifelse(str_detect(nDaysInArea_other, "Few weeks"), 21, nDaysInArea)) %>% 
  mutate(Comments = ifelse(grepl("Language barrier", nDaysInArea_other), "Language barrier", Comments)) %>% 
  dplyr::select(-nDaysInArea_other)

## Access to a private boat
SB_Dat <- SB_Dat %>%
  mutate(BoatAccess = ifelse(is.na(BoatAccess), "Yes", BoatAccess))

## Number of times visiting Exmouth in past 12 months - probably don't need to clean this right now

## Home postcode
SB_Dat <- SB_Dat %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "Albany"), 6330, Postcode)) %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "Perth"), 6000, Postcode)) %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "Mandurah|Mandurah "), 6210, Postcode)) %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "bindoom|bindoon"), 6502, Postcode)) %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "Traveling|Travelling"), NA, Postcode)) %>%
  mutate(Postcode = ifelse(str_detect(Postcode, "Monkey mia"), 6537, Postcode)) %>% 
  mutate(Postcode = ifelse(str_detect(Postcode, "656h"), 6566, Postcode)) %>% 
  mutate(Postcode = str_replace(Postcode, "(?<=^| )[:alpha:].*?(?=$)", ""))

#### Extractive Activities ####

## Fixing use that have been put in the wrong place

SB_Dat <- SB_Dat %>% 
  # Use Lat
  mutate(UseLat = ifelse(EXTRACTIVE %in% "Bar flat", "-26.022017", UseLat)) %>% 
  mutate(UseLat = ifelse(EXTRACTIVE %in% "Tamala", "-26.459280", UseLat)) %>% 
  mutate(UseLat = ifelse(EXTRACTIVE %in% "Southern channel", "-26.135593", UseLat)) %>% 
  # Use Long
  mutate(UseLong = ifelse(EXTRACTIVE %in% "Bar flat", "113.450316", UseLong)) %>% 
  mutate(UseLong = ifelse(EXTRACTIVE %in% "Tamala", "113.698491", UseLong)) %>% 
  mutate(UseLong = ifelse(EXTRACTIVE %in% "Southern channel", "113.172991", UseLong)) %>% 
  mutate(Comments = ifelse(!is.na(EXTRACTIVE), paste(EXTRACTIVE, Comments, sep = "|"), Comments)) %>%  # Still got the comment from Wet Patch about the trench to deal with 
  dplyr::select(-EXTRACTIVE)

## Fishing type

SB_Dat <- SB_Dat %>% 
  mutate(FishingType = ifelse(FishingType_other %in% c("Crabbed", "Crabbing", "Crabbing "), "Crabbing", FishingType)) %>% 
  mutate(FishingType = ifelse(FishingType_other %in% c("Cockling"), "Cockeling", FishingType)) %>% 
  mutate(FishingType = ifelse(FishingType_other %in% c("Squidding"), "Squidding", FishingType)) %>% 
  mutate(FishingType = ifelse(FishingType_other %in% c("Fly fishing"), "Casting", FishingType)) %>% 
  mutate(FishingType = ifelse(FishingType_other %in% c("Diving for crays"), "Crayfish Diving", FishingType)) %>% 
  mutate(FishingType = ifelse(str_detect(Comments, "Crabbing|crabbing|Crabbings") & is.na(FishingType), "Crabbing", FishingType)) %>% 
  mutate(FishingType = as.factor(FishingType)) %>% 
  dplyr::select(-FishingType_other)

## Bait or Lure

SB_Dat <- SB_Dat %>% 
  mutate(BaitLure = ifelse(BaitLure %in% c("Belly rankin cod, soft plastic lure, black lure", "Jigs, cuttlefish", "Mullet, prawns, squid, gold bomber, plastics", "Soft plastic, squid, occie", 
                                           "Squid, cuttlefish, mullet, lazerpros","Squid, fish, skirts (fast based lures)", "Squid, soft lures", "Soft plastics, minnows, poppers"), "Both", BaitLure)) %>% 
  mutate(BaitLure = ifelse(BaitLure %in% c("Lure", "Lures", "Middle/little jigs", "Nilsmaster lures", "Plastic lure"), "Lures", BaitLure)) %>% 
  mutate(BaitLure = ifelse(str_detect(BaitLure, "squid|Squid|Bait|bait|Chicken|heads|Cuttlefish|occy|Fish|Kangaroo|Herring|Lamb|Mule|Mules|Meulis|Meuli|Muley|Mullet|MUtton|Occie|Pilchards|cuttlefish
                                      Sandbar|Scaleys|gardies|Skirt|chicken|Whiting|Mueli|Mutton|Spleens|trevally"), "Bait", BaitLure)) %>% 
  mutate(BaitLure = as.factor(BaitLure))

## Max Hook Depth
SB_Dat <- SB_Dat %>%
  dplyr::mutate(MaxHook = gsub("[:]|[\\]|[a-zA-Z]|[ ]", "", MaxHook)) %>%
  dplyr::mutate(MaxHook = ifelse(MaxHook == "", NA, MaxHook)) %>%
  dplyr::mutate(MaxHook = as.numeric(MaxHook))

## Number of fish kept undamaged - multiplied up the number of cockles in kg by the average weight of a cockle (10g) 
# For Harry's data, this is just all the fish that they caught 
SB_Dat <- SB_Dat %>%
  mutate(KeptUndam = ifelse(is.na(KeptUndam) & str_detect(KeptUndam_other, "[:digit:]")|KeptUndam==0 & str_detect(KeptUndam_other, "[:digit:]"), KeptUndam_other, KeptUndam)) %>% 
  mutate(KeptUndam = ifelse(KeptUndam_other %in% "Kg", 300, KeptUndam)) %>% 
  mutate(Comments = ifelse(KeptUndam_other %in% c("Kg", "Crabs", "None at second site"), paste(KeptUndam_other, Comments, sep = "|"), Comments)) %>% 
  mutate(KeptUndam = as.numeric(KeptUndam)) %>% 
  dplyr::select(-KeptUndam_other) %>% 
  relocate(KeptUndam, .before="RelUndam")

## Number of fish released undamaged 
SB_Dat <- SB_Dat %>% 
  mutate(RelUndam = ifelse(is.na(RelUndam) & str_detect(RelUndam_other, "[:digit:]")|RelUndam==0 & str_detect(RelUndam_other, "[:digit:]")
                           |str_detect(RelUndam, "Other") & str_detect(RelUndam_other, "[:digit:]"), RelUndam_other, RelUndam)) %>%
  mutate(Comments = ifelse(grepl("[:alnum:]", RelUndam_other), paste(RelUndam_other, Comments, sep = "|"), Comments)) %>% 
  mutate(RelUndam = as.numeric(RelUndam)) %>% 
  dplyr::select(-RelUndam_other)

## Number of fish caught undamaged
SB_Dat <- SB_Dat %>% 
  mutate(KeptUndam = ifelse(is.na(KeptUndam), 0, KeptUndam)) %>% 
  mutate(RelUndam = ifelse(is.na(RelUndam), 0, RelUndam)) %>% 
  mutate(nCaught = ifelse(Date<"2018-08-08", KeptUndam, NA)) %>% 
  mutate(nCaught = ifelse(Date>"2021-09-27", KeptUndam+RelUndam, nCaught)) %>% 
  mutate(nCaught = ifelse(UseComments %in% c("Caught 6 butter fish that was used as bait"), nCaught+6, nCaught)) %>% 
  relocate(nCaught, .after="RelUndam") %>% 
  mutate(nCaught = as.numeric(nCaught))

## Number of fish depredated
SB_Dat <- SB_Dat %>%
  dplyr::mutate(Comments = ifelse(!is.na(nDP_other), paste(nDP_other, Comments, sep ="|"), Comments)) %>%
  dplyr::mutate(nDP = as.numeric(nDP)) %>% 
  dplyr::mutate(nDP = ifelse(is.na(nDP), 0, nDP)) %>% 
  relocate(nDP, .before="nDP_other") %>% 
  dplyr::select(-nDP_other)

## Total number of fish hooked
SB_Dat <- SB_Dat %>% 
  mutate(nHooked = nCaught+nDP) %>% 
  relocate(nHooked, .after="RelUndam") %>% 
  mutate(KeptUndam = ifelse(Date<"2018-08-08", NA, KeptUndam)) %>% 
  mutate(RelUndam = ifelse(Date<"2018-08-08", NA, RelUndam)) %>% 
  mutate(nHooked = as.numeric(nHooked))

## Percentage depredation
SB_Dat <- SB_Dat %>% 
  dplyr::mutate(perDP = (nDP/nHooked)*100, perDP) %>%
  dplyr::mutate(perDP = ifelse(is.nan(perDP), "0", perDP)) %>%
  mutate(perDP = ifelse(nHooked==0, NA, perDP)) %>%  
  dplyr::mutate(perDP = as.numeric(perDP)) %>%
  dplyr::relocate(perDP, .after = nDP) 

# Binary presence or absence of depredation
SB_Dat <- SB_Dat %>%
  mutate(DP = ifelse(nDP > 0, "1", "0")) %>%
  mutate(DP = ifelse(nHooked==0, NA, DP)) %>% 
  relocate(DP, .after = nCaught)

## Rates per hour
# Number of fish caught undamaged per hour
SB_Dat <- SB_Dat %>%
  dplyr::mutate(nUndamHr = nCaught/DecFishingHr) %>%
  dplyr::mutate(nUndamHr = ifelse(is.nan(nUndamHr), "0", nUndamHr)) %>%
  dplyr::mutate(nUndamHr = ifelse(is.infinite(nUndamHr), "0", nUndamHr)) %>%
  dplyr::mutate(nUndamHr = as.numeric(nUndamHr)) %>%
  dplyr::relocate(nUndamHr, .after = perDP)

# Number of fish depredated per hour
SB_Dat <- SB_Dat %>%
  dplyr::mutate(nDPHr = nDP/DecFishingHr) %>%
  dplyr::mutate(nDPHr = ifelse(is.nan(nDPHr), "0", nDPHr)) %>%
  dplyr::mutate(nDPHr = as.numeric(nDPHr)) %>%
  dplyr::relocate(nDPHr, .after = nUndamHr)

# Total fish hooked per hour
SB_Dat <- SB_Dat %>%
  dplyr::mutate(nHookedHr = nHooked/DecFishingHr) %>%
  dplyr::mutate(nHookedHr = ifelse(is.infinite(nHookedHr), "0", nHookedHr)) %>%
  dplyr::mutate(nHookedHr = as.numeric(nHookedHr)) %>%
  dplyr::relocate(nHookedHr, .after = nDPHr)

# Depredation rate per hour
SB_Dat <- SB_Dat %>%
  dplyr::mutate(DPrateHr = nDPHr/nHookedHr) %>%
  dplyr::mutate(DPrateHr = ifelse(is.nan(DPrateHr), "0", DPrateHr)) %>%
  dplyr::mutate(DPrateHr = as.numeric(DPrateHr)) %>%
  dplyr::relocate(DPrateHr, .after = nHookedHr)

## Species
SB_Dat <- SB_Dat %>%
  mutate(Emperors = ifelse(str_detect(Species, "blue lined|blue line|black snapper|spangled|Spangled|spango|Spango|Spangley|spangley|spangly|Grassy|red spot|Long nose|Long tongs|REd emp|Red emperor|Red lipped
                                      |Blue lined emp|Blue lines emp|Emperors|Emporer|slang emperor|norwester|nor west snapper|empowr|Blackie|grass|Spano|blue dots|grassies"), 1, NA)) %>% 
  mutate(Nearshore = ifelse(str_detect(Species, "Whiting|Bonefish|wrasse|grunter|Mullaway|Mullet|Mulloway|wrasses|Trumpeter|Mullie|trumpet|morning"), 1, NA)) %>% 
  mutate(Cods_Trouts_Groupers = ifelse(str_detect(Species, "estuarine cod|Charlie|charlie|rock cod|rockcod|sweetlips|coral|Coral|honeycomb|Honeycomb|honey|Honey|rankin|Coronation|coronation|nanny|Chinaman|chinamen|chinaman
                                                  |cod|Cod|Cud’s|code"), 1, NA)) %>% 
  mutate(Snappers = ifelse(str_detect(Species, "Rims on|Red throat|Flag|flags|Ruby|Goldband|goldband|Goldban|Gold band||Golden band|gold and|Mangrove|mangrove|Jack|jack|Red snapper|Spainish|Striped sea|stripey sera
                                      |fingermark bream|gold stripe|finger|finger markperch"), 1, NA)) %>% 
  mutate(Breams = ifelse(str_detect(Species, "Robinson's|seabream|Seabream|Pink bream|thredfin|threadfin|Tarwine"), 1, NA)) %>% 
  mutate(Perch = ifelse(str_detect(Species, "Pearl perch|Dhufish|dhufosh|pirch|markperch"), 1, NA)) %>% 
  mutate(Pelagic = ifelse(str_detect(Species, "black marlin|sailfish|Marlin|barracude|trevally|Trevally|trivali|trevallu|Cobia|Kobe|Obie|queenie|Queenie|queen|tuna|Yellowfin|mackerel|Mackerel|mackrel|Mackeral|mac,real|Mackey
                                     |Mahi|bludger|Wahoo|Waco|Trailer|Taylor|amberjacks|benido|Mackie|Macki|covier|mackerek|Yellow tail"), 1, NA)) %>% 
  mutate(Flatheads_Cobblers = ifelse(str_detect(Species, "Cobbler|Flathead|Flatty|catfish|Northern flathead|Sandbar flathead"), 1, NA)) %>% 
  mutate(Pigfish_Tuskfish_Parrotfish = ifelse(str_detect(Species, "blue bone|black spot|parrot|vine|Blue bone|Bald|bald|baldchin|Baldchin|Balding"), 1, NA)) %>% 
  mutate(Cephalopods_Crustaceans_Molluscs = ifelse(str_detect(Species, "Squid|squid|Squids|squids|Prawn|prawn|Prawns|prawns|Cockle|swimmer|crab|Manna|swimmers|crabs|manna|calamari|mana|crabs|Cray fish"), 1, NA)) %>% 
  mutate(Others = ifelse(str_detect(Species, "Blowfish|Blowly|northwest|Damsel|Leather jacket|Lizard fish|Shark|Whalers|shark|salmon|Butter|catfish|Gray|puffer|Garfish"), 1, NA)) %>% 
  relocate(Emperors, .after=Species) %>% 
  relocate(Snappers, .after=Emperors) %>% 
  relocate(Breams, .after=Snappers) %>% 
  relocate(Nearshore, .after=Snappers) %>% 
  relocate(Cods_Trouts_Groupers, .after=Breams) %>% 
  relocate(Perch, .after=Cods_Trouts_Groupers) %>% 
  relocate(Pelagic, .after=Perch) %>% 
  relocate(Flatheads_Cobblers, .after=Pelagic) %>% 
  relocate(Pigfish_Tuskfish_Parrotfish, .after=Flatheads_Cobblers) %>% 
  relocate(Cephalopods_Crustaceans_Molluscs, .after=Pigfish_Tuskfish_Parrotfish) %>% 
  relocate(Others, .after=Cephalopods_Crustaceans_Molluscs)

## Reasons for leaving 
SB_Dat <- SB_Dat %>% 
  mutate(exWhyLeave = ifelse(exWhyLeave_other %in% c("Necessity", "Necessity ", "Need to go to work.", "Wife would kill me."), "Necessity", exWhyLeave)) %>% 
  mutate(exWhyLeave = ifelse(exWhyLeave_other %in% c("Bag limit", "Bagged out", "Trip limit"), "FisheriesRegLimit", exWhyLeave)) %>% 
  mutate(exWhyLeave = ifelse(exWhyLeave_other %in% c("Got a feed"), "EndofDay", exWhyLeave)) %>% 
  mutate(exWhyLeave = ifelse(exWhyLeave_other %in% c("Too small", "Want to catch a pink"), "LowCatch", exWhyLeave)) %>% 
  mutate(Comments = ifelse(grepl("[:alnum:]", exWhyLeave_other), paste(exWhyLeave_other, Comments, sep = "|"), Comments)) %>% 
  dplyr::select(-exWhyLeave_other)
  
#### Fisher Demographics #####

## Year born - for some reason one of the dates is coming up at 198
SB_Dat <- SB_Dat %>%
  mutate(YrBorn = as.numeric(YrBorn)) %>%
  mutate(YrBorn=if_else(YrBorn==198, 1981, YrBorn))

## Years Fishing
SB_Dat2 <- SB_Dat %>%
  mutate(FishLife = ifelse(str_detect(exYrs, "Life|All|born|life|50+"), 1, 0))%>%
  mutate(FishLife = ifelse(is.na(FishLife), 0, FishLife))%>%
  mutate(FishOcc = ifelse(str_detect(exYrs, "on and off"), 1, 0))%>%
  mutate(FishOcc = ifelse(is.na(FishOcc), 0, FishOcc))%>%
  mutate(exYrs= ifelse(exYrs %in% c("Life", "All", "born", "life", "50+"), ((2021-YrBorn)+4), exYrs)) %>%  #They probably weren't fishing as newborns so add 4...
  mutate(exYrs = gsub("[a-zA-Z]|[ ]|[-]|[,]|[’]","",exYrs))%>%
  mutate(exYrs = ifelse(exYrs!="", exYrs, NA)) %>%
  relocate(exYrs, FishLife, FishOcc, .after = YrBorn) 


#### Other  Covariates ####

## Consolidate Comments
SB_Dat <- SB_Dat %>% 
  mutate(Comments = ifelse(!is.na(UseComments), paste(UseComments, Comments, sp = "|"), Comments)) %>% 
  dplyr::select(-UseComments)

## Lunar Phase
SB_Dat <- SB_Dat %>%
  mutate(LunarPhase = lunar.phase(Date, name = TRUE)) %>%
  mutate(LunarPhase = as.factor(LunarPhase)) %>%
  mutate(LunarPhase = as.factor(LunarPhase))

## Save Data
setwd(clean_dir)
write.csv(SB_Dat, "Full_Clean_SB.csv", row.names=FALSE)













