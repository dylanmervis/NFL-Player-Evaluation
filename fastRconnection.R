
## READ IN AND CLEAN ENTIRE DATASET
## DO NOT CLEAN EACH TIME & REBUILD WEEKLY 

library(dplyr)
library(DBI)
library(RSQLite)
library(nflfastR)
library(readr)
library(xml2)
library(XML)
library(rvest)
library(purrr)
library(na.tools)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(tidymodels)
library(lubridate)
library(gsisdecoder)

source("/Users/dylanmervis/Desktop/2019 NFL/R Scripts/Additional_Functions.R")

future::plan("multisession")

update_db(
  dbdir = "/Users/dylanmervis/Google Drive",
  dbname = "pbp_db",
  tblname = "pbp_fastr",
  force_rebuild = FALSE
)

pbp_db <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "/Users/dylanmervis/Google Drive/pbp_db"), "pbp_fastr")


pbp_fastr <- as.data.frame(pbp_db) %>%
  filter(season_type == "REG", season == 2021) %>%
  Clean_pbp()


## READ IN SEPARATE ROSTER DATA & CREATE NAME ABBREVIATION

all_roster <- nflreadr::load_rosters(1999:2021)

all_roster <- all_roster %>% 
  mutate(Abbr = paste(substr(full_name, start = 1, stop = 1), last_name, sep = ".")) %>%
  mutate(Month = 1,
         Day = 1) %>%
  mutate(Age_Date = make_date(season, Day, Month)) %>%
  mutate(Age = as.period(interval(start = birth_date, end = Age_Date))$year) %>%
  dplyr::select(Season = season, Full_Name = full_name, Abbr, Position = position, Team = team, 
                Depth_Chart = depth_chart_position, Jersey = jersey_number, Birth_Date = birth_date,
                Age, College = college, Years_Exp = years_exp, gsis_id, pff_id, sportradar_id, headshot_url) %>%
  mutate(Team = case_when(Team == "STL" ~ "LA",
                          Team == "OAK" ~ "LV",
                          Team == "SD" ~ "LAC",
                          TRUE ~ Team)) %>%
  write_rds("/Users/dylanmervis/Google Drive/1999-2021_roster_raw.rds")

## CREATE INDIVIDUAL DATASETS BY POSITION TO JOIN WITH PBP DATA
## NFL REMOVED GSIS_IDS IN 2020 SO EDIT PLAYER ABBREVIATIONS

qb_roster <- all_roster %>%
  filter(Position == "QB") %>%
  mutate(Abbr = case_when(Abbr == "A.Smith" & Full_Name == "Akili Smith" ~ "Ak.Smith",
                          Abbr == "C.Bramlet" & Full_Name == "Corey Bramlet" ~ "Co.Bramlet",
                          Abbr == "D.Anderson" & Full_Name == "Drew Anderson" ~ "Dr.Anderson",
                          Abbr == "D.Carr" & Full_Name == "David Carr" ~ "Da.Carr",
                          Abbr == "J.Allen" & Full_Name == "Jared Allen" ~ "Ja.Allen",
                          Abbr == "J.Booty" & Full_Name == "Josh Booty" ~ "Jos.Booty",
                          Abbr == "J.Johnson" & Full_Name == "Jerrod Johnson" ~ "Je.Johnson",
                          Abbr == "J.Palmer" & Full_Name == "Jesse Palmer" ~ "Je.Palmer",
                          Abbr == "R.Griffin" & Full_Name == "Ryan Griffin" ~ "Ry.Griffin",
                          Abbr == "T.Jackson" & Full_Name == "Tyree Jackson" ~ "Ty.Jackson",
                          Abbr == "T.Hill" & Full_Name == "Taysom Hill" ~ "Ta.Hill",
                          Abbr == "D.Brown" & Full_Name == "Dave Brown" ~ "Da.Brown",
                          Abbr == "J.Miller" & Full_Name == "Jim Miller" ~ "Ji.Miller",
                          Abbr == "D.Johnson" & Full_Name == "Doug Johnson" ~ "Do.Johnson",
                          Abbr == "T.Brown" & Full_Name == "Travis Brown" ~ "Tr.Brown",
                          Abbr == "M.Moore" & Full_Name == "Matt Moore" ~ "Ma.Moore",
                          Abbr == "R.Smith" & Full_Name == "Rusty Smith" ~ "Ru.Smith",
                          Abbr == "D.Davis" & Full_Name == "Dominique Davis" ~ "Do.Davis",
                          Abbr == "S.Morris" & Full_Name == "Stephen Morris" ~ "St.Morris",
                          Abbr == "C.Jones" & Full_Name == "Cardale Jones" ~ "Ca.Jones",
                          Abbr == "K.Allen" & Full_Name == "Kyle Allen" ~ "Ky.Allen",
                          Abbr == "D.Jones" & Full_Name == "Daniel Jones" ~ "Da.Jones",
                          Abbr == "J.Love" & Full_Name == "Josh Love" ~ "Jo.Love",
                          TRUE ~ Abbr)) 


skill_roster <- all_roster %>%
  filter(Position %in% c("QB", "RB", "HB", "FB", "WR", "TE")) %>%
  filter(!is.na(Team)) %>%
  mutate(Position = case_when(Position == "HB" ~ "RB",
                              TRUE ~ Position)) %>%
  mutate(Full_Name = case_when(Full_Name == "Aaron Brown" & gsis_id == "00-0024629" ~ "Aaron Brown_1",
                               Full_Name == "Ricky Williams" & gsis_id == "00-0020739" ~ "Ricky Williams_1",
                               Full_Name == "Steven Jackson" & gsis_id == "00-0023852" ~ "Steven Jackson_1",
                               Full_Name == "Josh Davis" & gsis_id == "00-0023160" ~ "Josh_Davis_1",
                               Full_Name == "Adrian Peterson" & gsis_id == "00-0021306" ~ "Adrian Peterson_1",
                               Full_Name == "Steve Smith" & gsis_id == "00-0025438" ~ "Steve Smith_1",
                               Full_Name == "Chris Davis" & gsis_id == "00-0022375" ~ "Chris Davis_1",
                               Full_Name == "Chris Davis" & gsis_id == "00-0025515" ~ "Chris Davis_2",
                               Full_Name == "Chris Henry" & gsis_id == "00-0023518" ~ "Chris Henry_1",
                               Full_Name == "Chris Brown" & gsis_id == "00-0026434" ~ "Chris Brown_1",
                               Full_Name == "Chris Brown" & gsis_id == "00-0022092" ~ "Chris Brown_2",
                               Full_Name == "Zach Miller" & gsis_id == "00-0027125" ~ "Zach Miller_1",
                               Full_Name == "Tim Brown" & gsis_id == "00-0027260" ~ "Tim Brown_1",
                               Full_Name == "Antonio Brown" & gsis_id == "00-0021425" ~ "Antonio Brown_1",
                               Full_Name == "Mike Williams" & gsis_id == "00-0023452" ~ "Mike Williams_1",
                               Full_Name == "Mike Williams" & gsis_id == "00-0027702" ~ "Mike Williams_2",
                               Full_Name == "Joe Horn" & gsis_id == "00-0035601" ~ "Joe Horn_1",
                               Full_Name == "Joe Horn" & gsis_id == "00-0028648" ~ "Joe Horn_2",
                               Full_Name == "Chris Givens" & gsis_id == "00-0029169" ~ "Chris Givens_1",
                               Full_Name == "Brandon Williams" & gsis_id == "00-0024299" ~ "Brandon Williams_1",
                               Full_Name == "Michael Smith" & gsis_id == "00-0029145" ~ "Michael Smith_1",
                               Full_Name == "Corey Brown" & gsis_id == "00-0020215" ~ "Corey Brown_1",
                               Full_Name == "Karl Williams" & gsis_id == "00-0017847" ~ "Karl Williams_1",
                               Full_Name == "Kevin Smith" & gsis_id == "00-0026204" ~ "Kevin Smith_1",
                               Full_Name == "Jerry Rice" & gsis_id == "00-0031429" ~ "Jerry Rice_1",
                               Full_Name == "Ryan Grant" & gsis_id == "00-0022984" ~ "Ryan Grant_1",
                               Full_Name == "David Johnson" & gsis_id == "00-0026957" ~ "David Johnson_1",
                               Full_Name == "Michael Bennett" & gsis_id == "00-0032312" ~ "Michael Bennett_1",
                               Full_Name == "Rod Smith" & gsis_id == "00-0015290" ~ "Rod Smith_1",
                               Full_Name == "Deon Butler" & gsis_id == "00-0032313" ~ "Deon Butler_1",
                               Full_Name == "Chris Harper" & gsis_id == "00-0030077" ~ "Chris Harper_1",
                               Full_Name == "Mike Davis" & gsis_id == "00-0031160" ~ "Mike Davis_1",
                               Full_Name == "Andre Davis" & gsis_id == "00-0031635" ~ "Andre Davis_1",
                               Full_Name == "Matt Jones" & gsis_id == "00-0023456" ~ "Matt Jones_1",
                               Full_Name == "Jonathan Williams" & gsis_id == "00-0032975" ~ "Jonathan Williams_1",
                               Full_Name == "Mike Thomas" & gsis_id == "00-0027076" ~ "Mike Thomas_1",
                               Full_Name == "Reggie Davis" & gsis_id == "00-0033815" ~ "Reggie Davis_1",
                               Full_Name == "Chris Thompson" & gsis_id == "00-0033855" ~ "Chris Thompson_1",
                               Full_Name == "Travis Wilson" & gsis_id == "00-0033208" ~ "Travis Wilson_1",
                               Full_Name == "B.J. Johnson" & gsis_id == "00-0033496" ~ "B.J. Johnson_1",
                               Full_Name == "Anthony Miller" & gsis_id == "00-0028889" ~ "Anthony Miller_1",
                               Full_Name == "Malcolm Johnson" & gsis_id == "00-0032068" ~ "Malcolm Johnson_1",
                               Full_Name == "James Butler" & gsis_id == "00-0034870" ~ "James Butler_1",
                               Full_Name == "Chris Warren" & gsis_id == "00-0017209" ~ "Chris Warren_1",
                               Full_Name == "Justin Watson" & gsis_id == "00-0017303" ~ "Justin Watson_1",
                               Full_Name == "Anthony Johnson" & gsis_id == "00-0035474" ~ "Anthony Johnson_1",
                               Full_Name == "Irv Smith" & gsis_id == "00-0015195" ~ "Irv Smith_1",
                               Full_Name == "Rodney Smith" & gsis_id == "00-0029934" ~ "Rodney Smith_1",
                               Full_Name == "Michael Pittman" & gsis_id == "00-0013013" ~ "Michael Pittman_1",
                               Full_Name == "James Robinson" & gsis_id == "00-0027166" ~ "James Robinson_1",
                               Full_Name == "Tony Jones" & gsis_id == "00-0031528" ~ "Tony Jones_1",
                               Full_Name == "Jeff Wilson" | Full_Name == "Jeffery Wilson" ~ "Jeff Wilson",
                               Full_Name == "Alex Smith" & gsis_id == "00-0023506" ~ "Alex Smith_1",
                               Full_Name == "Julio Jones" | Full_Name == "Julian Jones" ~ "Julio Jones",
                               Full_Name == "Chris Herndon" | Full_Name == "Christopher Herndon" ~ "Chris Herndon",
                               Full_Name == "Olabisi Johnson" | Full_Name == "Bisi Johnson" ~ "Olabisi Johnson",
                               Full_Name == "DJ Moore" | Full_Name == "D.J. Moore" ~ "D.J. Moore",
                               TRUE ~ Full_Name)) %>%
  mutate(Abbr = case_when(Full_Name == "Jaron Brown" ~ "Ja.Brown",
                          Full_Name == "Jaeden Graham" ~ "Ja.Graham",
                          Full_Name == "Dwayne Harris" ~ "Dw.Harris",
                          Full_Name == "DeMichael Harris" ~ "Demi.Harris",
                          Full_Name == "Demetrius Harris" ~ "Dem.Harris",
                          Full_Name == "Deonte Harris" ~ "De.Harris",
                          Full_Name == "Domanick Davis" ~ "Dom.Davis",
                          Full_Name == "Ty'Son Williams" ~ "Ty.Williams",
                          Full_Name == "Trayveon Williams" ~ "Tr.Williams",
                          Full_Name == "Justin Johnson" ~ "Ju.Johnson",
                          Full_Name == "Jon'Vea Johnson" ~ "Jo.Johnson",
                          Full_Name == "KeeSean Johnson" ~ "Ke.Johnson",
                          Full_Name == "Hunter Bryant" ~ "Hu.Bryant",
                          Full_Name == "Duke Williams" ~ "Du.Williams",
                          Full_Name == "Dexter Williams" ~ "De.Williams",
                          Full_Name == "Darrel Williams" ~ "Dar.Williams",
                          Full_Name == "D'Ernest Johnson" ~ "De.Johnson",
                          Full_Name == "Duke Johnson" ~ "Du.Johnson",
                          Full_Name == "David Johnson" ~ "Da.Johnson",
                          Full_Name == "Jason Moore" ~ "Ja.Moore",
                          Full_Name == "Jalin Moore" ~ "Jal.Moore",
                          Full_Name == "Jaylon Moore" ~ "Jay.Moore",
                          Full_Name == "Cody Thompson" ~ "Co.Thompson",
                          Full_Name == "Chris Thompson" & gsis_id == "00-0033855" ~ "Ch.Thompson",
                          Full_Name == "Colin Thompson" ~ "Col.Thompson",
                          Full_Name == "Malcolm Brown" ~ "Mal.Brown",
                          Full_Name == "Maxx Williams" ~ "Ma.Williams",
                          Full_Name == "Rashard Davis" ~ "Ra.Davis",
                          Full_Name == "Reggie Davis" ~ "Re.Davis",
                          Full_Name == "Irv Smith" ~ "Ir.Smith",
                          Full_Name == "J.J. Taylor" ~ "JJ.Taylor",
                          Full_Name == "Tony Jones" ~ "To.Jones",
                          Full_Name == "Dwayne Washington" ~ "Dw.Washington",
                          Full_Name == "Josh Hill" ~ "Jo.Hill",
                          Full_Name == "Mike Thomas" ~ "Mi.Thomas",
                          Full_Name == "Rod Smith" ~ "Ro.Smith",
                          Full_Name == "Alize Mack" ~ "Al.Mack",
                          Full_Name == "Ty Johnson" ~ "Ty.Johnson",
                          Full_Name == "Tyron Johnson" ~ "Tyr.Johnson",
                          Full_Name == "Josh Adams" ~ "Jo.Adams",
                          Full_Name == "D.J. Montgomery" ~ "DJ.Montgomery",
                          Full_Name == "Tyree Jackson" ~ "Ty.Jackson",
                          Full_Name == "Caleb Wilson" ~ "Ca.Wilson",
                          Full_Name == "Darius Jackson" ~ "Da.Jackson",
                          Full_Name == "Jerome Washington" ~ "Je.Washington",
                          Full_Name == "Joe Reed" ~ "Jo.Reed",
                          Full_Name == "Trent Taylor" ~ "Tr.Taylor",
                          Full_Name == "Jeff Smith" ~ "Je.Smith",
                          Full_Name == "Tanner Hudson" ~ "Ta.Hudson",
                          Full_Name == "Connor Davis" ~ "Co.Davis",
                          Full_Name == "A.J. Brown" ~ "AJ.Brown",
                          Full_Name == "Jonathan Williams" ~ "Jo.Williams",
                          Full_Name == "Tyler Davis" ~ "Ty.Davis",
                          Full_Name == "Domanick Williams" ~ "Dom.Williams",
                          Full_Name == "Steve Johnson" ~ "St.Johnson",
                          Full_Name == "Tyrell Williams" ~ "Tyr.Williams",
                          Full_Name == "Daryl Jones" ~ "Dar.Jones",
                          Full_Name == "Jonathan Williams_1" ~ "Jo.Williams",
                          Full_Name == "Rudi Johnson" ~ "Ru.Johnson",
                          Full_Name == "LeRon McCoy" ~ "Le.McCoy",
                          TRUE ~ Abbr)) %>%
  mutate(Abbr = case_when(Abbr == "J.Palmer" & gsis_id == "00-0020483" ~ "Je.Palmer",
                          Abbr == "A.Smith" & gsis_id == "00-0015082" ~ "Ak.Smith",
                          Abbr == "D.Carr" & gsis_id != "00-0031280" ~ "Da.Carr",
                          Abbr == "R.Griffin III" & gsis_id == "00-0029665" ~ "R.Griffin",
                          Abbr == "T.Hill" & gsis_id == "00-0033357" ~ "Ta.Hill",
                          Abbr == "J.Allen" & gsis_id == "00-0000210" ~ "Jam.Allen",
                          Abbr == "J.Johnson" & gsis_id == "00-0035290" ~ "Je.Johnson",
                          Abbr == "D.Brown" & gsis_id == "00-0001907" ~ "Da.Brown",
                          Abbr == "J.Miller" & gsis_id == "00-0011276" ~ "Ji.Miller",
                          Abbr == "D.Johnson" & gsis_id == "00-0019087" ~ "Do.Johnson",
                          Abbr == "T.Brown" & gsis_id == "00-0019192" ~ "Tr.Brown",
                          Abbr == "M.Moore" & gsis_id == "00-0025708" ~ "Ma.Moore",
                          Abbr == "R.Smith" & gsis_id == "32013030-2d30-3032-3737-37351d26fba2" ~ "Ru.Smith",
                          Abbr == "D.Davis" & gsis_id == "00-0028863" ~ "Do.Davis",
                          Abbr == "S.Morris" & gsis_id == "00-0030825" ~ "St.Morris",
                          Abbr == "C.Jones" & gsis_id == "00-0033098" ~ "Ca.Jones",
                          Abbr == "K.Allen" & gsis_id == "00-0034577" ~ "Ky.Allen",
                          Abbr == "D.Jones" & gsis_id == "00-0035710" ~ "Da.Jones",
                          Abbr == "B.Johnson" & gsis_id == "00-0035006" ~ "O.Johnson",
                          TRUE ~ Abbr)) %>%
  mutate(gsis_id = case_when(Full_Name == "Julio Jones" & Jersey == "16" ~ "00-0027944",
                             TRUE ~ gsis_id)) %>%
  dplyr::select(Season, Team, Abbr, Full_Name, gsis_id, Position, Jersey)

pbp_ridge <- pbp_fastr %>%
  filter(down %in% c(1:4),
         play_type %in% c("pass", "run", "no_play"),
         aborted_play == 0, qb_kneel == 0, qb_spike == 0,
         fumble == 0 | fumble == 1 & is.na(receiver_player_name),
         !is.na(yardline_100)) %>%
  mutate(passer = case_when(passer == "J.Palmer" & passer_id == "00-0020483" ~ "Je.Palmer",
                            passer == "A.Smith" & passer_id == "00-0015082" ~ "Ak.Smith",
                            passer == "D.Carr" & passer_id != "00-0031280" ~ "Da.Carr",
                            passer == "R.Griffin III" & passer_id == "00-0029665" ~ "R.Griffin",
                            passer == "T.Hill" & passer_id == "00-0033357" ~ "Ta.Hill",
                            passer == "J.Johnson" & passer_id == "00-0035290" ~ "Je.Johnson",
                            passer == "D.Brown" & passer_id == "00-0001907" ~ "Da.Brown",
                            passer == "J.Miller" & passer_id == "00-0011276" ~ "Ji.Miller",
                            passer == "D.Johnson" & passer_id == "00-0019087" ~ "Do.Johnson",
                            passer == "T.Brown" & passer_id == "00-0019192" ~ "Tr.Brown",
                            passer == "M.Moore" & passer_id == "00-0025708" ~ "Ma.Moore",
                            passer == "R.Smith" & passer_id == "00-0027775" ~ "Ru.Smith",
                            passer == "D.Davis" & passer_id == "00-0028863" ~ "Do.Davis",
                            passer == "S.Morris" & passer_id == "00-0030825" ~ "St.Morris",
                            passer == "C.Jones" & passer_id == "00-0033098" ~ "Ca.Jones",
                            passer == "K.Allen" & passer_id == "00-0034577" ~ "Ky.Allen",
                            passer == "D.Jones" & passer_id == "00-0035710" ~ "Da.Jones",
                            passer == "St. Pierre" & passer_id == "00-0022101" ~ "B.St. Pierre",
                            passer == "G.Minshew II" | passer == "G.Minshew" ~ "G.Minshew",
                            passer_player_name == "R.Fasani (3rd QB)" ~ "R.Fasani",
                            passer == "Aa.Rodgers" & passer_id == "00-0023459" ~ "A.Rodgers",
                            passer == "Ty.Taylor" & passer_id == "00-0028118" ~ "T.Taylor",
                            TRUE ~ passer)) %>%
  mutate(rusher = case_when(rusher == "J.Palmer" & rusher_id != "00-0020483" ~ "Je.Palmer",
                            rusher == "A.Smith" & rusher_id == "00-0015082" ~ "Ak.Smith",
                            rusher == "D.Carr" & rusher_id != "00-0031280" ~ "Da.Carr",
                            rusher == "R.Griffin III" & rusher_id == "00-0029665" ~ "R.Griffin",
                            rusher == "St. Pierre" & rusher_id == "00-0022101" ~ "B.St. Pierre",
                            rusher == "G.Minshew II" | rusher == "G.Minshew" ~ "G.Minshew",
                            rusher == "Aa. Rodgers" & rusher_id == "00-0023459" ~ "A.Rodgers",
                            rusher == "Ty.Taylor" & rusher_id == "00-0028118" ~ "T.Taylor",
                            rusher == "T.Hill" & rusher_id == "00-0033357" ~ "Ta.Hill",
                            rusher == "J.Allen" & rusher_id == "00-0000210" ~ "Jam.Allen",
                            rusher == "J.Johnson" & rusher_id == "00-0035290" ~ "Je.Johnson",
                            rusher == "D.Brown" & rusher_id == "00-0001907" ~ "Da.Brown",
                            rusher == "J.Miller" & rusher_id == "00-0011276" ~ "Ji.Miller",
                            rusher == "D.Johnson" & rusher_id == "00-0019087" ~ "Do.Johnson",
                            rusher == "T.Brown" & rusher_id == "00-0019192" ~ "Tr.Brown",
                            rusher == "M.Moore" & rusher_id == "00-0025708" ~ "Ma.Moore",
                            rusher == "R.Smith" & rusher_id == "00-0027775" ~ "Ru.Smith",
                            rusher == "D.Davis" & rusher_id == "00-0028863" ~ "Do.Davis",
                            rusher == "S.Morris" & rusher_id == "00-0030825" ~ "St.Morris",
                            rusher == "C.Jones" & rusher_id == "00-0033098" ~ "Ca.Jones",
                            rusher == "K.Allen" & rusher_id == "00-0034577" ~ "Ky.Allen",
                            rusher == "D.Jones" & rusher_id == "00-0035710" ~ "Da.Jones",
                            rusher == "J.Brown" & rusher_id == "00-0030300" ~ "Ja.Brown",
                            rusher == "J.Graham" & rusher_id == "00-0034788" ~ "Ja.Graham",
                            rusher == "D.Harris" & rusher_id == "00-0028114" ~ "Dw.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0035848" ~ "Demi.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0030155" ~ "Dem.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0035215" ~ "De.Harris",
                            rusher == "D.Davis" & rusher_id == "00-0021979" ~ "Dom.Williams",
                            rusher == "T.Williams" & rusher_id == "00-0036457" ~ "Ty.Williams",
                            rusher == "T.Williams" & rusher_id == "00-0035291" ~ "Tr.Williams",
                            rusher == "J.Johnson" & rusher_id == "00-0035092" ~ "Ju.Johnson",
                            rusher == "J.Johnson" & rusher_id == "00-0035417" ~ "Jo.Johnson",
                            rusher == "K.Johnson" & rusher_id == "00-0035287" ~ "Ke.Johnson",
                            rusher == "H.Bryant" & rusher_id == "00-0036008" ~ "Hu.Bryant",
                            rusher == "D.Williams" & rusher_id == "00-0032945" ~ "Du.Williams",
                            rusher == "D.Williams" & rusher_id == "00-0034995" ~ "De.Williams",
                            rusher == "D.Williams" & rusher_id == "00-0034301" ~ "Dar.Williams",
                            rusher == "D.Johnson" & rusher_id == "00-0035628" ~ "De.Johnson",
                            rusher == "D.Johnson" & rusher_id == "00-0032257" ~ "Du.Johnson",
                            rusher == "D.Johnson" & rusher_id == "00-0032187" ~ "Da.Johnson",
                            rusher == "J.Moore" & rusher_id == "00-0035432" ~ "Ja.Moore",
                            rusher == "J.Moore" & rusher_id == "00-0035505" ~ "Jal.Moore",
                            rusher == "J.Moore" & rusher_id == "00-0036125" ~ "Jay.Moore",
                            rusher == "C.Thompson" & rusher_id == "00-0035164" ~ "Co.Thompson",
                            rusher == "C.Thompson" & rusher_id == "00-0033855" ~ "Ch.Thompson",
                            rusher == "C.Thompson" & rusher_id == "00-0033720" ~ "Col.Thompson",
                            rusher == "M.Brown" & rusher_id == "00-0031806" ~ "Mal.Brown",
                            rusher == "M.Williams" & rusher_id == "00-0031558" ~ "Ma.Williams",
                            rusher == "R.Davis" & rusher_id == "00-0034034" ~ "Ra.Davis",
                            rusher == "R.Davis" & rusher_id == "00-0033815" ~ "Re.Davis",
                            rusher == "I.Smith" & rusher_id == "00-0034970" ~ "Ir.Smith",
                            rusher == "J.Taylor" & rusher_id == "00-0036096" ~ "JJ.Taylor",
                            rusher == "T.Jones" & rusher_id == "00-0035860" ~ "To.Jones",
                            rusher == "D.Washington" & rusher_id == "00-0032450" ~ "Dw.Washington",
                            rusher == "J.Hill" & rusher_id == "00-0030216" ~ "Jo.Hill",
                            rusher == "M.Thomas" & rusher_id == "00-0033114" ~ "Mi.Thomas",
                            rusher == "R.Smith" & rusher_id == "00-0031900" ~ "Ro.Smith",
                            rusher == "A.Mack" & rusher_id == "00-0035309" ~ "Al.Mack",
                            rusher == "T.Johnson" & rusher_id == "00-0035537" ~ "Ty.Johnson",
                            rusher == "Ty.Johnson" & rusher_id == "00-0035457" ~ "Tyr.Johnson",
                            rusher == "J.Adams" & rusher_id == "00-0034457" ~ "Jo.Adams",
                            rusher == "D.Montgomery" & rusher_id == "00-0035045" ~ "DJ.Montgomery",
                            rusher == "T.Jackson" & rusher_id == "00-0035356" ~ "Ty.Jackson",
                            rusher == "C.Wilson" & rusher_id == "00-0035320" ~ "Ca.Wilson",
                            rusher == "D.Jackson" & rusher_id == "00-0033049" ~ "Da.Jackson",
                            rusher == "J.Reed" & rusher_id == "00-0036377" ~ "Jo.Reed",
                            rusher == "T.Taylor" & rusher_id == "00-0033292" ~ "Tr.Taylor",
                            rusher == "J.Smith" & rusher_id == "00-0035510" ~ "Je.Smith",
                            rusher == "T.Hudson" & rusher_id == "00-0034613" ~ "Ta.Hudson",
                            rusher == "C.Davis" & rusher_id == "00-0036451" ~ "Co.Davis",
                            rusher == "A.Brown" & rusher_id == "00-0035676" ~ "AJ.Brown",
                            rusher == "J.Williams" & rusher_id == "00-0032975" ~ "Jo.Williams",
                            rusher == "T.Davis" & rusher_id == "00-0036250" ~ "Ty.Davis",
                            rusher == "C.Wells" & rusher_id == "00-0027007" ~ "B.Wells",
                            rusher == "K.Williams" | rusher == "Ka.Williams" & rusher_id == "00-0032152" ~ "K.Williams",
                            rusher == "A.Randle" & rusher_id == "00-0021190" ~ "A.Randle El",
                            rusher == "L.Shenault" | rusher == "L.Shenault Jr." ~ "L.Shenault",
                            rusher == "Daryl Jones" & rusher_id == "00-0021330" ~ "Dar.Jones",
                            rusher == "K.Whyte Jr." | rusher == "K.Whyte" ~ "K.Whyte",
                            rusher == "R.Johnson" & rusher_id == "00-0020407" | 
                              rusher == "Ru.Johnson" & rusher_id == "00-0020407" ~ "Ru.Johnson",
                            rusher == "Jo.Howard" | rusher == "J.Howard" & rusher_id == "00-0032780" ~ "J.Howard",
                            rusher == "Kevin Smith" ~ "K.Smith_1",
                            rusher == "De.Williams" | rusher == "D.Williams" & rusher_id == "00-0024242" ~ "D.Williams",
                            rusher == "W.Gallman" | rusher == "W.Gallman Jr." ~ "W.Gallman",
                            TRUE ~ rusher)) %>%
  mutate(receiver = case_when(receiver == "T.Hill" & receiver_id == "00-0033357" ~ "Ta.Hill",
                              receiver == "J.Brown" & receiver_id == "00-0030300" ~ "Ja.Brown",
                              receiver == "J.Graham" & receiver_id == "00-0034788" ~ "Ja.Graham",
                              receiver == "D.Harris" & receiver_id == "00-0028114" ~ "Dw.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0035848" ~ "Demi.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0030155" ~ "Dem.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0035215" ~ "De.Harris",
                              receiver == "D.Davis" & receiver_id == "00-0021979" ~ "Dom.Williams",
                              receiver == "T.Williams" & receiver_id == "00-0036457" ~ "Ty.Williams",
                              receiver == "T.Williams" & receiver_id == "00-0035291" ~ "Tr.Williams",
                              receiver == "Ty.Williams" & receiver_id == "00-0032160" ~ "Tyr.Williams",
                              receiver == "J.Johnson" & receiver_id == "00-0035092" ~ "Ju.Johnson",
                              receiver == "J.Johnson" & receiver_id == "00-0035417" ~ "Jo.Johnson",
                              receiver == "K.Johnson" & receiver_id == "00-0035287" ~ "Ke.Johnson",
                              receiver == "H.Bryant" & receiver_id == "00-0036008" ~ "Hu.Bryant",
                              receiver == "D.Williams" & receiver_id == "00-0032945" ~ "Du.Williams",
                              receiver == "D.Williams" & receiver_id == "00-0034995" ~ "De.Williams",
                              receiver == "D.Williams" & receiver_id == "00-0034301" ~ "Dar.Williams",
                              receiver == "D.Johnson" & receiver_id == "00-0035628" ~ "De.Johnson",
                              receiver == "D.Johnson" & receiver_id == "00-0032257" ~ "Du.Johnson",
                              receiver == "D.Johnson" & receiver_id == "00-0032187" ~ "Da.Johnson",
                              receiver == "J.Moore" & receiver_id == "00-0035432" ~ "Ja.Moore",
                              receiver == "J.Moore" & receiver_id == "00-0035505" ~ "Jal.Moore",
                              receiver == "J.Moore" & receiver_id == "00-0036125" ~ "Jay.Moore",
                              receiver == "C.Thompson" & receiver_id == "00-0035164" ~ "Co.Thompson",
                              receiver == "C.Thompson" & receiver_id == "00-0033855" ~ "Ch.Thompson",
                              receiver == "C.Thompson" & receiver_id == "00-0033720" ~ "Col.Thompson",
                              receiver == "M.Brown" & receiver_id == "00-0031806" ~ "Mal.Brown",
                              receiver == "M.Williams" & receiver_id == "00-0031558" ~ "Ma.Williams",
                              receiver == "R.Davis" & receiver_id == "00-0034034" ~ "Ra.Davis",
                              receiver == "R.Davis" & receiver_id == "00-0033815" ~ "Re.Davis",
                              receiver == "I.Smith" & receiver_id == "00-0034970" ~ "Ir.Smith",
                              receiver == "J.Taylor" & receiver_id == "00-0036096" ~ "JJ.Taylor",
                              receiver == "T.Jones" & receiver_id == "00-0035860" ~ "To.Jones",
                              receiver == "D.Washington" & receiver_id == "00-0032450" ~ "Dw.Washington",
                              receiver == "J.Hill" & receiver_id == "00-0030216" ~ "Jo.Hill",
                              receiver == "M.Thomas" & receiver_id == "00-0033114" ~ "Mi.Thomas",
                              receiver == "R.Smith" & receiver_id == "00-0031900" ~ "Ro.Smith",
                              receiver == "A.Mack" & receiver_id == "00-0035309" ~ "Al.Mack",
                              receiver == "Ty Johnson" & receiver_id == "00-0035537" ~ "Ty.Johnson",
                              receiver == "Ty.Johnson" & receiver_id == "00-0035457" ~ "Tyr.Johnson",
                              receiver == "J.Adams" & receiver_id == "00-0034457" ~ "Jo.Adams",
                              receiver == "D.Montgomery" & receiver_id == "00-0035045" ~ "DJ.Montgomery",
                              receiver == "T.Jackson" & receiver_id == "00-0035356" ~ "Ty.Jackson",
                              receiver == "C.Wilson" & receiver_id == "00-0035320" ~ "Ca.Wilson",
                              receiver == "D.Jackson" & receiver_id == "00-0033049" ~ "Da.Jackson",
                              receiver == "J.Reed" & receiver_id == "00-0036377" ~ "Jo.Reed",
                              receiver == "T.Taylor" & receiver_id == "00-0033292" ~ "Tr.Taylor",
                              receiver == "J.Smith" & receiver_id == "00-0035510" ~ "Je.Smith",
                              receiver == "T.Hudson" & receiver_id == "00-0034613" ~ "Ta.Hudson",
                              receiver == "C.Davis" & receiver_id == "00-0036451" ~ "Co.Davis",
                              receiver == "A.Brown" & receiver_id == "00-0035676" ~ "AJ.Brown",
                              receiver == "J.Williams" & receiver_id == "00-0032975" ~ "Jo.Williams",
                              receiver == "T.Davis" & receiver_id == "00-0036250" ~ "Ty.Davis",
                              receiver == "C.Wells" & receiver_id == "00-0027007" ~ "B.Wells",
                              receiver == "K.Williams" | receiver == "Ka.Williams" & receiver_id == "00-0032152" ~ "K.Williams",
                              receiver == "A.Randle" & receiver_id == "00-0021190" ~ "A.Randle El",
                              receiver == "D.Chark" | receiver == "D.Chark Jr." ~ "D.Chark",
                              receiver == "L.Shenault" | receiver == "L.Shenault Jr." ~ "L.Shenault",
                              receiver == "D.Parham" | receiver == "D.Parham Jr." ~ "D.Parham",
                              receiver == "Daryl Jones" & receiver_id == "00-0021330" ~ "Dar.Jones",
                              receiver == "E.Gates" & receiver_id == "00-0028049" ~ "C.Gates",
                              receiver == "E.St" & receiver_id == "00-0034279" ~ "E.St. Brown",
                              receiver == "J.Allen" & receiver_id == "00-0000210" ~ "Jam.Allen",
                              receiver == "R.Davis" & receiver_id == "00-0018608" ~ "Re.Davis",
                              receiver == "T.Johnson" & receiver_id == "00-0035537" ~ "Ty.Johnson",
                              receiver == "M.Minnis" ~ "S.Minnis",
                              receiver == "R.Johnson" & receiver_id == "00-0020407" | 
                                receiver == "Ru.Johnson" & receiver_id == "00-0020407" ~ "Ru.Johnson",
                              receiver == "L.McCoy" & receiver_id == "00-0023658" ~ "Le.McCoy",
                              receiver == "Jo.Howard" | receiver == "J.Howard" & receiver_id == "00-0032780" ~ "J.Howard",
                              receiver == "Kevin Smith" ~ "K.Smith_1",
                              receiver == "De.Williams" | receiver == "D.Williams" & receiver_id == "00-0024242" ~ "D.Williams",
                              receiver == "W.Gallman" | rusher == "W.Gallman Jr." ~ "W.Gallman",
                              receiver_player_name == "JulioJones" ~ "J.Jones",
                              receiver_player_name == "Andre' Davis" ~ "A.Davis",
                              receiver_player_name == "DanielThomas" ~ "D.Thomas",
                              TRUE ~ receiver)) %>%
  mutate(receiver_id = case_when(receiver_player_name == "J.Jones" & is.na(receiver_id) ~ "00-0027944",
                                 receiver_player_name == "D.Thomas" & is.na(receiver_id) ~ "00-0028000",
                                 receiver_player_name == "A.Davis" & is.na(receiver_id) ~ "00-0021175",
                                 TRUE ~ receiver_id)) %>%
  left_join(skill_roster, by = c("season" = "Season", "posteam" = "Team", "receiver" = "Abbr")) %>%
  rename(Receiver = Full_Name, Receiver_gsis = gsis_id) %>%
  left_join(skill_roster, by = c("season" = "Season", "posteam" = "Team", "rusher" = "Abbr")) %>%
  rename(Rusher = Full_Name, Rusher_gsis = gsis_id) %>%
  distinct(play_id, game_id, .keep_all = TRUE)

## ADD ROWS OF PLAYERS WHO PLAYED FOR MULTIPLE TEAMS IN ONE SEASON
missing_rushers <- pbp_ridge %>%
  filter(rush == 1 & is.na(Rusher)) %>%
  dplyr::select(season, posteam, desc, rusher_player_name, rusher_id, rush, penalty)
traded_players <- missing_rushers %>%
  group_by(rusher_player_name, rusher_id, season, posteam) %>%
  summarise(rush = sum(rush)) %>%
  filter(rush > 20) %>%
  left_join(skill_roster, by = c("rusher_id" = "gsis_id")) %>%
  distinct(rusher_player_name, season, Full_Name, .keep_all = TRUE) %>%
  ungroup()
traded_players <- traded_players %>%
  dplyr::select(Season = season, Team = posteam, Abbr, Full_Name, gsis_id = rusher_id, Position, Jersey) %>%
  mutate(Full_Name = case_when(gsis_id == "32013030-2d30-3032-3533-3934ceb6c025" ~ "Adrian Peterson",
                               gsis_id == "32013030-2d30-3032-3738-39308c123fd8" ~ "Ben Tate",
                               gsis_id == "32013030-2d30-3032-3633-3733685fb6e5" ~ "Justin Forsett",
                               gsis_id == "32013030-2d30-3032-3433-353924092e52" ~ "Jerome Harrison",
                               gsis_id == "32013030-2d30-3032-3733-3235a70c88f0" ~ "LeGarrette Blount",
                               gsis_id == "32013030-2d30-3032-3139-37365d176f27" ~ "Larry Johnson",
                               gsis_id == "32013030-2d30-3032-3533-39391f785c8e" ~ "Marshawn Lynch",
                               gsis_id == "32013030-2d30-3032-3632-3632e19db5d2" ~ "Tashard Choice",
                               gsis_id == "00-0032780" ~ "Jordan Howard",
                               TRUE ~ Full_Name)) %>%
  mutate(Abbr = case_when(gsis_id == "32013030-2d30-3032-3533-3934ceb6c025" ~ "A.Peterson",
                          gsis_id == "32013030-2d30-3032-3738-39308c123fd8" ~ "B.Tate",
                          gsis_id == "32013030-2d30-3032-3633-3733685fb6e5" ~ "J.Forsett",
                          gsis_id == "32013030-2d30-3032-3433-353924092e52" ~ "J.Harrison",
                          gsis_id == "32013030-2d30-3032-3733-3235a70c88f0" ~ "L.Blount",
                          gsis_id == "32013030-2d30-3032-3139-37365d176f27" ~ "L.Johnson",
                          gsis_id == "32013030-2d30-3032-3533-39391f785c8e" ~ "M.Lynch",
                          gsis_id == "32013030-2d30-3032-3632-3632e19db5d2" ~ "T.Choice",
                          gsis_id == "00-0032780" ~ "J.Howard",
                          TRUE ~ Abbr))

## REPEAT FOR RECEIVERS
missing_receivers <- pbp_ridge %>%
  filter(pass == 1 & sack == 0 & is.na(Receiver)) %>%
  dplyr::select(season, posteam, desc, receiver, receiver_id, pass, penalty)
traded_receivers <- missing_receivers %>%
  group_by(receiver, receiver_id, season, posteam) %>%
  summarise(target = sum(pass)) %>%
  filter(target > 15) %>%
  left_join(skill_roster, by = c("receiver_id" = "gsis_id")) %>%
  filter(!is.na(receiver_id)) %>%
  distinct(receiver, season, Full_Name, .keep_all = TRUE) %>%
  ungroup()

traded_receivers <- traded_receivers %>%
  dplyr::select(Season = season, Team = posteam, Abbr, Full_Name, gsis_id = receiver_id,
         Position, Jersey) %>%
  mutate(Full_Name = case_when(gsis_id == "32013030-2d30-3032-3334-333873e1f2db" ~ "Braylon Edwards",
                               gsis_id == "32013030-2d30-3032-3230-39376094ad7b" ~ "Brandon Lloyd",
                               gsis_id == "32013030-2d30-3032-3433-3334d8cecffc" ~ "Brandon Marshall",
                               gsis_id == "32013030-2d30-3032-3536-3232e385b1d6" ~ "Chansi Stuckey",
                               gsis_id == "32013030-2d30-3032-3131-3932b4b3b9f1" ~ "Deion Branch",
                               gsis_id == "32013030-2d30-3032-3432-393714e188c7" ~ "Derek Hagan",
                               gsis_id == "32013030-2d30-3031-3035-3030b8fff8aa" ~ "Derrick Mason",
                               gsis_id == "32013030-2d30-3032-3738-3734e1cdacfa" ~ "Demaryius Thomas",
                               gsis_id == "32013030-2d30-3032-3736-383548509814" ~ "Emmanuel Sanders",
                               gsis_id == "32013030-2d30-3032-3738-39319adae9d4" ~ "Golden Tate",
                               gsis_id == "32013030-2d30-3032-3433-32345833fa0d" ~ "Jason Avant",
                               gsis_id == "32013030-2d30-3032-3730-3036db9e19cc" ~ "Kenny Britt",
                               gsis_id == "32013030-2d30-3032-3237-3236fd40333d" ~ "Keary Colbert",
                               gsis_id == "32013030-2d30-3032-3637-38303e4f678e" ~ "Kevin Ogletree",
                               gsis_id == "32013030-2d30-3032-3736-30389bb0417b" ~ "Kyle Williams",
                               gsis_id == "32013030-2d30-3032-3632-3031d0233cfe" ~ "Martellus Bennett",
                               gsis_id == "32013030-2d30-3032-3534-36366e1678a7" ~ "Mike Sims-Walker",
                               gsis_id == "32013030-2d30-3032-3339-3638abb6a923" ~ "Michael Spurlock",
                               gsis_id == "32013030-2d30-3032-3730-3736b9572479" ~ "Mike Thomas_1",
                               gsis_id == "32013030-2d30-3032-3334-3532439be3f6" ~ "Mike Williams_1",
                               gsis_id == "32013030-2d30-3032-3639-39381fca7917" ~ "Percy Harvin",
                               gsis_id == "32013030-2d30-3031-3137-3534658285bc" ~ "Randy Moss",
                               gsis_id == "32013030-2d30-3032-3239-30392e97c147" ~ "Roy Williams",
                               gsis_id == "32013030-2d30-3032-3432-3231bd4de2d9" ~ "Vernon Davis",
                               TRUE ~ Full_Name)) %>%
  mutate(Abbr = case_when(gsis_id == "32013030-2d30-3032-3334-333873e1f2db" ~ "B.Edwards",
                          gsis_id == "32013030-2d30-3032-3230-39376094ad7b" ~ "B.Lloyd",
                          gsis_id == "32013030-2d30-3032-3433-3334d8cecffc" ~ "B.Marshall",
                          gsis_id == "32013030-2d30-3032-3536-3232e385b1d6" ~ "C.Stuckey",
                          gsis_id == "32013030-2d30-3032-3131-3932b4b3b9f1" ~ "D.Branch",
                          gsis_id == "32013030-2d30-3032-3432-393714e188c7" ~ "D.Hagan",
                          gsis_id == "32013030-2d30-3031-3035-3030b8fff8aa" ~ "D.Mason",
                          gsis_id == "32013030-2d30-3032-3738-3734e1cdacfa" ~ "D.Thomas",
                          gsis_id == "32013030-2d30-3032-3736-383548509814" ~ "E.Sanders",
                          gsis_id == "32013030-2d30-3032-3738-39319adae9d4" ~ "G.Tate",
                          gsis_id == "32013030-2d30-3032-3433-32345833fa0d" ~ "J.Avant",
                          gsis_id == "32013030-2d30-3032-3730-3036db9e19cc" ~ "K.Britt",
                          gsis_id == "32013030-2d30-3032-3237-3236fd40333d" ~ "K.Colbert",
                          gsis_id == "32013030-2d30-3032-3637-38303e4f678e" ~ "K.Ogletree",
                          gsis_id == "32013030-2d30-3032-3736-30389bb0417b" ~ "K.Williams",
                          gsis_id == "32013030-2d30-3032-3632-3031d0233cfe" ~ "M.Bennett",
                          gsis_id == "32013030-2d30-3032-3534-36366e1678a7" ~ "M.Sims-Walker",
                          gsis_id == "32013030-2d30-3032-3339-3638abb6a923" ~ "M.Spurlock",
                          gsis_id == "32013030-2d30-3032-3730-3736b9572479" ~ "M.Thomas",
                          gsis_id == "32013030-2d30-3032-3334-3532439be3f6" ~ "M.Williams",
                          gsis_id == "32013030-2d30-3032-3639-39381fca7917" ~ "P.Harvin",
                          gsis_id == "32013030-2d30-3031-3137-3534658285bc" ~ "R.Moss",
                          gsis_id == "32013030-2d30-3032-3239-30392e97c147" ~ "R.Williams",
                          gsis_id == "32013030-2d30-3032-3432-3231bd4de2d9" ~ "V.Davis",
                          TRUE ~ Abbr))

traded_players <- rbind(traded_players, traded_receivers)  

skill_roster <- rbind(skill_roster, traded_players) %>%
  filter(!is.na(Full_Name)) %>%
  left_join(all_roster, by = c("Season", "Team", "Abbr", "Full_Name", "gsis_id", "Position",
                               "Jersey")) %>%
  dplyr::select(Season, Team, Abbr, Full_Name, gsis_id, Position, Jersey, sportradar_id)
## MAKE SAME NAME CHANGES TO PBP DATA
## SPLIT DATA AND JOIN BY GSIS_ID FOR YEARS PRIOR TO 2020
## FILTER DATA AND SELECT ONLY RELEVANT VARIABLES

pbp_ridge <- pbp_fastr %>%
  filter(down %in% c(1:4),
         play_type %in% c("pass", "run", "no_play"),
         aborted_play == 0, qb_kneel == 0, qb_spike == 0,
         fumble == 0 | fumble == 1 & is.na(receiver_player_name),
         !is.na(yardline_100)) %>%
  mutate(passer = case_when(passer == "J.Palmer" & passer_id == "00-0020483" ~ "Je.Palmer",
                            passer == "A.Smith" & passer_id == "00-0015082" ~ "Ak.Smith",
                            passer == "D.Carr" & passer_id != "00-0031280" ~ "Da.Carr",
                            passer == "R.Griffin III" & passer_id == "00-0029665" ~ "R.Griffin",
                            passer == "T.Hill" & passer_id == "00-0033357" ~ "Ta.Hill",
                            passer == "J.Johnson" & passer_id == "00-0035290" ~ "Je.Johnson",
                            passer == "D.Brown" & passer_id == "00-0001907" ~ "Da.Brown",
                            passer == "J.Miller" & passer_id == "00-0011276" ~ "Ji.Miller",
                            passer == "D.Johnson" & passer_id == "00-0019087" ~ "Do.Johnson",
                            passer == "T.Brown" & passer_id == "00-0019192" ~ "Tr.Brown",
                            passer == "M.Moore" & passer_id == "00-0025708" ~ "Ma.Moore",
                            passer == "R.Smith" & passer_id == "00-0027775" ~ "Ru.Smith",
                            passer == "D.Davis" & passer_id == "00-0028863" ~ "Do.Davis",
                            passer == "S.Morris" & passer_id == "00-0030825" ~ "St.Morris",
                            passer == "C.Jones" & passer_id == "00-0033098" ~ "Ca.Jones",
                            passer == "K.Allen" & passer_id == "00-0034577" ~ "Ky.Allen",
                            passer == "D.Jones" & passer_id == "00-0035710" ~ "Da.Jones",
                            passer == "St. Pierre" & passer_id == "00-0022101" ~ "B.St. Pierre",
                            passer == "G.Minshew II" | passer == "G.Minshew" ~ "G.Minshew",
                            passer == "Aa.Rodgers" & passer_id == "00-0023459" ~ "A.Rodgers",
                            passer == "Ty.Taylor" & passer_id == "00-0028118" ~ "T.Taylor",
                            TRUE ~ passer)) %>%
  mutate(rusher = case_when(rusher == "J.Palmer" & rusher_id != "00-0020483" ~ "Je.Palmer",
                            rusher == "A.Smith" & rusher_id == "00-0015082" ~ "Ak.Smith",
                            rusher == "D.Carr" & rusher_id != "00-0031280" ~ "Da.Carr",
                            rusher == "Aa.Rodgers" & rusher_id == "00-0023459" ~ "A.Rodgers",
                            rusher == "Ty.Taylor" & rusher_id == "00-0028118" ~ "T.Taylor",
                            rusher == "R.Griffin III" & rusher_id == "00-0029665" ~ "R.Griffin",
                            rusher == "St. Pierre" & rusher_id == "00-0022101" ~ "B.St. Pierre",
                            rusher == "G.Minshew II" | rusher == "G.Minshew" ~ "G.Minshew",
                            rusher == "T.Hill" & rusher_id == "00-0033357" ~ "Ta.Hill",
                            rusher == "J.Allen" & rusher_id == "00-0000210" ~ "Jam.Allen",
                            rusher == "J.Johnson" & rusher_id == "00-0035290" ~ "Je.Johnson",
                            rusher == "D.Brown" & rusher_id == "00-0001907" ~ "Da.Brown",
                            rusher == "J.Miller" & rusher_id == "00-0011276" ~ "Ji.Miller",
                            rusher == "D.Johnson" & rusher_id == "00-0019087" ~ "Do.Johnson",
                            rusher == "T.Brown" & rusher_id == "00-0019192" ~ "Tr.Brown",
                            rusher == "M.Moore" & rusher_id == "00-0025708" ~ "Ma.Moore",
                            rusher == "R.Smith" & rusher_id == "00-0027775" ~ "Ru.Smith",
                            rusher == "D.Davis" & rusher_id == "00-0028863" ~ "Do.Davis",
                            rusher == "S.Morris" & rusher_id == "00-0030825" ~ "St.Morris",
                            rusher == "C.Jones" & rusher_id == "00-0033098" ~ "Ca.Jones",
                            rusher == "K.Allen" & rusher_id == "00-0034577" ~ "Ky.Allen",
                            rusher == "D.Jones" & rusher_id == "00-0035710" ~ "Da.Jones",
                            rusher == "J.Brown" & rusher_id == "00-0030300" ~ "Ja.Brown",
                            rusher == "J.Graham" & rusher_id == "00-0034788" ~ "Ja.Graham",
                            rusher == "D.Harris" & rusher_id == "00-0028114" ~ "Dw.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0035848" ~ "Demi.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0030155" ~ "Dem.Harris",
                            rusher == "D.Harris" & rusher_id == "00-0035215" ~ "De.Harris",
                            rusher == "D.Davis" & rusher_id == "00-0021979" ~ "Dom.Williams",
                            rusher == "T.Williams" & rusher_id == "00-0036457" ~ "Ty.Williams",
                            rusher == "T.Williams" & rusher_id == "00-0035291" ~ "Tr.Williams",
                            rusher == "J.Johnson" & rusher_id == "00-0035092" ~ "Ju.Johnson",
                            rusher == "J.Johnson" & rusher_id == "00-0035417" ~ "Jo.Johnson",
                            rusher == "K.Johnson" & rusher_id == "00-0035287" ~ "Ke.Johnson",
                            rusher == "H.Bryant" & rusher_id == "00-0036008" ~ "Hu.Bryant",
                            rusher == "D.Williams" & rusher_id == "00-0032945" ~ "Du.Williams",
                            rusher == "D.Williams" & rusher_id == "00-0034995" ~ "De.Williams",
                            rusher == "D.Williams" & rusher_id == "00-0034301" ~ "Dar.Williams",
                            rusher == "D.Johnson" & rusher_id == "00-0035628" ~ "De.Johnson",
                            rusher == "D.Johnson" & rusher_id == "00-0032257" ~ "Du.Johnson",
                            rusher == "D.Johnson" & rusher_id == "00-0032187" ~ "Da.Johnson",
                            rusher == "J.Moore" & rusher_id == "00-0035432" ~ "Ja.Moore",
                            rusher == "J.Moore" & rusher_id == "00-0035505" ~ "Jal.Moore",
                            rusher == "J.Moore" & rusher_id == "00-0036125" ~ "Jay.Moore",
                            rusher == "C.Thompson" & rusher_id == "00-0035164" ~ "Co.Thompson",
                            rusher == "C.Thompson" & rusher_id == "00-0033855" ~ "Ch.Thompson",
                            rusher == "C.Thompson" & rusher_id == "00-0033720" ~ "Col.Thompson",
                            rusher == "M.Brown" & rusher_id == "00-0031806" ~ "Mal.Brown",
                            rusher == "M.Williams" & rusher_id == "00-0031558" ~ "Ma.Williams",
                            rusher == "R.Davis" & rusher_id == "00-0034034" ~ "Ra.Davis",
                            rusher == "R.Davis" & rusher_id == "00-0033815" ~ "Re.Davis",
                            rusher == "I.Smith" & rusher_id == "00-0034970" ~ "Ir.Smith",
                            rusher == "J.Taylor" & rusher_id == "00-0036096" ~ "JJ.Taylor",
                            rusher == "T.Jones" & rusher_id == "00-0035860" ~ "To.Jones",
                            rusher == "D.Washington" & rusher_id == "00-0032450" ~ "Dw.Washington",
                            rusher == "J.Hill" & rusher_id == "00-0030216" ~ "Jo.Hill",
                            rusher == "M.Thomas" & rusher_id == "00-0033114" ~ "Mi.Thomas",
                            rusher == "R.Smith" & rusher_id == "00-0031900" ~ "Ro.Smith",
                            rusher == "A.Mack" & rusher_id == "00-0035309" ~ "Al.Mack",
                            rusher == "T.Johnson" & rusher_id == "00-0035537" ~ "Ty.Johnson",
                            rusher == "Ty.Johnson" & rusher_id == "00-0035457" ~ "Tyr.Johnson",
                            rusher == "J.Adams" & rusher_id == "00-0034457" ~ "Jo.Adams",
                            rusher == "D.Montgomery" & rusher_id == "00-0035045" ~ "DJ.Montgomery",
                            rusher == "T.Jackson" & rusher_id == "00-0035356" ~ "Ty.Jackson",
                            rusher == "C.Wilson" & rusher_id == "00-0035320" ~ "Ca.Wilson",
                            rusher == "D.Jackson" & rusher_id == "00-0033049" ~ "Da.Jackson",
                            rusher == "J.Reed" & rusher_id == "00-0036377" ~ "Jo.Reed",
                            rusher == "T.Taylor" & rusher_id == "00-0033292" ~ "Tr.Taylor",
                            rusher == "J.Smith" & rusher_id == "00-0035510" ~ "Je.Smith",
                            rusher == "T.Hudson" & rusher_id == "00-0034613" ~ "Ta.Hudson",
                            rusher == "C.Davis" & rusher_id == "00-0036451" ~ "Co.Davis",
                            rusher == "A.Brown" & rusher_id == "00-0035676" ~ "AJ.Brown",
                            rusher == "J.Williams" & rusher_id == "00-0032975" ~ "Jo.Williams",
                            rusher == "T.Davis" & rusher_id == "00-0036250" ~ "Ty.Davis",
                            rusher == "C.Wells" & rusher_id == "00-0027007" ~ "B.Wells",
                            rusher == "K.Williams" | rusher == "Ka.Williams" & rusher_id == "00-0032152" ~ "K.Williams",
                            rusher == "A.Randle" & rusher_id == "00-0021190" ~ "A.Randle El",
                            rusher == "L.Shenault" | rusher == "L.Shenault Jr." ~ "L.Shenault",
                            rusher == "Daryl Jones" & rusher_id == "00-0021330" ~ "Dar.Jones",
                            rusher == "K.Whyte Jr." | rusher == "K.Whyte" ~ "K.Whyte",
                            rusher == "R.Johnson" & rusher_id == "00-0020407" | 
                              rusher == "Ru.Johnson" & rusher_id == "00-0020407" ~ "Ru.Johnson",
                            rusher == "Jo.Howard" | rusher == "J.Howard" & rusher_id == "00-0032780" ~ "J.Howard",
                            rusher == "Kevin Smith" ~ "K.Smith_1",
                            rusher == "De.Williams" | rusher == "D.Williams" & rusher_id == "00-0024242" ~ "D.Williams",
                            rusher == "W.Gallman" | rusher == "W.Gallman Jr." ~ "W.Gallman",
                            TRUE ~ rusher)) %>%
  mutate(receiver = case_when(receiver == "T.Hill" & receiver_id == "00-0033357" ~ "Ta.Hill",
                              receiver == "J.Brown" & receiver_id == "00-0030300" ~ "Ja.Brown",
                              receiver == "J.Graham" & receiver_id == "00-0034788" ~ "Ja.Graham",
                              receiver == "D.Harris" & receiver_id == "00-0028114" ~ "Dw.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0035848" ~ "Demi.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0030155" ~ "Dem.Harris",
                              receiver == "D.Harris" & receiver_id == "00-0035215" ~ "De.Harris",
                              receiver == "D.Davis" & receiver_id == "00-0021979" ~ "Dom.Williams",
                              receiver == "T.Williams" & receiver_id == "00-0036457" ~ "Ty.Williams",
                              receiver == "T.Williams" & receiver_id == "00-0035291" ~ "Tr.Williams",
                              receiver == "Ty.Williams" & receiver_id == "00-0032160" ~ "Tyr.Williams",
                              receiver == "J.Johnson" & receiver_id == "00-0035092" ~ "Ju.Johnson",
                              receiver == "J.Johnson" & receiver_id == "00-0035417" ~ "Jo.Johnson",
                              receiver == "K.Johnson" & receiver_id == "00-0035287" ~ "Ke.Johnson",
                              receiver == "H.Bryant" & receiver_id == "00-0036008" ~ "Hu.Bryant",
                              receiver == "D.Williams" & receiver_id == "00-0032945" ~ "Du.Williams",
                              receiver == "D.Williams" & receiver_id == "00-0034995" ~ "De.Williams",
                              receiver == "D.Williams" & receiver_id == "00-0034301" ~ "Dar.Williams",
                              receiver == "D.Johnson" & receiver_id == "00-0035628" ~ "De.Johnson",
                              receiver == "D.Johnson" & receiver_id == "00-0032257" ~ "Du.Johnson",
                              receiver == "D.Johnson" & receiver_id == "00-0032187" ~ "Da.Johnson",
                              receiver == "J.Moore" & receiver_id == "00-0035432" ~ "Ja.Moore",
                              receiver == "J.Moore" & receiver_id == "00-0035505" ~ "Jal.Moore",
                              receiver == "J.Moore" & receiver_id == "00-0036125" ~ "Jay.Moore",
                              receiver == "C.Thompson" & receiver_id == "00-0035164" ~ "Co.Thompson",
                              receiver == "C.Thompson" & receiver_id == "00-0033855" ~ "Ch.Thompson",
                              receiver == "C.Thompson" & receiver_id == "00-0033720" ~ "Col.Thompson",
                              receiver == "M.Brown" & receiver_id == "00-0031806" ~ "Mal.Brown",
                              receiver == "M.Williams" & receiver_id == "00-0031558" ~ "Ma.Williams",
                              receiver == "R.Davis" & receiver_id == "00-0034034" ~ "Ra.Davis",
                              receiver == "R.Davis" & receiver_id == "00-0033815" ~ "Re.Davis",
                              receiver == "I.Smith" & receiver_id == "00-0034970" ~ "Ir.Smith",
                              receiver == "J.Taylor" & receiver_id == "00-0036096" ~ "JJ.Taylor",
                              receiver == "T.Jones" & receiver_id == "00-0035860" ~ "To.Jones",
                              receiver == "D.Washington" & receiver_id == "00-0032450" ~ "Dw.Washington",
                              receiver == "J.Hill" & receiver_id == "00-0030216" ~ "Jo.Hill",
                              receiver == "M.Thomas" & receiver_id == "00-0033114" ~ "Mi.Thomas",
                              receiver == "R.Smith" & receiver_id == "00-0031900" ~ "Ro.Smith",
                              receiver == "A.Mack" & receiver_id == "00-0035309" ~ "Al.Mack",
                              receiver == "Ty Johnson" & receiver_id == "00-0035537" ~ "Ty.Johnson",
                              receiver == "Ty.Johnson" & receiver_id == "00-0035457" ~ "Tyr.Johnson",
                              receiver == "J.Adams" & receiver_id == "00-0034457" ~ "Jo.Adams",
                              receiver == "D.Montgomery" & receiver_id == "00-0035045" ~ "DJ.Montgomery",
                              receiver == "T.Jackson" & receiver_id == "00-0035356" ~ "Ty.Jackson",
                              receiver == "C.Wilson" & receiver_id == "00-0035320" ~ "Ca.Wilson",
                              receiver == "D.Jackson" & receiver_id == "00-0033049" ~ "Da.Jackson",
                              receiver == "J.Reed" & receiver_id == "00-0036377" ~ "Jo.Reed",
                              receiver == "T.Taylor" & receiver_id == "00-0033292" ~ "Tr.Taylor",
                              receiver == "J.Smith" & receiver_id == "00-0035510" ~ "Je.Smith",
                              receiver == "T.Hudson" & receiver_id == "00-0034613" ~ "Ta.Hudson",
                              receiver == "C.Davis" & receiver_id == "00-0036451" ~ "Co.Davis",
                              receiver == "A.Brown" & receiver_id == "00-0035676" ~ "AJ.Brown",
                              receiver == "J.Williams" & receiver_id == "00-0032975" ~ "Jo.Williams",
                              receiver == "T.Davis" & receiver_id == "00-0036250" ~ "Ty.Davis",
                              receiver == "C.Wells" & receiver_id == "00-0027007" ~ "B.Wells",
                              receiver == "K.Williams" | receiver == "Ka.Williams" & receiver_id == "00-0032152" ~ "K.Williams",
                              receiver == "A.Randle" & receiver_id == "00-0021190" ~ "A.Randle El",
                              receiver == "D.Chark" | receiver == "D.Chark Jr." ~ "D.Chark",
                              receiver == "L.Shenault" | receiver == "L.Shenault Jr." ~ "L.Shenault",
                              receiver == "D.Parham" | receiver == "D.Parham Jr." ~ "D.Parham",
                              receiver == "Daryl Jones" & receiver_id == "00-0021330" ~ "Dar.Jones",
                              receiver == "E.Gates" & receiver_id == "00-0028049" ~ "C.Gates",
                              receiver == "E.St" & receiver_id == "00-0034279" ~ "E.St. Brown",
                              receiver == "J.Allen" & receiver_id == "00-0000210" ~ "Jam.Allen",
                              receiver == "R.Davis" & receiver_id == "00-0018608" ~ "Re.Davis",
                              receiver == "T.Johnson" & receiver_id == "00-0035537" ~ "Ty.Johnson",
                              receiver == "M.Minnis" ~ "S.Minnis",
                              receiver == "R.Johnson" & receiver_id == "00-0020407" | 
                                receiver == "Ru.Johnson" & receiver_id == "00-0020407" ~ "Ru.Johnson",
                              receiver == "L.McCoy" & receiver_id == "00-0023658" ~ "Le.McCoy",
                              receiver == "Jo.Howard" & receiver_id == "00-0032780" ~ "J.Howard",
                              receiver == "Kevin Smith" ~ "K.Smith_1",
                              receiver == "De.Williams" | receiver == "D.Williams" & receiver_id == "00-0024242" ~ "D.Williams",
                              receiver == "W.Gallman" | receiver == "W.Gallman Jr." ~ "W.Gallman",
                              receiver_player_name == "JulioJones" ~ "J.Jones",
                              receiver_player_name == "Andre' Davis" ~ "A.Davis",
                              receiver_player_name == "DanielThomas" ~ "D.Thomas",
                              TRUE ~ receiver)) %>%
  mutate(receiver_id = case_when(receiver == "J.Jones" & receiver_player_name == "JulioJones" ~ "00-0027944",
                                 receiver == "D.Thomas" & receiver_player_name == "DanielThomas" ~ "00-0028000",
                                 receiver == "A.Davis" & receiver_player_name == "Andre' Davis" ~ "00-0021175",
                                 TRUE ~ receiver_id)) %>%
  left_join(skill_roster, by = c("season" = "Season", "posteam" = "Team", "receiver" = "Abbr")) %>%
  rename(Receiver = Full_Name, Receiver_gsis = gsis_id) %>%
  left_join(skill_roster, by = c("season" = "Season", "posteam" = "Team", "rusher" = "Abbr")) %>%
  rename(Rusher = Full_Name, Rusher_gsis = gsis_id) %>%
  distinct(play_id, game_id, .keep_all = TRUE)



## CHECK ID'S FOR PLAYERS IN PBP AND JOIN TO QB AND SKILL ROSTERS
qb_id_check <- pbp_ridge %>%
  group_by(passer, passer_id) %>%
  summarise(attempts = sum(qb_dropback, na.rm = TRUE)) %>%
  mutate(d = duplicated(passer))

rb_id_check <- pbp_ridge %>%
  group_by(season, Rusher, rusher, Rusher_gsis, rusher_id, posteam) %>%
  summarise(attempts = sum(rush_attempt, na.rm = TRUE)) %>%
  dplyr::select(Season = season, Player = Rusher, Player_gsis = Rusher_gsis, Player_id = rusher_id, posteam)

wr_id_check <- pbp_ridge %>%
  group_by(season, Receiver, receiver, Receiver_gsis, receiver_id, posteam) %>%
  summarise(attempts = sum(complete_pass, na.rm = TRUE)) %>%
  dplyr::select(Season = season, Player = Receiver, Player_gsis = Receiver_gsis, Player_id = receiver_id, posteam)

skill_id_check <- rbind(rb_id_check, wr_id_check)

## QB ROSTER READY TO JOIN PBP DATA BY ID
qb_roster <- left_join(qb_roster, qb_id_check, by = c("Abbr" = "passer")) %>%
  filter(!is.na(passer_id)) %>%
  dplyr::select(Season, Team, Position, Jersey, Full_Name, Abbr, Passer_id = passer_id, 
         qb_radar_id = sportradar_id, headshot_url)

## SKILL ROSTER - NO NEED TO JOIN SINCE PRIOR JOIN
skill_roster <- left_join(skill_roster, skill_id_check, by = c("Full_Name" = "Player",
                                                               "gsis_id" = "Player_gsis",
                                                               "Season")) %>%
  dplyr::select(Season, Team, Position, Jersey, Full_Name, Abbr, gsis_id, Player_id,
         sportradar_id) %>%
  distinct(Full_Name, Season, .keep_all = TRUE)

qb_join <- qb_roster %>%
  dplyr::select(Season, Full_Name, Passer_id, Position)

## DATASET INCLUDING ALL FULL PLAYER NAMES AND ALL ID'S - REMOVE VARIABLES TO KEEP DATA MANAGEABLE
pbp_ridge <- left_join(pbp_ridge, qb_join, by = c("season" = "Season", "passer_id" = "Passer_id")) %>%
  dplyr::select(-field_goal_result, -kick_distance, -extra_point_result, -two_point_conv_result,
         -punt_blocked, -touchback, -punt_inside_twenty, -punt_in_endzone, -punt_out_of_bounds,
         -punt_downed, -punt_fair_catch, -kickoff_inside_twenty, -kickoff_in_endzone, -kickoff_out_of_bounds,
         -kickoff_downed, -kickoff_fair_catch, -own_kickoff_recovery, -own_kickoff_recovery_td,
         -return_touchdown, -extra_point_attempt, -two_point_attempt, -field_goal_attempt,
         -kickoff_attempt, -punt_attempt, -lateral_reception, -lateral_rush, -lateral_return,
         -lateral_recovery, -lateral_receiver_player_id, -lateral_receiver_player_name,
         -lateral_rusher_player_id, -lateral_rusher_player_name,
         -lateral_sack_player_id, -lateral_sack_player_name,
         -lateral_interception_player_id, -lateral_interception_player_name, -punt_returner_player_id,
         -punt_returner_player_name, -lateral_punt_returner_player_id, -kickoff_returner_player_name,
         -lateral_punt_returner_player_name, -kickoff_returner_player_id, -lateral_kickoff_returner_player_id,
         -lateral_kickoff_returner_player_name, -punter_player_id, -punter_player_name,
         -kicker_player_name, -kicker_player_id, -own_kickoff_recovery_player_id, 
         -own_kickoff_recovery_player_name, -blocked_player_id, -blocked_player_name,
         -forced_fumble_player_1_team, -forced_fumble_player_2_team, -solo_tackle_1_team,
         -solo_tackle_2_team, - assist_tackle_1_team, -assist_tackle_2_team, -assist_tackle_3_team,
         -assist_tackle_4_team, 
         -fumbled_1_team, -fumbled_2_team, -return_team, -return_yards, -defensive_two_point_attempt,
         -defensive_two_point_conv, -defensive_extra_point_attempt, -defensive_extra_point_conv,
         -special_teams_play, -st_play_type, -id,
         -Jersey.x, -Jersey.y, -Position.x, -Position.y) %>%
  rename(Passer = Full_Name)

Team_Snaps <- pbp_ridge %>%
  group_by(season, posteam) %>%
  summarise(total_plays = n())

pbp_ridge <- left_join(pbp_ridge, Team_Snaps, by = c("season", "posteam")) 



options(scipen = 999, digits = 2)

## SET UP VARIABLES TO RUN LASSO IN ORDER TO SELECT VARIABLES TO ADJUST EPA WITHIN RIDGE
pbp_edit <- pbp_ridge %>%
  filter(season >= 1999) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         wp_squared = wp^2,
         log_wp = log(wp),
         log_ydstogo = log(ydstogo),
         yardline_squared = yardline_100^2,
         gtg_down = down * goal_to_go,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         home = if_else(posteam == home_team, 1, 0),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0),
         in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0)) 

lasso_epa_table <- pbp_edit %>%
  dplyr::select(epa, season)

lasso_dataset <- pbp_edit %>%
  dplyr::select(epa, season, down, ydstogo, yardline_100, wp, shotgun, qtr, half_seconds_remaining, goal_to_go,
         home, no_huddle, total_line, drive_play_count, series, posteam_timeouts_remaining, gtg_down, 
         defteam_timeouts_remaining, wind, yards_to_go_square, yards_times_down, 
         wp_squared, log_wp, log_ydstogo, yardline_squared, vegas_wp_squared, log_vegas_wp,
         side_of_field_indicator, in_red_zone, in_fg_range) %>%
  mutate(across(c("ydstogo", "yardline_100", "wp", "qtr", "half_seconds_remaining", "total_line",
                  "drive_play_count", "series", "posteam_timeouts_remaining", "yards_times_down", "gtg_down",
                  "wind", "yards_to_go_square", "defteam_timeouts_remaining", "yards_times_down", "wp_squared",
                  "log_wp", "log_ydstogo", "yardline_squared", "vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4)))


lasso_data_vector <- as.vector(lasso_dataset) %>%
  dplyr::filter(!is.na(epa))
lasso_data_vector[is.na(lasso_data_vector)] <- 0


lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)
grid <- 10^seq(2,-2,length=100)

## SET SEED, SPLIT DATA FOR TRAINING & TESTING
set.seed(2021)

x_train <- lasso_data_vector %>%
  dplyr::filter(season <= 2018) %>%
  dplyr::select(-season, -epa)
x_test <- lasso_data_vector %>%
  dplyr::filter(season > 2018) %>%
  dplyr::select(-season, -epa)
y_train <- lasso_data_vector %>%
  dplyr::filter(season <= 2018) %>%
  dplyr::select(epa)
y_test <- lasso_data_vector %>%
  filter(season > 2018) %>%
  dplyr::select(epa)


lasso_context_vector <- lasso_data_vector %>%
  dplyr::select(-season, -epa)
lasso_epa_vector <- lasso_data_vector %>%
  dplyr::select(epa)

x_train_matrix = as.matrix(as.data.frame(lapply(x_train, as.numeric)))
y_train_matrix = as.matrix(as.data.frame(lapply(y_train, as.numeric)))
x_test_matrix = as.matrix(as.data.frame(lapply(x_test, as.numeric)))
y_test_matrix = as.matrix(as.data.frame(lapply(y_test, as.numeric)))

lasso_epa_matrix = as.matrix(as.data.frame(lapply(lasso_epa_vector, as.numeric)))
lasso_context_matrix = as.matrix(as.data.frame(lapply(lasso_context_vector, as.numeric)))

## FIT THE LASSO TO (SCALED) EPA AND THE CONTEXT VARIABLES
## TEST STANDARDIZING ONLY NON-BINARY PREDICTORS & RANDOMIZED LAMBDA SEQUENCE
library(glmnet)
library(tidyr)
library(broom)

cv_output <- cv.glmnet(x_train_matrix, y_train_matrix,
                       alpha = 1, lambda = grid, standardize = FALSE,
                       nfolds = 10)

best_lam <- cv_output$lambda.1se
best_lam

lasso_best <- glmnet(x_train_matrix, y_train_matrix, alpha = 1, lambda = best_lam, standardize = FALSE)
pred <- predict(lasso_best, s = best_lam, newx = x_test_matrix)

final <- cbind(y_test_matrix, pred)
head(final)
coef(lasso_best)



fit_lasso <- glmnet(lasso_context_matrix, lasso_epa_matrix, alpha = 1, lambda = grid)
summary(fit_lasso)

lasso_cv <- cv.glmnet(lasso_context_matrix, lasso_epa_matrix, alpha = 1, nfolds = 10, parallel = TRUE,
                      lambda = grid, standardize = FALSE)
best_lambda <- lasso_cv$lambda.1se
best_lambda

mse.min <- lasso_cv$cvm[lasso_cv$lambda == lasso_cv$lambda.1se]

best_fit <- lasso_cv$glmnet.fit
head(best_fit)

best_lasso_overall <- glmnet(lasso_context_matrix, lasso_epa_matrix, alpha = 1, lambda = best_lambda,
                             standardize = FALSE)
coef(best_lasso_overall)
tidy_lasso_coef <- tidy(best_lasso_overall)


context_regression <- lm(data=pbp_edit, epa ~ total_line + series + yardline_squared +
                           drive_play_count  + vegas_wp_squared)
summary(context_regression)
tidy_context_reg <- tidy(context_regression)


## LASSO SELECTED - YARDLINE_100, SHOTGUN, QTR, HOME, TOTAL_LINE, DRIVE_PLAY_COUNT, POSTEAM_TIMEOUTS,
## DEFTEAM_TIMEOUTS_REMAINING, YARDS_TIMES_DOWN, YARDLINE_SQUARED, VEGAS_WP_SQUARED, LOG_VEGAS_WP,
## SIDE_OF_FIELD_INDICATOR, YARDS_TO_GO_SQUARE
