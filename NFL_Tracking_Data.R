## NFL TRACKING DATA

library(RSQLite)
library(dplyr)
library(dbplyr)
library(DBI)
library(tidyverse)

## CREATE DATABASE

tracking_data <- RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite")

# Add tables from csv

# games file
games <- read_csv('/Users/dylanmervis/Google Drive/2021 BDB Data/games.csv')
dbWriteTable(tracking_data, "games", games, overwrite = TRUE)
rm(games)

# players file
players <- read_csv('/Users/dylanmervis/Google Drive/2021 BDB Data/players.csv')
dbWriteTable(tracking_data, "players", players, overwrite = TRUE)
rm(players)

# plays file 
# includes targeted receiver data not included in original data release
plays <- read_csv('/Users/dylanmervis/Google Drive/2021 BDB Data/plays.csv')
targeted_receivers <- read_csv('/Users/dylanmervis/Google Drive/2021 BDB Data/targetedReceiver.csv')
plays <- merge(plays, targeted_receivers, by = c("gameId", "playId"))

dbWriteTable(tracking_data, "plays", plays, overwrite = TRUE)
rm(plays)
rm(targeted_receivers)

# Check - list tables in a database
dbListTables(tracking_data)
# List columns in a table
dbListFields(tracking_data, "plays")


# tracking data, create table with week 1 data
tracking <- read_csv('/Users/dylanmervis/Google Drive/2021 BDB Data/week1.csv')

# Standardize tracking data so plays are always in direction of offense vs raw on-field coordinates.
tracking <- tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))
dbWriteTable(tracking_data, "tracking", tracking, overwrite = TRUE)
rm(tracking)

# Function to repeat process for Weeks 2-17
for (w in 2:17){
  #temporary dataframe used for reading week for given iteration
  tracking <- read_csv(paste0("/Users/dylanmervis/Google Drive/2021 BDB Data/week",w,".csv"),
                       col_types = cols())
  
  #Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
  tracking <- tracking %>%
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  
  #append to the sqlite db
  dbWriteTable(conn = tracking_data, name = "tracking", tracking, append=T, row.names=F)
  rm(tracking)
}

library(tidyverse)
library(gganimate)
library(cowplot)

## FUNCTION TO ANIMATE INDIVIDUAL PLAYS
animate_passing_play <- function(gameID, playID){
  
  #open the connection
  tracking_data <- RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite")
  
  query_tracking_result <- tbl(tracking_data, "tracking")
  query_play_result <- tbl(tracking_data, "plays")
  
  
  #get the data for only the play we are interested in
  single_play <- query_tracking_result %>% 
    dplyr::filter(playId == playID) %>% 
    dplyr::filter(gameId == gameID)  %>%
    collect()
  
  
  #get information about the play
  single_play_info <- query_play_result %>% 
    dplyr::filter(playId == playID) %>% 
    dplyr::filter(gameId == gameID)  %>%
    collect()
  
  plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(single_play_info$playDescription[1])))
  
  
  #disconnect the connection
  RSQLite::dbDisconnect(tracking_data)
  
  
  
  
  ## General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(single_play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(single_play$x, na.rm = TRUE) + 10, -1), 120)
  df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
  df_hash <- df_hash %>% filter(y < ymax, y > ymin)
  
  
  
  animate_play <- ggplot() +
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
    scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
    annotate("text", x = df_hash$x[df_hash$x < 55/2], 
             y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df_hash$x[df_hash$x > 55/2], 
             y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    geom_point(data = single_play, aes(x = (xmax-y), y = x, shape = team,
                                       fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
    geom_text(data = single_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    ylim(ymin, ymax) + 
    coord_fixed() +  
    theme_nothing() + 
    theme(plot.title = element_text()) +
    labs(title = plot_title) +
    transition_time(frameId)  +
    ease_aes('linear') + 
    NULL
  
  
  # animate_play
  
  
  ## Ensure timing of play matches 10 frames-per-second
  play.length.ex <- length(unique(single_play$frameId))
  animate(animate_play, fps = 10, nframe = play.length.ex)
  
  
}


# OPEN THE CONNECTION & CREATE DF
tracking_data <- dplyr::tbl(
  RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite"), "tracking")

df_tracking <- as.data.frame(tracking_data)

plays_data <- dplyr::tbl(
  RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite"), "plays")

df_plays <- as.data.frame(plays_data)

games_data <- dplyr::tbl(
  RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite"), "games")

df_games <- as.data.frame(games_data)

players_data <- dplyr::tbl(
  RSQLite::dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/2021 BDB Data/tracking_data.sqlite"), "players")

df_players <- as.data.frame(players_data)

# merging plays and tracking data
df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))
# merging games data to previously merged frame
df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))


## CALCULATE METRICS

# Defender distance at ball arrival
passArrivalEvents <- c('pass_outcome_caught',
                      'pass_arrived',
                      'pass_outcome_incomplete',
                      'pass_outcome_interception',
                      'pass_outcome_touchdown')


df_distanceToFootball <- df_merged %>%
  #determining side of ball
  mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)),
    
    #if either condition is true, offense
    "offense",
    #if neither condition is true, defense
    "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           visitorTeamAbbr,
                           homeTeamAbbr)) %>%
  
  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>%
  #checking if football reading is in frame
  mutate(footballInPlay = sum(displayName == "Football") > 0) %>%
  #using only frames with football marked; some plays its missing
  filter(footballInPlay) %>%
  #adding x and y location of football as columns
  mutate(xFootball = x[displayName == "Football"],
         yFootball = y[displayName == "Football"]) %>%
  #ungrouping
  ungroup() %>%
  #grouping by game and play
  group_by(gameId, playId) %>%
  #selecting frames that contain pass arrival events
  filter(event %in% passArrivalEvents) %>%
  #selecting first frame with in case there are multiple
  filter(frameId == min(frameId)) %>%
  #calculating distance to football
  mutate(
    distToFootballAtBallArrival = sqrt((x - xFootball) ^ 2 +
                                         (y - yFootball) ^ 2)
  )


# Average Distance to Football by Defender at Ball Arrival
#calculating the average distance to the football
averageDistToFootball <- df_distanceToFootball %>%   
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  #grouping by player's id
  group_by(nflId) %>%
  #taking mean of distance to football
  summarize(avgDistToFootballAtBallArrival = mean(distToFootballAtBallArrival)) %>%
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
set.seed(1)

averageDistToFootball  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     -avgDistToFootballAtBallArrival),
             avgDistToFootballAtBallArrival)) +
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  #labeling axis
  xlab('') +
  ylab("Avg Distance to Football at Pass Arrival") + 
  #flipping coordinates
  coord_flip() +
  #titling plot
  ggtitle("Avg Distance to Football at Pass Arrival by Player")


# Number of Plays as Closest Defender
numberOfPlaysClosestDefender <- df_distanceToFootball %>%   
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  #filtering for closest defender to ball
  filter(distToFootballAtBallArrival == min(distToFootballAtBallArrival)) %>%
  #ungrouping
  group_by(nflId) %>%
  summarize(numberOfPlaysAsClosestDefender = n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
set.seed(1)

numberOfPlaysClosestDefender  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     numberOfPlaysAsClosestDefender),
             numberOfPlaysAsClosestDefender)) +
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  #labeling axis
  xlab('') +
  ylab("# of Plays As closest Defender to Football at Pass Arrival") + 
  #flipping coordinates
  coord_flip() +
  #titling plot
  ggtitle("# of Plays As closest Defender to Ball at Pass Arrival By Player")


# Number of Plays as Closest Defender Per Play
numberOfPlaysClosestDefenderPerPlay <- df_distanceToFootball %>%   
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  #filtering for closest defender to ball
  mutate(isClosestDefender = distToFootballAtBallArrival == min(distToFootballAtBallArrival)) %>%
  #ungrouping
  ungroup() %>%
  #grouping by defender's id
  group_by(nflId) %>%
  #calculatign value of interest
  summarize(numberOfPlaysAsClosestDefenderPerPlay = sum(isClosestDefender) / n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
set.seed(1)

numberOfPlaysClosestDefenderPerPlay  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     numberOfPlaysAsClosestDefenderPerPlay), 
             numberOfPlaysAsClosestDefenderPerPlay)) +
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  #labeling axis
  xlab('') +
  ylab("# of Plays As Closest Defender to Football at Pass Arrival  Per Play") + 
  #flipping coordinates
  coord_flip() +
  #titling plot
  ggtitle("# of Plays As Closest Defender to Ball at Pass Arrival Per Play By Player")


# Completion Percentage Allowed as Closest Defender
completionPercentageAsClosest <- df_distanceToFootball %>%   
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId),
         #removing defensive PI
         !isDefensivePI) %>%
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  #filtering for closest defender to ball
  filter(distToFootballAtBallArrival == 
           min(distToFootballAtBallArrival)) %>%
  #ungrouping
  group_by(nflId) %>%
  summarize(compPercent = sum(passResult == "C") / n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
set.seed(1)

completionPercentageAsClosest  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     -compPercent), 
             compPercent)) +
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  #labeling axis
  xlab('') +
  ylab("Allowed Comp % As Closest Defender to Football at Pass Arrival") + 
  #changing to percentage scale
  scale_y_continuous(labels = scales::percent) +
  #flipping coordinates
  coord_flip() +
  #titling plot
  ggtitle("Allowed Comp % As Closest Defender to Ball at Pass Arrival By Player")
