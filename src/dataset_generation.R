# Date in English language
Sys.setlocale("LC_TIME", "C")
# Load packages
library(openxlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(stringi)

# Load data
# Metacritic rating data retrieved from metacritics.com
MC <- read.csv("../data/metacritic_data.csv")

# Game characteristic data retrieved from steam.store
df_char <- read.csv("../data/games.csv")

# Sales data retrieved from vgchartz.com
df_sales <- read.csv("../data/vgchartz_data.csv") 

###################################################################
################## METACRITIC DATA ###############################
###################################################################

# Selecting variables
MC_trimmed <- MC %>% 
  select(meta_game_name, meta_platform, meta_user_score, meta_critic_score, meta_esrb, meta_release_date)

# Creating numeric variables
MC_trimmed$meta_user_score <- as.numeric(MC_trimmed$meta_user_score)
MC_trimmed$meta_user_score <- MC_trimmed$meta_user_score*10
MC_trimmed$meta_critic_score <- as.numeric(MC_trimmed$meta_critic_score)

# Retrieving the dates by first adding a 0 for single digit day notations
MC_trimmed$meta_release_date <-gsub(" ", ",", MC_trimmed$meta_release_date)
MC_trimmed$meta_release_date <-sub(",", " ", MC_trimmed$meta_release_date)
MC_trimmed$meta_release_date <-gsub(",,", " ", MC_trimmed$meta_release_date)
MC_trimmed$meta_release_date <-sub(",", "0", MC_trimmed$meta_release_date)

MC_trimmed$meta_release_date <- as.Date(MC_trimmed$meta_release_date, format = "%b%d%Y")

# Creating string variables
MC_trimmed$meta_game_name <- str_trim(MC_trimmed$meta_game_name)
MC_trimmed$meta_platform <- str_trim(MC_trimmed$meta_platform)

# Filter and remove observation if user or critic score is missing
MC_trimmed_filter <- MC_trimmed %>% 
  filter(meta_user_score != 'N/A') %>% 
  filter(meta_critic_score != 'N/A')

# Change platform abbreviations to match with the other datasets
MC_trimmed_filter$meta_platform <- gsub('PSP', 'PlayStation Portable', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('PSV', 'PlayStation Vita', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('PSN', 'PlayStation Network ', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^PSP$', 'PlayStation Portable', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('PS', 'PlayStation ', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('X360', 'Xbox 360', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('XOne', 'Xbox One', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('XBL', 'Xbox Live', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('XB', 'Xbox', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('WiiU', 'Wii U', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('GG', 'Game Gear', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^NG$', 'Neo Geo', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^DSiW$', 'Nintendo DSiW', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^DSi$', 'Nintendo DSi', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^3DS$', 'Nintendo 3DS', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^DS$', 'Nintendo DS', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('GEN', 'Genesis', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('SCD', 'SEGA CD', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('Lynx', 'Atari Lynx', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('ApII', 'Apple II', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('N64', 'Nintendo 64', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^GBA$', 'Game Boy Advance', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^GBC$', 'Game Boy Color', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^GB$', 'Game Boy', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^GC$', 'GameCube', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- gsub('^SAT$', 'SEGA Saturn', MC_trimmed_filter$meta_platform)
MC_trimmed_filter$meta_platform <- str_trim(MC_trimmed_filter$meta_platform)

# Save and write the cleaned metacritic data to csv
write.csv(MC_trimmed_filter, "../gen/metacritic_data.csv")

###################################################################
######## Data cleaning for sales and characteristics data##########
###################################################################

# Assign correct classes and trim string for sales and characteristics data
df_char$ReleaseDate <- as.Date(df_char$ReleaseDate)
df_sales$global_sales <- str_sub(df_sales$global_sales, end=-2)
df_sales$global_sales <- as.numeric(df_sales$global_sales)
df_sales$na_sales <- str_sub(df_sales$na_sales, end=-2)
df_sales$eu_sales <- str_sub(df_sales$eu_sales, end=-2)
df_sales$jp_sales <- str_sub(df_sales$jp_sales, end=-2)
df_sales$other_sales <- str_sub(df_sales$other_sales, end=-2)
df_sales$na_sales <- as.numeric(df_sales$na_sales)
df_sales$eu_sales <- as.numeric(df_sales$eu_sales)
df_sales$jp_sales <- as.numeric(df_sales$jp_sales)
df_sales$other_sales <- as.numeric(df_sales$other_sales)

# All lower cases
names(df_sales) <- tolower(names(df_sales))
names(df_char) <- tolower(names(df_char))

# Splitting and trimming the platforms
df_char2 <- df_char %>% 
   mutate(platform = str_split(platform, ",")) %>% 
   unnest(platform)
 
df_char2$platform <- str_trim(df_char2$platform)
df_char2$presence <- as.numeric((df_char2$presence))
 
###################################################################
####### SEARCHING AND MATCHING PLATFORM NAMES #####################
###################################################################
 
# # Search for unique platforms and rename to make them matching (ps4 = PlayStation 4)
# 
# char2_platforms <- as.data.frame(unique(df_char2$platform))
# sales_platforms <- as.data.frame(unique(df_sales$platform))
# 
# char2_platforms <- char2_platforms %>% 
#   rename("platform" = "unique(df_char2$platform)")
# 
# sales_platforms <- sales_platforms %>% 
#   rename("platform" = "unique(df_sales$platform)")
# 
# df_platforms <- rbind(char2_platforms, sales_platforms)
# 
# df_platforms <- distinct(df_platforms)
# 
# # Renaming platforms
# df_platforms$platform <- gsub('PSN', 'PlayStation Network ', df_platforms$platform)
# df_platforms$platform <- gsub('PSP', 'PlayStation Portable', df_platforms$platform)
# df_platforms$platform <- gsub('PS', 'PlayStation ', df_platforms$platform)
# df_platforms$platform <- gsub('X360', 'Xbox 360', df_platforms$platform)
# df_platforms$platform <- gsub('XOne', 'Xbox One', df_platforms$platform)
# df_platforms$platform <- gsub('XBL', 'Xbox Live', df_platforms$platform)
# df_platforms$platform <- gsub('XB', 'Xbox', df_platforms$platform)
# df_platforms$platform <- gsub('WiiU', 'Wii U', df_platforms$platform)
# df_platforms$platform <- gsub('GG', 'Game Gear', df_platforms$platform)
# #df_platforms$platform <- gsub('PlayStation V', 'PlayStation 5', df_platforms$platform)
# df_platforms$platform <- gsub('GBA', 'Game Boy Advance', df_platforms$platform)
# df_platforms$platform <- gsub('GBC', 'Game Boy Color', df_platforms$platform)
# df_platforms$platform <- gsub('GB', 'Game Boy', df_platforms$platform)
# df_platforms$platform <- gsub('GC', 'GameCube', df_platforms$platform)
# df_platforms$platform <- gsub('SAT', 'SEGA Saturn', df_platforms$platform)
# # df_platforms$platform <- gsub('NGage', 'N Gage', df_platforms$platform)
# # df_platforms$platform <- gsub('NG', 'N Gage', df_platforms$platform)
# # df_platforms$platform <- gsub('N Gage', 'NGage', df_platforms$platform)
# df_platforms$platform <- gsub('DSiW', 'Nintendo DSIW', df_platforms$platform)
# df_platforms$platform <- gsub('Nintendo DSi', 'Nintendo DESi', df_platforms$platform)
# df_platforms$platform <- gsub('DSi', 'Nintendo DESi', df_platforms$platform)
# df_platforms$platform <- gsub('Nintendo DESi', 'Nintendo DSi', df_platforms$platform)
# df_platforms$platform <- gsub('Nintendo DSIW', 'DSiW', df_platforms$platform)
# df_platforms$platform <- gsub('GEN', 'Genesis', df_platforms$platform)
# df_platforms$platform <- gsub('SCD', 'SEGA CD', df_platforms$platform)
# # df_platforms$platform <- gsub(' Lynx', 'Lynx', df_platforms$platform)
# # df_platforms$platform <- gsub('AtariLynx', 'Lynx', df_platforms$platform)
# df_platforms$platform <- gsub('Lynx', 'Atari Lynx', df_platforms$platform)
# df_platforms$platform <- gsub('ApII', 'Apple II', df_platforms$platform)
# df_platforms$platform <- gsub('N64', 'Nintendo 64', df_platforms$platform)
 
# Renaming platforms

# Renaming platforms in df_char2
df_char2$platform <- gsub('PSP', 'PlayStation Portable', df_char2$platform)
df_char2$platform <- gsub('PS', 'PlayStation ', df_char2$platform)
df_char2$platform <- str_trim(df_char2$platform)

# Renaming platforms in df_sales
df_sales$platform <- gsub('PSV', 'PlayStation Vita', df_sales$platform)
df_sales$platform <- gsub('PSN', 'PlayStation Network ', df_sales$platform)
df_sales$platform <- gsub('^PSP$', 'PlayStation Portable', df_sales$platform)
df_sales$platform <- gsub('PS', 'PlayStation ', df_sales$platform)
df_sales$platform <- gsub('X360', 'Xbox 360', df_sales$platform)
df_sales$platform <- gsub('XOne', 'Xbox One', df_sales$platform)
df_sales$platform <- gsub('XBL', 'Xbox Live', df_sales$platform)
df_sales$platform <- gsub('XB', 'Xbox', df_sales$platform)
df_sales$platform <- gsub('WiiU', 'Wii U', df_sales$platform)
df_sales$platform <- gsub('GG', 'Game Gear', df_sales$platform)
df_sales$platform <- gsub('^NG$', 'Neo Geo', df_sales$platform)
df_sales$platform <- gsub('^DSiW$', 'Nintendo DSiW', df_sales$platform)
df_sales$platform <- gsub('^DSi$', 'Nintendo DSi', df_sales$platform)
df_sales$platform <- gsub('^3DS$', 'Nintendo 3DS', df_sales$platform)
df_sales$platform <- gsub('^DS$', 'Nintendo DS', df_sales$platform)
df_sales$platform <- gsub('GEN', 'Genesis', df_sales$platform)
df_sales$platform <- gsub('SCD', 'SEGA CD', df_sales$platform)
df_sales$platform <- gsub('Lynx', 'Atari Lynx', df_sales$platform)
df_sales$platform <- gsub('ApII', 'Apple II', df_sales$platform)
df_sales$platform <- gsub('N64', 'Nintendo 64', df_sales$platform)
df_sales$platform <- gsub('^GBA$', 'Game Boy Advance', df_sales$platform)
df_sales$platform <- gsub('^GBC$', 'Game Boy Color', df_sales$platform)
df_sales$platform <- gsub('^GB$', 'Game Boy', df_sales$platform)
df_sales$platform <- gsub('^GC$', 'GameCube', df_sales$platform)
df_sales$platform <- gsub('^SAT$', 'SEGA Saturn', df_sales$platform)
df_sales$platform <- str_trim(df_sales$platform)

##############################################################
############### CONTROL PLATFORMS   ##########################
##############################################################


# Search for unique platforms and rename to make them matching (ps4 = PlayStation 4)
# 
# char2_platforms <- as.data.frame(unique(df_char2$platform))
# sales_platforms <- as.data.frame(unique(df_sales$platform))
# 
# char2_platforms <- char2_platforms %>% 
#   rename("platform" = "unique(df_char2$platform)")
# 
# sales_platforms <- sales_platforms %>% 
#   rename("platform" = "unique(df_sales$platform)")
# 
# df_platforms <- rbind(char2_platforms, sales_platforms)
# 
# df_platforms <- distinct(df_platforms)
# 
# 
# df_platforms$platform <- str_trim(df_platforms$platform)
# df_platforms <- distinct(df_platforms)

##############################################################################
############ cOMBINE SALES AND CHARACTERISTICS DATASETS ######################
##############################################################################

df_char2$name <- str_trim(df_char2$name)
df_sales$name <- str_trim(df_sales$name)

df_combined <- df_char2 %>% 
  inner_join(df_sales, by = c("name", "platform"))

###############################################################################
##################### COMPILE TRIMMED DATASET #################################
###############################################################################

# df_com_trimmed <- df_combined %>% 
#   select(name, genres, indie, presence, platform, ratingsbreakdown, releasedate, soundtrack, franchise, originalcost, players, esrb, publisher.y, global_sales) %>% 
#   rename(global_sales_m = global_sales,
#          publisher = publisher.y)
#   
# df_com_trimmed <- df_com_trimmed %>% 
#   filter(global_sales_m != 'N/A') %>% 
#   filter(global_sales_m != '0.00')

##############################################################################
########### INCORPORATE CRITIC AND USER SCORE ###############################
#############################################################################

# Read the metacritic data
df_mc <- read.csv("../gen/metacritic_data.csv")

df_mc$meta_release_date <- as.Date(df_mc$meta_release_date)

# Combine the metacritic data with the previous combined data of the characteristics and sales
df_combined_user_score <- df_combined %>% 
  inner_join(df_mc, by = c("name" = "meta_game_name", "platform" = "meta_platform")) %>% 
  select(-c(rawgid, discountedcost, publisher.x, rank, X, x.y, total_shipped, x.x, id, genres, releasedate)) %>% 
  rename(global_sales_m = global_sales,
         na_sales_m = na_sales,
         eu_sales_m = eu_sales,
         jp_sales_m = jp_sales,
         other_sales_m = other_sales,
         publisher = publisher.y)


# Filter out and remove observation with no global sales
df_user_score_trimmed <- df_combined_user_score %>% 
  filter(global_sales_m != 'N/A') %>% 
  filter(global_sales_m != '0.00')

df_user_score_trimmed <- df_user_score_trimmed[!duplicated(df_user_score_trimmed[c("name","platform")]),]

# Reorder the dataset
df_games_complete <- df_user_score_trimmed[,c("name", "global_sales_m", "na_sales_m", "eu_sales_m", "jp_sales_m", "other_sales_m", "meta_critic_score", "meta_user_score", "platform", "publisher", "developer", "game_genre", "franchise", "originalcost", "meta_release_date", "players", "controller", "meta_esrb", "indie", "soundtrack", "languages", "description", "tags", "presence")]

################################################################################
############# CREATING DUMMIES FOR ESRB RATING AND FRANCHISE ###################
################################################################################

# Dummy creation for franchise
df_games_complete$franchise <- ifelse(df_games_complete$franchise == "", 0, 1)

# Dummy creation for esrb rating
df_games_complete$esrb_everyone <- ifelse(df_games_complete$meta_esrb == "E10+", 1, ifelse(df_games_complete$meta_esrb == "E", 1, 0))
df_games_complete$esrb_teens <- ifelse(df_games_complete$meta_esrb == "T", 1, 0)
df_games_complete$esrb_adults <- ifelse(df_games_complete$meta_esrb == "AO", 1, ifelse(df_games_complete$meta_esrb == "M", 1, 0))
df_games_complete$esrb_pending <- ifelse(df_games_complete$meta_esrb == "RP", 1, 0)

###############################################################################
######################## CREATING DUMMIES FOR PLAYERS #########################
###############################################################################

# # Separate the players options into separate columns
# df_games_complete <- df_games_complete %>% 
#   separate(players, c("players1", "players2", "players3", "players4", "players5"), sep = ",")
# 
# # Removing spaces
# df_games_complete$players2 <- str_replace_all(df_games_complete$players2, " ", "")
# df_games_complete$players3 <- str_replace_all(df_games_complete$players3, " ", "")
# df_games_complete$players4 <- str_replace_all(df_games_complete$players4, " ", "")
# df_games_complete$players5 <- str_replace_all(df_games_complete$players5, " ", "")
# 
# # Create dummies for singleplayer
# df_games_complete$players1_si <- ifelse(df_games_complete$players1 == "singleplayer", 1, 0)
# df_games_complete$players2_si <- ifelse(df_games_complete$players2 == "singleplayer", 1, 0)
# df_games_complete$players3_si <- ifelse(df_games_complete$players3 == "singleplayer", 1, 0)
# df_games_complete$players4_si <- ifelse(df_games_complete$players4 == "singleplayer", 1, 0)
# df_games_complete$players5_si <- ifelse(df_games_complete$players5 == "singleplayer", 1, 0)
# 
# # Combining the singleplayers dummies into one column
# df_games_complete$singleplayer <- ifelse(df_games_complete$players1_si | df_games_complete$players2_si | df_games_complete$players3_si | df_games_complete$players4_si | df_games_complete$players5_si == "1", 1, 0)
# 
# # Create dummies for multiplayer
# df_games_complete$players1_mp <- ifelse(df_games_complete$players1 == "multiplayer", 1, 0)
# df_games_complete$players2_mp <- ifelse(df_games_complete$players2 == "multiplayer", 1, 0)
# df_games_complete$players3_mp <- ifelse(df_games_complete$players3 == "multiplayer", 1, 0)
# df_games_complete$players4_mp <- ifelse(df_games_complete$players4 == "multiplayer", 1, 0)
# df_games_complete$players5_mp <- ifelse(df_games_complete$players5 == "multiplayer", 1, 0)
# 
# # Combining the multiplayers dummies into one column
# df_games_complete$multiplayer <- ifelse(df_games_complete$players1_mp | df_games_complete$players2_mp | df_games_complete$players3_mp | df_games_complete$players4_mp | df_games_complete$players5_mp == "1", 1, 0)
# 
# # Create dummies for coop
# df_games_complete$players1_co <- ifelse(df_games_complete$players1 == "coop", 1, 0)
# df_games_complete$players2_co <- ifelse(df_games_complete$players2 == "coop", 1, 0)
# df_games_complete$players3_co <- ifelse(df_games_complete$players3 == "coop", 1, 0)
# df_games_complete$players4_co <- ifelse(df_games_complete$players4 == "coop", 1, 0)
# df_games_complete$players5_co <- ifelse(df_games_complete$players5 == "coop", 1, 0)
# 
# # Combining the coop dummies into one column
# df_games_complete$coop <- ifelse(df_games_complete$players1_co | df_games_complete$players2_co | df_games_complete$players3_co | df_games_complete$players4_co | df_games_complete$players5_co == "1", 1, 0)
# 
# # Create dummies for online coop
# df_games_complete$players1_oc <- ifelse(df_games_complete$players1 == "online coop", 1, 0)
# df_games_complete$players2_oc <- ifelse(df_games_complete$players2 == "online coop", 1, 0)
# df_games_complete$players3_oc <- ifelse(df_games_complete$players3 == "online coop", 1, 0)
# df_games_complete$players4_oc <- ifelse(df_games_complete$players4 == "online coop", 1, 0)
# df_games_complete$players5_oc <- ifelse(df_games_complete$players5 == "online coop", 1, 0)
# 
# # Combining the online coop dummies into one column
# df_games_complete$o_coop <- ifelse(df_games_complete$players1_co | df_games_complete$players2_co | df_games_complete$players3_co | df_games_complete$players4_co | df_games_complete$players5_co == "1", 1, 0)
# 
# # Create dummies for pvp
# df_games_complete$players1_pvp <- ifelse(df_games_complete$players1 == "pvp", 1, 0)
# df_games_complete$players2_pvp <- ifelse(df_games_complete$players2 == "pvp", 1, 0)
# df_games_complete$players3_pvp <- ifelse(df_games_complete$players3 == "pvp", 1, 0)
# df_games_complete$players4_pvp <- ifelse(df_games_complete$players4 == "pvp", 1, 0)
# df_games_complete$players5_pvp <- ifelse(df_games_complete$players5 == "pvp", 1, 0)
# 
# # Combining the pvp dummies into one column
# df_games_complete$pvp <- ifelse(df_games_complete$players1_pvp | df_games_complete$players2_pvp | df_games_complete$players3_pvp | df_games_complete$players4_pvp | df_games_complete$players5_pvp == "1", 1, 0)
# 
# 
# # Remove obsolete column which were use for creating dummies
# df_games_complete <- df_games_complete %>% 
#   select(-c("players1_si", "players2_si", "players4_si", "players3_si", "players5_si", 
#             "players5_mp", "players4_mp", "players3_mp", "players2_mp", "players1_mp", 
#             "players5_co", "players4_co", "players3_co", "players2_co", "players1_co",
#             "players5_oc", "players4_oc", "players3_oc", "players2_oc", "players1_oc",
#             "players5_pvp", "players4_pvp", "players3_pvp", "players2_pvp", "players1_pvp",
#             "players5", "players4", "players3", "players2", "players1"))
# 
# # Replace na with zeros for each of the player options
# df_games_complete$singleplayer <- df_games_complete$singleplayer %>% 
#   replace_na(0)
# 
# df_games_complete$multiplayer <- df_games_complete$multiplayer %>% 
#   replace_na(0)
# 
# df_games_complete$coop <- df_games_complete$coop %>% 
#   replace_na(0)
# 
# df_games_complete$o_coop <- df_games_complete$o_coop %>% 
#   replace_na(0)
# 
# df_games_complete$pvp <- df_games_complete$pvp %>% 
#   replace_na(0)


###############################################################################
######################## CREATING DUMMIES FOR PLAYERS #########################
###############################################################################

# Separate the players options into separate columns
df_games_complete <- df_games_complete %>% 
  separate(players, c("players1", "players2", "players3", "players4", "players5"), sep = ",")

# Removing spaces
df_games_complete$players2 <- str_replace_all(df_games_complete$players2, " ", "")
df_games_complete$players3 <- str_replace_all(df_games_complete$players3, " ", "")
df_games_complete$players4 <- str_replace_all(df_games_complete$players4, " ", "")
df_games_complete$players5 <- str_replace_all(df_games_complete$players5, " ", "")

# Create dummies for singleplayer
df_games_complete$players1_si <- ifelse(df_games_complete$players1 == "singleplayer", 1, 0)
df_games_complete$players2_si <- ifelse(df_games_complete$players2 == "singleplayer", 1, 0)
df_games_complete$players3_si <- ifelse(df_games_complete$players3 == "singleplayer", 1, 0)
df_games_complete$players4_si <- ifelse(df_games_complete$players4 == "singleplayer", 1, 0)
df_games_complete$players5_si <- ifelse(df_games_complete$players5 == "singleplayer", 1, 0)

# Combining the singleplayers dummies into one column
df_games_complete$singleplayer <- ifelse(df_games_complete$players1_si | df_games_complete$players2_si | df_games_complete$players3_si | df_games_complete$players4_si | df_games_complete$players5_si == "1", 1, 0)

# Create dummies for multiplayer
df_games_complete$players1_mp <- ifelse(df_games_complete$players1 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, 0)))
df_games_complete$players2_mp <- ifelse(df_games_complete$players2 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, 0)))
df_games_complete$players3_mp <- ifelse(df_games_complete$players3 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, 0)))
df_games_complete$players4_mp <- ifelse(df_games_complete$players4 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, 0)))
df_games_complete$players5_mp <- ifelse(df_games_complete$players5 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, 0)))

# Combining the multiplayers dummies into one column
df_games_complete$multiplayer <- ifelse(df_games_complete$players1_mp | df_games_complete$players2_mp | df_games_complete$players3_mp | df_games_complete$players4_mp | df_games_complete$players5_mp == "1", 1, 0)

# Create dummies for coop
df_games_complete$players1_co <- ifelse(df_games_complete$players1 == "coop", 1, 0)
df_games_complete$players2_co <- ifelse(df_games_complete$players2 == "coop", 1, 0)
df_games_complete$players3_co <- ifelse(df_games_complete$players3 == "coop", 1, 0)
df_games_complete$players4_co <- ifelse(df_games_complete$players4 == "coop", 1, 0)
df_games_complete$players5_co <- ifelse(df_games_complete$players5 == "coop", 1, 0)

# Combining the coop dummies into one column
df_games_complete$coop <- ifelse(df_games_complete$players1_co | df_games_complete$players2_co | df_games_complete$players3_co | df_games_complete$players4_co | df_games_complete$players5_co == "1", 1, 0)

# # Create dummies for online coop
# df_games_complete$players1_oc <- ifelse(df_games_complete$players1 == "online coop", 1, 0)
# df_games_complete$players2_oc <- ifelse(df_games_complete$players2 == "online coop", 1, 0)
# df_games_complete$players3_oc <- ifelse(df_games_complete$players3 == "online coop", 1, 0)
# df_games_complete$players4_oc <- ifelse(df_games_complete$players4 == "online coop", 1, 0)
# df_games_complete$players5_oc <- ifelse(df_games_complete$players5 == "online coop", 1, 0)
# 
# # Combining the online coop dummies into one column
# df_games_complete$o_coop <- ifelse(df_games_complete$players1_co | df_games_complete$players2_co | df_games_complete$players3_co | df_games_complete$players4_co | df_games_complete$players5_co == "1", 1, 0)
# 
# # Create dummies for pvp
# df_games_complete$players1_pvp <- ifelse(df_games_complete$players1 == "pvp", 1, 0)
# df_games_complete$players2_pvp <- ifelse(df_games_complete$players2 == "pvp", 1, 0)
# df_games_complete$players3_pvp <- ifelse(df_games_complete$players3 == "pvp", 1, 0)
# df_games_complete$players4_pvp <- ifelse(df_games_complete$players4 == "pvp", 1, 0)
# df_games_complete$players5_pvp <- ifelse(df_games_complete$players5 == "pvp", 1, 0)
# 
# # Combining the pvp dummies into one column
# df_games_complete$pvp <- ifelse(df_games_complete$players1_pvp | df_games_complete$players2_pvp | df_games_complete$players3_pvp | df_games_complete$players4_pvp | df_games_complete$players5_pvp == "1", 1, 0)


# Remove obsolete column which were use for creating dummies
df_games_complete <- df_games_complete %>% 
  select(-c("players1_si", "players2_si", "players4_si", "players3_si", "players5_si", 
            "players5_mp", "players4_mp", "players3_mp", "players2_mp", "players1_mp", 
            "players5_co", "players4_co", "players3_co", "players2_co", "players1_co",
            "players5", "players4", "players3", "players2", "players1"))

# Replace na with zeros for each of the player options
df_games_complete$singleplayer <- df_games_complete$singleplayer %>% 
  replace_na(0)

df_games_complete$multiplayer <- df_games_complete$multiplayer %>% 
  replace_na(0)

df_games_complete$coop <- df_games_complete$coop %>% 
  replace_na(0)

# df_games_complete$o_coop <- df_games_complete$o_coop %>% 
#   replace_na(0)
# 
# df_games_complete$pvp <- df_games_complete$pvp %>% 
#   replace_na(0)

#############################################################################
################### DATA PREP ORIGINIAL COSTS ###############################
#############################################################################

# Change "Free To Play" to all lower cases
df_games_complete$originalcost <- df_games_complete$originalcost %>% 
  tolower()

# Change free to play to $0, remove $ and make it numeric
df_games_complete$originalcost[df_games_complete$originalcost == "free to play"] <- "$0"
df_games_complete$originalcost <- sub(".", "", df_games_complete$originalcost)
df_games_complete$originalcost <- as.numeric(df_games_complete$originalcost)

# Rename column to keep the $ information
df_games_complete <- df_games_complete %>% 
  rename("originalcost_$" = originalcost)

##############################################################################
#################### CONTROLLING FOR PUBLISHER SIZE ##########################
##############################################################################

# Extracting unique publishers from the sales dataset (vgchartz)
unique_pub <- df_sales %>% 
  distinct(name, .keep_all = TRUE)

# Count number of unique publishers
num_pub <- unique_pub %>% 
  group_by(publisher) %>% 
  summarize(num_g_pub=n())

# Creating dummies for small, medium or large publishers based on the number of published games
# dum_pub <- num_pub %>%   
#   mutate(small_pub = ifelse(num_pub$count <= 5, 1, 0)) %>% 
#   mutate(medium_pub = ifelse(num_pub$count >5 & num_pub$count <15,1,0)) %>% 
#   mutate(large_pub = ifelse(num_pub$count >= 15, 1, 0)) %>% 
#   select(-count)

# Adding the publisher dummies to the df_games_complete dataset
df_games_complete <- df_games_complete %>% 
  left_join(num_pub, by = "publisher")

##############################################################################
####################### LANGUAGES ############################################
##############################################################################

# Counting number of language options
df_games_complete$num_languages <- lengths(gregexpr("\\W+", df_games_complete$languages)) + 1
df_games_complete$num_languages <- as.numeric(df_games_complete$num_languages)

#############################################################################
################################ GENRE ######################################
#############################################################################

## Search for all unique genres
#unique_genre <- as.data.frame(unique(df_games_complete$game_genre))

# Creating dummies for genres
df_games_complete$gen_action <- ifelse(df_games_complete$game_genre == "Action", 1, ifelse(df_games_complete$game_genre == "Shooter", 1, ifelse(df_games_complete$game_genre == "Platform", 1, ifelse(df_games_complete$game_genre == "Action-Adventure", 1, ifelse(df_games_complete$game_genre == "Fighting", 1,0)))))
df_games_complete$gen_adventure <- ifelse(df_games_complete$game_genre == "Adventure", 1, ifelse(df_games_complete$game_genre == "Visual+Novel", 1, 0))
df_games_complete$gen_racing <- ifelse(df_games_complete$game_genre == "Racing", 1,0)
df_games_complete$gen_roleplaying <- ifelse(df_games_complete$game_genre == "Role-Playing", 1,0)
df_games_complete$gen_simulation <- ifelse(df_games_complete$game_genre == "Simulation", 1,0)
df_games_complete$gen_sports <- ifelse(df_games_complete$game_genre == "Sports", 1,0)
df_games_complete$gen_strategy <- ifelse(df_games_complete$game_genre == "Strategy", 1,0)
df_games_complete$gen_misc <- ifelse(df_games_complete$game_genre == "Misc", 1, ifelse(df_games_complete$game_genre == "Puzzle", 1, ifelse(df_games_complete$game_genre == "Music", 1, 0)))
df_games_complete$gen_mmo <- ifelse(df_games_complete$game_genre == "MMO", 1,0)

###############################################################################
####################### CREATE PLATFORM DUMMIES ###############################
###############################################################################

# unique_platform <- as.data.frame((unique(df_games_complete$platform)))

df_games_complete$plf_pc <- ifelse(df_games_complete$platform == "PC", 1, 0)
df_games_complete$plf_con <- ifelse(df_games_complete$platform != "PC", 1,0)

df_games_complete$plf_pc <- as.numeric(df_games_complete$plf_pc)
df_games_complete$plf_con <- as.numeric(df_games_complete$plf_con)

###############################################################################
####################### RE-ORDER, SAVING AND WRITING CSV ######################
###############################################################################

# Re-order the dataset
fin_games_dataset <- df_games_complete[,c("name", "global_sales_m", "na_sales_m", "eu_sales_m", "jp_sales_m", "other_sales_m", "meta_critic_score", "meta_user_score", "platform", "plf_con", "plf_pc", "publisher", "developer", "num_g_pub", "game_genre", "gen_action", "gen_adventure", "gen_racing", "gen_roleplaying", "gen_simulation", "gen_sports", "gen_strategy", "gen_mmo", "gen_misc", "franchise", "originalcost_$", "meta_release_date", "singleplayer", "multiplayer", "coop", "controller", "meta_esrb", "esrb_everyone", "esrb_teens", "esrb_adults", "esrb_pending", "indie", "soundtrack", "presence", "languages", "num_languages")]

# Read platform release dates dataset
platform_year <- read.xlsx("../data/platform_release_dates.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
                           rowNames = FALSE, detectDates = TRUE, skipEmptyRows = TRUE,
                           skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                           namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

# Add the platform release dates to the existing dataset
fin_games_dataset <- fin_games_dataset %>% 
  left_join(platform_year, by = "platform")

fin_games_dataset$meta_release_date <- as.Date(fin_games_dataset$meta_release_date, format = "%b-%d-%Y")

fin_games_dataset <- fin_games_dataset %>% 
  mutate(release_diff_days = meta_release_date - platform_release_date)

fin_games_dataset$release_diff_days <- gsub("days", "", fin_games_dataset$release_diff_days)
fin_games_dataset$release_diff_days <- as.numeric(fin_games_dataset$release_diff_days)
fin_games_dataset$release_diff_days <- ifelse(fin_games_dataset$release_diff_days < 0, "0", fin_games_dataset$release_diff_days)

fin_games_dataset$num_g_pub <- as.numeric(fin_games_dataset$num_g_pub)
fin_games_dataset$gen_action <- as.numeric(fin_games_dataset$gen_action)
fin_games_dataset$gen_adventure <- as.numeric(fin_games_dataset$gen_adventure)
fin_games_dataset$gen_racing <- as.numeric(fin_games_dataset$gen_racing)
fin_games_dataset$gen_roleplaying <- as.numeric(fin_games_dataset$gen_roleplaying)
fin_games_dataset$gen_simulation <- as.numeric(fin_games_dataset$gen_simulation)
fin_games_dataset$gen_sports <- as.numeric(fin_games_dataset$gen_sports)
fin_games_dataset$gen_strategy <- as.numeric(fin_games_dataset$gen_strategy)
fin_games_dataset$gen_mmo <- as.numeric(fin_games_dataset$gen_mmo)
fin_games_dataset$gen_misc <- as.numeric(fin_games_dataset$gen_misc)
fin_games_dataset$`originalcost_$` <- as.numeric(fin_games_dataset$`originalcost_$`)
fin_games_dataset$singleplayer <- as.numeric(fin_games_dataset$singleplayer)
fin_games_dataset$multiplayer <- as.numeric(fin_games_dataset$multiplayer)
fin_games_dataset$coop <- as.numeric(fin_games_dataset$coop)
# fin_games_dataset$o_coop <- as.numeric(fin_games_dataset$o_coop)
# fin_games_dataset$pvp <- as.numeric(fin_games_dataset$pvp)
fin_games_dataset$controller <- as.numeric(fin_games_dataset$controller)
fin_games_dataset$esrb_everyone <- as.numeric(fin_games_dataset$esrb_everyone)
fin_games_dataset$esrb_teens <- as.numeric(fin_games_dataset$esrb_teens) 
fin_games_dataset$esrb_adults <- as.numeric(fin_games_dataset$esrb_adults)
fin_games_dataset$esrb_pending <- as.numeric(fin_games_dataset$esrb_pending)
fin_games_dataset$indie <- as.numeric(fin_games_dataset$indie)
fin_games_dataset$soundtrack <- as.numeric(fin_games_dataset$soundtrack)
fin_games_dataset$presence <- as.numeric(fin_games_dataset$presence)
fin_games_dataset$num_languages <- as.numeric(fin_games_dataset$num_languages)
fin_games_dataset$release_diff_days <- as.numeric(fin_games_dataset$release_diff_days)


 

write.csv(fin_games_dataset, "../gen/full_videogame_dataset.csv", row.names = FALSE)

#################################################################################
############################ DESCRIPTIVE STATISTICS #############################
#################################################################################

summary(fin_games_dataset$global_sales_m)

summary(fin_games_dataset$meta_critic_score)

summary(fin_games_dataset$meta_user_score)

summary(fin_games_dataset$`originalcost_$`)

summary(fin_games_dataset$meta_release_date)

unique_platform <- fin_games_dataset %>% 
  distinct(platform, .keep_all = TRUE)

#################################################################################
############################ REGRESSION ANALYSES ################################
#################################################################################

