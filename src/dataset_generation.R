# Date in English language
Sys.setlocale("LC_TIME", "C")

# Load packages
library(openxlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(stringi)

# Creating a folder
folder <- "../gen"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}

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
##################### Renaming platforms ##########################
###################################################################

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


##############################################################################
############ cOMBINE SALES AND CHARACTERISTICS DATASETS ######################
##############################################################################

df_char2$name <- str_trim(df_char2$name)
df_sales$name <- str_trim(df_sales$name)

df_combined <- df_char2 %>% 
  inner_join(df_sales, by = c("name", "platform"))


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
  filter(global_sales_m != 0.00)

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

# Separate the players options into separate columns
df_games_complete <- df_games_complete %>%
  separate(players, c("players1", "players2", "players3", "players4", "players5"), sep = ",", remove = FALSE)

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
df_games_complete$players1_mp <- ifelse(df_games_complete$players1 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, ifelse(df_games_complete$players1 == "coop", 1, 0))))
df_games_complete$players2_mp <- ifelse(df_games_complete$players2 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, ifelse(df_games_complete$players2 == "coop", 1, 0))))
df_games_complete$players3_mp <- ifelse(df_games_complete$players3 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, ifelse(df_games_complete$players3 == "coop", 1, 0))))
df_games_complete$players4_mp <- ifelse(df_games_complete$players4 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, ifelse(df_games_complete$players4 == "coop", 1, 0))))
df_games_complete$players5_mp <- ifelse(df_games_complete$players5 == "multiplayer", 1, ifelse(df_games_complete$players1 == "online coop", 1, ifelse(df_games_complete$players1 == "pvp", 1, ifelse(df_games_complete$players5 == "coop", 1, 0))))

# Combining the multiplayers dummies into one column
df_games_complete$multiplayer <- ifelse(df_games_complete$players1_mp | df_games_complete$players2_mp | df_games_complete$players3_mp | df_games_complete$players4_mp | df_games_complete$players5_mp == "1", 1, 0)


# Remove obsolete column which were use for creating dummies
df_games_complete <- df_games_complete %>%
  select(-c("players1_si","players2_si", "players3_si", "players4_si", "players5_si", "players5_mp", "players4_mp", "players3_mp", "players2_mp", "players1_mp",
            "players5", "players4", "players3", "players2", "players1"))

# Replace na with zeros for each of the player options
df_games_complete$singleplayer <- df_games_complete$singleplayer %>%
  replace_na(0)

df_games_complete$multiplayer <- df_games_complete$multiplayer %>%
  replace_na(0)

df_games_complete$SP.MP <- ifelse(df_games_complete$singleplayer == "1" & df_games_complete$multiplayer == "1", 1, 0)
df_games_complete$OSIN <- ifelse(df_games_complete$singleplayer == "1" & df_games_complete$multiplayer == "0", 1, 0)
df_games_complete$OMUL <- ifelse(df_games_complete$singleplayer == "0" & df_games_complete$multiplayer == "1", 1, 0)


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
  summarise(num_g_pub=n())


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

# Creating dummies for genres
df_games_complete$gen_action <- ifelse(df_games_complete$game_genre == "Action", 1, ifelse(df_games_complete$game_genre == "Shooter", 1,  ifelse(df_games_complete$game_genre == "Action-Adventure", 1, ifelse(df_games_complete$game_genre == "Fighting", 1, ifelse(df_games_complete$game_genre == "Adventure", 1, 0)))))
df_games_complete$gen_RPG <- ifelse(df_games_complete$game_genre == "Role-Playing", 1, ifelse(df_games_complete$game_genre == "MMO", 1,0))
df_games_complete$gen_sports <- ifelse(df_games_complete$game_genre == "Sports", 1,ifelse(df_games_complete$game_genre == "Racing", 1,0))
df_games_complete$gen_misc <- ifelse(df_games_complete$game_genre == "Misc", 1, ifelse(df_games_complete$game_genre == "Puzzle", 1, ifelse(df_games_complete$game_genre == "Music", 1, ifelse(df_games_complete$game_genre == "Simulation", 1,ifelse(df_games_complete$game_genre == "Strategy", 1,ifelse(df_games_complete$game_genre == "Visual+Novel", 1, ifelse(df_games_complete$game_genre == "Platform", 1, 0 )))))))


###############################################################################
####################### CREATE PLATFORM DUMMIES ###############################
###############################################################################

# unique_platform <- as.data.frame((unique(df_games_complete$platform)))

df_games_complete$plf_pc <- ifelse(df_games_complete$platform == "PC", 1, 0)

df_games_complete$plf_pc <- as.numeric(df_games_complete$plf_pc)

###############################################################################
####################### RE-ORDER, SAVING AND WRITING CSV ######################
###############################################################################

# Re-order the dataset
fin_games_dataset <- df_games_complete[,c("name", "global_sales_m", "meta_user_score", "meta_critic_score",  "platform", "plf_pc", "publisher", "developer", "num_g_pub", "game_genre", "gen_action", "gen_sports", "gen_RPG", "gen_misc", "franchise", "originalcost_$", "meta_release_date","players", "OSIN", "OMUL", "SP.MP", "meta_esrb", "esrb_everyone", "esrb_teens", "esrb_adults", "esrb_pending", "indie", "soundtrack", "presence", "languages", "num_languages")]

fin_games_dataset$num_g_pub <- as.numeric(fin_games_dataset$num_g_pub)
fin_games_dataset$gen_action <- as.numeric(fin_games_dataset$gen_action)
fin_games_dataset$gen_sports <- as.numeric(fin_games_dataset$gen_sports)
fin_games_dataset$gen_RPG <- as.numeric(fin_games_dataset$gen_RPG)
fin_games_dataset$gen_misc <- as.numeric(fin_games_dataset$gen_misc)
fin_games_dataset$`originalcost_$` <- as.numeric(fin_games_dataset$`originalcost_$`)
fin_games_dataset$OSIN <- as.numeric(fin_games_dataset$OSIN)
fin_games_dataset$OMUL <- as.numeric(fin_games_dataset$OMUL)
fin_games_dataset$SP.MP <- as.numeric(fin_games_dataset$SP.MP)
fin_games_dataset$esrb_everyone <- as.numeric(fin_games_dataset$esrb_everyone)
fin_games_dataset$esrb_teens <- as.numeric(fin_games_dataset$esrb_teens) 
fin_games_dataset$esrb_adults <- as.numeric(fin_games_dataset$esrb_adults)
fin_games_dataset$esrb_pending <- as.numeric(fin_games_dataset$esrb_pending)
fin_games_dataset$indie <- as.numeric(fin_games_dataset$indie)
fin_games_dataset$soundtrack <- as.numeric(fin_games_dataset$soundtrack)
fin_games_dataset$presence <- as.numeric(fin_games_dataset$presence)
fin_games_dataset$num_languages <- as.numeric(fin_games_dataset$num_languages)

vg_data <- fin_games_dataset %>% 
  rename(USV = global_sales_m,
         CONREV = meta_user_score,
         PROFREV = meta_critic_score,
         GEN_AC.AD = gen_action,
         GEN_SP = gen_sports,
         GEN_RPG = gen_RPG,
         GEN_MI = gen_misc,
         OPUSD = "originalcost_$",
         FRANC = franchise,
         ESRB_RP = esrb_pending,
         ESRB_E = esrb_everyone,
         ESRB_T = esrb_teens,
         ESRB_M = esrb_adults,
         PUBS = num_g_pub,
         N_LAN = num_languages,
         INDIE = indie,
         SMPRES = presence,
         PLF_PC = plf_pc)

write.csv(vg_data, "../gen/full_videogame_dataset.csv", row.names = FALSE)

