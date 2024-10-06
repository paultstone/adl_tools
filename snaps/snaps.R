# ADL Percent of Snaps for Rostered Players
#
# This is a work-in-progress version of snaps where obtaining the roster
# information from MFL is offloaded to a module. The next thing which needs to
# happen is for the main code to be converted to a function, so that snaps for
# all teams can be obtained more easily. Currently, still only getting snaps
# for a single team.
#

library(ffscrapr)
library(dplyr)
library(tidyr)
library(stringr)
library(nflreadr)
library(modules)

# alternate names for certain players

# ADL name
from_name <- c(
  "Da'Ron Payne",
  "Trevon Moehrig",
  "Tariq Woolen",
  "Chauncey Gardner-Johnson",
  "Jeffery Wilson",
  "Jayson Oweh",
  "Davon Hamilton",
  "Joe Tryon",
  "Nate Landman",
  "Shaq Barrett",
  "Demarcus Lawrence",
  "Decobie Durant",
  "Joshua Palmer",
  "Dee Alford",
  "Foley Fatukasi"
)

# Snaps DB name
to_name <- c(
  "Daron Payne",
  "Tre'von Moehrig",
  "Riq Woolen",
  "CJ Gardner-Johnson",
  "Jeff Wilson",
  "Odafe Oweh",
  "DaVon Hamilton",
  "Joe Tryon-Shoyinka",
  "Nathan Landman",
  "Shaquil Barrett",
  "DeMarcus Lawrence",
  "Cobie Durant",
  "Josh Palmer",
  "DeAundre Alford",
  "Folorunso Fatukasi"
)

alt_names <- data.frame(from_name, to_name)

# create lookup vector
get_alt_name <- alt_names$to_name
names(get_alt_name) <- alt_names$from_name 

# Get the path to the adl_rosters module and import it
adl_rosters_module = ifelse(basename(getwd())=="adl_tools",
  "lib/adl_rosters.R", 
  "../lib/adl_rosters.R")
adl <- use(adl_rosters_module)

nfc_franchises <- c("Dallas Cowboys", "New York Giants", 
                    "Philadelphia Eagles", "Washington Commanders", 
                    "Chicago Bears", "Detroit Lions", 
                    "Green Bay Packers", "Minnesota Vikings", 
                    "Atlanta Falcons", "Carolina Panthers", 
                    "New Orleans Saints", "Tampa Bay Buccaneers", 
                    "Arizona Cardinals", "Los Angeles Rams", 
                    "San Francisco 49ers", "Seattle Seahawks")

afc_franchises <- c("Buffalo Bills", "Miami Dolphins", "New England Patriots", 
                    "New York Jets", "Baltimore Ravens", "Cincinnati Bengals", 
                    "Cleveland Browns", "Pittsburgh Steelers", "Houston Texans", 
                    "Indianapolis Colts", "Jacksonville Jaguars", "Tennessee Titans", 
                    "Denver Broncos", "Kansas City Chiefs", "Las Vegas Raiders", 
                    "Los Angeles Chargers")




current_year = format(Sys.Date(), "%Y")

adl_connection <- 
  mfl_connect(
    season = current_year, 
    league_id = 60206,
    rate_limit_number = 3, 
    rate_limit_seconds = 6)

adl_nfc_rosters <- adl$adl_rosters(adl_connection, "nfc")
adl_afc_rosters <- adl$adl_rosters(adl_connection, "afc")

adl_franchises <- ff_franchises(adl_connection)
adl_nfc_franchises <- adl_franchises[adl_franchises$conference == "00", ]
adl_afc_franchises <- adl_franchises[adl_franchises$conference == "01", ] 

adl_nfc_franchise_names <- 
  adl_nfc_franchises %>% 
  select("franchise_id", "franchise_name")

adl_afc_franchise_names <- 
  adl_afc_franchises %>% 
  select("franchise_id", "franchise_name")

snaps = load_snap_counts(
  seasons = most_recent_season(),
  file_type = getOption("nflreadr.prefer", default = "rds")
)

# Drop 'C','G','T','LS' positions
snaps <- snaps[!(snaps$position == 'C' |
                 snaps$position == 'G' |
                 snaps$position == 'T' |
                 snaps$position == 'LS'),]

# Drop 'K' and 'P' positions as well, since their snap percent is
# meaningless
snaps <- snaps[!(snaps$position == 'K' |
                 snaps$position == 'P'),]

# Only the following fields
snaps <- select(snaps, "week", "player", "position", "offense_pct", "defense_pct", "st_pct")

# Multiply fractional snaps columns by 100 to get percent
snaps$offense_pct <- snaps$offense_pct * 100
snaps$defense_pct <- snaps$defense_pct * 100
snaps$st_pct <- snaps$st_pct * 100

# append '%' to each snap percentage
#snaps$offense_pct <- sub("(.*)", "\\1%", snaps$offense_pct)
#snaps$defense_pct <- sub("(.*)", "\\1%", snaps$defense_pct)
#snaps$st_pct <- sub("(.*)", "\\1%", snaps$st_pct)

# Use the appropriate snaps percent column based on position
snaps <- snaps %>% 
  mutate(snaps_pct = 
    case_when(
      position == "QB" |
      position == "RB" |
      position == "FB" |
      position == "WR" |
      position == "TE" ~ offense_pct,
# Percent snaps for these positions are pretty meaningless
#      position == "K" |
#      position == "P" ~ st_pct,
      position == "DT" |
      position == "DE" |
      position == "LB" |
      position == "CB" |
      position == "SS" |
      position == "FS" ~ defense_pct
    )
)

# Remove the 3 individual types of snaps columns
snaps <- subset(snaps, select = -c(offense_pct, defense_pct, st_pct) )

# Only the player's first and last name; strip 'jr.', 'II', etc.
snaps$player <- word(snaps$player,1,2)

# Drop any periods in the player's name, e.g., 'A.J.' becomes 'AJ'
# This is because the snaps database isn't consistent with using
# periods.
snaps <- mutate(snaps, player = gsub('\\.', '', player))

get_snaps_for_team <- function(team) {
  # change lastname, firstname to firstname lastname
  team_players <- sub("([^,]+)\\s*,\\s*([^,]+)","\\2 \\1", team$player_name)

  team_snaps <- data.frame(player=character(), position=character(), 
                              w1=character(), w2=character(),
                              w3=character(), w4=character(),
                              w5=character(), w6=character(),
                              w7=character(), w8=character(),
                              w9=character(), w10=character(),
                              w11=character(), w12=character(),
                              w13=character(), w14=character(),
                              w15=character(), w16=character(),
                              w17=character(), w18=character(),
                              w19=character(), w20=character(),
                              w21=character(), w22=character()
                             )
  
  for (plyr in 1:length(team_players)) {
    cur_player = team_players[plyr]
    
    # Use alternate name, if one exists in the
    # lookup table
    cur_player_name = 
      ifelse(is.na(get_alt_name[cur_player]),
             cur_player,
             get_alt_name[cur_player])
    
    # First and last name only; strip 'jr.', 'II', etc.
    cur_player_name = word(cur_player_name,1,2)

    # Drop any periods in the player's name, e.g., 'A.J.' becomes 'AJ'
    # This is because the snaps database isn't consistent with using
    # periods.
    cur_player_name = gsub('\\.', '', cur_player_name)
    
    # Future development:
    #   * Add a field to snaps database, modified player name
    #   * Remove periods and apostrophes
    #   * First and last name only
    #   * Convert to all lower case
    #   * Truncate first and last name to first four letters
    #   * And do all of the above for the cur_player_name as well
    
    # Only the first 4 characters of first and last name, in order to
    # try to deal with name variations
    #trunc_player_name <- sapply(cur_player_name, function(str) {
    #  paste(strtrim(unlist(strsplit(str, " ")), 4), collapse = " ")  
    #})
    
    # get the snaps dataframe for current player
    player_snaps <- snaps[ snaps$player == cur_player_name, ]

    if (nrow(player_snaps) == 0) { 
      print(paste("Could not find snaps for:", cur_player))  
    }
    
    # sort by number of snaps and keep only the first entry for each week 
    # in case of duplicates
    player_snaps <- player_snaps[order(player_snaps$snaps_pct),]
    player_snaps <- player_snaps[!duplicated(player_snaps$week),]

    # sort by week
    player_snaps <- player_snaps[order(player_snaps$week),]

    # fill in any missing weeks with "NA"
    player_snaps <- complete(player_snaps, week = 1:22, fill = list())
    
    # save the player position from the last value which is not NA
    cur_pos = last(na.omit(player_snaps$position))
    
    # remove the player name and position column
    player_snaps <- subset(player_snaps, select = snaps_pct )

    # Perhaps automatic cell text color could go here?    
    # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#CellText_Specification
    
    # transpose the column into a row
    player_snaps <- t(player_snaps)
    
    # drop weeks 19-22, if any postseason play
    #team_snaps2 <- team_snaps %>% select((c(w23))
    ifelse (length(player_snaps) != 22, 
            {print(team_id)
             print(team_players)
             print(cur_player_name)
             print(player_snaps)
             stop(paste(
               "Expected exactly 22 snap entries, got:",
               length(player_snaps)))
            }
            , player_snaps)
                                         
    # rename the columns
    colnames(player_snaps) <- c(
      'w1','w2', 'w3','w4', 'w5','w6', 'w7','w8', 'w9','w10',
      'w11','w12', 'w13','w14', 'w15','w16', 'w17','w18', 'w19','w20',
      'w21','w22')
  
    row.names(player_snaps) <- cur_player
  
    player_snaps <- cbind(pos=cur_pos, player_snaps)
                              
    team_snaps <- rbind(team_snaps, player_snaps)
  }
  
  # Drop weeks 19+ (postseason)
  team_snaps <- subset(team_snaps, select = -c(w19,w20,w21,w22))

  return(team_snaps)
}

# Load snaps into nfc_team1, nfc_team2, etc.
for (i in 1:length(nfc_franchises)) {
  cur_team = nfc_franchises[i]
  # nfc_team1 = "Dallas Cowboys"; nfc_team2 = "New York Giants", etc.
  nfc_teamx=paste("nfc_team", i, sep = "")
  assign(nfc_teamx, cur_team)
  # "0001, "0002", etc.
  team_id <- 
    adl_nfc_franchise_names$franchise_id[
      adl_nfc_franchise_names$franchise_name==cur_team]
  # nfc_team1_snaps = {}; nfc_team2_snaps = {}
  nfc_teamx_snaps=paste(nfc_teamx, "_snaps", sep = "")
  # get the roster for the ADL franchise
  team <- adl_nfc_rosters[adl_nfc_rosters$franchise_id == team_id,]
  # drop kickers and punters
  team <- team[!(team$pos == "PK" | team$pos == "PN"),]
  # get the snaps
  cur_snaps <- get_snaps_for_team(team)
  assign(nfc_teamx_snaps, cur_snaps)    
}

# Load snaps into afc_team1, afc_team2, etc.
for (i in 1:length(afc_franchises)) {
  cur_team = afc_franchises[i]
  # afc_team1 = "Buffalo Bills"; afc_team2 = "Miami Dolphins", etc.
  afc_teamx=paste("afc_team", i, sep = "")
  assign(afc_teamx, cur_team)
  # "0017", "0018", etc.
  team_id <- 
    adl_afc_franchise_names$franchise_id[
      adl_afc_franchise_names$franchise_name==cur_team]
  # afc_team1_snaps = {}; afc_team2_snaps = {}
  afc_teamx_snaps=paste(afc_teamx, "_snaps", sep = "")
  # get the roster for the ADL franchise
  team <- adl_afc_rosters[adl_afc_rosters$franchise_id == team_id,]
  # drop kickers and punters
  team <- team[!(team$pos == "PK" | team$pos == "PN"),]
  # get the snaps
  cur_snaps <- get_snaps_for_team(team)
  assign(afc_teamx_snaps, cur_snaps)
}
