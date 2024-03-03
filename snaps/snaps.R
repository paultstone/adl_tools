# ADL NFC Projected 2024 Team Salaries

library(ffscrapr)
library(dplyr)
library(tidyr)
library(stringr)
library(nflreadr)

current_year = format(Sys.Date(), "%Y")
cur_week = 22

adl <- mfl_connect(season = current_year, 
                   league_id = 60206, # from the URL of your league
                   rate_limit_number = 3, 
                   rate_limit_seconds = 6)

adl_league <- ff_league(adl)
adl_rosters <- ff_rosters(adl)
adl_franchises <- ff_franchises(adl)
adl_nfc_franchises <- adl_franchises[adl_franchises$conference == "00", ]

# this is used for sorting the players by position
positions <- c("QB","RB","WR","TE","PK","PN","DT","DE","LB","CB","S")

adl_nfc_franchise_names <- 
  adl_nfc_franchises %>% 
    select("franchise_id", "franchise_name")

# separate the rosters by conference
adl_nfc_rosters_unsorted <- merge(x=adl_rosters, y=adl_nfc_franchises[, c("franchise_name")], by = "franchise_name")

# sort by franchise ID, then by position, and then by player name
adl_nfc_rosters_by_player <- arrange(adl_nfc_rosters_unsorted, player_name)
adl_nfc_rosters_sorted <- adl_nfc_rosters_by_player[order(factor(adl_nfc_rosters_by_player$pos, levels=unique(positions))),] %>% arrange(franchise_id)

# split out the contract info into signed_year and contract type
adl_nfc_rosters <- separate(adl_nfc_rosters_sorted, contractInfo, into = c("signed_year", "contract_type"), sep = " (?=[^ ]+$)")

# take a subset
adl_nfc_rosters_sub <- select(adl_nfc_rosters, "franchise_id", "player_name", "pos", "age")

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

# Concatenate all 3 types of snaps into one column
#snaps <- within(snaps, snaps_pct <- paste(offense_pct, defense_pct, st_pct, sep=' - '))
#snaps$snaps_pct <- pmax(snaps$offense_pct, snaps$defense_pct)

# Remove the 3 individual types of snaps columns
snaps <- subset(snaps, select = -c(offense_pct, defense_pct, st_pct) )

# Only the player's first and last name; strip 'jr.', 'II', etc.
snaps$player <- word(snaps$player,1,2)

# Drop any periods in the player's name, e.g., 'A.J.' becomes 'AJ'
# This is because the snaps database isn't consistent with using
# periods.
snaps <- mutate(snaps, player = gsub('\\.', '', player))

# Drop weeks 19+ (postseason)
#snaps <- snaps[ snaps$week <= 18, ]

# Only Seattle players
#snaps <- snaps[ snaps$player %in% team_players, ]

cur_team = 'Seattle Seahawks'

team_id <- adl_nfc_franchise_names$franchise_id[adl_nfc_franchise_names$franchise_name==cur_team]

team <- adl_nfc_rosters_sub[adl_nfc_rosters_sub$franchise_id == team_id,]

# Change lastname, firstname to firstname lastname
team_players <- sub("(\\w+\\s*\\w+).*,\\s(\\w.*)","\\2 \\1", team$player_name)

# start from scratch; remove if exists
rm(team_snaps)

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

#cur_player = 'Ifeatu Melifonwu'

for (i in 1:length(team_players)) { 
  cur_player = team_players[i]
  
  # First and last name only; strip 'jr.', 'II', etc.
  cur_player_shortened = word(cur_player,1,2)
    
  # Drop any periods in the player's name, e.g., 'A.J.' becomes 'AJ'
  # This is because the snaps database isn't consistent with using
  # periods.
  cur_player_shortened = gsub('\\.', '', cur_player_shortened)
  
  # get the snaps dataframe for current player
  player_snaps <- snaps[ snaps$player == cur_player_shortened, ]
  
  # sort by week just in case
  player_snaps <- player_snaps[order(player_snaps$week),]

  # weeks
  weeks <- c(1:22)
  
  # fill in any missing weeks; this will end up as row data, so don't
  # want any gaps
  player_snaps <- complete(player_snaps, week = 1:22, fill = list())
  
  # save the player position from the first value which is not NA
  cur_pos = first(na.omit(player_snaps$position))
  
  # Want to get the snaps appropriate for that player, based on position
  # rather than grabbing the larger of offense or defense, because that
  # leaves out PK and PN. Fortunately, we are working with a single player 
  # here.
  #if_else(cur_pos=='QB'|cur_pos=='RB'|cur_pos=='WR'|cur_pos=='TE',
  #        player_snaps$snap_pct <- player_snaps$offense_pct, NA)
  
  # remove the player name and position column
  player_snaps <- subset(player_snaps, select = snaps_pct )
  
  # It looks like if i use rbind(), then I don't need to tranpose the column
  # tranpose to get a row for the player
  player_snaps <- t(player_snaps)
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

sea_snaps <- team_snaps
#max_snaps <- pmax(meli$offense_pct, meli$defense_pct)

# https://stackoverflow.com/questions/63623456/dividing-data-frame-and-transposing-in-a-for-loop-r

# https://stackoverflow.com/questions/10013985/how-to-copy-row-from-one-data-frame-in-to-another-r


# Add back the franchise name
#seattle <- merge(x=seattle, y=adl_nfc_franchise_names, by = "franchise_id", all.x = T)
# Delete franchise_id column
#seattle$franchise_id <- NULL
# Put franchise name in the first column
#seattle <- seattle[, c(4,1,2,3)]
