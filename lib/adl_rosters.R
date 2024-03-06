# ADL Rosters
#
# Obtains a dataframe of all team rosters for the requested conference.
# Rosters are sorted by team, then by position, and then by player name (last,
# first).  The contract information is split out into signed_year and
# contract_type.
#
# Arguments:
#   adl_connection - connection object for MFL
#   conference - "nfc" or "afc"
#
# Example usage:
#   current_year = format(Sys.Date(), "%Y")
#
#   adl_connection <- mfl_connect(season = current_year, 
#     league_id = 60206,
#     rate_limit_number = 3, 
#     rate_limit_seconds = 6)
#
#   adl_nfc_rosters <- adl_rosters(adl_connection, "nfc")
#   adl_afc_rosters <- adl_rosters(adl_connection, "afc")
#

import(ffscrapr)
import(dplyr)
import(tidyr)

export("adl_rosters")

adl_rosters <- function(adl_connection, conference_str) {
  conference_id=switch(tolower(conference_str),
    "nfc"="00",
    "afc"="01",
    stop("Please provide a conference: nfc or afc"))
  
  adl_rosters <- ff_rosters(adl_connection)
  adl_franchises <- ff_franchises(adl_connection)

  adl_conf_franchises <- adl_franchises[adl_franchises$conference == conference_id, ]

  # this is used for sorting the players by position
  positions <- c("QB","RB","WR","TE","PK","PN","DT","DE","LB","CB","S")

  # separate the rosters by conference
  adl_conf_rosters_unsorted <- merge(x=adl_rosters, y=adl_conf_franchises[, c("franchise_name")], by = "franchise_name")
  
  # sort by franchise ID, then by position, and then by player name
  adl_conf_rosters_by_player <- arrange(adl_conf_rosters_unsorted, player_name)
  adl_conf_rosters_sorted <- adl_conf_rosters_by_player[order(factor(adl_conf_rosters_by_player$pos, levels=unique(positions))),] %>% arrange(franchise_id)
  
  # split out the contract info into signed_year and contract type
  adl_conf_rosters <- separate(adl_conf_rosters_sorted, contractInfo, into = c("signed_year", "contract_type"), sep = " (?=[^ ]+$)")

  return(adl_conf_rosters)
}
