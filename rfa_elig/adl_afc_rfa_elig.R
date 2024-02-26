# ADL AFC Projected 2024 Team Salaries

library(ffscrapr)
library(dplyr)
library(tidyr)
library(stringr)

current_year = format(Sys.Date(), "%Y")

adl <- mfl_connect(season = current_year, 
                   league_id = 60206, # from the URL of your league
                   rate_limit_number = 3, 
                   rate_limit_seconds = 6)

adl_league <- ff_league(adl)
adl_rosters <- ff_rosters(adl)
adl_franchises <- ff_franchises(adl)
adl_afc_franchises <- adl_franchises[adl_franchises$conference == "01", ]

# this is used for sorting the players by position
positions <- c("QB","RB","WR","TE","PK","PN","DT","DE","LB","CB","S")

adl_afc_franchise_names <- 
  adl_afc_franchises %>% 
    select("franchise_id", "franchise_name")

# separate the rosters by conference
adl_afc_rosters_unsorted <- merge(x=adl_rosters, y=adl_afc_franchises[, c("franchise_name")], by = "franchise_name")

# sort by franchise ID, then by position, and then by player name
adl_afc_rosters_by_player <- arrange(adl_afc_rosters_unsorted, player_name)
adl_afc_rosters_sorted <- adl_afc_rosters_by_player[order(factor(adl_afc_rosters_by_player$pos, levels=unique(positions))),] %>% arrange(franchise_id)

# split out the contract info into signed_year and contract type
adl_afc_rosters <- separate(adl_afc_rosters_sorted, contractInfo, into = c("signed_year", "contract_type"), sep = " (?=[^ ]+$)")

# take a subset
adl_afc_rosters_sub <- select(adl_afc_rosters, "franchise_name", "player_name", "pos", "age", "salary", "contract_years", "signed_year", "contract_type")

# add erfa eligibility
adl_afc_rosters_sub$erfa <- with(adl_afc_rosters_sub, 
    ifelse((contract_type!="ERFA" & contract_years==0),
           ifelse((signed_year==2023 & salary <= 0.99), "ERFA",
                  ifelse(signed_year==2022 & salary <=1.09, "ERFA","")),""))

# Create all-caps regex of ineligible contracts
# These contracts will be treated as substrings, so 
# "EXT" will also match "iEXT" and "oEXT"
rfa_ineligible_contracts =
  "EFT|NEFT|TT|NEFTOFF|TTOFF|FRFA|SRFA|ORFA|RRFA|B/R|EXT|5YO"

adl_afc_rosters_sub$rfa <- 
  with(adl_afc_rosters_sub,
    ifelse(contract_years == 0 & signed_year >= 2021,
      ifelse(
        !str_detect(toupper(contract_type), rfa_ineligible_contracts) &
        (contract_type != "UFA" | signed_year == 2023),
        "RFA", ""
      )
        , 
      ""
    )
  )    