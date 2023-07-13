# ADL NFC Projected 2024 Team Salaries

library(ffscrapr)
library(dplyr)
library(tidyr)

current_year = format(Sys.Date(), "%Y")
projected_year = 2024

# Manually enter in the 2024 salary adjustments
franchise_name <- c("Dallas Cowboys", "New York Giants", 
                    "Philadelphia Eagles", "Washington Commanders", 
                    "Chicago Bears", "Detroit Lions", 
                    "Green Bay Packers", "Minnesota Vikings", 
                    "Atlanta Falcons", "Carolina Panthers", 
                    "New Orleans Saints", "Tampa Bay Buccaneers", 
                    "Arizona Cardinals", "Los Angeles Rams", 
                    "San Francisco 49ers", "Seattle Seahawks")

# Table of total salary adjustments (so far) which will apply in 2024
salary_adj <- c(0, 0, 0, 1.32, 0, 0, 0, 0, 0, 0, 0, 4.55, 0, 0, 0, 5.53)
adl_nfc_salary_adj_2024 <- data.frame(franchise_name, salary_adj)

# Table of 5YO salary totals in 2024
# Giants
# - Justin Herbert $19.39
# - Jonathon Taylor $15.43
# Seahawks
# - Tua Tagovailoa $19.39
salaries_5yo <- c(0, 19.39+15.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19.39)
adl_nfc_5yo_salary_adj_2024 <- data.frame(franchise_name, salaries_5yo)

adl <- mfl_connect(season = 2023, 
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
adl_nfc_rosters_sub <- select(adl_nfc_rosters, "franchise_id", "franchise_name", "salary", "contract_years", "signed_year", "contract_type")

# If there are a significant number of contracts with zero years, then assume 
# that contracts expiration has taken place
after_contracts_expiration <- 
  ifelse(nrow(adl_rosters[adl_rosters$salary == 0, ]) > 10, TRUE, FALSE)

# Function to determine if contract either has expired or will have expired 
# next offseason
contract_expired <- function(contract_years) {
  ifelse(after_contracts_expiration == TRUE, 
         return(ifelse(contract_years == 0, TRUE, FALSE)),
         return(ifelse(contract_years == 1, TRUE, FALSE)))
}

# take the top 45 contracts
adl_nfc_rosters_top45 <- adl_nfc_rosters_sub %>%
  arrange(desc(salary)) %>%
  group_by(franchise_id) %>%
  slice(1:45)

# take a subset of only the expiring contracts
adl_nfc_rosters_expiring <- 
  adl_nfc_rosters_top45[
    contract_expired(adl_nfc_rosters_top45$contract_years),]

# subset only the contracts which will carry through to next year
adl_nfc_rosters_unexpired <- 
  adl_nfc_rosters_top45[
    !contract_expired(adl_nfc_rosters_top45$contract_years),]

# simple sum of the unexpired contracts in 2024
# in order to keep this table in expected order, use franchise_id rather than franchise_name
adl_nfc_rosters_sum <-
  adl_nfc_rosters_unexpired %>%
    group_by(franchise_id) %>%
      summarise(salaries = sum(salary))

# increase rosters by 10% and round to 2 decimal places
adl_nfc_rosters_sum$salaries <- round(1.10 * adl_nfc_rosters_sum$salaries, digits = 2)

# Add back franchise_name column
adl_nfc_2024_cap <- 
  merge(x = adl_nfc_rosters_sum, y = adl_nfc_franchise_names, 
        by = "franchise_id", all.x = T)

# Remove the franchise_id column, and put the franchise_name column first
#adl_nfc_2024_cap <- adl_nfc_2024_cap %>% select("franchise_name", "salaries")

adl_nfc_2024_cap <- 
  merge(x = adl_nfc_2024_cap, y = adl_nfc_5yo_salary_adj_2024, 
        by = "franchise_name", all.x = T) %>% arrange(franchise_id)

adl_nfc_2024_cap <- 
  merge(x = adl_nfc_2024_cap, y = adl_nfc_salary_adj_2024, 
        by = "franchise_name", all.x = T) %>% arrange(franchise_id)

adl_nfc_2024_cap <- 
  adl_nfc_2024_cap %>% 
    mutate(total = salaries + salaries_5yo + salary_adj) %>% 
      arrange(total) %>%
        select("franchise_name", "salaries", "salaries_5yo", 
               "salary_adj", "total")
