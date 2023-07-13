# ADL NFC Projected 2024 Team Salaries

library(ffscrapr)
library(dplyr)
library(tidyr)

current_year = format(Sys.Date(), "%Y")
projected_year = 2024

# Manually enter in the 2024 salary adjustments
franchise_name <- c("Buffalo Bills", "Miami Dolphins", "New England Patriots", 
                    "New York Jets", "Baltimore Ravens", "Cincinnati Bengals", 
                    "Cleveland Browns", "Pittsburgh Steelers", "Houston Texans", 
                    "Indianapolis Colts", "Jacksonville Jaguars", "Tennessee Titans", 
                    "Denver Broncos", "Kansas City Chiefs", "Las Vegas Raiders", 
                    "Los Angeles Chargers")

# Table of total salary adjustments (so far) which will apply in 2024
salary_adj <- c(0, 3.43, 0.92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
adl_afc_salary_adj_2024 <- data.frame(franchise_name, salary_adj)

# Table of 5YO salary totals in 2024
# Bills
# - Joe Burrow $24.41
# Steelers
# - Jonathon Taylor $15.43
# Chiefs
# - Justin Jefferson $37.41
salaries_5yo <- c(24.41, 0, 0, 0, 0, 0, 0, 15.43, 0, 0, 0, 0, 0, 37.41, 0, 0)
adl_afc_5yo_salary_adj_2024 <- data.frame(franchise_name, salaries_5yo)

adl <- mfl_connect(season = 2023, 
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
adl_afc_rosters_sub <- select(adl_afc_rosters, "franchise_id", "franchise_name", "salary", "contract_years", "signed_year", "contract_type")

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
adl_afc_rosters_top45 <- adl_afc_rosters_sub %>%
  arrange(desc(salary)) %>%
  group_by(franchise_id) %>%
  slice(1:45)

# take a subset of only the expiring contracts
adl_afc_rosters_expiring <- 
  adl_afc_rosters_top45[
    contract_expired(adl_afc_rosters_top45$contract_years),]

# subset only the contracts which will carry through to next year
adl_afc_rosters_unexpired <- 
  adl_afc_rosters_top45[
    !contract_expired(adl_afc_rosters_top45$contract_years),]

# simple sum of the unexpired contracts in 2024
# in order to keep this table in expected order, use franchise_id rather than franchise_name
adl_afc_rosters_sum <-
  adl_afc_rosters_unexpired %>%
    group_by(franchise_id) %>%
      summarise(salaries = sum(salary))

# increase rosters by 10%
adl_afc_rosters_sum$salaries <- 1.10 * adl_afc_rosters_sum$salaries

# Add back franchise_name column
adl_afc_2024_cap <- 
  merge(x = adl_afc_rosters_sum, y = adl_afc_franchise_names, 
        by = "franchise_id", all.x = T)

# Remove the franchise_id column, and put the franchise_name column first
#adl_afc_2024_cap <- adl_afc_2024_cap %>% select("franchise_name", "salaries")

adl_afc_2024_cap <- 
  merge(x = adl_afc_2024_cap, y = adl_afc_5yo_salary_adj_2024, 
        by = "franchise_name", all.x = T) %>% arrange(franchise_id)

adl_afc_2024_cap <- 
  merge(x = adl_afc_2024_cap, y = adl_afc_salary_adj_2024, 
        by = "franchise_name", all.x = T) %>% arrange(franchise_id)

adl_afc_2024_cap <- 
  adl_afc_2024_cap %>% 
    mutate(total = salaries + salaries_5yo + salary_adj) %>% 
      arrange(total) %>%
        select("franchise_name", "salaries", "salaries_5yo", 
               "salary_adj", "total")

