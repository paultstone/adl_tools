#
# Next steps:
#  - have a button which:
#      - removes the first tab, 
#      - unhides all tabs, and 
#      - switches to tab 2
#
#  - only show the roster for the selected team
#


library(shiny)
library(ffscrapr)
library(dplyr)
library(tidyr)
library(stringr)
library(nflreadr)
library(modules)

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

adl_nfc_franchise_names <- 
  adl_nfc_franchises %>% 
  select("franchise_id", "franchise_name")

adl_afc_franchise_names <- 
  adl_afc_franchises %>% 
  select("franchise_id", "franchise_name")

getConferenceRosters <- function(input_conference) {
  if (input_conference == "AFC")
    adl$adl_rosters(adl_connection, "afc")
  else
    adl$adl_rosters(adl_connection, "nfc")
}

ui <- fluidPage(
    # Application title
    #titlePanel("Select your conference and team"),
    #renderText(
    #  {"Select your conference and franchise:"}),
  tabsetPanel(type="hidden",
    tabPanel("Select franchise", 
      radioButtons("conference_name", "Conference", c("AFC", "NFC")),
      uiOutput('franchises'),
      actionButton("Team Selected", "Continue")),
    tabPanel("Summary",
      uiOutput('summary'))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #conference_name <- reactive(input$conference_name)

  output$franchises = renderUI({
    franchise_list <- 
      if(input$conference_name == "AFC")
        afc_franchises
      else 
        nfc_franchises
    radioButtons('franchise_name', "Team Name", franchise_list)
  })
  
#  team_id <- 
#    adl_nfc_franchise_names$franchise_id[
#      adl_nfc_franchise_names$franchise_name==cur_team]
  
  output$conference_rosters <- 
    renderDataTable({
      getConferenceRosters(input$conference_name)
    })
  
  output$summary <- renderUI({
       dataTableOutput("conference_rosters")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
