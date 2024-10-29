#
# Next steps:
#  - Have two tabsetpanels, one with hidden tabset type
#    and the other with pills tabset type. The two tabsets
#    will be wrapped in conditionalPanel, such that only 
#    one tabSetPanel can be active at a time, and this
#    is changed by pressing the "Continue" button.
# 
#  - Eliminate unneeded fields in the team roster
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

getTeamRoster <- function(input_conference, team_name) {
  if (input_conference == "AFC") {
    conf_rosters <- adl$adl_rosters(adl_connection, "afc")
    team_id <- 
      adl_afc_franchise_names$franchise_id[
        adl_afc_franchise_names$franchise_name==team_name]
    team_roster <- adl_afc_rosters[adl_afc_rosters$franchise_id == team_id,]
  } else {
    conf_rosters <- adl$adl_rosters(adl_connection, "nfc")
    team_id <- 
      adl_nfc_franchise_names$franchise_id[
        adl_nfc_franchise_names$franchise_name==team_name]
    team_roster <- adl_nfc_rosters[adl_nfc_rosters$franchise_id == team_id,]
  }
}

ui <- fluidPage(
    # Application title
    #titlePanel("Select your conference and team"),
    #renderText(
    #  {"Select your conference and franchise:"}),
  tabsetPanel(type="pills", id="FirstPanel",
    tabPanel("SelectTeam",
      radioButtons("conference_name", "Conference", c("AFC", "NFC")),
      uiOutput('franchises'),
      actionButton("TeamSelected", "Continue")),
    #tabPanel("Summary", type="hidden",
    #  uiOutput('summary')#)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #conference_name <- reactive(input$conference_name)

  output$franchises = renderUI({
    #hideTab(inputId = "FirstPanel", target = "SelectTeam")
        franchise_list <- 
      if(input$conference_name == "AFC")
        afc_franchises
      else 
        nfc_franchises
    radioButtons('team_name', "Team Name", franchise_list)
  })
  
  observeEvent(input$TeamSelected, {
    appendTab(inputId = 
      "FirstPanel", tab = tabPanel("Summary", uiOutput('summary')))
    #appendTab(inputId = "FirstPanel", tab = tabPanel("tab3 with a lot of stuff in it",value = "tab3_val", br(), h4("this is tab3")))
    
    updateTabsetPanel(session, "FirstPanel",
                      selected = "Summary")
    removeTab(inputId = "FirstPanel", target = "SelectTeam")
  })
  
#  team_id <- 
#    adl_nfc_franchise_names$franchise_id[
#      adl_nfc_franchise_names$franchise_name==cur_team]
  
  output$summary_table <- 
    renderDataTable({
      team_roster <- 
        getTeamRoster(input$conference_name, input$team_name)
    })
  
  output$summary <- renderUI({
       dataTableOutput("summary_table")
  })
  
#  output$conference_rosters <- 
#    renderDataTable({
#      getConferenceRosters(input$conference_name)
#    })
#  
#  output$summary <- renderUI({
#    dataTableOutput("conference_rosters")
#  })
}

# Run the application 
shinyApp(ui = ui, server = server)
