# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(stringr)
library(lubridate)
library(dplyr)
library(forcats)
library(purrr)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinyvalidate)
library(ggplot2)
library(plotly)
library(lpSolve)
library(png)

source("helperFuncs.R")
source("perfectLineupMod.R")
source("howToPlayFunc.R")
source("rosterBuilderMod_v2.R")
source("nflPlayerStatsMod.R")
source("fantasyResultsByRosterMod.R")
source("fantasyResultsByPlayerMod.R")
source("additionalAnalysisMod.R")

# this is important for getting the png file loaded within the UI later,
# since the data folder isn't available by default for img()
addResourcePath("data", "data")

playoff_year <- 2025L
# season_type <- c("REG","POST")
season_teams <- c(
  "ARI","ATL","BAL","BUF","CAR",
  "CHI","CIN","CLE","DAL","DEN",
  "DET","GB","HOU","IND","JAX",
  "KC","LA","LAC","LV","MIA",
  "MIN","NE","NO","NYG","NYJ",
  "PHI","PIT","SEA","SF","TB",
  "TEN","WAS"
)

playoff_teams <- c(
  "DEN",
  "NE",
  "JAX",
  "PIT",
  "HOU",
  "LAC",
  "BUF",
  "SEA",
  "CHI",
  "PHI",
  "CAR",
  "SF",
  "LAR",
  "GB"
)


dt_team_info <- fread(get_last_data("team_info"))

dt_nfl_rosters <- fread(get_last_data("rosters"))

dt_stats <- fread(get_last_data("stats"))

team_lookupstring_position <- fread(get_last_data("lookups"))

# ensure at least an empty table with headers is in this file
dt_scores <- fread(get_last_data("NFL Fantasy Scores"))

# just a placeholder until actual post season scores are available
if (dim(dt_scores)[1]==0L){
  dt_scores <- data.table(
    player_id = c("TB","TB","HOU","HOU"),
		position = c("Defense","Defense","QB","QB"),
		week = c(19L,19L,19L,19L),
		season_type = c("Post","Post","Post","Post"),
		lookup_string = c(
		  "Defense, TB (NFC South)",
		  "Defense, TB (NFC South)",
		  "QB, HOU: C.J. Stroud (AFC South, ID: 00-0039163)",
		  "QB, HOU: C.J. Stroud (AFC South, ID: 00-0039163)"),
		player_name = c("TB","TB","C.J. Stroud","C.J. Stroud"),
		team_abbr = c("TB","TB","HOU","HOU"),
		team_conf = c("NFC","NFC","AFC","AFC"),
		team_division = c("NFC South","NFC South","AFC South","AFC South"),
		stat_type = c("football_values","fantasy_points","football_values","fantasy_points"),
		stat_label = c("def_sack","passing_yards"),
		stat_values = c(1,2,40,1),
		fantasy_team_name = c("TBD","TBD","TBD","TBD"),
		roster = c("TBD","TBD","TBD","TBD"),
		position_type = c("Defense / Special teams","Defense / Special teams","Player","Player"),
		position_code = c("Defense","Defense","QB1","QB1"),
		source_file = c("TBD.csv","TBD.csv","TBD.csv","TBD.csv"),
		fantasy_team_and_initials = c("TBD","TBD","TBD","TBD")
  )
}

# this filtering was previously performed in the pipeline
# dt_scores <- dt_scores[stat_type == "fantasy_points"]

dt_fantasy_rosters <- fread(get_last_data("Playoff Fantasy"))
if("Fantasy Owner Email" %in% names(dt_fantasy_rosters)){
  dt_fantasy_rosters[,`Fantasy Owner Email`:=NULL]
}

# add in player_name to data
dt_fantasy_rosters <- merge(
  dt_fantasy_rosters,
  unique(dt_stats[,player_id:=ifelse(position=="Defense",team_abbr,player_id)][,.(player_id,player_name)]),
  by = "player_id",
  all.x = TRUE
)
dt_fantasy_rosters[,position_code:=ifelse(position_code=="D","Defense",position_code)]
dt_fantasy_rosters[,position_code:=factor(position_code, c("QB1","QB2","QB3","RB1","RB2","RB3","RB4","WR1","WR2","WR3","WR4","TE1","TE2","TE3","K","Defense"))]
setorder(dt_fantasy_rosters, fantasy_team_and_initials, position_code, player_name)

summary_by_team <- dt_fantasy_rosters |>
  distinct(fantasy_team_and_initials) |>
  left_join(
    dt_scores |>
      group_by(fantasy_team_and_initials, week) |>
      reframe(fantasy_points = sum(stat_values)),
    by = c("fantasy_team_and_initials")
  ) |>
  pivot_wider(names_from = week, names_prefix = "week_", values_from = fantasy_points, values_fill = 0) |>
  mutate(
    fantasy_points = rowSums(across(starts_with("week")))
  ) |>
  arrange(-fantasy_points) |>
  mutate(rank = 1:n()) |>
  as.data.table()

summary_by_team_and_player <- dt_fantasy_rosters |>
  select(fantasy_team_and_initials, team_abbr, position_code, player_name, player_id) |>
  left_join(
    dt_stats |>
      filter(season_type=="Post" & stat_type=="fantasy_points") |>
      group_by(player_id, week) |>
      reframe(fantasy_points = sum(stat_values)) |>
      pivot_wider(names_from = week, names_prefix = "week_", values_from = fantasy_points),
    by = c("player_id"),
    relationship = "many-to-one"
  ) |>
  mutate(
    across(starts_with("week"), ~ifelse(is.na(.x),0,.x)),
    fantasy_team_and_initials =
      factor(fantasy_team_and_initials,
      levels=fct_inorder(summary_by_team$fantasy_team_and_initials)
    )
  ) |>
  mutate(fantasy_points = rowSums(across(starts_with("week")))) |>
  arrange(fantasy_team_and_initials, position_code) |>
  as.data.table()

# add in rank to fantasy rosters
dt_fantasy_rosters <- dt_fantasy_rosters |>
  left_join(summary_by_team |>
              select(fantasy_team_and_initials, rank),
            by = c("fantasy_team_and_initials"))

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 900000); // performs logout function after 15 minutes
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 900000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"


last_refresh <- "12/30/2025 11:43PM"


ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$script(inactivity),
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Tom's Playoff Fantasy Football League")
  ),
  tags$h1("Tom's Playoff Fantasy Football League", style = "text-align:center; margin-bottom:0px"),
  tags$h3("(2025-2026)", style = "text-align:center; margin-top:0px"),
  tags$p("This web page will close automatically if idle for 15 minutes. If you are building a roster, you will lose unsaved work.", style="font-size:75%; text-align:center"),
  tabsetPanel(
    tabPanel(
      "Rules & Scoring",
      fluidPage(howToPlayUIonly())
    ),
    # TODO uncomment this code when needed for creating rosters
    tabPanel(
      "Build Roster",
      buildRosterUI("b_r", team_lookupstring_position)
    ),
    tabPanel(
      "Bracket",
      tags$img(
        src = get_last_data(key = "bracket", file_type = "png"),
        style = "width: 100%; height: auto;"
      )
    ),
    # tabPanel(
    #   "Bracket",
    #   fluidPage(
    #     tags$img(
    #       src = png_bracket,
    #       style = "width: 100%; height: auto;"
    #     )
    #   )
    # ),
    # tabPanel(
    #   "Fantasy Results",
    #   br(),
    #   tabsetPanel(
    #     type = "pills",
    #     tabPanel(
    #       "By Roster",
    #       br(),
    #       #tags$h4("Today's scores expected to be updated ~7AM the next day."),
    #       tags$h4("Week 4 should be final."),
    #       tags$p(paste0("Scores last refreshed: ",last_refresh)),
    #       fantasyResultsbyRosterUI("by_roster", summary_by_team)
    #     ),
    #     tabPanel(
    #       "By Player",
    #       br(),
    #       fantasyResultsbyPlayerUI("by_player", dt_team_info, playoff_teams, playoff_year)
    #     ),
    #     tabPanel(
    #       "Perfect Lineup",
    #       perfectLineupUI("perf", dt_stats)
    #     ),
    #     tabPanel(
    #       "Additional Analysis",
    #       additionalAnalysisUI("a_a")
    #     )
    #   )
    # ),
    tabPanel(
      "NFL Player Stats",
      nflPlayerStatsUI("nfl_ps", dt_team_info, playoff_teams, playoff_year)
    ),
    tabPanel(
      "About",
      p(),
      tags$span("If you are interested in the details behind this R Shiny Dashboard, "),
      tags$span("scripts are available on my "),
      tags$a(href="https://github.com/hillad3/PlayoffFantasyFootball_2025_2026_R", "github", .noWS="after"),
      tags$span("."),
    )
  )
)

server <- function(input, output, session) {

  # # by roster sub-tab
  # fantasyResultsbyRosterServer("by_roster", summary_by_team, summary_by_team_and_player)
  #
  # # by player sub-tab
  # fantasyResultsbyPlayerServer("by_player", dt_stats, dt_team_info, playoff_teams)
  #
  # # perfect_lineup sub-tab
  # perfectLineupServer("perf", dt_stats, playoff_teams)
  #
  # # additional analysis sub-tab
  # additionalAnalysisServer("a_a", dt_fantasy_rosters)

  # explore stats tab
  nflPlayerStatsServer("nfl_ps", dt_stats, dt_team_info, playoff_teams)

  # TODO this section is for Roster Selection; uncomment to make active
  buildRosterServer("b_r", team_lookupstring_position)


  ##### this handles the timeout_counter

  idle <- reactiveVal(0) # reactive variable to track idle time
  lastActivity <- reactiveVal(isolate(Sys.time()))   # initialize reactive to track current time

  observe({
    # ShinyApps can timeout after 1 minute. This ensures that the window stays open until
    # the JavaScript inactivity variable closes the window
    invalidateLater(59000, session) # check observe() every 60 seconds
    idle(difftime(isolate(Sys.time()), lastActivity(), units = "mins")) # number of minutes since last activity

  })

}


shinyApp(ui, server)

