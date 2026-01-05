

buildRosterUI <- function(id,team_lookupstring_position_){
  tagList(
    br(),
    actionButton(
      inputId = NS(id,"toggleRosterSelector"),
      label = "Show / Hide Menu",
      icon = icon("bars"),
      style = "margin-bottom:10px"
    ),
    sidebarLayout(
      div(id = NS(id,"rosterSelector"),
          sidebarPanel(
            selectizeInput(
              inputId = NS(id,"roster_selections_available"),
              label = "Select Player or Defensive Team",
              choices = c("",as.list(unique(team_lookupstring_position_[,lookup_string]))),
              selected = "",
              options = list(maxItems = 1)
            ),
            actionButton(
              inputId = NS(id,"add_player"),
              label = "Add",
              icon = icon("add"),
              style="color: white; background-color: #0086b3; border-color: #2e6da4"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"roster_slots_remaining_text")),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"positions_available_text")),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"teams_available_text")),
            tags$h1("", style = 'margin:100px'),
            selectizeInput(
              inputId = NS(id,"roster_selections_removable"),
              label = "Remove Player or Defensive Team",
              choices = NULL,
              options = list(maxItems = 1),
            ),
            actionButton(
              inputId = NS(id,"remove_player"),
              label = "Remove",
              icon = icon("trash", lib = "glyphicon"),
              style="color: white; background-color: gray; border-color: black"
            ),
            tags$p("", style="margin-top:10px"),
            actionButton(
              inputId = NS(id,"remove_player_all"),
              label = "All",
              icon = icon("trash", lib = "glyphicon"),
              style="color: white; background-color: black; border-color: black"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"positions_on_roster_text")),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"teams_on_roster_text")),
            tags$p("", style='margin-bottom:25px'),
            fluidPage(
              tags$p("", style="margin:8px"),
              tags$span("Participant Information", style='font-weight:bold; font-size:16px; margin-right: 3px'),
              tags$p("", style="margin:8px"),
              textInput(NS(id,"fantasy_owner_name"), label = "Name *", placeholder = "Dick Butkus"),
              textInput(NS(id,"fantasy_owner_email"), label = "Email *", placeholder = "iamold@aol.com"),
              textInput(NS(id,"fantasy_team_name"), label = "Fantasy Team Name *", placeholder = "Mahomes Alone2"),
              checkboxInput(NS(id,"paid_checkbox"), label = "I have paid the Commish because I am not a delinquent *"),
              tags$p("Note: Fantasy Team Name will be displayed in rankings", style='margin-top:20px'),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            tags$p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = NS(id,"download_roster"),
              label = "Download Roster",
              style = "color: white; background-color: #F62817;"
            ),
            tags$p(""),
            tags$span("The Download button will activate once you have "),
            tags$span("14 players on your roster ", style = "font-weight:bold;"),
            tags$span("and "),
            tags$span("the participant information is complete.", style = "font-weight:bold;"),
            tags$span("Email your CSV-file roster(s) to the Commish by 1pm on 1/10/2026!", style="color:#E01713; font-weight:bold;"),
            tags$span("(Don't take a picture. Don't convert it to Excel. If you send it as a selfie, I will literally die."),
            tags$span("Please, for the love of all that is sane in this world, reply to the Commish's email with your CSV-file roster(s) attached."),
            tags$span("Thank you.", .noWS="after", style="vertical-align:super; font-size:80%"),
            tags$span(")"),
            width = 3
          )
      ),
      mainPanel(
        fluidRow(
          tags$h3("Current Roster"),
          DTOutput(outputId = NS(id,"players_on_roster_DT")),
          style="margin-left:2px"
        ),
        fluidRow(
          tags$h3("Valid Player Selections Remaining", style="margin-top:100px"),
          DTOutput(outputId = NS(id,"players_remaining_DT")),
          style="margin-left:2px"
        )
      ) # close MainPanel
    ) # close SidebarLayout
  ) # close tagList
}

filter_players_by_constraints <- function(dt, teams_selected, positions_selected) {
  # dt here is the full team_lookupstring_position_

  # default behavior: no constraints
  if (length(teams_selected) == 0) teams_selected <- character(0)
  if (length(positions_selected) == 0) positions_selected <- character(0)

  dt <- dt[!(team_abbr %in% teams_selected)]

  if (sum(positions_selected == "Defense") >= 1) dt <- dt[position != "Defense"]
  if (sum(positions_selected == "K") >= 1) dt <- dt[position != "K"]
  if (sum(positions_selected == "QB") >= 3) dt <- dt[position != "QB"]

  # for RB, TE and WR, need to consider the flex position when filtering
  if(
    (sum(positions_selected == "RB") >= 4) | # 3 RB + plus 1 Flex (RB)
    (sum(positions_selected == "RB") >= 3 & (sum(positions_selected == "TE") >= 3 | sum(positions_selected == "WR") >= 4) ) # 3 RB + some other Flex Position
  ){
    dt <- dt[position != "RB"]
  }

  if(
    (sum(positions_selected == "WR") >= 4) | # 3 WR + plus 1 Flex (WR)
    (sum(positions_selected == "WR") >= 3 & (sum(positions_selected == "TE") >= 3 | sum(positions_selected == "RB") >= 4) ) # 3 WR + some other Flex Position
  ){
    dt <- dt[position != "WR"]
  }

  if(
    (sum(positions_selected == "TE") >= 3) | # 2 TE + plus 1 Flex (TE)
    (sum(positions_selected == "TE") >= 2 & (sum(positions_selected == "WR") >= 4 | sum(positions_selected == "RB") >= 4) ) # 2 TE + some other Flex Position
  ){
    dt <- dt[position != "TE"]
  }

  dt
}


buildRosterServer <- function(id, team_lookupstring_position_){
  moduleServer(
    id,
    function(input,output,session){

      observeEvent(input$toggleRosterSelector, {
        shinyjs::toggle(id = "rosterSelector")
      })

      # initial container for holding players
      roster <- reactiveValues(players = character(0))

      observeEvent(input$add_player,{
        req(input$roster_selections_available)
        roster$players <- c(roster$players, input$roster_selections_available) |> sort()
      })

      observeEvent(input$remove_player,{
        roster$players <- roster$players[!(roster$players %in% input$roster_selections_removable)]
      })

      observeEvent(input$remove_player_all,{
        roster$players <- character(0)
      })

      output$roster_slots_remaining_text <- renderText({
        paste0("Roster slot(s) remaining: ", 14-length(roster$players), " of 14")
      })

      # keep track of teams selected on the roster
      teams_on_roster <- reactive({
        unique(team_lookupstring_position_[lookup_string %in% roster$players, team_abbr])
      })

      output$teams_on_roster_text <- renderText({
        if(is_empty(teams_on_roster())){
          "Teams on roster: None"
        } else {
          paste0("Teams on roster: ", paste0(teams_on_roster() |> unlist(), collapse = ",  "))
        }
      })

      output$teams_available_text <- renderText({
        paste0(
          "Teams remaining: ",
          paste0(
            team_lookupstring_position_[!(team_abbr %in% teams_on_roster()), team_abbr] |> unique() |> sort() |> unlist(),
            collapse = ",  "
          )
        )
      })

      # keep track of positions on the roster
      positions_selected <- reactive({
        team_lookupstring_position_[lookup_string %in% roster$players, position]
      })

      output$positions_on_roster_text <- renderText({
        if(is_empty(positions_selected())){
          "Positions Filled: None"
        } else {
          paste0("Positions Filled: ", paste0(count_positions(positions_selected()) |> unlist(), collapse = ",  "))
        }
      })

      output$positions_available_text <- renderText({
        if(length(positions_selected())==14L){
          "Positions Remaining: None"
        } else {
          all_positions <- c("K","QB1","QB2","QB3","RB1","RB2","RB3","TE1","TE2","WR1","WR2","WR3","FLEX","Defense")
          current_positions <- count_positions(positions_selected())
          current_positions <- str_remove(current_positions," .[:alpha:]{2}.")
          remaining_positions <- all_positions[!(all_positions %in% current_positions)]
          paste0("Positions Remaining: ", paste0(remaining_positions |> unlist(), collapse = ",  "))
        }
      })

      players_remaining <- reactive({

        # helper function that takes the full list of team_lookup_string_positions_ and
        # filters it to exclude the current positions_selected() and teams_on_roster()
        filter_players_by_constraints(
          team_lookupstring_position_,
          teams_on_roster(),
          positions_selected()
        )

      })

      output$players_remaining_DT <- renderDT({
        DT::datatable(
          players_remaining(),
          rownames = FALSE
        )
      })

      output$players_on_roster_DT <- renderDT({
        if(is_empty(roster$players)){
          DT::datatable(
            data.table(" " = "Roster is empty"),
            rownames = FALSE,
            options = list(
              dom = 'frti'  # removed 'l' and 'p' to hide the length menu and pagination respectively
            )
          )
        } else {
          DT::datatable(
            team_lookupstring_position_[lookup_string %in% roster$players],
            rownames = FALSE,
            options = list(
              pageLength = 14,
              dom = 'frti'  # removed 'l' and 'p' to hide the length menu and paginatino respectively
            )
          )
        }
      })


      observeEvent(
        eventExpr = c(input$add_player, input$remove_player),
        handlerExpr = {

          updateSelectizeInput(
            session,
            inputId = "roster_selections_available",
            choices = c("", as.list(players_remaining()[, lookup_string])),
            selected = ""
          )

          updateSelectizeInput(
            session,
            inputId = "roster_selections_removable",
            choices = roster$players
          )
        }
      )

      observeEvent(
        eventExpr = c(input$remove_player_all),
        handlerExpr = {

          updateSelectizeInput(
            session,
            inputId = "roster_selections_available",
            choices = c("",as.list(unique(team_lookupstring_position_[,lookup_string]))),
            selected = ""
          )

          updateSelectizeInput(
            session,
            inputId = "roster_selections_removable",
            choices = character(0),
            selected = character(0)
          )
        }
      )

      observeEvent(
        roster$players,
        {
          if(length(roster$players) >= 14L) {
            shinyjs::disable("add_player")

          } else {
            shinyjs::enable("add_player")
          }
        }
      )


      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("fantasy_owner_email", sv_email(message = "Valid email required."))
      iv$add_rule("fantasy_owner_name", sv_required(message = "Participant name required."))
      iv$add_rule("fantasy_team_name", sv_required(message = "Unique team name required."))
      iv$add_rule("paid_checkbox", sv_equal(TRUE, "Check the box to confirm."))
      iv$enable()

      observeEvent(
        eventExpr = c(iv$is_valid(), roster$players),
        {

          if(iv$is_valid() && length(roster$players) == 14) {
            shinyjs::enable("download_roster")

          } else {
            shinyjs::disable("download_roster")
          }
        }
      )

      # create final roster for downloadHandler
      roster_data <- reactive({

        team_lookupstring_position_ |>
          filter(lookup_string %in% roster$players) |>
          select(position, team_abbr, lookup_string) |>
          mutate(
            `Fantasy Owner` = rep(input$fantasy_owner_name,14),
            `Fantasy Owner Email` = rep(input$fantasy_owner_email,14),
            `Fantasy Team Name` = rep(input$fantasy_team_name,14),
            `Roster` = 1:14,
            `Position Type` = if_else(position == "Defense", "Defense / Special teams", "Player"),
            `Automation Mapping` = if_else(
              position == "Defense",
              team_abbr,
              str_remove(str_remove(lookup_string, "^.*, ID: "),"\\)")
            ),
            `Check 1 - Selection is Unique` = TRUE,
            `Check 2 - Team is Unique` = TRUE
          ) |>
          group_by(
            position
          ) |>
          mutate(
            `Position Code` = if_else(position %in% c("QB","WR","TE","RB"), paste0(position,1:n()),
                                      if_else(position == "Defense", "D", position))
          ) |>
          ungroup() |>
          rename(
            `Position Group` = position,
            `Team Abbr.` = team_abbr,
            `Selection` = lookup_string
          ) |>
          mutate(
            `Position Group` = fcase(
              `Position Code` == "K", "SPEC",
              `Position Code` %in% c("RB4","WR4","TE3"), "FLEX",
              `Position Code` == "D", "D",
              default = `Position Group`)
          ) |>
          select(
            `Fantasy Owner`,
            `Fantasy Owner Email`,
            `Fantasy Team Name`,
            `Automation Mapping`,
            `Roster`,
            `Position Type`,
            `Position Code`,
            `Position Group`,
            `Team Abbr.`,
            `Selection`,
            `Check 1 - Selection is Unique`,
            `Check 2 - Team is Unique`,
            everything()
          )
      })

      output$download_roster <- downloadHandler(
        filename = function() {
          paste0('Playoff Fantasy Roster - ',
                 input$fantasy_owner_name,
                 " - ",
                 input$fantasy_team_name,
                 ", ",
                 Sys.Date(),
                 '.csv')
        },
        content = function(file) {
          write.csv(roster_data(), file, row.names = FALSE)
        }
      )

    }
  )
}