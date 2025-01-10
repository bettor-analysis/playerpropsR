# Player Prop Bet Finder Shiny App
# Adam Wickwire - Bettor Analysis


# Load necessary libraries
library(shiny)
library(tidyverse)
library(oddsapiR)
library(lubridate)
library(shinycssloaders)
library(shinyjs)          # Added shinyjs for UI control
library(shinythemes)
library(reactable)
library(htmltools)
library(shinyWidgets)  


# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"),  # Apply theme
  
  # Include custom fonts and styles
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:400,500&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* Custom CSS styles for the reactable table */
      .player-prop-table {
        font-family: 'Roboto', Helvetica, Arial, sans-serif;
        font-size: 11px;
      }
      /* Header styling */
      .player-prop-table .rt-th {
        background-color: #1a3e5c;
        color: #fff;
        font-weight: 500;
        text-transform: uppercase;
        padding: 4px;
      }
      /* Cell styling */
      .player-prop-table .rt-td {
        font-size: 1.125rem;
        padding: 4px;
      }
      /* Row striping */
      .player-prop-table .rt-tr.-odd {
        background-color: #f9f9f9;
      }
      /* Highlight on hover */
      .player-prop-table .rt-tr:hover {
        background-color: #e5e5e5;
      }
      /* Highlight positive EV and Play cells */
      .highlight-positive {
        background-color: #dff0d8 !important;
        color: green !important;
        font-weight: bold;
      }
      /* Pagination buttons */
      .player-prop-table .-pagination .-btn {
        color: #333;
        background-color: #fff;
        border: 1px solid #ccc;
      }
      /* Sidebar styling */
      .sidebar {
        font-family: 'Roboto', Helvetica, Arial, sans-serif;
        background-color: #1a3e5c;
        color: #fff;
        padding: 15px;
        font-size: 13px;
      }
      .sidebar .control-label, .sidebar .shiny-input-container {
        color: #fff;
      }
      .sidebar .btn {
        background-color: #337ab7;
        color: #fff;
        border-color: #2e6da4;
        font-size: 13px;
      }
      .sidebar .btn:hover {
        background-color: #286090;
        border-color: #204d74;
      }
      .sidebar .selectize-input, .sidebar .selectize-dropdown {
        color: #333;
      }
      /* Shake effect for incorrect login */
      @keyframes shake {
        0% { transform: translateX(0); }
        25% { transform: translateX(-5px); }
        50% { transform: translateX(5px); }
        75% { transform: translateX(-5px); }
        100% { transform: translateX(0); }
      }
      .shake {
        animation: shake 0.5s;
      }
    "))
  ),
  
  # Wrap the entire UI in a hidden div
  hidden(
    div(
      id = "main_ui",  # Assign an ID to control visibility
      titlePanel("Player Prop Bet Finder"),
      
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput(
            inputId = "selected_sport",
            label = "Select Sport:",
            choices = list(
              "NFL - Football" = "americanfootball_nfl",
              "NBA - Basketball" = "basketball_nba",
              "MLB - Baseball" = "baseball_mlb",
              "NHL - Hockey" = "icehockey_nhl",
              "NCAA - College Football" = "americanfootball_ncaaf",
              "NCAA - College Basketball" = "basketball_ncaab",
              "WNBA - Basketball" = "basketball_wnba",
              "MLS - Soccer" = "soccer_usa_mls",
              "EPL - Soccer" = "soccer_epl"
            ),
            selected = c("americanfootball_nfl")
          ),
          uiOutput("markets_ui"),  # Placeholder for markets selection
          selectInput(
            inputId = "selected_sharpbook",
            label = "Select Sharp:",
            choices = list(
              "Pinnacle" = "pinnacle",
              "BetOnline" = "betonlineag"
            ),
            selected = c("pinnacle")
          ),
          checkboxGroupInput(
            inputId = "selected_sportsbooks",
            label = "Select Sportsbooks:",
            choices = list(
              "Fliff" = "fliff",
              "Novig" = "novig",
              "ProphetX" = "prophetx",
              #"ESPN Bet" = "espnbet",
              #"DraftKings" = "draftkings",
              #"FanDuel" = "fanduel",
              #"Caesars" = "williamhill_us",
              #"BetMGM" = "betmgm",
              #"SuperBook" = "superbook",
              #"Bally Bet" = "ballybet",
              #"BetRivers" = "betrivers",
              #"Bovada" = "bovada",
              #"BetUs" = "betus",
              #"LowVig.ag" = "lowvig",
              #"MyBookie.ag" = "mybookieag",
              #"BetAnySports" = "betanysports",
              #"betPARX" = "betparx",
              "PrizePicks" = "prizepicks"
              #"Underdog" = "underdog"
            ),
            selected = c("fliff", "novig", "prophetx")
          ),
          conditionalPanel(
            condition = "input.selected_sportsbooks.includes('prizepicks')",
            selectInput(
              inputId = "prizepicks_play_type",
              label = "Select PrizePicks Play Type:",
              choices = list(
                "2-man power" = "manpower2",
                "3-man flex" = "manflex3",
                "3-man power" = "manpower3",
                "4-man flex" = "manflex4",
                "4-man power" = "manpower4",
                "5-man flex" = "manflex5",
                "6-man flex" = "manflex6"
              ),
              selected = "manflex5"
            )
          ),
          conditionalPanel(
            condition = "input.selected_sportsbooks.includes('underdog')",
            selectInput(
              inputId = "underdog_play_type",
              label = "Select Underdog Play Type:",
              choices = list(
                "2-man non-insured" = "mannoninsured2",
                "3-man non-insured" = "mannoninsured3",
                "3-man insured" = "maninsured3",
                "4-man non-insured" = "mannoninsured4",
                "4-man insured" = "maninsured4",
                "5-man non-insured" = "mannoninsured5",
                "5-man insured" = "maninsured5"
              ),
              selected = "maninsured5"
            )
          ),
          actionButton("run_button", "Get EV Plays"),
          br(),
          br(),
          downloadButton("download_data", "Download CSV"),
          width = 2  # Adjust sidebar width
        ),
        mainPanel(
          h4("Expected Value Plays"),
          withSpinner(reactableOutput("ev_table"), type = 6),
          width = 10  # Adjust main panel width
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  values <- reactiveValues()  # Initialize reactiveValues to store data
  
  # User credentials
  credentials <- data.frame(
    user = c("admin"),  # Replace with real usernames
    password = c("adminpass"),  # Replace with real passwords
    stringsAsFactors = FALSE
  )

  # Function to check credentials
  check_credentials <- function(username, password) {
    any(credentials$user == username & credentials$password == password)
  }

  # Show login modal on startup
  showModal(modalDialog(
    title = "Login",
    textInput("username", "Username:"),
    passwordInput("password", "Password:"),
    footer = tagList(
      actionButton("login_button", "Login")
    ),
    easyClose = FALSE,  # Prevent closing the modal by clicking outside
    fade = FALSE
  ))

  # Handle login
  observeEvent(input$login_button, {
    if (check_credentials(input$username, input$password)) {
      removeModal()
      shinyjs::show("main_ui")  # Show the main UI upon successful login
    } else {
      # Shake the login modal to indicate error (visual feedback)
      shinyjs::addClass(selector = ".modal-dialog", class = "shake")
      delay(1000, shinyjs::removeClass(selector = ".modal-dialog", class = "shake"))

      # Display an error message
      showNotification("Incorrect username or password. Please try again.", type = "error")
    }
  })

  # Hide the main UI initially
  shinyjs::hide("main_ui")


  
  # Market lists for each sport
  markets_list_nfl <- c(
    "player_pass_tds",
    "player_pass_yds",
    "player_rush_yds",
    "player_rush_longest",
    "player_kicking_points",
    "player_field_goals",
    "player_reception_yds",
    "player_reception_longest",
    "player_assists",
    "player_pass_attempts",
    "player_pass_completions",
    "player_pass_interceptions",
    "player_pass_longest_completion",
    "player_pass_rush_reception_tds",
    "player_pass_rush_reception_yds",
    "player_pats",
    "player_receptions",
    "player_rush_attempts",
    "player_rush_reception_tds",
    "player_rush_reception_yds",
    "player_sacks",
    "player_solo_tackles",
    "player_tackles_assists"
  )
  
  markets_list_nba <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_mlb <- c(
    "batter_home_runs",
    "batter_hits",
    "batter_total_bases",
    "batter_rbis",
    "batter_runs_scored",
    "batter_hits_runs_rbis",
    "batter_singles",
    "batter_doubles",
    "batter_triples",
    "batter_walks",
    "batter_strikeouts",
    "pitcher_strikeouts",
    "pitcher_outs",
    "pitcher_hits_allowed",
    "pitcher_walks",
    "pitcher_earned_runs"
  )
  
  markets_list_nhl <- c(
    "player_points",
    "player_power_play_points",
    "player_assists",
    "player_blocked_shots",
    "player_shots_on_goal",
    "player_goals",
    "player_total_saves"
  )
  
  markets_list_ncaaf <- c(
    "player_pass_tds",
    "player_pass_yds",
    "player_rush_yds",
    "player_rush_longest",
    "player_kicking_points",
    "player_field_goals",
    "player_reception_yds",
    "player_reception_longest",
    "player_assists",
    "player_pass_attempts",
    "player_pass_completions",
    "player_pass_interceptions",
    "player_pass_longest_completion",
    "player_pass_rush_reception_tds",
    "player_pass_rush_reception_yds",
    "player_pats",
    "player_receptions",
    "player_rush_attempts",
    "player_rush_reception_tds",
    "player_rush_reception_yds",
    "player_sacks",
    "player_solo_tackles",
    "player_tackles_assists"
  )
  
  markets_list_ncaab <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_wnba <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_mls <- c(
    "player_shots_on_target",
    "player_shots",
    "player_assists"
  )
  
  markets_list_epl <- c(
    "player_shots_on_target",
    "player_shots",
    "player_assists"
  )
  
  # Render markets selection UI
  output$markets_ui <- renderUI({
    req(input$selected_sport)
    markets_list <- switch(
      input$selected_sport,
      "americanfootball_nfl" = markets_list_nfl,
      "basketball_nba" = markets_list_nba,
      "baseball_mlb" = markets_list_mlb,
      "icehockey_nhl" = markets_list_nhl,
      "americanfootball_ncaaf" = markets_list_ncaaf,
      "basketball_ncaab" = markets_list_ncaab,
      "basketball_wnba" = markets_list_wnba,
      "soccer_usa_mls" = markets_list_mls,
      "soccer_epl" = markets_list_epl,
      character(0)
    )
    markets_choices <- setNames(markets_list, gsub("_", " ", markets_list))
    
    pickerInput(
      inputId = "selected_markets",
      label = "Select Markets:",
      choices = markets_choices,
      selected = markets_list,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} markets selected",
        `none-selected-text` = "No markets selected"
      )
    )
  })
  
  # Observe the "Get EV Plays" button
  observeEvent(input$run_button, {
    req(input$selected_markets)  # Ensure markets are selected
    # Show a loading message
    showModal(modalDialog("Fetching data, please wait...", footer = NULL))
    
    # Wrap the data fetching and processing in a tryCatch to handle errors
    tryCatch({
      # Your data fetching and processing logic
      
      # Update the sportsbooks based on user selection
      sharp_book <- input$selected_sharpbook
      other_bookmakers <- input$selected_sportsbooks
      
      # Combine all bookmakers into one vector, including the sharp_book
      all_bookmakers <- c(sharp_book, other_bookmakers)
      
      # Create the bookmakers string separated by commas
      bookmakers <- paste(all_bookmakers, collapse = ",")
      
      # Define the outcomes_price values for PrizePicks and Underdog
      # PrizePicks play types
      prizepicks_prices <- list(
        manpower2 = 1.74,
        manflex3 = 1.69,
        manpower3 = 1.71,
        manflex4 = 1.76,
        manpower4 = 1.78,
        manflex5 = 1.84,
        manflex6 = 1.84
      )
      
      # Underdog play types
      underdog_prices <- list(
        mannoninsured2 = 1.74,
        mannoninsured3 = 1.82,
        maninsured3 = 1.73,
        mannoninsured4 = 1.78,
        maninsured4 = 1.81,
        mannoninsured5 = 1.82,
        maninsured5 = 1.83
      )
      
      # Set the selected outcomes_price for PrizePicks and Underdog
      selected_prizepicks_price <- prizepicks_prices[[input$prizepicks_play_type]]
      selected_underdog_price <- underdog_prices[[input$underdog_play_type]]
      
      # Get the list of games for today
      todays_games <- toa_sports_odds(
        sport_key = input$selected_sport,
        regions = "eu",
        markets = "h2h",
        odds_format = "decimal",
        date_format = "iso"
      )
      
      # Filter for the selected sharp_book
      sharp_line_h2h <- todays_games %>%
        filter(bookmaker_key == sharp_book)
      
      todays_game_ids <- sharp_line_h2h %>%
        select(id) %>%
        unique() %>%
        pull()
      
      # Initialize an empty list to store the odds for each game and market
      all_odds_list <- list()
      
      # Loop through each game ID and each market to get the odds
      for (event_id in todays_game_ids) {
        for (market in input$selected_markets) {
          # Try to retrieve the odds for the current event_id and market
          event_odds <- tryCatch(
            {
              suppressMessages(
                suppressWarnings(
                  toa_event_odds(
                    sport_key = input$selected_sport,
                    event_id = event_id,
                    markets = market,
                    odds_format = "decimal",
                    date_format = "iso",
                    bookmakers = bookmakers
                  )
                )
              )
            },
            error = function(e) {
              NULL
            }
          )
          # If event_odds is not NULL, add to list
          if (!is.null(event_odds)) {
            all_odds_list[[paste(event_id, market, sep = "_")]] <- event_odds
          }
        }
      }
      
      # Combine all the odds into a single data frame
      all_odds_df <- bind_rows(all_odds_list, .id = "event_market_id") %>%
        rename(event_id = event_market_id)
      
      if (sharp_book %in% unique(all_odds_df$bookmaker_key)) {
        # Filter for sharp_book data
        sharp_odds <- all_odds_df %>% filter(bookmaker_key == sharp_book)
        
        # Calculate implied probabilities and holds
        sharp_odds <- sharp_odds %>%
          mutate(implied_probability = 1 / outcomes_price) %>%
          group_by(event_id, market_key, outcomes_description) %>%
          mutate(hold = sum(implied_probability) - 1) %>%
          ungroup()
        
        # Calculate average hold per event and market
        sharp_average_hold <- sharp_odds %>%
          group_by(event_id, market_key) %>%
          summarize(average_hold = mean(hold, na.rm = TRUE)) %>%
          ungroup()
        
        # Function to process each bookmaker
        process_bookmaker <- function(sharp_data, bookmaker_data, bookmaker_name) {
          # # Special handling for 'prizepicks'
          # if (bookmaker_name == "prizepicks") {
          #   # Duplicate data to create 'Under' outcomes
          #   bk_over <- bookmaker_data
          #   bk_over$outcomes_name <- "Over"
          #   bk_under <- bookmaker_data
          #   bk_under$outcomes_name <- "Under"
          #   bookmaker_data <- bind_rows(bk_over, bk_under)
          #   
          #   # Set the outcomes_price to the selected value
          #   bookmaker_data$outcomes_price <- selected_prizepicks_price
          # }
          # 
          # # Special handling for 'underdog'
          # if (bookmaker_name == "underdog") {
          #   # Duplicate data to create 'Under' outcomes
          #   bk_over <- bookmaker_data
          #   bk_over$outcomes_name <- "Over"
          #   bk_under <- bookmaker_data
          #   bk_under$outcomes_name <- "Under"
          #   bookmaker_data <- bind_rows(bk_over, bk_under)
          #   
          #   # Set the outcomes_price to the selected value
          #   bookmaker_data$outcomes_price <- selected_underdog_price
          # }
          
          # Merge sharp book data with bookmaker data
          merged_data <- sharp_data %>%
            left_join(
              bookmaker_data %>%
                select(event_id, market_key, outcomes_description, outcomes_name,
                       bk_outcomes_price = outcomes_price, bk_outcomes_point = outcomes_point),
              by = c("event_id", "market_key", "outcomes_name", "outcomes_description")
            )
          
          # Calculate bookmaker's implied probability and EV
          merged_data <- merged_data %>%
            mutate(
              bk_probability = 1 / bk_outcomes_price,
              ev = implied_probability - bk_probability,
              bookmaker_name = bookmaker_name  # Add bookmaker_name column
            ) %>%
            left_join(sharp_average_hold, by = c("event_id", "market_key")) %>%
            mutate(play = ifelse(ev > 0.5 * average_hold, toupper(bookmaker_name), bookmaker_name))
          
          # Filter and clean the data
          merged_data <- merged_data %>%
            filter(outcomes_point == bk_outcomes_point) %>%
            mutate(ev = round(ev, 4),
                   play = as.character(play)) %>%
            rename(
              sharp_price = outcomes_price,
              sharp_point = outcomes_point,
              retail_price = bk_outcomes_price,
              retail_point = bk_outcomes_point
            ) %>%
            select(event_id, sport_title, commence_time, home_team, away_team,
                   market_key, outcomes_name, outcomes_description, sharp_price,
                   sharp_point, retail_price, retail_point, ev, play, bookmaker_name, average_hold)
          
          return(merged_data)
        }
        
        # Initialize an empty list to store the results
        list_of_dfs <- list()
        
        # Loop over each bookmaker
        for (bk in other_bookmakers) {
          # Extract data for this bookmaker
          bk_data <- all_odds_df %>% filter(bookmaker_key == bk)
          
          # Only process if bk_data is not empty
          if (nrow(bk_data) > 0) {
            # Process the bookmaker's data
            processed_data <- process_bookmaker(sharp_odds, bk_data, bk)
            
            # Add the result to the list
            list_of_dfs[[bk]] <- processed_data
          }
        }
        
        # Combine all processed data frames
        player_prop_ev <- bind_rows(list_of_dfs)
        
        player_prop_ev <- player_prop_ev %>%
          select(-event_id)
        
        player_prop_ev <- player_prop_ev %>%
          mutate(commence_time = format(
            with_tz(ymd_hms(commence_time, tz = "UTC"),
                    tz = Sys.timezone()),
            "%Y-%m-%d %I:%M:%S %p"))
        
        # If play is "WILLIAMHILL_US", change it to "CAESARS"
        player_prop_ev <- player_prop_ev %>%
          mutate(play = ifelse(play == "WILLIAMHILL_US", "CAESARS", play),
                 bookmaker_name = ifelse(bookmaker_name == "williamhill_us", "caesars", bookmaker_name))
        
        player_prop_ev <- player_prop_ev %>%
          mutate(ev = round(ev * 100, 2))
        
        # Ensure the decimal_to_american function works correctly on vectors
        decimal_to_american <- function(decimal_odds) {
          american_odds <- ifelse(
            decimal_odds >= 2,
            (decimal_odds - 1) * 100,
            -100 / (decimal_odds - 1)
          )
          return(american_odds)
        }
        
        # Convert decimal odds to American odds
        player_prop_ev <- player_prop_ev %>%
          mutate(sharp_price = round(decimal_to_american(as.numeric(sharp_price))),
                 retail_price = round(decimal_to_american(as.numeric(retail_price))))
        
        # Remove underscores from all character columns
        player_prop_ev <- player_prop_ev %>%
          mutate(across(where(is.character), ~ gsub("_", " ", .)))
        
        # Create a unique identifier for each bet (including bookmaker_name)
        player_prop_ev <- player_prop_ev %>%
          mutate(bet_id = paste(home_team, away_team, market_key, outcomes_description, retail_point, bookmaker_name, sep = "_"))
        
        # Split the data into two parts: main bets and their counterparts
        main_bets <- player_prop_ev %>%
          filter(play == toupper(play))  # Bets that meet the EV criteria
        
        # Prepare the data for the details (other side of the bet)
        # Get the other outcomes for the same bets from the same bookmaker
        other_bets <- player_prop_ev %>%
          filter(play != toupper(play))  # Bets that do not meet the EV criteria
        
        other_bets <- other_bets %>%
          mutate(bet_id = paste(home_team, away_team, market_key, outcomes_description, retail_point, bookmaker_name, sep = "_"))
        
        other_bets <- other_bets %>%
          filter(bet_id %in% main_bets$bet_id) 
        
        # Remove 'bet_id' and 'bookmaker_name' from displayed data
        main_bets_display <- main_bets %>% select(-bet_id, -bookmaker_name, -average_hold)
        other_bets_display <- other_bets %>% select(-bet_id, -bookmaker_name, -average_hold)
        
        # Store the main bets and their counterparts
        values$main_bets <- main_bets
        values$main_bets_display <- main_bets_display
        values$other_bets <- other_bets
        values$other_bets_display <- other_bets_display
        
        # Store the result in reactiveValues for download
        values$player_prop_ev <- player_prop_ev %>% select(-bet_id, -bookmaker_name, -average_hold)  # Store data for export
        
        # Display the result in the table
        output$ev_table <- renderReactable({
          reactable(
            values$main_bets_display,
            columns = list(
              sport_title = colDef(name = "Sport"),
              commence_time = colDef(name = "Game Time"),
              home_team = colDef(name = "Home Team"),
              away_team = colDef(name = "Away Team"),
              market_key = colDef(name = "Market"),
              outcomes_name = colDef(name = "Over/Under"),
              outcomes_description = colDef(name = "Player"),
              sharp_price = colDef(name = "Sharp Price"),
              sharp_point = colDef(name = "Sharp Point"),
              retail_price = colDef(name = "Retail Price"),
              retail_point = colDef(name = "Retail Point"),
              ev = colDef(
                name = "EV %",
                cell = function(value, index) {
                  if (values$main_bets_display$play[index] == toupper(values$main_bets_display$play[index])) {
                    div(class = "highlight-positive", value)
                  } else {
                    value
                  }
                },
                format = colFormat(digits = 2)
              ),
              play = colDef(
                name = "Play",
                cell = function(value) {
                  if (value == toupper(value)) {
                    div(class = "highlight-positive", value)
                  } else {
                    value
                  }
                }
              )
            ),
            defaultColDef = colDef(
              align = "center",
              minWidth = 70
            ),
            defaultPageSize = 25,
            resizable = TRUE,
            highlight = TRUE,
            bordered = TRUE,
            striped = TRUE,
            pagination = TRUE,
            defaultSorted = "ev",
            defaultSortOrder = "desc",
            fullWidth = TRUE,
            theme = reactableTheme(),
            class = "player-prop-table",
            # Add row details to show the other side of the bet
            details = function(index) {
              bet_id <- values$main_bets$bet_id[index]
              # Get the other side of the bet
              counterpart <- values$other_bets %>%
                filter(bet_id == !!bet_id)
              if (nrow(counterpart) > 0) {
                reactable(
                  values$other_bets_display %>% filter(values$other_bets$bet_id == bet_id),
                  columns = list(
                    sport_title = colDef(name = "Sport"),
                    commence_time = colDef(name = "Game Time"),
                    home_team = colDef(name = "Home Team"),
                    away_team = colDef(name = "Away Team"),
                    market_key = colDef(name = "Market"),
                    outcomes_name = colDef(name = "Over/Under"),
                    outcomes_description = colDef(name = "Player"),
                    sharp_price = colDef(name = "Sharp Price"),
                    sharp_point = colDef(name = "Sharp Point"),
                    retail_price = colDef(name = "Retail Price"),
                    retail_point = colDef(name = "Retail Point"),
                    ev = colDef(name = "EV %", format = colFormat(digits = 2)),
                    play = colDef(name = "Play")
                  ),
                  fullWidth = TRUE,
                  class = "details-table"
                )
              } else {
                HTML("<i>No additional details available</i>")
              }
            }
          )
        })
      } else {
        showNotification(
          paste("Sharp book", sharp_book, "odds not available in the data."),
          type = "error"
        )
      }
      
    }, error = function(e) {
      # Handle any errors that occur during processing
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
    })
  })
  
  # Download handler for the CSV file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("EV_Plays_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure that the data exists before attempting to write it
      req(values$player_prop_ev)
      write.csv(values$player_prop_ev, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)