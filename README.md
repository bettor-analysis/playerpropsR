# Player Prop Bet Finder Shiny App

This Shiny application enables you to discover potential *Expected Value (EV) Plays* for 
player prop bets across multiple sportsbooks. Built with **R** and the **oddsapiR** package, it 
fetches real-time odds and compares them to a selected “sharp” book for potential edge identification.

## Features
- **User Authentication**: Simple login modal for basic credential-based access.
- **Dynamic Market Selection**: Choose from multiple leagues (NFL, NBA, MLB, etc.) and corresponding player prop markets.
- **Custom Sportsbook List**: Select one or more sportsbooks to compare against a “sharp” book (e.g., Pinnacle).
- **Odds & EV Calculation**: Calculates implied probabilities, expected value (EV), and highlights positive EV plays.
- **Interactive Table**:
  - Expand rows to see the *other side* of the bet.
  - Column sorting, pagination, and easy data scanning.
- **Download Results**: Export EV plays to a \`.csv\` file.

## Requirements
1. **R (4.0+)**  
2. **Required R packages**:  
   \`\`\`r
   install.packages(c(
     "shiny", 
     "tidyverse", 
     "oddsapiR", 
     "lubridate", 
     "shinycssloaders", 
     "shinyjs", 
     "shinythemes", 
     "reactable", 
     "htmltools", 
     "shinyWidgets"
   ))
   \`\`\`
3. **Odds API Key**: [Obtain a free key](https://the-odds-api.com/) and set as an environment variable:
   \`\`\`r
   Sys.setenv("ODDS_API_KEY" = "YOUR_API_KEY")
   \`\`\`

## How to Run
1. Clone or download this repository.
2. Open the \`playerpropapp.R\` file in RStudio (or any R IDE).
3. Run the app via:
   \`\`\`r
   shiny::runApp("path/to/playerpropapp.R")
   \`\`\`
4. Enter your **Username** and **Password** to access the app (default credentials are \`admin\` / \`adminpass\` within the code).
5. Select your preferred sport, prop markets, sharp book, and sportsbooks, then click **Get EV Plays**.

## Notes
- Customize \`credentials\` in \`playerpropapp.R\` to secure your application.
- Adjust the **markets** lists to fit your needs or add new sports as they become available from the Odds API.


`;
