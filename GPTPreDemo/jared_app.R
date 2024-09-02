# Read the panel data from the CSV file
panel_data <- read.csv("panel_election_results_state.csv")

# Convert the Date column to Date format
panel_data$Date <- as.Date(panel_data$Date)

# Add electoral votes information for each state
electoral_votes <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
            "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
            "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
            "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
            "VA", "WA", "WV", "WI", "WY"),
  
  Electoral_Votes = c(9, 3, 11, 6, 54, 10, 7, 3, 30, 16, 
                      4, 4, 19, 11, 6, 6, 8, 8, 4, 10, 
                      11, 15, 10, 6, 10, 4, 5, 6, 4, 14, 
                      5, 28, 16, 3, 17, 7, 8, 19, 4, 9, 
                      3, 11, 40, 6, 3, 13, 12, 4, 10, 3))


filtered_data <-
  panel_data %>%
    mutate(Voice = tolower(Voice)) %>%  # Convert Voice to lowercase
    group_by(State) %>%
    summarize(Percent_Dem_Wins = mean(Result == 0) * 100) %>%
    rename(state = State) %>%  # Rename to `state` to match usmap's expectations
    left_join(electoral_votes, by = "state")


# Reactive function to calculate overall trend data for all voices
trend_data <- 
  panel_data %>%
    mutate(Voice = tolower(Voice)) %>%  # Convert Voice to lowercase
    group_by(Date, Voice, State) %>%
    summarize(
      Percent_Dem_Wins = mean(Result == 0) * 100,
      Percent_Repub_Wins = mean(Result == 1) * 100
    ) 

dataJared<-trend_data%>%
    left_join(electoral_votes, by = c("State" = "state")) %>%
    group_by(Date, Voice)%>%
    summarize(
      Democrat_Votes = sum(Electoral_Votes * Percent_Dem_Wins),
      Repub_Votes = sum(Electoral_Votes * Percent_Repub_Wins)
    )

dataJared2<-trend_data%>%
  left_join(electoral_votes, by = c("State" = "state")) %>%
  group_by(Date, Voice)%>%
  summarize(
    Democrat_Votes = sum(Electoral_Votes * Percent_Dem_Wins /100),
    Repub_Votes = sum(Electoral_Votes * Percent_Repub_Wins /100)
  )%>%
  filter(Voice=="bbc")

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Electoral Vote Predictions"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Select Date:",
                  min = min(panel_data$Date),
                  max = max(panel_data$Date),
                  value = max(panel_data$Date),
                  timeFormat = "%Y-%m-%d",
                  animate = TRUE),
      selectInput("voice",
                  "Select Voice:",
                  choices = c("Direct", "Fox", "MSNBC", "BBC"),
                  selected = "Direct")  # Default to "Direct"
    ),
    
    mainPanel(
      plotOutput("hexMap"),
      textOutput("demVotes"),
      textOutput("repVotes"),
      hr(),  # Adds a horizontal line separator
      plotOutput("trendLine")  # Placeholder for the trend-line plot
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  
  # Reactive function to filter data based on selected date and voice
  filtered_data <- reactive({
    panel_data %>%
      mutate(Voice = tolower(Voice)) %>%  # Convert Voice to lowercase
      filter(Voice == tolower(input$voice), Date == input$date) %>%
      group_by(State) %>%
      summarize(Percent_Dem_Wins = mean(Result == 0) * 100) %>%
      rename(state = State) %>%  # Rename to `state` to match usmap's expectations
      left_join(electoral_votes, by = "state")
  })
  
  # Reactive function to calculate overall trend data for all voices
  trend_data <- reactive({
    panel_data %>%
      mutate(Voice = tolower(Voice)) %>%  # Convert Voice to lowercase
      filter(Voice %in% tolower(c("Direct", "Fox", "MSNBC", "BBC"))) %>%
      group_by(Date, Voice, State) %>%
      summarize(
        Percent_Dem_Wins = mean(Result == 0) * 100
      ) %>%
      left_join(electoral_votes, by = c("State" = "state")) %>%
      group_by(Date, Voice) %>%
      summarize(
        Democrat_Votes = sum(Electoral_Votes * Percent_Dem_Wins / 100)
      )
  })
  
  # Render the hex map
  output$hexMap <- renderPlot({
    plot_usmap(data = filtered_data(), values = "Percent_Dem_Wins", regions = "states") +
      scale_fill_gradient(low = "red", high = "blue", name = "Democrat Wins (%)") +
      labs(title = "Electoral Vote Predictions",
           subtitle = paste("Voice:", input$voice, "- Date:", input$date),
           caption = "538 electoral votes") +
      theme_void()
  })
  
  # Calculate the total electoral votes won by Democrats
  output$demVotes <- renderText({
    dem_votes <- sum(filtered_data()$Electoral_Votes * filtered_data()$Percent_Dem_Wins / 100)
    paste("Democrat Electoral Votes: ", round(dem_votes))
  })
  
  # Calculate the total electoral votes won by Republicans
  output$repVotes <- renderText({
    rep_votes <- sum(filtered_data()$Electoral_Votes * (100 - filtered_data()$Percent_Dem_Wins) / 100)
    paste("Republican Electoral Votes: ", round(rep_votes))
  })
  
  # Render the trend-line plot with all voices for Democrat votes
  output$trendLine <- renderPlot({
    ggplot(data = trend_data(), aes(x = Date, y = Democrat_Votes, color = Voice)) +
      geom_line(size = 1) +
      scale_color_manual(
        values = c("direct" = "blue", "fox" = "red", "msnbc" = "purple", "bbc" = "green"),  # Match lowercase voice names
        labels = c("Direct", "Fox", "MSNBC", "BBC")  # Restore labels for the legend
      ) +
      geom_hline(yintercept = 270, linetype = "dashed", color = "black", size = 1.5) +  # Thick black dashed line at 270
      scale_y_continuous(limits = c(200, 400)) +  # Set y-axis limits from 200 to 400
      scale_x_date(
        date_breaks = "4 day",  # Show a label for every day
        date_labels = "%b %d"   # Format the labels as 'Month Day' (e.g., 'Aug 27')
      ) +
      labs(title = "Democrat Electoral Votes Trend Over Time",
           y = "Democrat Electoral Votes",
           x = "Date",
           color = "Voice") +  # Add a label for the color legend
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui, server) 
