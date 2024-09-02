# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)

# Define the UI
ui <- fluidPage(
  titlePanel("Projected Democrat Electoral College Victory Likelihood"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("voice", "Choose News Source(s):",
                         choices = c("Direct", "Fox", "MSNBC", "BBC"),
                         selected = c("Direct", "Fox", "MSNBC", "BBC")), # All selected by default
      dateRangeInput("dateRange", "Select Date Range:",
                     start = as.Date("2024-08-13"), end = Sys.Date(),  # Set default date range
                     min = "2024-08-13", max = Sys.Date())
    ),
    
    mainPanel(
      plotOutput("forecastPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Load the data
  data <- read_csv("panel_election_results_state.csv", show_col_types = FALSE)  # Ensure the same data source
  
  # Ensure Date column is in Date format
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")  # Explicitly convert to Date format
  
  # Updated electoral vote count per state (2024 numbers)
  electoral_votes <- c(
    AL = 9, KY = 8, ND = 3, AK = 3, LA = 8, OH = 17, AZ = 11, ME = 4, OK = 7, AR = 6, MD = 10, 
    OR = 8, CA = 54, MA = 11, PA = 19, CO = 10, MI = 15, RI = 4, CT = 7, MN = 10, SC = 9, DE = 3, 
    MS = 6, SD = 3, DC = 3, MO = 10, TN = 11, FL = 30, MT = 4, TX = 40, GA = 16, NE = 5, UT = 6, 
    HI = 4, NV = 6, VT = 3, ID = 4, NH = 4, VA = 13, IL = 19, NJ = 14, WA = 12, IN = 11, NM = 5, 
    WV = 4, IA = 6, NY = 28, WI = 10, KS = 6, NC = 16, WY = 3
  )
  
  electoral_votes_df <- data.frame(State = names(electoral_votes), ElectoralVotes = electoral_votes, stringsAsFactors = FALSE)
  
  # Function to calculate Democrat win percentage
  calculate_democrat_win_percentage <- function(data, voice) {
    data %>%
      filter(Voice %in% voice) %>%
      group_by(Date, State, Voice) %>%
      summarize(average_result = mean(Result, na.rm = TRUE), .groups = "drop") %>%
      mutate(Democrat_Win = ifelse(average_result <= 0.5, 1, 0)) %>%
      left_join(electoral_votes_df, by = "State") %>%
      group_by(Date, Voice) %>%
      summarize(Democrat_Votes = sum(Democrat_Win * ElectoralVotes), 
                Total_Votes = sum(ElectoralVotes), .groups = "drop") %>%
      mutate(Democrat_Percent = Democrat_Votes / Total_Votes * 100) %>%
      select(Date, Voice, Democrat_Percent)
  }
  
  # Create a reactive plot based on user inputs
  output$forecastPlot <- renderPlot({
    # Ensure date range input is in Date format
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    
    # Filter data based on input
    voice_data <- calculate_democrat_win_percentage(data, input$voice)
    
    # Filter the data by date range
    filtered_data <- voice_data %>%
      filter(Date >= start_date & Date <= end_date)
    
    # Check if filtered_data is empty
    if(nrow(filtered_data) == 0) {
      return(ggplot() + labs(title = "No data available in the selected date range"))
    }
    
    # Define the Democratic National Convention date range
    dnc_start <- as.Date("2024-08-19")
    dnc_end <- as.Date("2024-08-22")
    dnc_midpoint <- dnc_start + (dnc_end - dnc_start) / 2
    
    # Plot the results with lighter blue and red shading and visible lines
    ggplot(filtered_data, aes(x = as.Date(Date), y = Democrat_Percent, color = Voice, linetype = Voice)) +
      # Add the vertical bar for the DNC
      annotate("rect", xmin = dnc_start, xmax = dnc_end, ymin = 0, ymax = 100, alpha = 0.4, fill = "gray") + 
      geom_line(size = 1.5) +  # Increase line thickness for better visibility
      geom_hline(yintercept = 50, color = "black", size = 1.5) +  # Black line at 50%
      scale_color_manual(
        values = c("Direct" = "#0072B2", "Fox" = "#D55E00", "MSNBC" = "#CC79A7", "BBC" = "#009E73"),
        name = "News Source"
      ) +
      scale_x_date(
        breaks = seq(start_date, end_date, by = "4 days"),  # Set breaks every 4 days
        labels = date_format("%b %d"),  # Format as "Month Day"
        expand = c(0.02, 0)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(x) paste0(x, "%"),
        expand = c(0.02, 0)
      ) +
      # Make the red and blue shading more transparent but add it only once
      annotate("rect", xmin = start_date, xmax = end_date, ymin = 0, ymax = 50, fill = "red", alpha = 0.05) +
      annotate("rect", xmin = start_date, xmax = end_date, ymin = 50, ymax = 100, fill = "blue", alpha = 0.05) +
      # Add the label for the DNC dates
      annotate("text", x = dnc_midpoint, y = 10, 
               label = "<- 19-22 Aug Democratic National Convention ->", color = "black", size = 4, hjust = 0.68) +
      labs(
        title = "Projected Democrat Electoral College Victory Likelihood",
        x = "Date",
        y = "Projected Democrat Win Probability (%)",
        caption = "Data source: Simulated election news data"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)