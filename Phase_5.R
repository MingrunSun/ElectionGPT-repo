library(usmap) #import the package
library(shiny)
library(ggplot2) #use ggplot2 to add layer for visualization
library(maps)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)

library(tidytuesdayR) # to get tidytuesday data
library(tidyverse) # for ggplot
library(janitor) # for clean_names
library(ggeasy) # making ggplot customisation easy
library(gganimate) # for animating map by year
library(transformr) # i think i need this for gganimate
library(patchwork) # to patch plots together
library(PNWColors)

library(tidyquant)
library(plotly)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

library(shinyBS)
library(scales)
library(shinyWidgets)
library(bslib)



# Melt the dataframe to long format
data<- read_excel("/Users/sunmingrun/Desktop/AI Project/panel_election_results.xlsx")

#View(data)
# Melt the dataframe to long format and convert the Date to date format
melted_data <- data %>%
  pivot_longer(cols = -c(Date, Voice, Trial), names_to = "state", values_to = "value") %>%
  rename(Type = Voice) %>%
  #mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
  mutate(value = as.numeric(value))%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))

averaged_data <- melted_data %>%
  group_by(state, Type, Date) %>%
  summarise(average_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

#View(averaged_data)
sum(is.na(melted_data$value))


subdata1 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Type, Date) %>%
  summarise(
    TotalTrial = n(),
    No_Republican = sum(value == 1, na.rm = TRUE),
    No_Democratic = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Republican = No_Republican / TotalTrial,
    Democratic = No_Democratic / TotalTrial
  )

#Creating the second subset with state-specific totals and proportions
subdata2 <- melted_data %>%
  group_by(Date, Type, state) %>%
  summarise(
    TotalTrial_byState = n(),
    .groups = 'drop'
  ) 
# Creating the second subset with state-specific totals and proportions
subdata3 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Date, StateFull, Type, state, value) %>%
  summarise(
    TotalTrial_byStatebyParty = n()
  )

# Joining the summarized data back to the original melted_data
extended_data <- melted_data %>%
  left_join(subdata1, by = c("Type", "Date")) %>%
  left_join(subdata2, by = c("Date","Type", "state"))%>%
  left_join(subdata3, by = c("Date","Type", "state", "value")) %>%
  mutate(Percent_byStateParty=TotalTrial_byStatebyParty/TotalTrial_byState) %>%
  mutate(Date=as.Date(Date))

#------------Data process done

#View(extended_data)
head(extended_data)
#------color
pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue




#css format
margin_css <- HTML(".dataTables_wrapper {
        margin-top: -400px; /* Adjust this value as needed */}")

button_color_css <- "
#EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = bslib::bs_theme(primary = "orange"),
  tags$head(
    tags$style(margin_css)
  ),
  # Define UI for application that draws a histogram
  
  navbarPage("2024 Presidential Election", theme = shinytheme("lumen"),
             tabPanel("United States Map", icon = icon("map-marker"),
                      titlePanel("Map"),
                      
                      # Organize elements horizontally using fluidRow and column
                      fluidRow(
                        column(4, 
                               fileInput("file1", "Choose your CSV File", accept = ".csv")
                        ),
                        column(4,
                               radioButtons("variablechoice", "Choice of Voice", 
                                            choices = c("Direct", "Fox", "BBC", "MSNBC", "ANONYMOUS"), 
                                            selected = "Direct"),
                               bsTooltip(id = "variablechoice", 
                                         title = "Here is some text with your instructions")
                        ),
                        
                        
                        fluidRow(
                          column(4,
                                 dateInput("date", "Date:", value = "2024-08-13"),
                                 
                                 pickerInput(inputId = "MapID",
                                             label = "Select States (Max 4)",
                                             choices = state.name,  # Make sure averaged_data$Type is available
                                             multiple = TRUE,
                                             options = list(maxItems = 4, placeholder = 'Enter type name',
                                                            onInitialize = I('function() { this.setValue(""); }'))
                                 ))  # Close column
                          
                        ),
                        
                        fluidRow(
                          column(12,
                                 plotOutput(outputId = "Map")
                          )
                        ),
                        
                        dataTableOutput(outputId = "Election")# Close fluidRow
                      )),  # Close tabPanel
             
             tabPanel("Time Series", fluid = TRUE, icon = icon("chart-bar"),
                      tags$style(button_color_css),
                      titlePanel("Time Series"),
                      fluidRow(
                        column(6,
                               pickerInput(inputId = "TimeSeriesSelectState",
                                           label = "Select States",
                                           choices = c(state.name),  # Make sure averaged_data$Type is available
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE,
                                                          title='Enter state name')
                               ),
                               selectizeInput(inputId = "TimeseriesVoices",
                                              label = "Select Voices ",
                                              choices = c("ANONYMOUS"="Direct", "Fox", "BBC", "MSNBC", "Overall"),  # Make sure averaged_data$Type is available
                                              multiple = TRUE,
                                              options = list(maxItems = 1, placeholder = 'select voice name',
                                                             onInitialize = I('function() { this.setValue(""); }'))
                               ),
                               helpText("Select state and voice type to create plots"),
                               
                        ),
                        column(6,
                               dateRangeInput("date", "Date:",   
                                              start = "2024-08-13",
                                              end   = Sys.Date() ),
                               #actionButton(inputId = "EnterTimes", label = "Confirm Times"),
                               
                               radioButtons("partychoice", "Choice of Party", 
                                            choices = c("Republican", "Democratic"), 
                                            selected = "Republican"),
                               helpText("Note: adding notes")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(8,offset=2,
                               plotlyOutput("plot1"),
                               plotlyOutput("plot2")
                        )
                      )
             )  # Close tabPanel
  ) # Close navbarPage
  
  # Place titlePanel and sidebarLayout outside navbarPage
  #titlePanel("US Election Tracker"),
)
#server started



#View(averaged_data)
server <- function(input, output) {
  
  # define function for each type
  data_subsetMap_Direct <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "Direct")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  data_subsetMap_Fox <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "Fox")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  data_subsetMap_BBC <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "BBC")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  data_subsetMap_MSNBC <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "MSNBC")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  data_subsetMap_ANONYMOUS <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "ANOYMOUS")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  # Time Series
  data_subsetMap_ANONYMOUS <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "ANOYMOUS")%>%
      dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  
  #------------time serious render
  # set_1 for
  
  output$Map <- renderPlot({
    # Define color palette
    
    # Conditional rendering based on the selected variable
    if(input$variablechoice == "Direct") {
      plot_usmap(data = data_subsetMap_Direct(), values = "party", color = "white", linewidth = 0.5,
                 labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
      
    } else if(input$variablechoice == "Fox") {
      plot_usmap(data = data_subsetMap_Fox(), values = "party", color = "white", linewidth = 0.5,
                 labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
    } else if(input$variablechoice == "BBC") {
      plot_usmap(data = data_subsetMap_BBC(),  values = "party", color = "white", linewidth = 0.5,
                 labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
    } else if(input$variablechoice == "MSNBC") {
      plot_usmap(data = data_subsetMap_MSNBC(), values = "party", color = "white", linewidth = 0.5,
                 labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
    } else if(input$variablechoice == "ANONYMOUS") {
      plot_usmap(data = data_subsetMap_ANONYMOUS(), values = "party", color = "white", linewidth = 0.5,
                 exclude = c("HI"),labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
    }
  })
  
  output$Election <- DT::renderDataTable({
    DT::datatable(averaged_data,
                  options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  #--------Time Series Tab Line Chart
  Count_data <- reactive({
    req(input$TimeseriesVoices) 
    req(input$TimeSeriesSelectState)
    req(input$date)
    req(input$partychoice)
    filter(extended_data, Type %in% input$TimeseriesVoices) %>%
      filter(StateFull %in% input$TimeSeriesSelectState) %>%
      filter(Date %in% input$date)%>%
      filter(party %in% input$partychoice)
    
  })
  
  #------Time Series Tab Line Chart
  
  
  observe({
    data <- Count_data()
    print(data, n = 8, width = Inf)  # Look at this output in your R console or log
  })
  
  
  
  output$plot1 <- renderPlotly({
    input$EnterTimes
    input$TimeseriesVoices
    input$TimeSeriesSelectState
    input$date
    input$partychoice
    isolate({
      if (length(Count_data()$party) ==0) {
        fig <- ggplotly(
          ggplot(data.frame(x = 1), aes(x = x)) +
            ggtitle("No party fits selected characteristics. \nPlease modify selections.") +
            theme_void() +
            theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 15))
        )
      } else {
        # Initialize the plotly object
        fig <- plot_ly()
        # Loop through each selected state and add a trace for it
        for (k in seq_along(input$TimeSeriesSelectState)){
          state_data <- Count_data()[Count_data()$StateFull == input$TimeSeriesSelectState[k], ]
          fig <- add_trace(fig, data = state_data, 
                           x = ~Date, y = ~Percent_byStateParty, type = 'scatter', mode = 'lines',
                           name = input$TimeSeriesSelectState[k],
                           line = list(width = 2)) %>%
            layout(
              title = "Trend",
              xaxis = list(title = "Date",
                           showgrid = FALSE,
                           zeroline = FALSE,
                           showline = FALSE),
                           #tickformat = "%b %d, %Y"),
              yaxis = list(
                title = "Percent",
                showline = TRUE,
                showgrid = FALSE,
                showticklabels = TRUE,
                tickformat = ".2%",  # Format ticks as percentage
                range = c(-0.5, 1.5),
                tickmode = "linear",
                tick0 = 0,
                dtick = 0.2
              )
            )
        }
        fig  # Return the plotly figure
      }
    })
  })
  
  #View(extended_data)
  #part 1
  # UI - PATIENTS - 1 ----------------------------------------------------------
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)


#
library(shinydashboard)
library(checkpoint)
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)

# Melt the dataframe to long format
data<- read_excel("/Users/sunmingrun/Desktop/AI Project/panel_election_results2.xlsx")


#View(data)
# Melt the dataframe to long format and convert the Date to date format
melted_data <- data %>%
  pivot_longer(cols = -c(Date, Voice, Trial), names_to = "state", values_to = "value") %>%
  rename(Type = Voice) %>%
  #mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
  mutate(value = as.numeric(value))%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))

averaged_data <- melted_data %>%
  group_by(state, Type, Date) %>%
  summarise(average_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

data<-read_csv("/Users/sunmingrun/Desktop/AI Project/panel_election_results.csv",show_col_types = FALSE)
View(data)

melted_data<-data%>%
  rename(
    value = Result,  # Renaming 'Result' to 'value'
    state = State,
    Type= Voice# Renaming 'State' to 'state'
  )%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))






#View(melted_data)
head(melted_data)
#View(averaged_data)
sum(is.na(melted_data$value))


subdata1 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Type, Date) %>%
  summarise(
    TotalTrial = n(),
    No_Republican = sum(value == 1, na.rm = TRUE),
    No_Democratic = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Republican = No_Republican / TotalTrial,
    Democratic = No_Democratic / TotalTrial
  )

#Creating the second subset with state-specific totals and proportions
subdata2 <- melted_data %>%
  group_by(Date, Type, state) %>%
  summarise(
    Percent_byState = mean(value, na.rm = TRUE),
    TotalTrial_byState = n(),
    .groups = 'drop'
  )  %>% 
  mutate(Percent_byState_chr=sprintf("%1.2f%%", 100*Percent_byState))
head(subdata2)
# Creating the second subset with state-specific totals and proportions
subdata3 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Date, StateFull, Type, state, value) %>%
  summarise(
    TotalTrial_byStatebyParty = n()
  )

# ------------FIP merge
# Load necessary library


# Create the dataframe

#Final dataset

extended_data <- melted_data %>%
  left_join(subdata1, by = c("Type", "Date")) %>%
  left_join(subdata2, by = c("Date","Type", "state"))%>%
  left_join(subdata3, by = c("Date","Type", "state", "value")) %>%
  mutate(Percent_byStateParty=TotalTrial_byStatebyParty/TotalTrial_byState) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(year = as.numeric(format(as.Date(Date), "%Y")))%>%
  mutate(abb=state)%>%
  mutate(Predicted_party=ifelse(Percent_byState>=0.5, "Republican", "Democratic")) 

#------------Data process done


head(extended_data)

#write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue



#data
#function 1 state unique label
state_group <- extended_data %>%
  select(StateFull) %>%
  arrange(StateFull) %>%
  distinct()

voice_group <- extended_data %>%
  select(Type) %>%
  arrange(Type) %>%
  distinct()

party_group <- extended_data %>%
  select(party) %>%
  arrange(party) %>%
  distinct()






# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}

# UI
ui <- dashboardPage(
  skin = "red",
  title = "Presidential Election",
  
  dashboardHeader(title = "Presidential Election"),
  dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    sidebarMenu(
      div(id = "sidebar_button",
          bsButton(inputId = "confirm", 
                   label = "START EXPLORE", 
                   icon = icon("play-circle"), 
                   style = "danger")
      ),
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      menuItem(
        "STATES",
        tabName = "stateselect",
        icon = icon("spinner"),
        checkboxGroupButtons(
          inputId = "allInput",
          label = "CHOOSE GROUPS OF ANTIMICROBIALS",
          choices = "ALL / NONE",
          size = "sm",
          selected = "ALL / NONE"
        ),
        checkboxGroupInput(
          inputId = "abGroupInput",
          label = "",
          choices = state_group$StateFull
        )
      ),
      br(),
      br(),
      menuItem(
        "VOICE",
        tabName = "voice",
        icon = icon("user-md"),
        checkboxGroupInput(
          inputId = "voicechoice",
          label = "VOICECHOICE",
          choices = voice_group$Type,
          selected = "Direct"
        )
      ),
      br(),
      br(),
      menuItem(
        "YEAR",
        tabName = "year",
        icon = icon("calendar"),
        sliderInput(
          inputId = "yearInput",
          label = "Year",
          value = c(min(extended_data$year, na.rm = TRUE), max(extended_data$year, na.rm = TRUE)),
          min = min(extended_data$year, na.rm = TRUE),
          max = max(extended_data$year, na.rm = TRUE),
          step = 1L,
          sep = ""
        ),
        dateInput("date", "DAYS TO FIRST PREDICTION (IN RELATION TO START OF NEWS EXTRACTION):",   
                  value = "2024-08-13")
      ),
      br(),
      br(),
      menuItem(
        "PARTY",
        tabName = "Party",
        icon = icon("male"),
        radioButtons(
          inputId = "partychoice",
          label = "PARTYCHOICE",
          choices = party_group$party,
          selected = "Republican"
        )
      ),
      br(),
      br(),
      menuItem(
        "DOWNLOAD SELECTION",
        tabName = "download",
        icon = icon("download"),
        textInput(
          inputId = "filename",
          placeholder = "Name download file",
          label = ""
        ),
        div(
          downloadButton(
            outputId = "downloadData",
            label = "Save Election Prediction Data",
            icon = icon("download"),
            style = "color: black; margin-left: 15px; margin-bottom: 5px;"
          )
        )
      ),
      br()
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar_style.css")
    ),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        bsButton("Map", 
                 label = "Map", 
                 icon = icon("map-marker"), 
                 style = "success"),
        bsButton("Trend", 
                 label = "Trend", 
                 icon = icon("chart-bar"), 
                 style = "success"),
        bsButton("diagnostics", 
                 label = "DIAGNOSTICS", 
                 icon = icon("flask", class = "flask-box"), 
                 style = "success"),
        bsButton("outcome", 
                 label = "OUTCOME", 
                 icon = icon("download"), 
                 style = "success")
      )
    ),
    
    fluid_design("trend_panel", "box1", "box2", "box3", "box4"),
    fluid_design("diagnostics_panel", "box5", "box6", "box7", "box8"),
    fluid_design("outcome_panel", "box_los1", "box_los2", "box_los3", NULL),
    
    fluidRow(
      div(
        id = "map_panel", 
        column(
          width = 12,
          uiOutput("box_pat")
        )
      ),
      column(
        width = 6,
        uiOutput("box_map_table")
      ),
      column(
        width = 6,
        uiOutput("box_year")
      )
    )
  )
)



server <- function(input, output, session) {
  
  
  # UI - Map - 1 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map <- reactive({
    req(input$voicechoice)
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type %in% input$voicechoice) %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Predicted Party:", Predicted_party,"<br>",
                           "Percent",Percent_byState_chr))
  })
  
  
  # Debugging: observe the USA_map() and print its data
  observe({
    data <- USA_map()
    print(data, n = 8, width = Inf)  # Look at this output in your R console or log
  })
  
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat1",
                label = NULL, 
                choices = c("Show all", "Show top 10 only"), 
                selected = "Show all", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          )
        ),
        withSpinner(
          plotlyOutput("box_map_hover", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  
  # Render the Plotly map
  output$box_map_hover <- renderPlotly ({
    input$voicechoice
    input$date
    
    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1)  # Democratic = blue, Republican = red
    )
    
    
    fig <- fig %>% colorbar(title = "Party", tickvals = c(0, 1), ticktext = c("Democratic", "Republican"))
    
    fig <- fig %>% layout(
      title = '2024 Presidential Election Prediction by State',
      geo = g
    )
    
    fig
  })  
}

shinyApp(ui, server)
