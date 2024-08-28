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
  
  dashboardHeader(title = "2024 Election"),
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
          width = 6,
                   uiOutput("box_pat")
          )
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
      column(
        width = 6,
        uiOutput("box_pat3")
      ),
        column(
          width = 6,
          uiOutput("box_pat4")
        )
      )
    )
  )



server <- function(input, output, session) {

  
  # UI - Map - 1 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Anonymous <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="Direct") %>%
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
  
  

  
  # UI - Map - 2 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_BBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="BBC") %>%
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
  
  # UI - Map - 3 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Fox <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="Fox") %>%
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
  
  # UI - Map - 4 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_MSNBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="MSNBC") %>%
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
    data <- USA_map_BBC()
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
          title = "United States Map Anonymous",
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
            plotlyOutput("box_map_anonymous", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
})

  # UI - Map - 2 BBC-------------------------------------------------------
  output$box_pat2 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map BBC",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat2",
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
          plotlyOutput("box_map_BBC", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })

  # UI - Map 3 - Fox -------------------------------------------------------
  output$box_pat3 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map Fox",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat3",
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
          plotlyOutput("box_map_Fox", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  # UI - MSNBC - 4 -------------------------------------------------------
  output$box_pat4 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map MSNBC",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat4",
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
          plotlyOutput("box_map_MSNBC", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  # Render the Plotly map #1
  output$box_map_anonymous <- renderPlotly ({
    input$voicechoice
    input$date
    
    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map_Anonymous(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1),
      showscale = FALSE
    )
    
    
    fig <- fig %>% layout(
      title = 'Voice of Anonymous',
      geo = g
    )
    
    fig
  })  


#-------Map 2 BBC

# Render the Plotly map #1
output$box_map_BBC <- renderPlotly ({
  input$date
  
  l <- list(color = toRGB("white"), width = 1)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE
  )
  
  fig <- plot_geo(USA_map_BBC(), locationmode = 'USA-states', marker = list(line = l)
  )
  fig <- fig %>% add_trace(
    z = ~party_numeric,  # Map the party numeric variable (0 or 1)
    text = ~hover,       # Hover text with details
    locations = ~state,  # State abbreviations
    color = ~party_numeric, # Color based on the party
    colors = c(color_for_0, color_for_1),
    showscale = FALSE
  )
  
  
  fig <- fig %>% colorbar(title = "Party", tickvals = c(0, 1), ticktext = c("Democratic", "Republican"))
  
  fig <- fig %>% layout(
    title = 'Voice of BBC',
    geo = g
  )
  
  fig
})  


#-------Map 3 BBC

# Render the Plotly map #1
output$box_map_Fox <- renderPlotly ({
  input$date
  
  l <- list(color = toRGB("white"), width = 1)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE
  )
  
  fig <- plot_geo(USA_map_Fox(), locationmode = 'USA-states', marker = list(line = l)
  )
  fig <- fig %>% add_trace(
    z = ~party_numeric,  # Map the party numeric variable (0 or 1)
    text = ~hover,       # Hover text with details
    locations = ~state,  # State abbreviations
    color = ~party_numeric, # Color based on the party
    colors = c(color_for_0, color_for_1),
    showscale = FALSE# Democratic = blue, Republican = red
  )
  
  
  fig <- fig %>% layout(
    title = 'Voice of Fox',
    geo = g
  )
  
  fig
})  

#-------Map 4 MSNBC

# Render the Plotly map #1
output$box_map_MSNBC <- renderPlotly ({
  input$date
  
  l <- list(color = toRGB("white"), width = 1)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE
  )
  
  fig <- plot_geo(USA_map_MSNBC(), locationmode = 'USA-states', marker = list(line = l)
  )
  fig <- fig %>% add_trace(
    z = ~party_numeric,  # Map the party numeric variable (0 or 1)
    text = ~hover,       # Hover text with details
    locations = ~state,  # State abbreviations
    color = ~party_numeric, # Color based on the party
    colors = c(color_for_0, color_for_1),
    showscale = FALSE
  )
  
  
  fig <- fig %>% colorbar(title = "Party", tickvals = c(0, 1), ticktext = c("Democratic", "Republican"))
  
  fig <- fig %>% layout(
    title = 'Voice of MSNBC',
    geo = g
  )
  
  fig
})  
}

shinyApp(ui, server)