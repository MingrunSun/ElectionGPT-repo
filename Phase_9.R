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
library(rjson)

library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)


library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(survival)
library(ggpubr)
library(survminer)
library(viridis)
library(zoo)


data<-read_csv("/Users/sunmingrun/Desktop/AI Project/panel_election_results.csv",show_col_types = FALSE)



melted_data<-data%>%
  rename(
    value = Result,  # Renaming 'Result' to 'value'
    state = State,
    Type= Voice# Renaming 'State' to 'state'
  )%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))

#electoral_votes
electoral_votes <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  Electoral_Votes = c(9, 3, 11, 6, 54, 10, 7, 3, 30, 16, 
                      4, 4, 19, 11, 6, 6, 8, 8, 4, 10, 
                      11, 15, 10, 6, 10, 4, 5, 6, 4, 14, 
                      5, 28, 16, 3, 17, 7, 8, 19, 4, 9, 
                      3, 11, 40, 6, 3, 13, 12, 4, 10, 3, 3))

melted_data<-melted_data%>%
left_join(electoral_votes, by ="state")

# Print the total electoral votes
# Check tab

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



# Comments: sub1 data generates varialbe based on four types of avg gorup by Type and Date
sub1 <- subdata1 %>%
  select(Date, Type, Democratic) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = Democratic, names_prefix = "avg_")%>% 
  arrange(Date)

# View the reshaped data
sub2 <-subdata1 %>%
  select(Date, Type, No_Democratic) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = No_Democratic, names_prefix = "Count_")%>% 
  arrange(Date)


View(sub2)

#Creating the second subset with state-specific totals and proportions
subdata2 <- melted_data %>%
  group_by(Date, Type, state) %>%
  summarise(
    Percent_byState = mean(value, na.rm = TRUE),
    TotalTrial_byState = n(),
    No_Republican_State = sum(value == 1, na.rm = TRUE),
    No_Democratic_State = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  )  %>% 
  mutate(Percent_byState_chr=sprintf("%1.2f%%", 100*Percent_byState))%>% 
  mutate(Percent_byState_Demo=1-Percent_byState)%>% 
  mutate(Percent_byState_chr_Demo=sprintf("%1.2f%%", 100*Percent_byState_Demo))

View(subdata2)
           
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

#View(extended_data)
head(extended_data)

#write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue


pal2<- pnw_palette("Moth",5)
color_plot6_Repub <- pal2[3]

pal3<- pnw_palette("Sunset",5)
color_plot6_Demo <- pal3[1]

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

filtered_data <- extended_data %>%
  filter(Date == "2024-08-15")

View(filtered_data)

distinct <- filtered_data %>%
  distinct(Type,Date, No_Republican,No_Democratic,Republican,Democratic,TotalTrial_byState)







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
          label = "CHOOSE GROUPS OF STATES",
          choices = "ALL / NONE",
          size = "sm",
          selected = "ALL / NONE"
        ),
        checkboxGroupInput(
          inputId = "statesInput",
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
        dateInput("date", "DAYS TO FIRST PREDICTION (USE SINGLE DATE TO PLOT MAP):",   
                  value = "2024-08-13"),
        dateRangeInput("date2", "START DATE TO END DATE ((USE SINGLE DATE TO PLOT MAP):",   
                       start = "2024-08-13",
                       end   = Sys.Date() )
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
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
          bsButton("map", 
                   label = "MAPS", 
                   icon = icon("map-marker"), 
                   style = "success"),
          bsButton("trend", 
                   label = "TREND", 
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
    
    
    
    fluidRow(
      div(
        id = "map_panel", 
        column(
          width = 6,
          uiOutput("box_pat")
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
    ),
    
    
    fluidRow(
      div(
        id = "trend_panel", 
        column(
          width = 12,
          uiOutput("box_pat5")
        ),
        column(
          width = 12,
          uiOutput("box_pat6")
        ),
      )
    )
  )
)
)

server <- function(input, output, session) {
  

  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Maps", "Trends", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  
  
  observeEvent(input$map, {
    update_all("Maps")
  })
  observeEvent(input$trend, {
    update_all("Trends")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })
  
  # update confirm button
  
  observeEvent(input$confirm, {
    updateButton(
      session, 
      inputId = "confirm", 
      label = "CONFIRM SELECTION", 
      icon = icon("bar-chart"), 
      style = "primary")
  })
  
  # hide the underlying selectInput in sidebar for better design
  observeEvent("", {
    hide("tab")
  })
  
  # update all/none group in sidebar and antimicrobials by group
  observe({
    x <- input$allInput
    if (!is.null(x)) {
      x <- state_group$StateFull
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "statesInput",
      label = NULL, 
      choices = state_group$StateFull,
      selected = x
    )
  })
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    show("map_panel")
    hide("trend_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
  }, once = TRUE)
  
  
  observeEvent(input$map, {
    show("map_panel")
    hide("trend_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
  })
  observeEvent(input$trend, {
    show("trend_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
    hide("map_panel")
  })
  observeEvent(input$diagnostics, {
    show("diagnostics_panel")
    hide("trend_panel")
    hide("outcome_panel")
    hide("map_panel")
  })
  observeEvent(input$outcome, {
    show("outcome_panel")
    hide("diagnostics_panel")
    hide("trend_panel")
    hide("map_panel")
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "map", style = {
      if (x == "Maps") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "trend", style = {
      if (x == "Trends") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  
  ######################################3
  
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

  
  
  # UI 5- Time Series 1
  
  Count_data_4Voice <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(sub1,Date >= input$date2[1] & Date <= input$date2[2])
  })
  


  Count_data <- reactive({
    req(input$voicechoice) 
    req(input$statesInput)
    req(input$date2)
    #req(input$partychoice)
    filter(extended_data, Type %in% input$voicechoice) %>%
      filter(StateFull %in% input$statesInput) %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2])
    #%>%filter(party %in% input$partychoice)
    
  })
  
  
  
  observe({
    data <- Count_data()
    print(data, n = 8, width = Inf)  # Look at this output in your R console or log
  })
  
 
 
  
 
  
  
  
  # Render UI -Map 1
  
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
  
  
  # UI - Time Trend - 1 ------------------------------------------------------------------
  output$box_pat5 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Projected Democrat Electoral College Victory Likelihood",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat5",
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
          plotlyOutput("plot_Overall", height = 250),
          type = 4,
          color = "#d33724", 
          size = 0.7
        )
      )
    )
  })
  
  
  # Time Trend - 2 ------------------------------------------------------------------
  output$box_pat6 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "State Projected Democrat Electoral College Victory Likelihood",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_pat6",
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
          plotlyOutput("plot_state", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
 
  # Data Table
  output$box_pat7 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box4",
        width = NULL,
        height = 400,
        tabPanel(
          title = "DDD / DOT table",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box4.0",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "ab_type", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box4.1",
                label = "Select group", 
                choices = c("DDD per 100 bed days", "DOT per 100 bed days"), 
                selected = "DDD per 100 bed days", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            DT::dataTableOutput("table_ab"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  #-------Map 1 Anonymous
  output$box_map_anonymous <- renderPlotly ({
    
    input$confirm
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


#-------Map 3 Fox

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


# UI #5 Overall need to be revised with add DNC 
output$plot_Overall<- renderPlotly({
    input$date2
    
  fig <- plot_ly(Count_data_4Voice(), x = ~Count_data_4Voice()$Date, y = ~avg_Direct, name = 'Proj Anonymous', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(205, 12, 24)', width = 4)) 
  fig <- fig %>% add_trace(y = ~avg_BBC, name = 'Proj BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
  fig <- fig %>% add_trace(y = ~avg_Fox, name = 'ProjFox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~avg_MSNBC, name = 'Proj MSNBC', line = list(color = 'rgb(22, 96, 167)', width =4 , dash = 'dot')) %>%
    layout(
      title = NULL,
      xaxis = list(title = "Date",
                   showgrid = FALSE),
      yaxis = list(title = "Proj Democrat Win Percent", 
                   range = c(0.4, 0.6),
                   showgrid = FALSE),
      shapes = list(
        type = "rect",
        fillcolor = "rgba(22, 96, 167, 0.2)", # Adjust fill color and transparency as needed
        line = list(color = "rgba(22, 96, 167, 0)"), # No borderline = list(color = "rgba(22, 96, 167, 0)"), # No border
        x0 = as.Date("2024-08-19"), x1 = Sys.Date() ,
        y0 = 0.4, y1 = 0.6
      ))%>%
    layout(annotations = list(
      list(
        x = min(Count_data_4Voice()$Date+5),
        y = 0.55,
        text = "Democrat Win",
        showarrow = FALSE,
        font = list(size = 12, weight = "bold", color = color_plot6_Demo),
        showgrid = FALSE
      ),
      list(
        x = min(Count_data_4Voice()$Date+5),
        y = 0.45,
        text = "Republican Win",
        showarrow = FALSE,
        font = list(size = 12, weight = "bold", color = color_plot6_Repub)
      )
    ))
})

# UI #6 Time series by state cannot get graph working
output$plot_state <- renderPlotly({
  input$allInput
  input$voicechoice
  input$statesInput
  input$date2
  #input$partychoice
  
  isolate({
    if (length(Count_data()$party) ==0) {
      fig <- ggplotly(
        ggplot(data.frame(x = 1), aes(x = x)) +
          ggtitle("No party fits selected characteristics. \nPlease modify selections.") +
          theme_void() +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 12))
      )
    } else {
      # Initialize the plotly object
      fig <- plot_ly()
      # Loop through each selected state and add a trace for it
      for (k in seq_along(input$statesInput)){
        state_data <- Count_data()[Count_data()$StateFull == input$statesInput[k], ]
        fig <- add_trace(fig, data = state_data, 
                         x = ~Date, y = ~Percent_byState_Demo, type = 'scatter', mode = 'lines',
                         name = input$statesInput[k],
                         line = list(width = 2)) %>%
          layout(
            xaxis = list(title = "Date",
                         showgrid = FALSE,
                         zeroline = FALSE,
                         showline = FALSE),
            #tickformat = "%b %d, %Y"),
            yaxis = list(
              title = "Proj Democrat Win Percent",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              tickformat = ".0%",  # Format ticks as percentage
              range = c(-0.1, 1.1),
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


}








shinyApp(ui, server)