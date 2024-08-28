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

View(extended_data)
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
                                             label = "Select Voices (Max 5)",
                                             choices = c("ANONYMOUS"="Direct", "Fox", "BBC", "MSNBC", "Overall"),  # Make sure averaged_data$Type is available
                                             multiple = TRUE,
                                             options = list(maxItems = 5, placeholder = 'select voice name',
                                                            onInitialize = I('function() { this.setValue(""); }'))
                                 ),
                                 helpText("Select state and voice type to create plots"),
                                 
                          ),
                          column(6,
                                 dateRangeInput("date", "Date:",   
                                                start = "2024-08-13",
                                                end   = Sys.Date()+2),
                                 actionButton(inputId = "EnterTimes", label = "Confirm Times"),
                                 
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
        fig <- plot_ly(Count_data())
        # Loop through each selected state and add a trace for it
        for (k in seq_along(input$TimeSeriesSelectState)) {
          state_data <- Count_data()[Count_data()$StateFull == input$TimeSeriesSelectState[k], ]
          fig <- add_trace(fig, data = state_data, 
                           x = ~Date, y = ~Percent_byStateParty, type = 'scatter', mode = 'lines',
                           name = input$TimeSeriesSelectState[k],
                           line = list(width = 2)) %>%
          layout(
            title = "Trend",
            xaxis = list(title = "Date",
                         tickformat = "%b %d, %Y",
                         showgrid=FALSE),
            yaxis = list(
              title = "Percent",
              tickformat = ".2%",  # Format ticks as percentage
              range = c(0, 1),
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


