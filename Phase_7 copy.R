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





data<-read_csv("/Users/sunmingrun/Desktop/AI Project/panel_election_results.csv",show_col_types = FALSE)

filtered_data <- data %>%
  filter(Date == " 2024-08-13")

# Calculate the percentage of value = 1 for each Type
percentage_by_type <- filtered_data %>%
  group_by(Voice) %>%
  summarize(Percentage = mean(Result == 0) * 100)

# Print the result
print(percentage_by_type)

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
    No_Republican_State = sum(value == 1, na.rm = TRUE),
    TotalTrial_byState = n(),
    .groups = 'drop'
  )  %>% 
  mutate(Percent_byState_chr=sprintf("%1.2f%%", 100*Percent_byState)) %>% 
  mutate(Percent_byState2_Repub=No_Republican_State/TotalTrial_byState)

View(subdata2)


head(subdata2)
# Creating the second subset with state-specific totals and proportions
subdata3 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Date, StateFull, Type, state, value) %>%
  summarise(
    TotalTrial_byStatebyParty = n()
  )



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

View(extended_data)
head(extended_data)

write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue




#css format
margin_css <- HTML("
.dataTables_wrapper {
    margin-top: -400px; /* Adjust this value as needed */
}
.about-section { 
    margin-bottom: 0px;  /* Remove bottom margin */
}
.about-image {
    margin-top: -20px; /* Reduce the gap between content and image */
    text-align: center; /* Center the image */
}
")

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
    tags$style(margin_css),
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
                                              choices = c("ANONYMOUS"="Direct", "Fox", "BBC", "MSNBC", "Overall"), 
                                              # Make sure averaged_data$Type is available
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
                      )),
             
             
             tabPanel("Time Series All", fluid = TRUE, icon = icon("chart-bar"),
                      tags$style(button_color_css),
                      titlePanel("Time Series All"),
                      fluidRow(
                        column(6,
                               pickerInput(inputId = "TimeSeriesAllState",
                                           label = "Select States",
                                           choices = c(state.name),  # Make sure averaged_data$Type is available
                                           multiple = TRUE,
                                           options = list(maxItems = 1, placeholder = 'select voice name',
                                                          onInitialize = I('function() { this.setValue(""); }'))
                               ),
                              
                               helpText("Select state to create plots"),
                               
                        ),
                        column(6,
                               dateRangeInput("date2", "Date:",   
                                              start = "2024-08-13",
                                              end   = Sys.Date() ),
                               #actionButton(inputId = "EnterTimes", label = "Confirm Times"),
                            
                               helpText("Note: adding notes")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(8,offset=2,
                               plotlyOutput("Overall_2")
                        )
                      )),
             
             
             tabPanel("US Map", icon = icon("map-marker"),
                      titlePanel("Map2"),
                      
                      # Organize elements horizontally using fluidRow and column
                      fluidRow(
                        column(4, 
                               fileInput("file2", "Choose your CSV File", accept = ".csv")
                        ),
                        column(4,
                               radioButtons("voicechoice", "Choice of Voice", 
                                            choices = c("ANONYMOUS"="Direct", "Fox", "BBC", "MSNBC"), 
                                            selected = "Fox"),
                               bsTooltip(id = "voicechoice", 
                                         title = "Select voice type")
                        ),
                        
                        
                        fluidRow(
                          column(4,
                                 dateInput("Date_map", "Date:", value = "2024-08-13"),
                                 
                                 pickerInput(inputId = "USMap_state",
                                             label = "Select States (Max 4)",
                                             choices = state.name,  # Make sure averaged_data$Type is available
                                             multiple = TRUE,
                                             options = list(maxItems = 4, placeholder = 'Enter state name',
                                                            onInitialize = I('function() { this.setValue(""); }'))
                                 ))  # Close column
                          
                        ),
                        
                        fluidRow(
                          column(
                            12,
                            plotlyOutput(outputId = "Plot_Map")
                          )
                        ),
                        
                      ),
                      
             ),
             
             
             
             # Show a plot of the generated distribution
             
             
             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("Other", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("School Types")),
                                          h5(p("US News and World Report uses four categories of schools for their rankings system:"),
                                             p("National universities are those that offer a “full range” of undergraduate majors, while also offering graduate programs, including at the doctoral level.  Intercollegiate sports, including swimming, are generally pursued by undergrads, or occasionally students in master’s degree programs, so a university having nor not having doctoral programs isn’t directly relevant.  That said, doctoral programs and faculty research go hand-in-hand, so faculty at national universities are nearly always active in research, in addition to their teaching duties.  National universities are usually, though not always, large.  Most state flagship universities would fall under this category."),
                                             p("Regional universities are similar to national universities in that they have a wide range of undergrad programs, and some master’s programs as well.  They generally do not have large doctoral programs, and correspondingly less faculty research."),
                                             p("National liberal arts colleges are undergraduate focused, with few graduate programs.  They award the majority of their degrees in arts and sciences, and may or may not have other undergraduate programs, like engineering or professional studies."),
                                             p("Regional colleges are also undergraduate focused institutions, but do not award the majority of their degrees in arts and/or sciences.  These colleges may have a particular focus, like technology or agriculture, or they may be primarily two year institutions that also grant some four year degrees.")
                                          )
                                   ),
                                   column(6,
                                          h4(p("US News Rankings")),
                                          h5(p("Every year the US News and World Report issues a set of rankings for US colleges and universities.  They are a used in this setting as a guideline, and a general comparative device, but can often be misinterpreted or overvalued.  The major component of a given school’s rankings are graduation and retention rates, academic reputation (basically name recognition), and faculty resources (class size, faculty salary etc.).  Each school is given a score, and then placed in order.  That said the scored differences between schools of different rank can be quite small, so take the rankings with a grain of salt.
                                    The full methodology for the US News and World report college rankings can be found ",
                                               a("here.",
                                                 href = "https://www.usnews.com/education/best-colleges/articles/ranking-criteria-and-weights"))
                                          )
                                   ))
                                 
                        ),
                        
                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("The project began as an attempt to combine our interest in artificial intelligence, focusing on its predictive power and potential to shape the future.")),
                                          h5(p("Step 1: Pull 100 news stories from Event Registry: API Search for news stories related to the prompt: “2024 US presidential election”."),
                                             p("Step 2: Feed stories to Chat-GPT in 4 distinct voices：Four characters, each representing different perspectives, will generate 100 stories:"),
                                             h6(p("• Voice 1: Anonymous/Direct truthful reporter"),
                                                p("• Voice 2: Fox Reporter Bret Baier"),
                                                p("• Voice 3: MSNBC Reporter Rachel Maddow"),
                                                p("• Voice 4: BBC Reporter Laura Kuenssberg"),
                                             ),
                                             p("Step 3: Generate election stories from each character’s perspective:For each character, 100 stories are written about the election outcome in each state."),
                                             p("Step 4: Extract the election winners from each story: Use GPT to extract only the name of the winners from the stories for each character."),
                                             p("Step 5: Save winners and display percentage of daily trials that went to each party in each state: 1 = Trump/Republican and 0 = Harris/Democrat"),
                                             p("Step 6: Repeat the process daily, appending new results to the previous day’s data panel.")
                                          ),
                                          br(),
                                          h5(p("We hope you find it interesting and/or useful.  Any comments or questions are welcome at email address"),
                                             p("The source for this project is available ", a("on github", href = "https://github.com/"), ".")),
                                          #hr(),
                                          
                                   ),
                                   column(6,
                                          #br(),
                                          # HTML('<img src="GregPicCrop.png", height="110px"
                                          # style="float:right"/>','<p style="color:black"></p>'),
                                          h4(p("About the Author")),
                                          h5(p("Scott Cunningham is the Ben H. Williams Professor of Economics at Baylor University.  He specializes in a range of topics in applied microeconomics, such as mental illness, drug policy and sex work."),
                                             p("Jared Black"),
                                             p("Coco Mingrun Sun")
                                          ),
                                          
                                          
                                   ),
                                   fluidRow(
                                     column(12,
                                            div(style="text-align: center;",
                                                imageOutput("home_img", width = "50%", height = "auto")
                                            )
                                     )
                                   ),
                                   
                                 )
                        )
             )
  )
)
# Close tabPanel

# Place titlePanel and sidebarLayout outside navbarPage
#titlePanel("US Election Tracker"),



#server started



#View(averaged_data)
server <- function(input, output) {
  
  # define function for each type
  data_subsetMap_Direct <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "Anonymous")%>%
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
  
                                                                              #Function
  #-------- Time Series Tab Line Chart
  Count_data <- reactive({
    req(input$TimeseriesVoices) 
    req(input$TimeSeriesSelectState)
    req(input$date)
    req(input$partychoice)
    filter(extended_data, Type %in% input$TimeseriesVoices) %>%
      filter(StateFull %in% input$TimeSeriesSelectState) %>%
      filter(Date >= input$date[1] & Date <= input$date[2])%>%
      filter(party %in% input$partychoice)
    
  })
  
  
  ##----------
  USA_map <- reactive({
    req(input$voicechoice)
    req(input$Date_map)
    #req(input$USMap_state)
    filter(extended_data, Type %in% input$voicechoice) %>%
      filter(Date %in% input$Date_map)%>%
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
  
  
  Count_data_4Voice <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(extended_data,Date >= input$date2[1] & Date <= input$date2[2])
  })
    

data_subsetMap_ANONYMOUS <- reactive({
  req(input$TimeseriesVoices)
  req(input$date)# Make sure inputs and data are not NULL
  # Filter and bin data
  filter(extended_data, Type == "Direct")%>%
    filter(Date %in% input$date)
})


data_subset_Fox <- reactive({
  req(input$TimeseriesVoices) 
  req(input$date)# Make sure inputs and data are not NULL
  # Filter and bin data
  filter(extended_data, Type == "Fox")%>%
    filter(Date %in% input$date)
})

data_subset_BBC <- reactive({
  req(input$TimeseriesVoices) 
  req(input$date)# Make sure inputs and data are not NULL
  # Filter and bin data
  filter(extended_data, Type == "BBC")%>%
    filter(Date %in% input$date)
})

data_subset_MSNBC <- reactive({
  req(input$TimeseriesVoices)
  req(input$date)# Make sure inputs and data are not NULL
  # Filter and bin data
  filter(extended_data, Type == "MSNBC")%>%
    filter(Date %in% input$date)
})



                                                                             #Render Graph
  ##
output$Overall_1<- renderPlotly({
  fig <- plot_ly(Count_data_4Voice(), x = ~Date, y = ~Republican, color = ~Type, type = 'scatter', mode = 'lines') %>%
    layout(
      title = "Projected Democrat Electoral College Victory Likelihood",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Projected Democrat Win Probability (%)", range = c(0.4, 0.6)),
      shapes = list(
        list(
          type = "rect",
          fillcolor = "rgba(135, 206, 235, 0.3)", # Light blue
          line = list(color = "rgba(135, 206, 235, 0.3)"),
          x0 = min(Count_data_4Voice()$Date), x1 = max(Count_data_4Voice()$Date),
          y0 = 0.5, y1 = 1,
          layer = "above"
        ),
        list(
          type = "rect",
          fillcolor = "rgba(255, 192, 203, 0.3)", # Light red
          line = list(color = "rgba(255, 192, 203, 0.3)"),
          x0 = min(Count_data_4Voice()$Date), x1 = max(Count_data_4Voice()$Date),
          y0 = 0, y1 = 0.5,
          layer = "below"
        )
      )
    ) %>%
    layout(annotations = list(
      list(
        x = min(Count_data_4Voice()$Date),
        y = 0.55,
        text = "Democrat Win",
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      ),
      list(
        x = min(Count_data_4Voice()$Date),
        y = 0.45,
        text = "Republican Win",
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    ))
})


output$Overall_2<- renderPlotly({
  input$date2
  
  fig <- plot_ly(Count_data_4Voice(), x = ~Date, y = ~Democratic, color = ~Type, type = 'scatter', mode = 'lines') %>%
    layout(
      title = "Projected Democrat Electoral College Victory Likelihood",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Projected Democrat Win Probability (%)", range = c(0.4, 0.6)),
      shapes = list(
        list(
          type = "rect",
          fillcolor = "rgba(135, 206, 235, 0.3)", # Light blue
          line = list(color = "rgba(135, 206, 235, 0.3)"),
          x0 = min(Count_data_4Voice()$Date), x1 = max(Count_data_4Voice()$Date),
          y0 = 0.5, y1 = 1,
          layer = "above"
        ),
        list(
          type = "rect",
          fillcolor = "rgba(255, 192, 203, 0.3)", # Light red
          line = list(color = "rgba(255, 192, 203, 0.3)"),
          x0 = min(Count_data_4Voice()$Date), x1 = max(Count_data_4Voice()$Date),
          y0 = 0, y1 = 0.5,
          layer = "below"
        )
      )
    ) %>%
    layout(annotations = list(
      list(
        x = min(Count_data_4Voice()$Date+5),
        y = 0.55,
        text = "Democrat Win",
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      ),
      list(
        x = min(Count_data_4Voice()$Date+5),
        y = 0.45,
        text = "Republican Win",
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    ))
})


  
  #------Time Series Tab Line Chart
  

  
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
                range = c(0, 1.2),
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
  
  
  
  observe({
    data <- Count_data_4Voice()
    print(data, n = 8, width = Inf)  # Look at this output in your R console or log
  })
  
  # Map hover
  output$Plot_Map <- renderPlotly ({
    input$voicechoice
    input$Date_map
    
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
  
  #View(extended_data)
  #part 1
  # UI - PATIENTS - 1 ----------------------------------------------------------
  
  output$home_img <- renderImage({
    
    list(src = "/Users/sunmingrun/Desktop/AI Project/Testing/www/Jared_Image2.png"
         
    )
    
  }, deleteFile = F)
  
}



# Run the application 
shinyApp(ui = ui, server = server)


