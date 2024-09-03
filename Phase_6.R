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
data<- read_excel("panel_election_results.xlsx")

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
                                              end   = "2024-08-19"),
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
                      
                                 tabPanel("School Types & Rankings", fluid = TRUE,
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
             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("School Types & Rankings", fluid = TRUE,
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
                                          h5(p("Scott Cunningham is the Ben H. Williams Professor of Economics at Baylor University, where he has been a faculty member since 2007. He began his career as a senior business consultant with Customer Integrated Solutions in Atlanta, bringing a practical, real-world perspective to his academic endeavors. After earning his PhD from the University of Georgia, Cunningham transitioned into academia, where he quickly established himself as a leading voice in the field of economics. His commitment to applying economic principles to societal issues led him to collaborate with the Texas Hunger Initiative as a research fellow in 2017. In 2022, his contributions to the field were recognized with his appointment as the Ben H. Williams Professor of Economics."),
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


