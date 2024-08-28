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

data <-read_excel('/Users/sunmingrun/Desktop/AI Project/election_results_direct_2024-08-11_01-50-01.xlsx')

data_numeric <- data
data_numeric[, -1] <- lapply(data_numeric[, -1], as.numeric)

#1----Data Reshape
reshaped_data <- data_numeric %>%
  gather(key = "state", value = "value", -Trial)

#reshaped_data <- reshaped_data %>%
#mutate(state = state.name[match(state, state.abb)])
# View the reshaped data
head(reshaped_data)
write.csv(reshaped_data, file = "/Users/sunmingrun/Desktop/AI Project/reshaped_election_results_direct_2024-08-11_01-50-01.csv", row.names = FALSE)
#--------------------------

data3<-read_excel("/Users/sunmingrun/Desktop/AI Project/reshaped_election_results_direct_2024-08-11_01-50-01.xlsx")

#For testing
data3 <- data3 %>%
  mutate(Date = as.Date(Date))

data3 <- data3 %>%
  mutate(value = ifelse(Type == "BBC" & state == "AK", 0, value))

data3 <- data3 %>%
  mutate(value = ifelse(Type == "FOX" & state == "TX", 0, value))


averaged_data <- data3 %>%
  group_by(state, Type, Date) %>%
  summarise(average_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

#View(averaged_data)


#css format
margin_css <- HTML(".dataTables_wrapper {
        margin-top: -400px; /* Adjust this value as needed */}")



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(margin_css)
  ),
  
  navbarPage("2024 Presidential Election", theme = shinytheme("lumen")),
  titlePanel("US Presidential Election Tracker"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose your CSV File", accept = ".csv"),
      hr(),
      
      radioButtons("variablechoice", "Choice of Source", 
                   choices = c("Direct", "FOX", "BBC", "MSNBC", "ANONYMOUS"), 
                   selected = "Direct"),
      hr(),
      dateInput("date", "Date:", value = "2024-08-26")
    ),
    
    mainPanel(
      plotOutput("Map"),
      plotOutput("Time Series"),
      tableOutput("Data"),
      dataTableOutput(outputId = "Election")
    )
  )
)


#server started

pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue

#View(averaged_data)
server <- function(input, output) {
  
  # define function for each type
  data_subsetMap_Direct <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "Direct")%>%
    dplyr::mutate("party"=ifelse(average_value>=0.5, "Republican", "Democratic"))
  })
  
  data_subsetMap_FOX <- reactive({
    req(input$variablechoice)  # Make sure inputs and data are not NULL
    # Filter and bin data
    filter(averaged_data, Type == "FOX")%>%
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
  
  
  
  output$Map <- renderPlot({
    # Define color palette
    
    # Conditional rendering based on the selected variable
    if(input$variablechoice == "Direct") {
      plot_usmap(data = data_subsetMap_Direct(), values = "party", color = "white", linewidth = 0.5,
                 exclude = c("HI"),labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
      
    } else if(input$variablechoice == "FOX") {
      plot_usmap(data = data_subsetMap_FOX(), values = "party", color = "white", linewidth = 0.5,
                 exclude = c("HI"),labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
    } else if(input$variablechoice == "BBC") {
      plot_usmap(data = data_subsetMap_BBC(),  values = "party", color = "white", linewidth = 0.5,
                 exclude = c("HI"),labels=TRUE,
                 label_color = "white") +
        scale_fill_manual(values = c("Democratic" = color_for_0, "Republican" = color_for_1), name = NULL,
                          na.translate = FALSE) +
        labs(title = paste("Map of", input$variablechoice, "Prediction"), x = input$variablechoice) +
        easy_move_legend(to = c("right")) +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 10))
      
    } else if(input$variablechoice == "MSNBC") {
      plot_usmap(data = data_subsetMap_MSNBC(), values = "party", color = "white", linewidth = 0.5,
                 exclude = c("HI"),labels=TRUE,
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
}



# Run the application 
shinyApp(ui = ui, server = server)


fig <- plot_ly(sub1, x = ~Date, y = ~avg_Direct, name = 'Proj Anonymous', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) 
fig <- fig %>% add_trace(y = ~avg_BBC, name = 'Proj BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
fig <- fig %>% add_trace(y = ~avg_Fox, name = 'Proj Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
fig <- fig %>% add_trace(y = ~avg_MSNBC, name = 'Proj MSNBC', line = list(color = 'rgb(22, 96, 167)', width =4 , dash = 'dot')) 
fig <- fig %>%
  layout(
    xaxis = list(title = "Date",
                 showgrid = FALSE),
    yaxis = list(title = "Projected Probability", range = c(0.4, 0.6),
                 showgrid = FALSE)
  ) %>%
  layout(annotations = list(
    list(
      x = min(sub1$Date+5),
      y = 0.55,
      text = "Democrat Win",
      showarrow = FALSE,
      font = list(size = 12, color = "black"),
      showgrid = FALSE
    ),
    list(
      x = min(sub1$Date+5),
      y = 0.45,
      text = "Republican Win",
      showarrow = FALSE,
      font = list(size = 12, color = "black")
    )
  ))
})

