# title: "Module3_Shiny_APP"
# subtitle: "CUNY 608 Knowledge and Visual Analytics DATA 2021"
# author: "Karim Hammoud"


# Project Description

# I have provided you with data about mortality from all 50 states and the District of Columbia.

# Please access it at https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data.

# You are invited to gather more data from our provider, the CDC WONDER system, at https://wonder.cdc.gov/ucd-icd10.html.

#+ This assignment must be done in R. It must be done using the 'shiny' package.

#+ It is recommended you use an R package that supports interactive graphing such as plotly, or vegalite, but this is not required.

#+ You will turn in four files to me: one called 'ui.R' and one called 'server.R' for each of problem 1 and problem 2. I will run the apps myself. Please make sure you turn in both scripts and that they work together!

#  + I will create a quick intro video for Shiny, general concepts about interactive visualizations, and packages.


# Question 1

# As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.

## Gathering info.

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)

# Github
url <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'




# Reading file
my.mortality.data <- read.csv(url, header=TRUE, sep=",", stringsAsFactors=FALSE)



### Cleaning Data


# Prep Work
my.mortality.data <-arrange(my.mortality.data,desc(Crude.Rate)) 

my.data_tbl <- my.mortality.data 

# my.data_tbl <- filter(my.mortality.data, Year == '2010') # Filter for 2010 only

#my.tally_tbl <- tally(group_by(my.data_tbl, ICD.Chapter, State)) # Tally groups

#my.tally_tbl$n <- as.numeric(my.tally_tbl$n) # Need as numeric

# Creating a summary
#my.ICD.Chapter_tbl <- my.data_tbl %>%
#                  group_by(ICD.Chapter, State) %>%
#                  summarise('Median.Crude.Rate' = median(Crude.Rate))

df <- my.data_tbl

# Need to Rank
df$Rank <-  df$Crude.Rate %>%
  rank() %>%
  round(0)

df$Rank <- max(df$Rank+1) - df$Rank






# Creating a National Average Summary ----
national_average_summary <- my.mortality.data %>%
  group_by(ICD.Chapter, Year) %>%
  summarise(
    'Deaths' = sum(Deaths), 
    'Population' = sum(Population), 
    'NationalAverage' = round(100000*sum(Deaths)/sum(Population),1)) %>%
  arrange(desc(NationalAverage))

#a1 <- select(filter(df2_summary, State == 'NY' & Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths, NationalAverage))

#a <- select(filter(my.mortality.data, Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths))
#sum(a$Deaths)









# server code filters and returns data in plot.ly

server2 <- function(input, output) {
  
  # Filter REACTIVE Data by Year and ICD.Chapter
  selectedData <- reactive({
    dfSlice <- df %>%
      filter((State == input$option_State), (Year == input$option_Year) , (ICD.Chapter == input$option_ICD.Chapter) ) %>%
      mutate(Rank=min_rank(Rank)) %>%
      arrange(Rank)
  })
  
  # National Average
  national_average <-reactive({ 
    dfSlice <- national_average_summary %>%
      filter((Year == input$option_Year) , (ICD.Chapter == input$option_ICD.Chapter) )
    
  })
  
  # National Average
  national_average_all <-reactive({ 
    dfSlice <- national_average_summary %>%
      filter((ICD.Chapter == input$option_ICD.Chapter) )  %>%
      arrange(Year)
    
  })
  
  # National Average
  State_average_all <-reactive({ 
    dfSlice <- my.mortality.data %>%
      filter((State == input$option_State), (ICD.Chapter == input$option_ICD.Chapter) )  %>%
      arrange(Year)
    
  })
  
  # Plot using plot.ly
  output$plot1 <- renderPlotly({
    
    plot_ly(selectedData(), x = ~ICD.Chapter, y = national_average()$NationalAverage, type=input$option_Graph, name = "National Average") %>%
      add_trace(y = ~Crude.Rate, name = input$option_State) %>%
      layout(title = paste('Year', input$option_Year, sep = " "), #" Single Mortality Rates by State and Year",
             xaxis = list(categoryorder = "array",
                          categoryarray = ~Crude.Rate,
                          type = "category"),
             yaxis = list(title = "Crude Mortality Rate"))
  }
  )
  
  # Plot using plot.ly
  output$plot2 <- renderPlotly({
    
    plot_ly(national_average_all(), x = ~Year, y = ~NationalAverage, type = 'scatter', mode = 'lines+markers', name = 'National Average') %>%
      add_trace(y = State_average_all()$Crude.Rate, mode = 'lines+markers', name =  input$option_State) %>%
      layout(title = paste('Mortality Rates', input$option_State, sep = " - "), #" Single Mortality Rates by State and Year",
             xaxis = list(categoryorder = "array",
                          categoryarray = ~Year,
                          type = "category"),
             yaxis = list(title = "Crude Mortality Rate"))
  }
  )
  
  
  # Generate a summary of the data ----
  
  
  
  # Downloadable csv of selected dataset ----
  #output$downloadData <- downloadHandler(
  #  filename = function() {
  #    paste(input$dataset, ".csv", sep = "")
  #  },
  #  content = function(file) {
  #    write.csv(datasetInput(), file, row.names = FALSE)
  #  })
  
  #output$txt <- renderText({
  #  paste("Rendering: ", input$option_ICD.Chapter)
  #})
  
}

### Run

# Command to **run** shiny app.

# Run the Shiny application 
# shinyApp(ui = ui2, server = server2, options = list(height = 1200) )












