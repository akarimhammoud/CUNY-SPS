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

## Gathering info

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)
library(DT)

df <-  read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)


# Need to Rank
df$Rank <-  df$Crude.Rate %>%
  rank() %>%
  round(0)

df$Rank <- max(df$Rank+1) - df$Rank


# Creating a National Average Summary ----
national_average_summary <- df %>%
  group_by(ICD.Chapter, Year) %>%
  summarise(
    'Deaths' = sum(Deaths), 
    'Population' = sum(Population), 
    'NationalAverage' = round(100000*sum(Deaths)/sum(Population),1)) %>%
  arrange(desc(NationalAverage))

#a1 <- select(filter(df2_summary, State == 'NY' & Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths, NationalAverage))

#a <- select(filter(my.mortality.data, Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths))
#sum(a$Deaths)


### About the data

# **Crude Rates**

# Crude Rates are expressed as the number of deaths reported each calendar year per the factor you select. The default factor is per 100,000 population, reporting the death rate per 100,000 persons.

# More information can be seeing here: https://wonder.cdc.gov/wonder/help/ucd.html#



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("US Mortality Rate", id="main", theme = shinytheme("lumen"),
             
             tabPanel("About", fluid = TRUE,column(6,
                                                   #br(),
                                                   h4(p("About the Project")),
                                                   h4(p("Please Click on the links below for each question")),
                                                   br(),
                                                   
                                                   h5(p("The data about the cause of mortality from all 50 states and the District of Columbia.")),
                                                   br(),
                                                   h4(p("You can access the data on ",a("GitHub", href = "https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data."))),
                                                   
                                                   h5(p("For more info about the data you can access the provider system, at ",a("CDC Website", href = "https://wonder.cdc.gov/ucd-icd10.html."))),
                                                   
                                                   h5(p("")),
                                                   
                                                   h5(p("Crude Rates are expressed as the number of deaths reported each calendar year per the factor you select. The default factor is per 100,000 population, reporting the death rate per 100,000 persons. More information can be seeing here: https://wonder.cdc.gov/wonder/help/ucd.html#")),
                                                   br(),
                                                   h4(p("Please Click on the link below to see Question 2", style = "color:red")),
                                                   br(),
                                                   h4(p(a("TheQuestion 1:", href = "https://karimhammoud.shinyapps.io/shinyq1/"))),
                                                   h5(p("As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.")),
                                                   br(),
                                                   h4(p(a("TheQuestion 2:", href = "https://karimhammoud.shinyapps.io/shinyq2/"))),
                                                   h5(p("Often you are asked whether particular States are improving their mortality rates (per cause) faster than, or slower than, the national average. Create a visualization that lets your clients see this for themselves for one cause of death at the time. Keep in mind that the national average should be weighted by the national population."))),
                      column(6,
                             h4(p("About the Author Karim Hammoud")),
                             h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at karimalhammoud@gmail.com"),
                                p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/akarimhammoud/CUNY-SPS/tree/master/608_Data_Visualization"), ".")))
             ),
             
             tabPanel("Question 1", DT::dataTableOutput("Question 1"),
                      
                      # Application title
                      titlePanel('Question 1 - Mortality Rates by State and Year'),
                      helpText("Karim Hammoud - CUNY 608 Knowledge and Visual Analytics DATA"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectInput('year', 'Select Year', unique(df$Year), selected='2010'),
                          
                          selectInput("cause", "Select Mortality Cause", 
                                      choices = unique(df$ICD.Chapter))
                          
                        ),
                        
                        mainPanel(
                          #selectInput('option_Graph', 'Type', c('bar','scatter'), selected='scatter'),                      # Option to Select Graph
                          radioButtons("option_Graph", "Graph Type",
                                       choiceNames = list(
                                         #icon("glyphicon glyphicon-align-left", "fa-2x", lib = "glyphicon"),
                                         HTML("<div style='font-size:1em; color:Tomato'> <i class='glyphicon glyphicon-record'></i>  Scatter </div"),
                                         HTML("<div style='font-size:1em; color:Tomato'> <i class='glyphicon glyphicon-align-left'></i> Bar </div> ")
                                       ),
                                       choiceValues = list(
                                         "scatter", 
                                         "bar"
                                       ),
                                       inline = TRUE),
                          #textOutput("txt"),
                          plotlyOutput('plot1'),  # Option to zplot unsing plot.ly
                          
                          helpText("Reporting the death rate per 100,000 persons."))
                      )
             )             
             )
  )

