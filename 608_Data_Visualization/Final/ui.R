# title: "Final Project"
# subtitle: "CUNY 608 Knowledge and Visual Analytics DATA 2021"
# author: "Karim Hammoud"


##### Final Project
# Your final project is to create a public visualization (static or interactive) using data relevant
# to a current policy, business, or justice issue. You may use any dataset you can find for this
# assignment, as long as it is either public or you have permission from the data’s
# owner/administrator to work with it and share it.
# 
# Recommended data sources are: governmental data, data provided by a
# non-profit/Nongovernmental organizations, and data available from large, semi-structured
# data sets (ie social networks, company financials, etc).
# 
# You must document each step of your data analysis process (excluding data acquisition) in
# code: this will include changing the format of the data and the creation of any images or
# interactive displays that are made.
# 
# You must also include a short (2-3 paragraph) write-up on the visualization. This write-up
# must include the following: the data source, what the parameters of the data set are
# (geography, timeframe, what the data points are, etc) what the data shows, and why it is
# important. Your proposal and your final visualization must be cleared by the instructor, and
# will be hosted on a public page (it will be available for the world to see).
# 
##### Grading:
#   
#   This assignment will account for 40% of your final grade. Points will be awarded for the
# following components:
#   
#   25% - finding your dataset(s) and getting approval for your project on-time, recognition of
# strength/weaknesses of data, analysis to find insights in the data
# 
# 25% - data handling: cleaning, outlier/null handling, and transfer/loading data to the web
# 
# 40% - data presentation: compliance with best data visualization practices, clarity,
# information-to-ink ratio, how memorable the visualization is
# 
# 10% - contextual write-up: why the data is important, why the insights are important Due
# Dates:
#   
#   Note - The type of deliverable you provide will depend on the strategy you use for this
# project. If you put together an interactive visualization, you should be able to provide code
# that I will be able to run and host locally. If you are choosing static visualizations, your write
# up will be more important to your overall grade, and it may be useful to think about how
# you’re presenting these visualizations (in a formated R markdown document for example.)




###### My Project Proposal 
# The main purpose of this data is to explore the pollution data and understand the insights, the risk factors and deaths related to that.

# Per Wikipedia `Air pollution is the presence of substances in the atmosphere that are harmful to the health of humans and other living beings, or cause damage to the climate or to materials. There are many different types of air pollutants, such as gases (including ammonia, carbon monoxide, sulfur dioxide, nitrous oxides, methane, carbon dioxide and chlorofluorocarbons), particulates (both organic and inorganic), and biological molecules. Air pollution may cause diseases, allergies and even death to humans; it may also cause harm to other living organisms such as animals and food crops, and may damage the natural environment (for example, climate change, ozone depletion or habitat degradation) or built environment (for example, acid rain). Both human activity and natural processes can generate air pollution.`
# 
# # In this project I will build a shiny app for the data to show the pollution per country and year.
# 
# # I acquired the data from Kaggle you can check it [here]("https://www.kaggle.com/pavan9065/air-pollution) the data consists of the Country and the number of deaths due to pollution and other factors.
# 
# 
# # Another thing I would like to analyze the risk factors and deaths and show visualization of them
# 
# 
# # read the data
# 
# 
# ```{r}
# total <- read.csv("https://raw.githubusercontent.com/akarimhammoud/CUNY-SPS/master/608_Data_Visualization/Final_Project/Data/death-rates-total-air-pollution.csv")
# 
# factor <- read.csv("https://raw.githubusercontent.com/akarimhammoud/CUNY-SPS/master/608_Data_Visualization/Final_Project/Data/number-of-deaths-by-risk-factor.csv")
# 
# death <- read.csv("https://raw.githubusercontent.com/akarimhammoud/CUNY-SPS/master/608_Data_Visualization/Final_Project/Data/death-rates-from-air-pollution.csv")
# 
# share_deaths <- read.csv("https://raw.githubusercontent.com/akarimhammoud/CUNY-SPS/master/608_Data_Visualization/Final_Project/Data/share-deaths-air-pollution.csv")
# ```
# 
# 
# # Lets see the summary of the factors table
# ```{r}
# summary(factor)
# ```
# 
# 
# # Lets see the summary of the share deaths 
# ```{r}
# names(share_deaths)[4] <- "total"
# 
# summary(share_deaths)
# ```
# 
# Now lets plot the top 20 areas with share of death caused of Air Pollution in 20
# 
# ```{r}
# library(tidyverse)
# 
# # filter just 2017
# share_deaths1 <- share_deaths[share_deaths$Year == "2017",]
# 
# # get the top 20
# share_deaths2 <- share_deaths1 %>% top_n(20)
# share_deaths2
# 
# # plot them
# ggplot(data = share_deaths2, aes(x = total, y = Entity)) +
#   geom_point(axes=FALSE) +
#   labs(title = "Top 20 areas with share of death")
# 
# ```
# 
# 
# # Here we can ssee the top 20 areas and countires with Air Pullotions.
# 
# # This project seems really interesting especially in what we see in clmate change and other factors are effecting our planet.
# 




#### The analysis

# I will start by visulizing the data of Deaths - Air pollution - Sex: Both - Age: Age-standardized (Rate)

# This analysis based on country from 1990 to 2017

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)


df <-  read.csv("https://raw.githubusercontent.com/akarimhammoud/CUNY-SPS/master/608_Data_Visualization/Final_Project/Data/share-deaths-air-pollution.csv")

names(df)[4] <- "Total"


# Need to Rank
df$Rank <-  df$Year %>%
  rank() %>%
  round(0)

df$Rank <- max(df$Rank+1) - df$Rank



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Global Air Poluttion", id="main", theme = shinytheme("lumen"),
             
             tabPanel("About", fluid = TRUE,column(6,
                                                   #br(),
                                                   h4(p("Global Air Poluttion in The Last 20-30 Years")),
                                                   h4(p("About the Project")),
                                                   h5(p("")),
                                                   h5(p("The main prupose of this data is to explore the pollution data and understand the insights, the risk factors and deaths related to that.")),
                                                   h5(p("Per Wikipedia `Air pollution is the presence of substances in the atmosphere that are harmful to the health of humans and other living beings, or cause damage to the climate or to materials. There are many different types of air pollutants, such as gases (including ammonia, carbon monoxide, sulfur dioxide, nitrous oxides, methane, carbon dioxide and chlorofluorocarbons), particulates (both organic and inorganic), and biological molecules. Air pollution may cause diseases, allergies and even death to humans; it may also cause harm to other living organisms such as animals and food crops, and may damage the natural environment (for example, climate change, ozone depletion or habitat degradation) or built environment (for example, acid rain). Both human activity and natural processes can generate air pollution")),
                                                   br(),
                                  
                                                   h4(p("You can access the data on ",a("GitHub", href = "https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data."))),
                                                   br(),
                                                   h5(p("For more info about the data you can access the provider system, at ",a("Kaggle", href = "https://www.kaggle.com/pavan9065/air-pollution"))),
                                                   br(),
                                                   h4(p("About the Author Karim Hammoud")),
                                                   h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at karimalhammoud@gmail.com"),
                                                  br(),
                                                  p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/akarimhammoud/CUNY-SPS/tree/master/608_Data_Visualization"), ".")))
             ),
             
             tabPanel("Analysis", DT::dataTableOutput("Analysis"),
                      
                      # Application title
                      titlePanel('Global Air Poluttion in The Last 20-30 Years'),
                      helpText("Karim Hammoud - CUNY 608 Knowledge and Visual Analytics DATA"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectInput('entity', 'Select Location', unique(df$Entity), selected='Afghanistan')),
                        
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
                          
                          helpText("Global Air Pollution Death Rate per 100,000 persons."))
                      )
             )             
  )
)

