library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(shinyjs)

months <- read.csv("./covid-data-monthly.csv") %>% select(month) %>% unique() %>% arrange(month) %>% pull(month)
dashboardPage(
  
  skin = "black",
  dashboardHeader(title = "COVID-19 Pandemic Analysis"),
  dashboardSidebar(
    sidebarMenu(
      useShinyjs(),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Summary Table", tabName = "table", icon = icon("table")),
      menuItem("Country Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Global statistics", tabName="globalStats", icon= icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    htmlOutput("logo")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidPage(
                box(title = "COVID-19 Cases Map", width = 12,height = 7, status = "primary", solidHeader = TRUE,
                    column(width=8,leafletOutput("covidMap",width="100%",height="60vh")), 
                    column(width=4,
                           h4(textOutput("selected_name")),
                           plotOutput("selected_plot"),
                           sliderInput("date",
                                       "Month:",
                                       min = as.Date(months[1],"%Y-%m-%d"),
                                       max = as.Date(months[length(months)],"%Y-%m-%d"),
                                       value = as.Date(months[1],"%Y-%m-%d"),
                                       timeFormat="%Y-%m"),
                           selectInput("selectedVar",
                                       "Variable:",
                                       c("Total Cases" = "total_cases",
                                         "Total Deaths" = "total_deaths",
                                         "New Cases" = "new_cases",
                                         "ICU Patients" = "icu_patients",
                                         "Hospitalized Patients" = "hosp_patients",
                                         "Total Tests" = "total_tests",
                                         "People Fully Vaccinated" = "people_fully_vaccinated"))
                    )
                ),
                conditionalPanel(
                  box(id="myBox",textInput("country_clicked", "Country Clicked:",value=""),style='display:none;text-color:rgba(0,0,0,0);'),
                  condition = "input.country_clicked != ''",
                  uiOutput("stats")
                )
              )),
      tabItem(tabName = "table",
              fluidPage(
                box(title = "Summary Table", width = 12, status = "primary", solidHeader = TRUE, 
                    DT::dataTableOutput("covidTable"),style = "overflow-x: scroll;")
              )),
      tabItem(tabName = "stats",
              fluidPage(
                box(title = "Country trends", width = 12, status = "primary", solidHeader = TRUE, 
                    column(width=10,
                            fluidRow(
                              plotlyOutput("covidTrends"),
                              plotlyOutput("covidTrendsPC")
                           
                            )),
                      
                    column(width=2,
                           h4("Select a variable to plot:"),
                           selectInput("selectedVar",
                                       "Variable:",
                                       c("Total Cases" = "total_cases",
                                         "Total Deaths" = "total_deaths",
                                         "New Cases" = "new_cases",
                                         "ICU Patients" = "icu_patients",
                                         "Hospitalized Patients" = "hosp_patients",
                                         "Total Tests" = "total_tests",
                                         "People Fully Vaccinated" = "people_fully_vaccinated"))
                           )
                    )
              )),
      tabItem(tabName="globalStats",
              fluidPage(
                box(title = "Global COVID-19 Trends", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("covidTrendsGlobal"))
              )),
      tabItem(tabName = "about",
              box(title = "About", width = 12, height=12, status = "primary", solidHeader = TRUE, style = "overflow-y: scroll; text-align:justify; font-size: 16px",
                  h1("Covid 19 Dashboard"),
                  p("This dashboard provides an analysis of the COVID-19 pandemic using data from the Our World in Data COVID-19 dataset."),
                  h2("Sections"),
                  list(
                    h3("Map"),
                    p("The map section provides an interactive map of COVID-19 cases around the world."),
                    p("You can select one of the variables at the right side of the map to view the data on the map. The date slider will also filter the data for the selected month for each country."),
                    p("Countries are colored based on the selected variable, compared to an all time high (from start to selected date, per capita) value of the variable."),
                    p("Click on a country to view more information about it (it's ranking in terms of cases, deaths and vaccinations compared to other countries)."),
                    p("The graph on the right side of the map shows the selected variable for the world or a selected country for the entire time period."),
                    h3("Summary Table"),
                    p("The summary table can be used to find data of interest. It presents the entire dataset in a filterable way."),
                    h3("Country statistics"),
                    p("This section offers a few graphs with regards to new cases, total cases, total deaths and total vaccinated people. Every plot represents a different metric and every line on the graph is a country. It's interactive to make it easy to compare the rate of adoption of the newest safety standards among countries."),
                    h3("Global statistics"),
                    p("The global statistics section shows a simple interactive graph with which you can compare new cases, total cases, total deaths, vaccinated and hospitalized people over time for the entire world. It gives a very general overview of the development of the global pandemic.")
                  )
              ))
    )
  )
)
