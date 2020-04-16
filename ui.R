library(shiny)
library(shinythemes)
library(formattable)
library(shinyjs)

playerNames <- c("Select_name", "Coco", "Dianne", "Hilco", "Iris", "Jessica", 
  "Joji", "Joost", "Joram", "Justine", "Marieke", 
  "Nena", "Rogier", "Sanne", "Tajji", "Tim", "Timo")


# Define UI for application that draws a histogram
shinyUI(
  
  # Application title
  navbarPage("One percent better",
             theme = shinytheme("united"),
             tabPanel("Home",
                      includeCSS("custom.css"),
                      includeCSS("odometer-theme-minimal.css"),
                      useShinyjs(),
                      tags$script(src = "odometer.min.js"),
                      # Sidebar with a slider input for number of bins  
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          dateInput("date", "Date:", format = "dd-mm-yyyy"),
                          selectInput("name", "Name:", choices = playerNames),
                          checkboxGroupInput("workoutType", 
                                             "What did you do today?",
                                             choices = c("Daily", "Workout", "Conditioning", "Training", "Pod", "Throwing", "Other")),
                          hr(),
                          textAreaInput("fill_in_field","Explain further (optional)"),
                          actionButton("submit","submit workout")
                          ),
                        # Show a plot of the generated distribution
                        mainPanel(width = 9,
                          fluidRow(
                            column(12,
                                   fluidRow(width = 12,
                                            column(width = 3,
                                                   #uiOutput("total_dailies"),
                                                   div(class = "valuebox",
                                                     div("Total dailies", style = "color: #fff;text-transform: uppercase; font-weight: 700;"),
                                                     div(0, class = "odometer", id = "total_dailies", style = "color: #fff;
                                                         ")
                                                     )
                                                   ),
                                            column(width = 3,
                                                   #uiOutput("days_until_ECBU")
                                                   div(class = "valuebox",
                                                     div("Days until ECBU", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                     div(0, class = "odometer", id = "days_until_ECBU", style = "color: #fff;")
                                                     )
                                                   ),
                                            column(width = 3,
                                                   #uiOutput("team_days")
                                                   div(class = "valuebox",
                                                       div("Team days", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                       div(0, class = "odometer", id = "team_days", style = "color: #fff;")
                                                   )
                                            ),
                                            column(width = 3,
                                                   #uiOutput("hours_invested")
                                                   div(class = "valuebox",
                                                       div("Total events", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                       div(0, class = "odometer", id = "total_events", style = "color: #fff;")
                                                   )
                                            )
                                   ),
                                   fluidRow(width = 12,
                                            column(width = 3,
                                                   #uiOutput("team_days")
                                                   div(class = "valuebox",
                                                     div("Dailies / day", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                     div(0, class = "odometer", id = "avg_day", style = "color: #fff;")
                                                     )
                                            ),
                                            column(width = 3,
                                                   #uiOutput("hours_invested")
                                                   div(class = "valuebox",
                                                     div("Proj. total dailies", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                     div(0, class = "odometer", id = "proj_total", style = "color: #fff;")
                                                   )
                                            ),
                                            column(width = 3,
                                                   #uiOutput("team_days")
                                                   div(class = "valuebox",
                                                       div("Dailies yesterday", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                       div(0, class = "odometer", id = "dailies_yesterday", style = "color: #fff;")
                                                   )
                                            ),
                                            column(width = 3,
                                                   #uiOutput("hours_invested")
                                                   div(class = "valuebox",
                                                       div("Dailies today", style = "color: #fff; text-transform: uppercase; font-weight: 700;"),
                                                       div(0, class = "odometer", id = "dailies_today", style = "color: #fff;")
                                                   )
                                            )
                                   )
                            )
                          ),
                          fluidRow(
                            column(12, uiOutput("disclaimer"))
                          ),
                          fluidRow(
                            column(12,
                                   div(class = "panel panel-default", id = "table",
                                       div(class = "panel-heading", tags$b("One percents (loggers per day)"), 
                                           style = "color: #fff; background-color: #E95420;"),
                                       div(class = "panel-body",
                                           plotOutput("overal_history", width = "100%" , height = "200px")
                                       )
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(12,
                                   div(class = "panel panel-default", id = "table",
                                       div(class = "panel-heading", tags$b("Dailies history"), 
                                           style = "color: #fff; background-color: #E95420;"),
                                       div(class = "panel-body",
                                          plotOutput("dailies_history", width = "100%" , height = "200px")
                                       )
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(12,
                                   div(class = "panel panel-default", id = "table",
                                       div(class = "panel-heading", tags$b("Totals"), 
                                           style = "color: #fff; background-color: #E95420;"),
                                       div(class = "panel-body",
                                           selectInput("total_or_last7", 
                                                       NULL, 
                                                       choices = c("Last 7 days" = "L7", "Overall" = "All")),
                                           formattableOutput("totals", width = "100%")
                                           )
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(12,
                                   div(class = "panel panel-default", id = "table",
                                       div(class = "panel-heading", tags$b("What's done today?"), 
                                           style = "color: #fff; background-color: #E95420;"),
                                       div(class = "panel-body",
                                           dateInput("which_day", NULL, value = NULL, min = "2018-12-20"),
                                           formattableOutput("last_logs")
                                       )
                                   )
                            )
                          )
                          )
                        )
                      ),
             tabPanel("personal",
                      wellPanel(
                        selectInput("personal.name", "Name:", choices = playerNames, width = "20%"),
                        dateRangeInput("personal.range", "Date range:", start = "2018-12-31", width = "30%"),
                        formattableOutput("raw_csv")
                      ))
             )
)
