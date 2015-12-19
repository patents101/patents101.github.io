## app.R ##
library(shinydashboard)
library(DT)

ui <- dashboardPage(skin="black",
  dashboardHeader(title = "GeT-First dashboard"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Ranking Documents", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Trending", icon = icon("th"), tabName = "trending",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Topics Interpolation", icon = icon("th"), tabName = "interpolation",
               badgeLabel = "new", badgeColor = "green"),
      sliderInput("slider", "Topic Number:", 1, 20, 1),
      selectInput("select", label = "Class Item",
                  choices = list("345173" = 345173, "370328" = 370328,
                                 "370252" = 370252), selected = 345173)
      
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Dash board 1
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
            
                box(     width=5, title = "Words Cloud", status="primary",
                          collapsible = TRUE,
                          plotOutput("plot1")),
                box(    width=7,  title = "Weight", status="primary",
                          collapsible = TRUE,
                          plotOutput("plot2",width=400)
                )
                
              ),
              fluidRow(
                column(width = 12,
                box(      title = "Diagram", status="primary",
                          collapsible = TRUE, width = NULL,
                          
                          plotOutput("plot3")
                ))
                
              )
      ),
      
      tabItem(tabName = "widgets",
              fluidRow(
                column(width = 12,
                       box(width = NULL, collapsible = TRUE,
                           plotlyOutput("trend",width="100%"),
                           class="plot-center")
                )),
              fluidRow(
                column(width = 12,
                box(width = NULL,DT::dataTableOutput("weights"))
              ))
          ),
      tabItem(tabName = "trending",
              fluidRow(
                column(width = 12,
                       box(width = NULL, collapsible = TRUE,
                           plotlyOutput("alltrend",width="100%",height=800),
                           class="plot-center")
                ))),
      tabItem(tabName = "interpolation",
              fluidRow(
                           visOutput("ldavis")
                ))
      
      
    )
   
  )
)