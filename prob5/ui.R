library(shinydashboard)


body <- dashboardBody(
  
  fluidRow(
    
    box(plotOutput("previousweek"), title = "Weekly Candidate Summary", align = "center"),
    box(plotOutput("partyWeekPlot"), title = "Weekly Party Summary")
    
  ),
  
  
  
  h1(
    "Tweets Trending-",textOutput("currentTime", container = span), align = "center"
   ),
  fluidRow(
    box(plotOutput("partyPlot"))#Here I show the users and the sentiment
    ,
    box(status = "primary",
      plotOutput("candidatePlot") #Here I will show the bars graph
    )),
    
  fluidRow(
    column(4,
      box(title="Donald Trump",plotOutput("trump_wordcloud"),width="300")),
    column(4,
           box(title="Ted Cruz", plotOutput("ted_wordcloud"),width="300")),
    column(4,
      box(title="Ben Carson",plotOutput("carson_wordcloud"),width="300"))),
  fluidRow(
    column(4,
           box(title="Hillary Clinton",plotOutput("hillary_wordcloud"),width="300")),
    column(4,
           box(title="Bernie Sanders",plotOutput("sanders_wordcloud"),width="300")),
    column(4,box(title="John Kasich",plotOutput("kasich_wordcloud"),width="300"))
  )
    
  )

dashboardPage(
  dashboardHeader(title="Presidential Election 2016(USA) - Twitter Analysis", titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  body
)