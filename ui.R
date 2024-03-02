
library(ggplot2)
library(shinythemes)
#library(DT)

batting <- read.csv("data/batting.csv")
filters <- read.csv("data/FiltersTeam.csv")
batsmen <- as.character(as.factor(unique(unlist(batting$batsman)))) 
city <- as.character(as.factor(unique(unlist(batting$city)))) 
team1 <- as.character(as.factor(unique(unlist(filters$Team))))
team2 <- as.character(as.factor(unique(unlist(filters$Team))))
role <- as.character(as.factor(unique(unlist(filters$Role))))


fluidPage(align="center",
  theme = shinytheme("slate"),
  titlePanel(title=, "IPL Analytics"),
  tags$head(tags$style(
    HTML('
         body, label, input, button, select { 
         background-color: black    ;
         }')
    )),
  tags$head(includeScript("google-analytics.js")),
  # Create a new Row in the UI for selectInputs
  navbarPage("", id="nav",
  # tabPanel("Game Analyzer",
  #          fluidRow(splitLayout(align="center", style="font-style: italic", height = "400px",cellWidths = c("500px","250px","500px"),
  #            selectInput("team1","Select First Team",team1), 
  #            selectInput("city","Select playing ground",city), 
  #            selectInput("team2","Select Second Team",team2)
  #          )),
  #          
  #          # Create a new row for the table.
  #          
  #         fluidRow(
  #           splitLayout(align="center",cellWidths = c("500px","250px", "500px" ), 
  #                       plotOutput(outputId = "team1", height = "400px"), 
  #                       textOutput("p1s1"),
  #                       plotOutput(outputId = "team2", height = "400px"))
  #           )
  #          
  # ),
  
  tabPanel("Player Analyzer",
  fluidRow(
    selectInput("bat","Select a player",batsmen)
    ),

  # Create a new row for the table.
 
  fluidRow(splitLayout(align="center", style="font-style: italic", cellWidths = c("500px","50px","500px"),
                       textOutput("text1"), 
                       textOutput("space1"),
                       textOutput("text2"))),
  fluidRow(
    splitLayout(align="center",cellWidths = c("500px","50px","500px" ), 
                tableOutput("preview"), 
                textOutput("space2"),
                tableOutput("table"))),
    
  fluidRow(
  splitLayout(align="center",cellWidths = c("1000px" ), 
              plotOutput(outputId = "main_plot", height = "400px")
              # ,
              # textOutput("space3"),
              # plotOutput(outputId = "year_plot", height = "400px")
              )
)
),
tabPanel("About",
         align="left",
         h3("Welcome"),
         p("If you have come to page from Facebook ads, then THANK YOU! it means a lot to me. This is a mini project I am working on IPL Analytics,
           as a Data Scientist and sport fanatics I always wanted to do something like this. Now this is only a starting point, based on your feedback 
           I will have a new version of this website. Why I am doing this! well, this year I played fantasy IPL for first time, and I loved it, I follwed 
           analytics to make best team, and it trust me it works, data tells the story, so before next season kicks off I will have this ready, I promise, 
           and with your help I will add new features, hence, please give your valuable opinion, peace."),
         h3("About this website"),
         p("Every year a summit of extraordinary players creating extravagant events. The money, time, competitions on this event clearly shows 
            it’s not just a domestic occasion but a worldwide phenomenon. This league 
           has another exciting aspect, i.e. statistics", em("(well, in short, cricket is all about it) "),
           " and undoubtedly, has attracted larger crowed who actively participate in fantasy sports. This website is for those enthusiast who believes that data speaks for itself 
           when combined with mathematical models, together it tells how, why, and who are the best picks among the large pool who can make it for 
           fantasy team."),
         
         br(),
         br(),
         h3("About the Data and Measures"),
         h4("The Data"),
         p("I have been collecting this data since 2 years from various, it’s not my day job, so took a while shaping everything together,
           however, the data was mostly reconciled with most of the publishers, but I cannot guarantee the accuracy of the numbers, the data
           may not represent the actual records."),
         h4("Basic Measures"),
         p("Some of the measures are pretty straightforward, and directly referenced from www.wikipedia.com,such as 
           batting/bowling average, bowler’s economy, total runs made."), 
         h4("Derived Measures"),
         p("Hard Hit Ratio(%): Ratio of total runs made in boundaries and sixes by total runs."),
         p("Runs made on 1st innings: Runs made by batsmen in 1st innings."),
         p("Runs made on 2nd innings: Runs made by batsmen in 2nd innings, this measure when compared with above tells us whether the 
           batsman is a better attacker or chaser."),
         br(),
         br(),
         h3("The analysis of Similar players"),
         p("You might have observed that, when a player is selected, the table with similar players also gets updated, this is based on a
           mathematical modeling method, known as K-Mean clustering. K-means clustering (each observation to the cluster whose mean 
           has the least squared Euclidean distance, this is intuitively the nearest mean) is popular for cluster analysis in data mining.
           you can read more about this method, here, https://en.wikipedia.org/wiki/K-means_clustering."),
         br(),
         br(),
         h3("About Me"),
         p("For an introduction, follow me on LinkedIn",
           a("Amit Rath", 
             href = "https://www.linkedin.com/in/amirath"))
         
         
         
) 
)
)



