# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(plotly)


# Load the ggplot2 package which provides
# the 'mpg' dataset.
function(input, output) {

  
    
  # P2 Table 2 (Similarity Index)
  output$table <- renderTable({
    data <- read.csv("data/Final_Data.csv")
    data$season <- as.factor(data$season)
    data$player <- as.factor(data$player)

    p_data <- data %>% 
        select(Category) %>% 
        filter(data$player == input$bat)
      p_data <- toString(unique(p_data[,1]))
    
      similar_player <- data %>% 
      #select(player, Team, Category, Batting_Avg, MadMax_Mode) %>% 
      filter(data$Category == p_data ) %>% 
      group_by(player, Role, Team) %>%  
      summarise("Batting Avg." = round(sum(Runs)/(sum(R25[gotout==1]) + sum(R50[gotout==1]) + sum(R75[gotout==1]) + sum(R100[gotout==1]))))

    head(similar_player, 10)
    
  }, bordered = TRUE, hover = TRUE, striped = TRUE, caption = "(based on k-means algorithm)", rownames = TRUE, colnames = TRUE, width = "500px")
  
  ## Preview
  output$preview <- renderTable({
    data <- read.csv("data/Final_Data.csv")
    data$season <- as.factor(data$season)
    data$player <- as.factor(data$player)
    
    data <- data %>% 
         filter(data$player == input$bat )
    
  
    colnames(data)[colnames(data)=="player"] <- "Player"
    colnames(data)[colnames(data)=="Category"] <- "SkillLevel"
 
    dt <- data %>%
      group_by(Player, SkillLevel,Role) %>%
      summarise("Batting Avg." = round(sum(Runs)/(sum(R25[gotout==1]) + sum(R50[gotout==1]) + sum(R75[gotout==1]) + sum(R100[gotout==1]))),
                "Runs Made" = sum(Runs),
                "Hard Hit Ratio(%)" = round(sum(Big_Runs) / sum(Runs)),
                "Bowling Avg." = round(sum(Runs_Given)/sum(Wickets)),
                "Wickets Taken" = sum(Wickets),
                "Runs_Given" = sum(Runs_Given),
                "Runs made on 1st innings" = sum(Runs[inning==1]),
                "Runs made on 2nd innings" = sum(Runs[inning==2]))
    
    dt <- as.data.frame(t(dt))
    #colnames(dt)[colnames(dt)=="V1"] <- "F"
    
    
  }, caption = "numbers based on IPL season 2008-2017", bordered = TRUE, hover = TRUE, striped = TRUE, rownames = TRUE, colnames = FALSE, width = "500px")
  
  output$text1 <- renderText({"Player Statistics"})
  output$text2 <- renderText({"Similar Players"})
  output$space1 <- renderText({"   "})
  output$space2 <- renderText({"   "})
  output$space3 <- renderText({"   "})
  
## Plot 1 the by city stats
  output$main_plot <- renderPlot({
    data <- read.csv("data/Final_Data.csv")
    data <- data[data$player == input$bat,]
    data <- 
      data %>%
      select(city, Runs, R25,R50, R75, R100, gotout) %>% 
      filter(city  != "") %>% 
      group_by(city) %>%
      summarize(Batting_Avg_Num = sum(Runs),
                Batting_Avg_Den = sum(R25[gotout==1]) + sum(R50[gotout==1]) + sum(R75[gotout==1]) + sum(R100[gotout==1]))
      data <- data %>% 
              group_by(city) %>% 
              summarize(Batting_Avg = round(mean(Batting_Avg_Num/Batting_Avg_Den)),0)
      
      if(is.infinite(data$Batting_Avg) == TRUE)
      data$Batting_Avg[is.infinite(data$Batting_Avg)]<-NA
      data <- as.data.frame(data)
      data <- na.omit(data)
      
      
      gg <- ggplot(data, aes(reorder(city,Batting_Avg,mean),Batting_Avg, fill = "red", label = Batting_Avg) )
      gg+geom_bar(stat = "identity" )+
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      coord_flip()+
      labs(title="Batting Performance (Avg.) by City") + 
      theme_dark() + theme(axis.text.x = element_blank(),
                                              axis.title.y=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.ticks = element_blank(),
                                              legend.position="none")

  })
## Plot 2 the by year stats
#   output$year_plot <- renderPlot({
#       data <- read.csv("data/Final_Data.csv")
#       data <- data[data$player == input$bat,]
#       data <- 
#         data %>%
#         select(season, Runs, R25,R50, R75, R100, gotout) %>% 
#         group_by(season) %>%
#         summarize(Batting_Avg_Num = sum(Runs),
#                   Batting_Avg_Den = sum(R25[gotout==1]) + sum(R50[gotout==1]) + sum(R75[gotout==1]) + sum(R100[gotout==1]))
#       data <- data %>% 
#         group_by(season) %>% 
#         summarize(Batting_Avg = round(mean(Batting_Avg_Num/Batting_Avg_Den)),0)
#       
# 
#     ggplot(data, aes(as.integer(season), Batting_Avg) ) + 
#       labs(title="Batting Performance (Avg.) by Season") +
#       theme_dark() +
#       theme(legend.position="none",
#             axis.text.y = element_blank(),
#             axis.title.y=element_blank(),
#             axis.title.x=element_blank(),
#             axis.ticks = element_blank())
#      
#     
#   })
#   
# # ## Page 1 Plot 1
# #   ## Plot 1 the by city stats
# #   output$team1 <- renderPlot({
# #     data <- read.csv("data/batting.csv")
#     data$Batsman <- as.factor(data$batsman)
#     data <- data[data$city == input$city ,]
#     data <- data[data$Team == input$team1 ,]
#     
#     
#     data <- data %>%
#       #select(Batsman,Role, SkillLevel,inning, NotOuts,  Bowling_Avg, Runs_Given, Wickets, Runs, MadMax_Mode) %>% 
#       group_by(Batsman, city, Role, Category) %>%
#       summarise(mean_batting = round(mean(sum(Runs/(sum(R25+R50+R75+R100) - sum(NotOuts)))))) %>% 
#       arrange(desc(mean_batting))
#      
#     ggplot(data = data, aes(x = reorder(Batsman,mean_batting,sum), Batsman, y = mean_batting,  fill=Category) ) + 
#       geom_bar(stat="identity", width = 0.5 )  +
#       coord_flip()+ 
#       scale_fill_brewer(palette="Set1")+
#       labs(title="Batting Performance (Avg.)") +
#       theme_hc(bgcolor = "darkunica") +
#       ylim(0, 100)+
#       theme(axis.text.x = element_blank(),
#             axis.title.y=element_blank(),
#             axis.title.x=element_blank(),
#             axis.ticks = element_blank())
#     
#     
#   })
# 
#   ## Page 1 Space
# output$p1s1 <- renderText({"   "})
#   
#   ## Page 1 Plot 2
#   ## Plot 1 the by city stats
#   output$team2 <- renderPlot({
#     data <- read.csv("data/batting.csv")
#     data$Batsman <- as.factor(data$batsman)
#     data <- data[data$city == input$city ,]
#     data <- data[data$Team == input$team2 ,]
#     
#     
#     data <- data %>%
#       #select(Batsman,Role, SkillLevel,inning, NotOuts,  Bowling_Avg, Runs_Given, Wickets, Runs, MadMax_Mode) %>% 
#       group_by(Batsman, city, Role, Category) %>%
#       summarise(mean_batting = round(mean(sum(Runs/(sum(R25+R50+R75+R100) - sum(NotOuts)))))) %>% 
#       arrange(desc(mean_batting))
#     
#     ggplot(data = data, aes(x = reorder(Batsman,mean_batting,sum), Batsman, y = mean_batting,  fill=Category) ) + 
#       geom_bar(stat="identity", width = 0.5 )  +
#       coord_flip()+ 
#       scale_fill_brewer(palette="Set1")+
#       labs(title="Batting Performance (Avg.)") +
#       theme_hc(bgcolor = "darkunica") +
#       ylim(0, 100)+
#       theme(axis.text.x = element_blank(),
#             axis.title.y=element_blank(),
#             axis.title.x=element_blank(),
#             axis.ticks = element_blank())
#     
#     
#   })
  

  
  
  
  

  
}
