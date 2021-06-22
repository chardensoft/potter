#Final Shiny App
library(shiny)
library(tidyverse)
library(ggrepel)
library(rsconnect)
library(randomForest)
library(DT)

# Lead Generation (I want players above x threshold or whatever)
# Raw Player
# Player Selection 

setwd("~/Desktop/Data/potterdata")
tot_career <- read.csv(file = "total_career.csv")
names_dfs <- read.csv(file = "full_names.csv")
mod_res <- read.csv(file = "rf_results.csv")
# tiles <- mod_res[order(as.numeric(mod_res$yes_nba)),]
# tiles <- tiles[tiles$yes_nba > 0,]
# n = length(mod_res$yes_nba[mod_res$yes_nba > 0])
# mod_res$tiles <- (1:n - 1)/(n - 1)
mod <- readRDS("model.rds")
# names_dfs <- names_dfs[-1]
# 
# mod_res$name <- NA
# for (i in 1:length(mod_res$player)) {
#   mod_res$name[i] <- names_dfs$names[which(names_dfs$id == mod_res$player[i])]
# }

# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), substring(s, 2),
#         sep="", collapse=" ")
# }

# names11$name <- sapply(names11$name, simpleCap)

ui <- shinyUI(
  fluidPage(
    
    titlePanel("Potter Sports Group: Predicting NBA Players"),
    
    mainPanel(
      column(12, 
             tabsetPanel(
               tabPanel("Lead Generator", 
                        fluidRow(DT::dataTableOutput("playerList"),
                                 wellPanel(
                                   sliderInput(inputId = "topn",
                                               label = "Select Likelihood",
                                               min = 1,
                                               max = 99,
                                               value = 10,
                                               step = 1),
                                   sliderInput(inputId = "agen",
                                               label = "Age or younger:",
                                               min = 18,
                                               max = 35,
                                               value = 22,
                                               step = 1),
                                   sliderInput(inputId = "mpn",
                                               label = "Max Minutes Played Per Game",
                                               min = 0,
                                               max = 35,
                                               value = 30,
                                               step = .5)
                                 )
                        )
               ),
               tabPanel("Select a Player", 
                        fluidRow(tableOutput("rPlayer"),
                                 wellPanel(
                                   htmlOutput("selectPlayer"),
                                   htmlOutput("selectrSchool"),
                                   htmlOutput("selectYear")
                                 )
                        )
               ),
               tabPanel("Input a Player", 
                        fluidRow(textOutput("iPlayer"),
                                 tags$head(tags$style("#iPlayer{color: black;
                                                      font-size: 30px;
                                                      font-style: bold;
                                                      }"
                                          )
                                 ),
                                 wellPanel(
                                   sliderInput(inputId = "age",
                                               label = "Age",
                                               min = 18,
                                               max = 35,
                                               value = 29,
                                               step = 1),
                                   htmlOutput("selectSchool"),
                                   sliderInput(inputId = "sos",
                                               label = "Strength of Schedule",
                                               min = -15,
                                               max = 15,
                                               value = -.5,
                                               step = .25),
                                   sliderInput(inputId = "pts",
                                               label = "Points Per Game",
                                               min = 0,
                                               max = 35,
                                               value = 5,
                                               step = .5),
                                   sliderInput(inputId = "g",
                                               label = "Games Played",
                                               min = 1,
                                               max = 160,
                                               value = 59,
                                               step = 1),
                                   sliderInput(inputId = "gs",
                                               label = "Games Started",
                                               min = 1,
                                               max = 150,
                                               value = 29,
                                               step = 1),
                                   sliderInput(inputId = "mp",
                                               label = "Minutes Per Game",
                                               min = 0,
                                               max = 40,
                                               value = 15,
                                               step = .5),
                                   sliderInput(inputId = "ast",
                                               label = "Assists Per Game",
                                               min = 0,
                                               max = 9,
                                               value = 1,
                                               step = .5),
                                   sliderInput(inputId = "orb",
                                               label = "Offensive Rebounds Per Game",
                                               min = 0,
                                               max = 6,
                                               value = 1,
                                               step = .5),
                                   sliderInput(inputId = "drb",
                                               label = "Defensive Rebounds Per Game",
                                               min = 0,
                                               max = 9,
                                               value = 1.5,
                                               step = .5),
                                   sliderInput(inputId = "trb",
                                               label = "Total Rebounds Per Game",
                                               min = 0,
                                               max = 13,
                                               value = 2.5,
                                               step = .5),
                                   sliderInput(inputId = "stl",
                                               label = "Steals Per Game",
                                               min = 0,
                                               max = 5,
                                               value = .5,
                                               step = .5),
                                   sliderInput(inputId = "blk",
                                               label = "Blocks Per Game",
                                               min = 0,
                                               max = 6,
                                               value = .25,
                                               step = .25),
                                   sliderInput(inputId = "tov",
                                               label = "Turnovers Per Game",
                                               min = 0,
                                               max = 6,
                                               value = 1,
                                               step = .5),
                                   sliderInput(inputId = "fg",
                                               label = "Field Goals Made",
                                               min = 0,
                                               max = 10,
                                               value = 2,
                                               step = .25),
                                   sliderInput(inputId = "fga",
                                               label = "Field Goal Attempts Per Game",
                                               min = 0,
                                               max = 21.5,
                                               value = 4,
                                               step = .5),
                                   sliderInput(inputId = "fg_2",
                                               label = "Field Goal Percentage",
                                               min = 0,
                                               max = 100,
                                               value = 41,
                                               step = .5),
                                   sliderInput(inputId = "x2p",
                                               label = "2 Points Made Per Game",
                                               min = 0,
                                               max = 10,
                                               value = .5,
                                               step = .25),
                                   sliderInput(inputId = "x2pa",
                                               label = "2 Point Attempts Per Game",
                                               min = 0,
                                               max = 20,
                                               value = 3,
                                               step = .5),
                                   sliderInput(inputId = "x2p_2",
                                               label = "2 Point Percentage",
                                               min = 0,
                                               max = 100,
                                               value = 45,
                                               step = .5),
                                   sliderInput(inputId = "x3p",
                                               label = "3 Points Made Per Game",
                                               min = 0,
                                               max = 4,
                                               value = .5,
                                               step = .5),
                                   sliderInput(inputId = "x3pa",
                                               label = "3 Point Attempts Per Game",
                                               min = 0,
                                               max = 11,
                                               value = 1.5,
                                               step = .5),
                                   sliderInput(inputId = "x3p_2",
                                               label = "3 Point Percentage",
                                               min = 0,
                                               max = 100,
                                               value = 28,
                                               step = .5),
                                   sliderInput(inputId = "ft",
                                               label = "Free Throws Made Per Game",
                                               min = 0,
                                               max = 8,
                                               value = 1,
                                               step = .5),
                                   sliderInput(inputId = "fta",
                                               label = "Free Throw Attempts Per Game",
                                               min = 0,
                                               max = 10,
                                               value = 1.5,
                                               step = .5),
                                   sliderInput(inputId = "ft_2",
                                               label = "Free Throws Percentage",
                                               min = 0,
                                               max = 100,
                                               value = 64,
                                               step = .5),
                                   sliderInput(inputId = "pf",
                                               label = "Personal Fouls Per Game",
                                               min = 0,
                                               max = 5,
                                               value = 1.5,
                                               step = .5)
                                 )
                        )
               )
             )
      ),
      width = 12)
  )
)

server <- shinyServer(function(input, output) {
  
  output$playerList <- DT::renderDataTable({
    dataTable <- mod_res %>% 
      filter(yes_nba > input$topn/100) %>% 
      filter(player %in% tot_career$player[tot_career$year.start >= (2020-(input$agen-18))]) %>% 
      filter(player %in% tot_career$player[tot_career$mp <= input$mpn]) %>% 
      select(yes_nba, reality, player) %>% 
      arrange(yes_nba) 
    # namesTable <- names_dfs %>% 
    #   filter(id %in% dataTable$player) %>% 
    #   arrange()
    data.frame(cbind(LikelihoodOfNBA=dataTable$yes_nba*100, 
                     InNBA=ifelse(dataTable$reality==1,"no","yes"),
                     Name=dataTable$player))
                     
  })
  
  output$selectrSchool <- renderUI({
    
    selectInput(inputId = "schoolR", 
                label = "Select a school (or 'Overall' if played at multiple D-I):", 
                choices = unique(tot_career$school[order(tot_career$school)]), 
                selected = "Davidson", multiple = FALSE)
  })
  
  output$selectYear <- renderUI({
    selectInput(inputId = "year", 
                label = "Select a year they were on the team:", 
                choices = unique(tot_career$year.start), 
                selected = "2006", multiple = FALSE)
  })
  
  output$selectPlayer <- renderUI({ 
    yr <- input$year
    choice_list <- unique(tot_career %>% 
                            filter(school == input$schoolR) %>% 
                            filter(as.numeric(year.start) %in% 
                                     c(as.numeric(yr):(as.numeric(yr)-4))) %>% 
                            select(player))
    selectInput(inputId = "playerR", label = "Choose a player!",
                choices = choice_list, 
                selected = choice_list[1],
                multiple = FALSE)
  })

  output$rPlayer <- renderTable({
    statsTable <- data.frame(cbind(Name=tot_career$player, School=tot_career$school,
                              CollegeStart=tot_career$year.start, 
                              Games=tot_career$g, GamesStarted=tot_career$gs,
                              Minutes=tot_career$mp, Points=tot_career$pts,
                              Rebounds=tot_career$trb, Assists=tot_career$ast,
                              StrengthOfSchedule=tot_career$sos))
    predTable <- data.frame(cbind(InNBA=ifelse(mod_res$reality==1,"no","yes"),
                                  Prediction=mod_res$yes_nba,
                                  player=mod_res$player))
    if (input$playerR %in% mod_res$player) {
      statsTable1 <- statsTable %>% 
        filter(Name == input$playerR)
      predTable1 <- predTable %>% 
        filter(player==input$playerR) %>% 
        select(InNBA, Prediction)
      data.frame(cbind(statsTable1, predTable1))
    } else {
      statsTable1 <- statsTable %>% 
        filter(Name == input$playerR)
    }
  })
  
  output$selectSchool <- renderUI({
    selectInput(inputId = "school", 
                label = "Select a school (or 'Overall' if played at multiple D-I):", 
                choices = unique(tot_career$school), 
                selected = "Overall", multiple = FALSE)
  })
  
  output$iPlayer <- renderText({
    if(input$school > 0) {
      dat <- data.frame(school=input$school,
                         g=input$g,
                         gs=input$gs,
                         mp=input$mp,
                         fg=input$fg,
                         fga=input$fga,
                         fg_2=input$fg_2/100,
                         x2p=input$x2p,
                         x2pa=input$x2pa,
                         x2p_2=input$x2p_2/100,
                         x3p=input$x3p,
                         x3pa=input$x3pa,
                         x3p_2=input$x3p_2/100,
                         ft=input$ft,
                         fta=input$fta,
                         ft_2=input$ft_2/100,
                         orb=input$orb,
                         drb=input$drb,
                         trb=input$trb,
                         ast=input$ast,
                         stl=input$stl,
                         blk=input$blk,
                         tov=input$tov,
                         pf=input$pf,
                         pts=input$pts,
                         sos=input$sos)
      prob <- predict(mod, dat,
                      type = "prob")
      paste0("This player has a ", round(prob[2]*100, 2), "% chance of making the NBA!")
      # pie(c(prob[1], prob[2]), col = c("red","blue"), labels = c("Chance of not making NBA","Chance of making NBA"))
    }
  })
  
})

shinyApp(ui = ui, server = server)
