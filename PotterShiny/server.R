#Final Shiny App
library(shiny)
library(tidyverse)
library(ggrepel)
library(rsconnect)
library(randomForest)
library(DT)

tot_career <- read.csv(file = "total_career copy.csv")
#names_dfs <- read.csv(file = "full_names copy.csv") ##This file is not complete and isn't being used
mod_res <- read.csv(file = "rf_results copy.csv")
mod <- readRDS("model copy.rds")

choice_list <- unique(mod_res %>%
                        select(player))

server <- shinyServer(function(input, output, session) {
  
  thedata <- reactive({
    dataTable <- mod_res %>% 
      filter(yes_nba > input$topn/100) %>% 
      filter(player %in% tot_career$player[tot_career$year.start >= (2020-(input$agen-18))]) %>% 
      filter(player %in% tot_career$player[tot_career$mp <= input$mpn]) %>% 
      filter(reality == 1) %>% 
      select(yes_nba, reality, player, school, class) %>% 
      arrange(desc(yes_nba)) 
    # namesTable <- names_dfs %>% 
    #   filter(id %in% dataTable$player) %>% 
    #   arrange()
    dataTable$yes_nba <- paste0(dataTable$yes_nba*100, "%")
    data.frame(cbind(ChanceOfNBA=dataTable$yes_nba, 
                     InNBA=ifelse(dataTable$reality==1,"no","yes"),
                     Name=dataTable$player,
                     School=dataTable$school,
                     Year=dataTable$class))
  })
  
  output$playerList <- DT::renderDataTable({
    thedata()
  })
  
  output$download <- downloadHandler(
    filename = function(){"report.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
  
  
  # 
  # output$selectrSchool <- renderUI({
  #   
  #   selectInput(inputId = "schoolR", 
  #               label = "Select a school (or 'Overall' if played at multiple D-I):", 
  #               choices = unique(tot_career$school[order(tot_career$school)]), 
  #               selected = "Davidson", multiple = FALSE)
  # })
  # 
  # output$selectYear <- renderUI({
  #   selectInput(inputId = "year", 
  #               label = "Select a year they were on the team:", 
  #               choices = unique(tot_career$year.start), 
  #               selected = "2006", multiple = FALSE)
  # })
  # 
  
  # updateSelectizeInput(session, "playerR", choices = choice_list)
  
  # observeEvent(input$showChart, {
  # output$selectPlayer <- renderUI({
  #   yr <- input$year
  #   choice_list <- unique(tot_career %>%
  #                           filter(school == input$schoolR) %>%
  #                           filter(as.numeric(year.start) %in%
  #                                    c(as.numeric(yr):(as.numeric(yr)-4))) %>%
  #                           select(player))
  #   selectInput(inputId = "playerR", label = "Choose a player!",
  #               choices = choice_list,
  #               selected = choice_list[1],
  #               multiple = FALSE)
  # })
  # })
  
  output$rPlayer <- renderTable({
    statsTable <- data.frame(cbind(Name=tot_career$player, School=tot_career$school,
                                   CollegeStart=tot_career$year.start, 
                                   Games=tot_career$g, GamesStarted=tot_career$gs,
                                   Minutes=tot_career$mp, Points=tot_career$pts,
                                   Rebounds=tot_career$trb, Assists=tot_career$ast,
                                   StrengthOfSchedule=tot_career$sos))
    predTable <- data.frame(cbind(InNBA=ifelse(mod_res$reality==1,"no","yes"),
                                  ChanceOfNBA=mod_res$yes_nba,
                                  player=mod_res$player))
    if (!(input$playerR %in% mod_res$player)) {
      NULL
    } else {
      statsTable1 <- statsTable %>% 
        filter(Name == input$playerR)
      predTable1 <- predTable %>% 
        filter(player==input$playerR) %>% 
        select(InNBA, ChanceOfNBA)
      predTable1$ChanceOfNBA <- paste0(as.numeric(predTable1$ChanceOfNBA)*100, "%")
      data.frame(cbind(statsTable1, predTable1))
    }
  })
  
  output$selectSchool <- renderUI({
    selectInput(inputId = "school", 
                label = "Select a school (or 'Overall' if played at multiple D-I):", 
                choices = unique(tot_career$school), 
                selected = "Overall", multiple = FALSE)
  })
  
  output$iPlayer <- renderText({
    if (length(input$school) == 0) {
      NULL
    } else {
      dat <- data.frame(g=input$g,
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
      paste0(input$name, " has a ", round(prob[2]*100, 2), "% chance of making the NBA!")
      # pie(c(prob[1], prob[2]), col = c("red","blue"), labels = c("Chance of not making NBA","Chance of making NBA"))
    }
  })
  
})