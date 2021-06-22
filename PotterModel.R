###Potter Project: Predicting NBA Players ###
##Setup & Download Packages ##
library(rvest)
library(httr)
library(xml2)
library(XML)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(ggrepel)
library(tidyverse)
library(corrplot)
library(bestglm) 
library(car)
library(pROC)
library(ggplot2)
library(randomForest)
library(WVPlots)

##Step 1: Scrape Needed Data ##
# grab all player data from college basketball reference - 
# be careful running this, it takes like 2 days!
player_dfs <- list()
season_dfs <-matrix(ncol = 30)
career_dfs <- matrix(ncol = 30)
player_career <- list()
cbb_player_href_use <- data.frame()
cbb_sportref_url <- "https://www.sports-reference.com"

for(i in 26:26) {
  cbb_player_list_link <- paste0("https://www.sports-reference.com/cbb/players/",
                                 letters[i],"-index.html")
  cbb_player_list <- read_html(cbb_player_list_link)
  
  cbb_player_note_all <- cbb_player_list %>% 
    html_nodes(".note") %>% 
    html_text()
  # print("made it1")

  cbb_player_href_all <- cbb_player_list %>% 
    html_nodes("#content p > a") %>%
    html_attr("href")
  count <- 1
  # print("made it2")

  for (j in 1:length(cbb_player_href_all)) {
    # print("made it3")
    if (as.numeric(strsplit(strsplit(strsplit(cbb_player_note_all[j], "(", 
                                              fixed = T)[[1]][2], ")", 
                                     fixed = T)[[1]][1], "-", fixed = T)[[1]][1])==2018) {
      cbb_player_href_use[count, i] <- cbb_player_href_all[j]
      count <- count + 1
    }
  }
  
  # print("made it3")

  player_dfs[[i]] <- cbb_player_href_use[i]
  # print("made it4")
  
  for(j in 1:(count-1)){
    # print("made it5")
    colnames(season_dfs) <- c("Player", "Season", "School", "Conf", "G", "GS", "MP", 
                              "FG", "FGA", "FG%", "2P", 
                              "2PA", "2P%", "3P", "3PA", "3P%", "FT", "FTA", "FT%", 
                              "ORB", "DRB", "TRB", "AST", 
                              "STL", "BLK", "TOV", "PF", "PTS", "Var.29", "SOS")
    names(season_dfs) <- colnames(season_dfs)
    colnames(career_dfs) <- colnames(season_dfs)
    names(career_dfs) <- colnames(season_dfs)
    cbb_player_href_use[j,i]
    plain_link <- paste0(cbb_sportref_url,cbb_player_href_use[j,i])
    player_link <- read_html(plain_link)
    player_seasons <- player_link %>% 
      html_nodes("#players_per_game") %>% 
      html_table(fill = TRUE)
    # print(ncol(player_seasons[[1]]))
    if((ncol(player_seasons[[1]]) == 30)) {
      # print(j)
      player_seasons[[1]] <- player_seasons[[1]][,1:(ncol(player_seasons[[1]])-1)]
    }
    # View(player_seasons[[1]])
    if(((ncol(player_seasons[[1]]) == 27))) {
      player_seasons[[1]] <- data.frame(cbind(player_seasons[[1]][1:18], 
                                              ORB = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))), 
                                              DRB= c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              player_seasons[[1]][19:27]))
      colnames(player_seasons[[1]]) <- c("Season", "School", "Conf", "G", "GS", "MP", 
                                         "FG", "FGA", "FG%", "2P", 
                                          "2PA", "2P%", "3P", "3PA", "3P%", "FT", "FTA", "FT%", 
                                          "ORB", "DRB", "TRB", "AST", 
                                          "STL", "BLK", "TOV", "PF", "PTS", "Var.29", "SOS")
      names(player_seasons[[1]]) <- colnames(player_seasons[[1]])
    }
    if(((ncol(player_seasons[[1]]) == 21))) {
      player_seasons[[1]] <- data.frame(cbind(player_seasons[[1]][1:10], 
                                              P2 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              PA2 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              PP2 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              P3 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              PA3 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              PP3 = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              player_seasons[[1]][11:13],
                                              ORB = c(rep(na_if(1, 1), nrow(player_seasons[[1]]))), 
                                              DRB= c(rep(na_if(1, 1), nrow(player_seasons[[1]]))),
                                              player_seasons[[1]][14:21]))
      colnames(player_seasons[[1]]) <- c("Season", "School", "Conf", "G", "GS", "MP", 
                                         "FG", "FGA", "FG%", "2P", 
                                         "2PA", "2P%", "3P", "3PA", "3P%", "FT", "FTA", 
                                         "FT%", "ORB", "DRB", "TRB", "AST", 
                                         "STL", "BLK", "TOV", "PF", "PTS", "Var.29", "SOS")
      names(player_seasons[[1]]) <- colnames(player_seasons[[1]])
    }
    

    player_id <- c(rep(strsplit(strsplit(plain_link, ".", fixed = TRUE)[[1]][3], "/", 
                                fixed = TRUE)[[1]][4], nrow(player_seasons[[1]])))
    player_seasons[[1]] <- cbind(Player = player_id, player_seasons[[1]])
    
    if(which(apply(player_seasons[[1]], 1, function(x) any(x %in% c("Career")))) != 
       (nrow(player_seasons[[1]]))) {
      # print("I made it")
      player_career[[1]] <- player_seasons[[1]][(which(apply(
      player_seasons[[1]], 1, function(x) any(x %in% c("Career"))))):(nrow(player_seasons[[1]])),]
      # print("I made it")
      player_seasons[[1]] <- player_seasons[[1]][-((which(apply(
      player_seasons[[1]], 1, function(x) any(x %in% c("Career"))))):(nrow(player_seasons[[1]]))),]
      # print("I made it")
    } else {
      # print("NOPE")
      player_career[[1]] <- player_seasons[[1]][nrow(player_seasons[[1]]),]
      player_seasons[[1]] <- player_seasons[[1]][-nrow(player_seasons[[1]]),]
    }
    season_dfs <- data.frame(rbind(season_dfs, player_seasons[[1]]))
    career_dfs <- data.frame(rbind(career_dfs, player_career[[1]]))
    print(paste0(letters[i], ": ", j))

  }

  print(paste0(letters[i], ": done!"))
}

# pred_set <- read.csv("prediction_career.csv")
career_dfs <- career_dfs[career_dfs$Season=="Career",]
write.csv(career_dfs, file = "new_career.csv")

# grab all nba player data, this one is quite quick
player_dfs <- list()
names_dfs <- data.frame()
season_counter = game_counter <- 1
for(i in c(1:23, 25, 26)) {
  nba_player_list <- read_html(paste0("https://www.basketball-reference.com
                                      /players/",letters[i],"/"))
  nba_player_df <- nba_player_list %>% html_table()
  
  # filter out players
  
  player_dfs[[i]] <- nba_player_df
  
  nba_player_href <- nba_player_list %>% 
    html_nodes("th") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  nba_player_names <- nba_player_list %>% 
    html_nodes("th") %>% 
    html_nodes("a") %>% 
    html_text()
  
  rows <- which(nba_player_df[[1]]$To>1995)
  
  nba_recent_href <- nba_player_href[rows]
  nba_recent_names <- nba_player_names[rows]
  
  player_dfs[[i]] <- cbind(nba_recent_href, nba_recent_names)
  
  print(letters[i])
}

for(i in c(1:23, 25:26)) {
  names_dfs  <- data.frame(rbind(names_dfs, player_dfs[[i]]))
  print(letters[i])
}

#save the needed data
write.csv(career_dfs, file = "~/Desktop/Data/potterdata/cbb_career_dfs.csv")
earnxstat <- read.csv(file = "~/Desktop/Data/potterdata/earningsxstats.csv")
earnxstat$ptst <- earnxstat$pts * earnxstat$g
earnxstat.valid <- earnxstat[!(earnxstat$player == ""),]
write.csv(names_dfs, "~/Desktop/Data/potterdata/names_dfs.csv")

#Some extra scraping I went back and did
# extra_names <- read_html("https://www.basketball-reference.com/leagues/NBA_2021_totals.html#totals_stats::mp") %>% 
#   html_nodes("th+ .left a") %>% 
#   html_attr("href")
# 
# extra_names <- as.matrix(extra_names, ncol = 1)
# write.csv(extra_names, "extra_names1.csv")

#Ignore this - it was for my shiny project earlier in the year
# cut <- data.frame(table(cut(x = earnsxptsna$unknown, breaks = seq(0, 3400, 200))))
# earnsxpts$unknown[which(apply(earnsxpts, 1, function(x) any(x %in% c(
#   "Kareem Abdul-Jabbar"))))[[1]]]
#       
# ggplot(earnsxptsna) +
#   geom_histogram(
#     aes(x = unknown),
#     binwidth = 200,
#     breaks = seq(0, 3500, 200),
#     color = "white",
#     #For the group with maximum value, assign red, otherwise assign black
#     fill = replace(rep("blue", NROW(cut)), which.max(cut$Freq), "red"))
# 
# ggplot(earnxstat.valid, mapping = aes(x = fullbox, y = earns_perc)) + 
#   geom_point(shape = 21, colour = "blue", size = 1, fill = NA,
#              alpha = .7, stroke = .5) + 
# geom_point(data = earnxstat.valid[earnxstat.valid$ptst == 2323.2, ], 
#            shape = 21, fill = NA, size = 1,
#            color = "red", stroke = .5) +
# geom_label_repel(data = earnxstat.valid[earnxstat.valid$ptst == 2323.2, ], 
#                  aes(label = name),
#                  box.padding   = 0.3,
#                    point.padding = 0.1,
#                    col = "red",
#                    segment.color = 'red') + 
#   geom_vline(xintercept = mean(earnxstat.valid$fullbox), linetype = "dashed", col = "purple") + 
#   scale_x_continuous(limits = c(-160, 160), 
#                      breaks = seq(-160, 160, by = 160), 
#                      minor_breaks = seq(-160, 160, by = 80)) +
#   scale_y_continuous(limits = c(-100, 1200),                  
#                      breaks = seq(-100, 1200, by = 100)) + 
#   theme_bw()
# 
# names <- earnxstat.valid$name
# tail(names)
# typeof(substr("Chris", 0, 1))
# typeof("C")
# "C" < "H"
# tail(earnxstat.valid$name[earnxstat.valid$category == 1])
# earnxstat.valid$earns_perc <- (earnxstat.valid$earns_per-1)*100
# 
# saveRDS(earnxstat.valid, file = "~/Desktop/potterdata/EarnXStatValid")
# saveRDS(earnxstat, file = "~/Desktop/potterdata/EarnXStat")

setwd("~/Desktop/Data/potterdata")

##Step 2: Data Cleaning and set up ##
names_dfs <- read.csv(file="names_dfs.csv")

mod_car <- read.csv(file = "model_career.csv")
mod_earn <- names_dfs

mod_car$nba <- ifelse(mod_car$player %in% mod_earn$names, 1, 0)

model_c <- data.frame(cbind(mod_car))

model_c$nba <- as.factor(model_c$nba)
model_c$school <- as.factor(model_c$school)
model_corr <- model_c[complete.cases(model_c), ]

write.csv(model_corr, "model_corr.csv")

##Step 3: EDA ##
ggplot(data = model_corr, mapping = aes(y = g, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = gs, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = mp, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = fg, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = fga, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = fg_2, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x2p, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x2pa, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x2p_2, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x3p, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x3pa, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = x3p_2, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = ft, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = fta, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = ft_2, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = orb, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = drb, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = trb, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = ast, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = stl, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = blk, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = tov, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = pf, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = pts, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = sos, x = nba)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = model_corr, mapping = aes(y = ))

# model_corr <- model_corr[,-c(1,5,6,8,9,11,12,14,15,18,2,3,24)]
plot(model_corr$nba)

corrplot(cor(model_corr[-c(1,2,3,29)]), type = "upper")

#This takes about 45 minutes to run, and I didn't end up using it, so be warned!
# best.subsets <- bestglm(model_corr,
#                         IC = "BIC",
#                         method = "exhaustive",
#                         TopModels = 1,
#                         family = binomial)
# summary(best.subsets$BestModel)


##Step 4: Model Training ##

#train random forest
model_set <- model_corr[-c(1,2,3)]
rf_mod <- randomForest(model_set$nba ~ ., data=model_set)
# rf_mod_sub <- randomForest(model_corr$nba ~ mp + x2p_2 + x3p_2 + ft_2 + trb + 
#                              ast + stl + blk + pts + sos, data=model_corr)

summary(rf_mod)
# summary(rf_mod_sub)

varImpPlot(rf_mod)
# varImpPlot(rf_mod_sub)

plot(rf_mod)
# plot(rf_mod_sub)

#test random forest

test_corr <- read.csv(file = "prediction_career.csv")
names_dfs <- read.csv(file = "names_dfs.csv")

test_corr$paid <- ifelse(test_corr$player %in% names_dfs$names, 1, 0)

test_corr <- test_corr[complete.cases(test_corr), ]
test_corr$paid <- as.factor(test_corr$paid)

test_results <- data.frame(cbind(predict(rf_mod, test_corr, type = "prob"), 
                                 test_corr$paid, test_corr$player))
colnames(test_results) <- c("no_nba", "yes_nba", "reality", "player")

#check out how the test went
#confusion matrix
con_matrix_test <- table(data.frame(ifelse(test_results[2]>.5,1,0),ifelse(test_results[3]==1,0,1)))
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(con_matrix_test[1], con_matrix_test[2], con_matrix_test[3], con_matrix_test[4])
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), color = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = .75, color = "white", size = 10) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none") +
  ylab("Model Prediction") +
  xlab("Reality") + 
  ggtitle(("Success at predicting nba players from 2011-2016"))

#lift plot
LiftCurvePlot(test_results,"yes_nba","reality",title="Model Lift",wizard_color = "blue")

#a plot I made up
qu_res_test <- test_results[order(as.numeric(test_results$yes_nba)),]
n = length(qu_res_test$yes_nba)
qu_res_test$tiles <- (1:n - 1)/(n - 1)

ggplot(data = qu_res_test, mapping = aes(x=tiles, y = as.numeric(yes_nba))) + 
  geom_line(color = "purple", size = 1.5) +
  geom_vline(xintercept = .9661, color = "red", size = 1.25) + 
  geom_point(mapping = aes(y = ifelse(reality==1,0,1)), color = "blue", 
             size = .5) +
  theme_bw() +
  xlab("Percentile") +
  ylab("% chance at NBA") +
  ggtitle("Showing predictions by percentile and who actually made it")

#redo random forest with all data
tot_career <- read.csv(file = "total_career.csv")
names_dfs <- read.csv(file = "names_dfs.csv")

tot_career$nba <- ifelse(tot_career$player %in% names_dfs$names,1,0)

tot_career <- tot_career[complete.cases(tot_career),]
tot_career$nba <- as.factor(tot_career$nba)
final_set <- tot_career[-c(1,2,3)]

# rf_full <- randomForest(final_set$nba ~ ., data=final_set)
varImpPlot(rf_full)
results_full <- data.frame(cbind(predict(rf_full, final_set, type = "prob"), final_set$nba, 
                                 tot_career$player, tot_career$school, tot_career$year.start))
colnames(results_full) <- c("no_nba", "yes_nba", "reality", "player", "school", "year")

results_full$class <- NA
for (i in 1:length(results_full$year)) {
  if (results_full$year[i] == 2020) {
    results_full$class[i] <- "freshman"
  } else if (results_full$year[i] == 2019) {
    results_full$class[i] <- "sophomore"
  } else if (results_full$year[i] == 2018) {
    results_full$class[i] <- "junior"
  } else if (results_full$year[i] == 2017) {
    results_full$class[i] <- "senior"
  } else if (results_full$year[i] == 2016) {
    results_full$class[i] <- "fifth year"
  } else if (results_full$year[i] <= 2015) {
    results_full$class[i] <- "graduated"
  }
}
write.csv(results_full, "rf_results.csv")
saveRDS(rf_full, "model.rds")

##Step 5: Results! ##
# confusion matrix
con_matrix <- table(data.frame(ifelse(results_full[2]>.5,1,0),ifelse(results_full[3]==1,0,1)))
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(con_matrix[1], con_matrix[2], con_matrix[3], con_matrix[4])
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none") +
  ylab("Model Prediction") +
  xlab("Reality")

#lift plot - code in learning suite (percentiles and what percent accurate)
LiftCurvePlot(results_full,"yes_nba","reality",title="Model Lift",wizard_color = "blue")

#a plot I made up
qu_res <- results_full[order(as.numeric(results_full$yes_nba)),]
n = length(qu_res$yes_nba)
qu_res$tiles <- (1:n - 1)/(n - 1)

ggplot(data = qu_res, mapping = aes(x=tiles, y = as.numeric(yes_nba))) + 
  geom_line(color = "purple", size = 1.5) +
  geom_vline(xintercept = .9661, color = "red", size = 1.25) + 
  geom_point(mapping = aes(y = ifelse(reality==1,0,1)), color = "blue", 
             size = .5) +
  theme_bw() +
  xlab("Percentile") + 
  ylab("Predicted chance at making it")











     