library(shiny)
library(DT)
library(rvest)
library(ggpubr)
library(DT)
library(gt)
library(cowplot)
library(shinythemes)
library(ggplot2)
library(plotly)
library(fmsb)
library(tidyverse)
library(devtools)
library(geomtextpath)
library(dplyr)
library(extrafont)
library(shinyWidgets)
library(ggpubr)
library(magick)
library(formattable)
library(scales)
library(summaryBox)
library(shinydashboard)
library(magick)
library(png)
library(showtext)
library(rlang)
library(Matrix)
library(glmnet)
library(knitr)
library(sp)
library(httr)
library(tidyverse)

# setwd("~/Desktop/Belmont Basketball OneDrive/Practice copy")

database <- read.csv("Practice Stat Database.csv")
total_team_advanced <- read.csv("2023 Team Advanced.csv")
lineups <- read.csv("Practice Score Tracker.csv")
links <- read.csv("Practice Clip Links.csv")
plays <- read.csv("Play Sheet.csv")
terms <- read.csv("Term Sheet.csv")
Coach_Score <- read.csv("Coach Score Tracker.csv")
Coach_Images <- read.csv("Coach Images.csv")

database$Player <- gsub("<a0>", " ", database$Player)

#Establish our theme for a bucnh of different graphs and charts and whatnot
theme_bryce <- function () {
  #Very simple, probably don't even really need this but whatever
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

#Function to get stats to put them into a basic table
get_daily_stats <- function(date = Sys.Date(), week = "All", choice = "Week", type = "Totals") {
  
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  database <- database %>%
    filter(Type == "Practice")
  
  #Date filter
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date)
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  }
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  if (type == "Per 70 Possessions") {
    stats <- stats %>%
      mutate(Points = round(Points/Poss*70,1),
             Reb = round((Reb)/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  }
  
  
  stats <- merge(stats, links, by='Player') %>%
    arrange(desc(Points)) %>%
    select(url, Link, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
  
  
  # totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
  #             "Totals", sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$TwoM), sum(stats$TwoA), round(sum(stats$TwoM)/sum(stats$TwoA)*100,1),
  #             sum(stats$ThreeM), sum(stats$ThreeA), round(sum(stats$ThreeM)/sum(stats$ThreeA)*100,1), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Oreb),
  #             sum(stats$Dreb), sum(stats$Foul), sum(stats$DrawFoul), sum(stats$Deflects))
  # stats[nrow(stats) + 1,] = totals
  stats[, 3:19] <- lapply(stats[, 3:19], as.numeric)
  colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
  
  return(stats)
}

get_daily_totals <- function(date = Sys.Date(), week = "All", choice = "Week", type = "Totals") {
  
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date)
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  }
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  if (type == "Per 70 Possessions") {
    stats <- stats %>%
      mutate(Points = round(Points/Poss*70,1),
             Reb = round((Reb)/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  }
  
  
  stats <- merge(stats, links, by='Player') %>%
    arrange(desc(Points)) %>%
    select(url, Link, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
  
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Totals", sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$TwoM), sum(stats$TwoA), round(sum(stats$TwoM)/sum(stats$TwoA)*100,1),
              sum(stats$ThreeM), sum(stats$ThreeA), round(sum(stats$ThreeM)/sum(stats$ThreeA)*100,1), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Oreb),
              sum(stats$Dreb), sum(stats$Foul), sum(stats$DrawFoul), sum(stats$Deflects))
  stats[nrow(stats) + 1,] = totals
  stats[, 3:19] <- lapply(stats[, 3:19], as.numeric)
  
  stats <- stats %>%
    filter(Link == "Totals")
  
  colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
  
  
  
  return(stats)
}

get_team_overall <- function(date = Sys.Date(), week = "All", choice = "Week", type = "Totals") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date)
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  }
  
  totals <- c(sum(stats$Points), sum(stats$Oreb)+sum(stats$Dreb), sum(stats$Ast), sum(stats$TwoM), sum(stats$TwoA), round(sum(stats$TwoM)/sum(stats$TwoA)*100,1),
              sum(stats$ThreeM), sum(stats$ThreeA), round(sum(stats$ThreeM)/sum(stats$ThreeA)*100,1), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Oreb),
              sum(stats$Dreb), sum(stats$Foul), sum(stats$DrawFoul), sum(stats$Deflects))
  
  if (type == "Per 70 Possessions") {
    stats <- stats %>%
      mutate(Points = round(Points/Poss*70,1),
             Reb = round((Oreb+Dreb)/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
    
    totals <- c(round(sum(stats$Points)/2,1), 
                round(sum(stats$Oreb)+sum(stats$Dreb)/2,1), 
                round(sum(stats$Ast)/2,1), 
                round(sum(stats$TwoM)/2,1), 
                round(sum(stats$TwoA)/2,1), 
                totals[6],
                round(sum(stats$ThreeM)/2,1), 
                round(sum(stats$ThreeA)/2,1), 
                totals[9], 
                round(sum(stats$Stl)/2,1), 
                round(sum(stats$Blk)/2,1), 
                round(sum(stats$Tov)/2,1), 
                round(sum(stats$Oreb)/2,1),
                round(sum(stats$Dreb)/2,1), 
                round(sum(stats$Foul)/2,1), 
                round(sum(stats$DrawFoul)/2,1), 
                round(sum(stats$Deflects)/2,1))
  }
  
  team_stats <- data.frame(Stat = c("Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections"),
                           Num = totals)
  
  return(team_stats)
  
}

get_team_overall_adv <- function(date = Sys.Date(), week = "All", choice = "Week", type = "Totals") {
  database <- read.csv("Practice Stat Database.csv")
  
  database$Player <- gsub("<a0>", " ", database$Player)
  
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    database <- database %>%
      filter(Date == date)
  }
  else if (choice == "Week") {
    if (week != "All") {
      database <- database %>%
        filter(Week == week)
    }
    else (
      database <- database
    )
    
  }
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  advanced_totals <- c(round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                       round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                       round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
                       round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
  
  team_stats <- data.frame(Stat = c("PPP", "eFG%", "OReb%", "Tov%", "FT Rate", "3pt Rate", "Blk Rate", "Ast Rate", "Ast/Tov Ratio"),
                           Num = advanced_totals)
  
  return(team_stats)
  
}

get_log_stats <- function(date = Sys.Date(), week = "All", choice = "Week", player = "Aidan Braccia", type = "Totals") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date,
             Player == player) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1)) %>%
      arrange(Date) %>%
      select(url, Date, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
    totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
                "Total", sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$TwoM), sum(stats$TwoA), round(sum(stats$TwoM)/sum(stats$TwoA)*100,1),
                sum(stats$ThreeM), sum(stats$ThreeA), round(sum(stats$ThreeM)/sum(stats$ThreeA)*100,1), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Oreb),
                sum(stats$Dreb), sum(stats$Foul), sum(stats$DrawFoul), sum(stats$Deflects))
    stats[nrow(stats) + 1,] = totals
    stats[, 3:19] <- lapply(stats[, 3:19], as.numeric)
    colnames(stats) <- c("", "Date", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    stats <- stats %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1))
    
    if (type == "Per 70 Possessions") {
      stats <- stats %>%
        mutate(Points = round(Points/Poss*70,1),
               Reb = round((Reb)/Poss*70,1),
               Ast = round(Ast/Poss*70,1),
               TwoM = round(TwoM/Poss*70,1),
               TwoA = round(TwoA/Poss*70,1),
               ThreeM = round(ThreeM/Poss*70,1),
               ThreeA = round(ThreeA/Poss*70,1),
               Stl = round(Stl/Poss*70,1),
               Blk = round(Blk/Poss*70,1),
               Tov = round(Tov/Poss*70,1),
               Oreb = round(Oreb/Poss*70,1),
               Dreb = round(Dreb/Poss*70,1),
               Foul = round(Foul/Poss*70,1),
               DrawFoul = round(DrawFoul/Poss*70,1),
               Deflects = round(Deflects/Poss*70,1),
               FGM = round((TwoM+ThreeM)/Poss*70,1),
               FGA = round((TwoA+ThreeA)/Poss*70,1),
               FGPer = round(FGM/FGA*100,1),
               eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
    }
    
    stats <- stats %>%
      arrange(Date) %>%
      filter(Player == player) %>%
      arrange(desc(Date)) %>%
      select(url, Date, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
    # totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
    #             "Total", sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$TwoM), sum(stats$TwoA), round(sum(stats$TwoM)/sum(stats$TwoA)*100,1),
    #             sum(stats$ThreeM), sum(stats$ThreeA), round(sum(stats$ThreeM)/sum(stats$ThreeA)*100,1), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Oreb),
    #             sum(stats$Dreb), sum(stats$Foul), sum(stats$DrawFoul), sum(stats$Deflects))
    # stats[nrow(stats) + 1,] = totals
    stats[, 3:19] <- lapply(stats[, 3:19], as.numeric)
    colnames(stats) <- c("","Date", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
    
  }
  return(stats)
}

get_shooting <- function() {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  stats <- database %>%
    filter(Type == "FT" | Type == "TS")
  
  stats <- aggregate(cbind(TwoM, TwoA, ThreeM, ThreeA) ~ url + Player + Num, data = stats, FUN = sum)
  
  stats <- stats %>%
    arrange(Num) %>%
    mutate(FTPer = round(TwoM/TwoA*100,1),
           TimedShooting = round(ThreeM/ThreeA*100,1)) %>%
    select(url, Player, ThreeM, ThreeA, TimedShooting, TwoM, TwoA, FTPer)
  
  colnames(stats) <- c("", "Player", "Timed Shooting Makes", "Timed Shooting Attempts", "Timed Shooting %","Free Throw Makes", "Free Throw Attempts", "Free Throw %")
  
  return(stats)
}

get_shooting_totals <- function() {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  stats <- database %>%
    filter(Type == "FT" | Type == "TS") %>%
    mutate(Player = "Total",
           url = '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>')
  
  stats <- aggregate(cbind(TwoM, TwoA, ThreeM, ThreeA) ~ url + Player, data = stats, FUN = sum)
  
  stats <- stats %>%
    mutate(FTPer = round(TwoM/TwoA*100,1),
           TimedShooting = round(ThreeM/ThreeA*100,1)) %>%
    select(url, Player, ThreeM, ThreeA, TimedShooting, TwoM, TwoA, FTPer)
  
  
  
  colnames(stats) <- c("", "Player", "Timed Shooting Makes", "Timed Shooting Attempts", "Timed Shooting %","Free Throw Makes", "Free Throw Attempts", "Free Throw %")
  
  return(stats)
}

create_line <- function(Y, players = c("Aidan Braccia"), graphType = "Cumulative Total") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  if (graphType == "Per Practice") {
    stats <- database %>%
      filter(Player %in% players) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1)) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, eFG, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
  }
  else if (graphType == "Cumulative Total") {
    stats <- database %>%
      filter(Player %in% players) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1)) %>%
      arrange(Date) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    
    stats <- stats %>%
      group_by(Player) %>%
      arrange(Date) %>%
      mutate(Points = cumsum(Points),
             Reb = cumsum(Reb), 
             Ast = cumsum(Ast), 
             TwoM = cumsum(TwoM), 
             TwoA = cumsum(TwoA), 
             TwoPer = round(TwoM/TwoA*100,1), 
             ThreeM = cumsum(ThreeM), 
             ThreeA = cumsum(ThreeA), 
             ThreePer = round(ThreeM/ThreeA*100,1), 
             Stl = cumsum(Stl), 
             Blk = cumsum(Blk), 
             Tov = cumsum(Tov), 
             Oreb = cumsum(Oreb), 
             Dreb = cumsum(Dreb), 
             Foul = cumsum(Foul), 
             DrawFoul = cumsum(DrawFoul), 
             Deflects = cumsum(Deflects),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1)) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, eFG, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
  }
  else if (graphType == "Cumulative Per 70 Possessions") {
    stats <- database %>%
      filter(Player %in% players) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1)) %>%
      arrange(Date) %>%
      select(url, Player, Poss, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    
    stats <- stats %>%
      group_by(Player) %>%
      arrange(Date) %>%
      mutate(FGM = cumsum(TwoM)+cumsum(ThreeM),
             FGA = cumsum(TwoA)+cumsum(ThreeA),
             FGPer = round(FGM/FGA*100,1),
             Poss = cumsum(Poss),
             Points = cumsum(Points),
             Reb = cumsum(Reb), 
             Ast = cumsum(Ast),
             TwoM = cumsum(TwoM),
             TwoA = cumsum(TwoA),
             TwoPer = round(TwoM/TwoA*100,1),
             ThreeM = cumsum(ThreeM),
             ThreeA = cumsum(ThreeA),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Stl = cumsum(Stl),
             Blk = cumsum(Blk),
             Tov = cumsum(Tov),
             Oreb = cumsum(Oreb),
             Dreb = cumsum(Dreb),
             Foul = cumsum(Foul),
             DrawFoul = cumsum(DrawFoul),
             Deflects = cumsum(Deflects),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1),) %>%
      mutate(FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             Points = round(Points/Poss*70,1),
             Reb = round(Reb/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             #TwoPer = round(TwoM/TwoA*100,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             #ThreePer = round(ThreeM/ThreeA*100,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),) %>%
      arrange(Date) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, eFG, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    
    stats[is.na(stats)] <- 0
  }
  
  
  colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
  
  unique_players <- unique(stats$Player)
  num_players <- length(unique_players)
  colors <- rainbow(num_players)
  
  player_string <- paste(players, collapse = ", ")
  #print(colnames(stats))
  p <- plot_ly(stats, x = ~Date, y = ~.data[[Y]], color = ~Player, colors = colors,
               type = "scatter", mode = "lines") %>%
    layout(title = paste0(Y, " Over Time<br>", player_string), xaxis = list(title = "Date"), yaxis = list(title = Y))
  
  return(p)
}

create_team_line <- function(Y, graphType = "Cumulative Total") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice") %>%
    mutate(Player = "Belmont")
  
  database <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Player + Date, data = database, FUN = sum) %>%
    mutate(TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           url = "",
           Week = "")
  
  if (graphType == "Per Practice") {
    stats <- database %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1),
             PPP = round((Points+DrawFoul*1.25)/Poss,2),
             ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
             TovRate = round(Tov/Poss*100,1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
             BlkRate = round(Blk/(TwoA)*100,1),
             AstRate = round(Ast/(TwoM+ThreeM)*100,1),
             AstTo = round(Ast/Tov,2)) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "PPP", "eFG%", "ORebRate", "TovRate", "FTr", "ThreeRate", "BlkRate", "AstRate", "AstTo", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
  }
  else if (graphType == "Cumulative Total") {
    stats <- database %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1)) %>%
      arrange(Date) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    
    stats <- stats %>%
      group_by(Player) %>%
      arrange(Date) %>%
      mutate(Points = cumsum(Points),
             #Poss = cumsum(Poss),
             Reb = cumsum(Reb), 
             Ast = cumsum(Ast), 
             TwoM = cumsum(TwoM), 
             TwoA = cumsum(TwoA), 
             TwoPer = round(TwoM/TwoA*100,1), 
             ThreeM = cumsum(ThreeM), 
             ThreeA = cumsum(ThreeA), 
             ThreePer = round(ThreeM/ThreeA*100,1), 
             Stl = cumsum(Stl), 
             Blk = cumsum(Blk), 
             Tov = cumsum(Tov), 
             Oreb = cumsum(Oreb), 
             Dreb = cumsum(Dreb), 
             Foul = cumsum(Foul), 
             DrawFoul = cumsum(DrawFoul), 
             Deflects = cumsum(Deflects),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1),
             Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             PPP = round((Points+DrawFoul*1.25)/Poss,2),
             ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
             TovRate = round(Tov/Poss*100,1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
             BlkRate = round(Blk/(TwoA)*100,1),
             AstRate = round(Ast/(TwoM+ThreeM)*100,1),
             AstTo = round(Ast/Tov,2)) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "PPP", "eFG%", "ORebRate", "TovRate", "FTr", "ThreeRate", "BlkRate", "AstRate", "AstTo", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
  }
  else if (graphType == "Cumulative Per 70 Possessions") {
    stats <- database %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1)) %>%
      arrange(Date) %>%
      select(url, Player, Poss, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    
    stats <- stats %>%
      group_by(Player) %>%
      arrange(Date) %>%
      mutate(FGM = cumsum(TwoM)+cumsum(ThreeM),
             FGA = cumsum(TwoA)+cumsum(ThreeA),
             FGPer = round(FGM/FGA*100,1),
             Points = cumsum(Points),
             Reb = cumsum(Reb), 
             Ast = cumsum(Ast),
             TwoM = cumsum(TwoM),
             TwoA = cumsum(TwoA),
             TwoPer = round(TwoM/TwoA*100,1),
             ThreeM = cumsum(ThreeM),
             ThreeA = cumsum(ThreeA),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Stl = cumsum(Stl),
             Blk = cumsum(Blk),
             Tov = cumsum(Tov),
             Oreb = cumsum(Oreb),
             Dreb = cumsum(Dreb),
             Foul = cumsum(Foul),
             DrawFoul = cumsum(DrawFoul),
             Deflects = cumsum(Deflects),
             #Poss = cumsum(Poss),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1),) %>%
      mutate( Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
              PPP = round((Points+DrawFoul*1.25)/Poss,2),
              ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
              TovRate = round(Tov/Poss*100,1),
              FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
              ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
              BlkRate = round(Blk/(TwoA)*100,1),
              AstRate = round(Ast/(TwoM+ThreeM)*100,1),
              AstTo = round(Ast/Tov,2),
              FGM = round((TwoM+ThreeM)/Poss*70,1),
              FGA = round((TwoA+ThreeA)/Poss*70,1),
              Points = round(Points/Poss*70,1),
              Reb = round(Reb/Poss*70,1),
              Ast = round(Ast/Poss*70,1),
              TwoM = round(TwoM/Poss*70,1),
              TwoA = round(TwoA/Poss*70,1),
              #TwoPer = round(TwoM/TwoA*100,1),
              ThreeM = round(ThreeM/Poss*70,1),
              ThreeA = round(ThreeA/Poss*70,1),
              #ThreePer = round(ThreeM/ThreeA*100,1),
              Stl = round(Stl/Poss*70,1),
              Blk = round(Blk/Poss*70,1),
              Tov = round(Tov/Poss*70,1),
              Oreb = round(Oreb/Poss*70,1),
              Dreb = round(Dreb/Poss*70,1),
              Foul = round(Foul/Poss*70,1),
              DrawFoul = round(DrawFoul/Poss*70,1),
              Deflects = round(Deflects/Poss*70,1)) %>%
      select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects, Date, Week)
    colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "PPP", "eFG%", "ORebRate", "TovRate", "FTr", "ThreeRate", "BlkRate", "AstRate", "AstTo", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
    
    stats[is.na(stats)] <- 0
  }
  
  
  colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "PPP", "eFG%", "ORebRate", "TovRate", "FTr", "ThreeRate", "BlkRate", "AstRate", "AstTo", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections", "Date", "Week")
  
  unique_players <- unique(stats$Player)
  num_players <- length(unique_players)
  colors <- rainbow(num_players)
  
  #player_string <- paste(players, collapse = ", ")
  #print(colnames(stats))
  p <- plot_ly(stats, x = ~Date, y = ~.data[[Y]], color = ~Player, colors = colors,
               type = "scatter", mode = "lines") %>%
    layout(title = paste0(Y, " Over Time<br>"), xaxis = list(title = "Date"), yaxis = list(title = Y))
  
  return(p)
}

create_scatter_plot <- function(X, Y, date = "2023-06-16", week = "All", choice = "Week", graphType = "Cumulative Total") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoPer*100,1),
             ThreePer = round(ThreePer*100,1),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1)) %>%
      arrange(Num)
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    
    stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             FGM = TwoM+ThreeM,
             FGA = TwoA+ThreeA,
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1)) %>%
      arrange(Num)
  }
  
  if (graphType == "Cumulative Per 70 Possessions") {
    stats <- stats %>%
      mutate(Points = round(Points/Poss*70,1),
             Reb = round(Reb/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             TwoPer = round(TwoM/TwoA*100,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round(FGM/Poss*70,1),
             FGA = round(FGA/Poss*70,1),
      )
  }
  
  stats <- stats %>%
    select(url, Player, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, FGM, FGA, FGPer, eFG, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
  stats[, 3:19] <- lapply(stats[, 3:19], as.numeric)
  colnames(stats) <- c("","Player", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
  
  # Create ggplot object with PPP and dPPP as x and y variables
  p <- ggplotly(
    ggplot(stats, aes(x = .data[[X]], y = .data[[Y]], text = paste("Player: ", Player))) +
      geom_point() +
      geom_text(aes(label = Player), nudge_y = mean(stats[[Y]])*0.04) +
      theme_bryce() +
      labs(title = paste0(X, " vs. ", Y), x = X, y = Y) +
      theme(strip.text.x = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            plot.title.position = 'plot',
            plot.title = element_text(face =  'bold', size = 15),
            plot.subtitle = element_text(size = 12),
            plot.margin = unit(c(.5, .5, 1, .5), "lines"),
            legend.position = "none")
  )
  
  # Convert ggplot object to plotly object
  return(p)
}

create_leaderboard <- function(date = "2023-06-16", week = "All", choice = "Week") {
  
  plot_1 <- ggdraw() + draw_image("Points Leaders.png", scale = 1)
  plot_2 <- ggdraw() + draw_image("Assist Leaders.png", scale = 1)
  plot_3 <- ggdraw() + draw_image("Rebound Leaders.png", scale = 1)
  plot_4 <- ggdraw() + draw_image("Two Leaders.png", scale = 1)
  plot_5 <- ggdraw() + draw_image("Three Leaders.png", scale = 1)
  plot_6 <- ggdraw() + draw_image("eFG Leaders.png", scale = 1)
  plot_7 <- ggdraw() + draw_image("Steal Leaders.png", scale = 1)
  plot_8 <- ggdraw() + draw_image("Block Leaders.png", scale = 1)
  plot_9 <- ggdraw() + draw_image("Deflection Leaders.png", scale = 1)
  plot_10 <- ggdraw() + draw_image("Turnover Leaders.png", scale = 1)
  plot_11 <- ggdraw() + draw_image("Foul Leaders.png", scale = 1)
  plot_12 <- ggdraw() + draw_image("Drawn Leaders.png", scale = 1)
  total_plot<-ggarrange(plot_1, plot_3, plot_2, plot_4, plot_5, plot_6, plot_7, plot_8, plot_9, plot_10, plot_11, plot_12, ncol=3, nrow=4) + bgcolor("floralwhite")
  
  annotate_figure(total_plot, top = text_grob("Total Practice Leaderboard", 
                                              color = "black", face = "bold", size = 50, family = "Consolas"))
}

get_leaders <- function(Stat, type, date = Sys.Date(), week = "All", choice = "Week") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  if (choice == "Day") {
    stats <- database %>%
      filter(Date == date)
  }
  else if (choice == "Week") {
    if (week != "All") {
      stats <- database %>%
        filter(Week == week)
    }
    else (
      stats <- database
    )
    
    stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  }
  
  # stats <- database %>%
  #   filter(Type == "Practice")
  
  #stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  
  Stat <- sym(Stat)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1))
  
  if (type == "Per 70 Possessions") {
    stats <- stats %>%
      mutate(Points = round(Points/Poss*70,1),
             Reb = round((Reb)/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             FGPer = round(FGM/FGA*100,1),
             #eFG = round((FGM+ThreeM*0.5)/FGA*100,1)
      )
  }
  
  stats <- stats %>%
    arrange(desc(!!Stat))
  
  if (as_string(Stat) %in% c("TwoPer", "ThreePer", "eFG")) {
    if (as_string(Stat) == "TwoPer") {
      stats <- stats %>%
        mutate(Two = paste0(TwoM, "/", TwoA)) %>%
        select(url, Player, Two, !!Stat) %>%
        slice(1:3)
      
      colnames(stats) <- c("", "Player", "", "2pt%")
    }
    else if (as_string(Stat) == "ThreePer") {
      stats <- stats %>%
        mutate(Three = paste0(ThreeM, "/", ThreeA)) %>%
        select(url, Player, Three, !!Stat) %>%
        slice(1:3)
      
      colnames(stats) <- c("", "Player", "", "3pt%")
    }
    else if (as_string(Stat) == "eFG") {
      stats <- stats %>%
        mutate(FG = paste0(TwoM+ThreeM, "/", TwoA+ThreeA)) %>%
        select(url, Player, FG, !!Stat) %>%
        slice(1:3)
      
      colnames(stats) <- c("", "Player", "FG", "eFG%")
    }
  }
  else {
    stats <- stats %>%
      select(url, Player, !!Stat) %>%
      slice(1:3)
    
    colnames(stats) <- c("", "Player", as_string(Stat))
  }
  
  return(stats)
}

create_radar_chart <- function(selected_player = "Aidan Braccia") {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  stats <- database %>%
    filter(Type == "Practice")
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = stats, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           eFG = round((TwoM+ThreeM+ThreeM*0.5)/(TwoA+ThreeA)*100,1),)
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),)
  
  players <- stats %>%
    select(url, Player, Num, Points, eFG, TwoPer, ThreePer, DrawFoul, FGA, Ast, Tov, Oreb, Blk, Stl, Deflects, Foul, Dreb) %>%
    mutate(Pts_Per = floor(rank(-Points)),
           eFG_per = floor(rank(-eFG)),
           two_pct_per = floor(rank(-TwoPer)),
           three_pct_per = floor(rank(-ThreePer)),
           ftr_per = floor(rank(-DrawFoul)),
           fga_per = floor(rank(-FGA)),
           ast_per = floor(rank(-Ast)),
           to_per = floor(rank(Tov)),
           oreb_rate_per = floor(rank(-Oreb)),
           blk_per = floor(rank(-Blk)),
           stl_per = floor(rank(-Stl)),
           deflect = floor(rank(-Deflects)),
           foul = floor(rank(Foul)),
           dreb_rate_per = floor(rank(-Dreb))) %>%
    filter(Player == selected_player)
  
  
  index <- 1:14
  stat <- c(paste0("Points\n",round(as.numeric(players[4]),1)), paste0("Effective FG %\n",round(as.numeric(players[5]),1),"%"), 
            paste0("2-Point %\n",round(as.numeric(players[6]),1),"%"), paste0("3-Point %\n",round(as.numeric(players[7]),1),"%"), 
            paste0(round(as.numeric(players[8]),1),"\n","Fouls Drawn"), paste0(round(as.numeric(players[9]),1),"\n","FGA"), 
            paste0(round(as.numeric(players[10]),1),"\n","Assists"), paste0(round(as.numeric(players[11]),1),"\n","Turnovers"),
            paste0(round(as.numeric(players[12]),1),"\n","Off. Rebounds"), paste0(round(as.numeric(players[13]),1),"\n","Blocks"), 
            paste0("Steals\n",round(as.numeric(players[14]),1)), paste0("Deflections\n",round(as.numeric(players[15]),1)), 
            paste0(round(as.numeric(players[16]),1),"\n","Fouls"), paste0("Def. Rebounds\n",round(as.numeric(players[17]),1)))
  label_value <- c(as.numeric(players[18]), as.numeric(players[19]), as.numeric(players[20]), as.numeric(players[21]), as.numeric(players[22]), 
                   as.numeric(players[23]), as.numeric(players[24]), as.numeric(players[25]), as.numeric(players[26]), as.numeric(players[27]), 
                   as.numeric(players[28]), as.numeric(players[29]), as.numeric(players[30]), as.numeric(players[31]))
  value <- c(abs(as.numeric(players[18])-15), abs(as.numeric(players[19])-15), abs(as.numeric(players[20])-15), abs(as.numeric(players[21])-15), abs(as.numeric(players[22])-15), 
             abs(as.numeric(players[23])-15), abs(as.numeric(players[24])-15), abs(as.numeric(players[25])-15), abs(as.numeric(players[26])-15), abs(as.numeric(players[27])-15), 
             abs(as.numeric(players[28])-15), abs(as.numeric(players[29])-15), abs(as.numeric(players[30])-15), abs(as.numeric(players[31])-15))
  #value <- c(1.005,2,3,4,5,6,7,8,9,10,11,12)
  data <- as.data.frame(cbind(index, stat, value))
  data <- data %>%
    mutate(type = case_when(
      index %in% 1:5 ~ "Scoring",
      index %in% 6:10 ~ "Possession",
      index %in% 11:14 ~ "Defense"
    ))
  data$index <- as.numeric(data$index)
  data$value <- as.numeric(data$value)
  data$type <- factor(data$type, levels = c("Scoring", "Possession", "Defense"))
  
  color1 <- "#002469"
  color2 <- "#C9262D"
  color3 <- "gray"
  
  ggplot(data = data, aes(x = reorder(stat, index), y = value, label= label_value, fill = type)) +
    # add the bar/pizza slices that are colored
    geom_bar(data = data, width = 1,
             color = "oldlace",
             stat = "identity") +
    # wrap bar chart as around polar center
    coord_curvedpolar() +
    # add the background behind each bar (alpha at .5 for slight transparency so the bars standout)
    geom_bar(aes(y=14, fill=type), stat="identity", width=1, alpha=0.5) +
    # add & customize line that border whole pizza
    geom_hline(yintercept = seq(0, 14, by = 14),
               color = "oldlace",
               size = 1) +
    # add & customize lines between each pizza slice
    geom_vline(xintercept = seq(.5, 14, by = 1),
               color = "oldlace",
               size = .5) +
    # add percentile labels (labels are fill by bar colors) - option 1
    #geom_label(aes(label=value, fill=type), color = "white", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
    # add percentile labels (labels are choice of fill and color) - option 2
    geom_label(color = "gray20", fill = "oldlace", size=10, fontface="bold", show.legend = FALSE) +
    # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
    scale_fill_manual(values=c(color1, color2, color3)) +
    # theme manipulation to customize plot (play around with these!)
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.background = element_rect(fill = "oldlace", color="oldlace"),
          legend.title = element_blank(),
          legend.text = element_text(colour = "gray20", face = "bold", size = 15),
          legend.key.size = unit(.5, "cm"),
          legend.box.spacing = unit(0, "mm"),
          plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 25),
          plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 22),
          plot.background = element_rect(fill = "oldlace", color="oldlace"),
          panel.background = element_rect(fill = "oldlace", color="oldlace"),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(face = "bold", size = 15, colour = "gray20"),
          axis.title = element_blank(),
          axis.text.x = element_text(face = "bold", size = 20)) +
    # add title and subtitle
    labs(title = paste0(selected_player," Rankings"),
         subtitle = "Stats per 70 Possessions | Labels are Player Ranks in Each Category", x = NULL, y = NULL
    )
  
  
  
  
  # geom_label(color = "gray20", fill = "oldlace", size=10, fontface="bold", family = "Consolas", show.legend = FALSE) +
  # # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
  # scale_fill_manual(values=c(color1, color2, color3)) +
  # # theme manipulation to customize plot (play around with these!)
  # theme(legend.position = "top",
  #       legend.direction = "horizontal",
  #       legend.background = element_rect(fill = "oldlace", color="oldlace"),
  #       legend.title = element_blank(),
  #       legend.text = element_text(colour = "gray20", family = "Consolas", face = "bold", size = 15),
  #       legend.key.size = unit(.5, "cm"),
  #       legend.box.spacing = unit(0, "mm"),
  #       plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 25, family = "Consolas"),
  #       plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 22, family = "Consolas"),
  #       plot.background = element_rect(fill = "oldlace", color="oldlace"),
  #       panel.background = element_rect(fill = "oldlace", color="oldlace"),
  #       panel.grid = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_text(face = "bold", size = 15, colour = "gray20"),
  #       axis.title = element_blank(),
  #       axis.text.x = element_text(face = "bold", size = 20, family = "Consolas")) +
  # # add title and subtitle
  # labs(title = paste0(selected_player," Rankings"),
  #      subtitle = "Stats per 70 Possessions | Labels are Player Ranks in Each Category", x = NULL, y = NULL
  # )
}

get_team_stats <- function(Type) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  stats <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
    ) %>%
    select(Date, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) %>%
    arrange(desc(Date))
  
  if (Type == "Per 70 Possessions") {
    stats <- stats %>%
      mutate(Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             Points = round(Points/Poss*70,1),
             Reb = round((Oreb+Dreb)/Poss*70,1),
             Ast = round(Ast/Poss*70,1),
             TwoM = round(TwoM/Poss*70,1),
             TwoA = round(TwoA/Poss*70,1),
             ThreeM = round(ThreeM/Poss*70,1),
             ThreeA = round(ThreeA/Poss*70,1),
             Stl = round(Stl/Poss*70,1),
             Blk = round(Blk/Poss*70,1),
             Tov = round(Tov/Poss*70,1),
             Oreb = round(Oreb/Poss*70,1),
             Dreb = round(Dreb/Poss*70,1),
             Foul = round(Foul/Poss*70,1),
             DrawFoul = round(DrawFoul/Poss*70,1),
             Deflects = round(Deflects/Poss*70,1),
             FGM = round((TwoM+ThreeM)/Poss*70,1),
             FGA = round((TwoA+ThreeA)/Poss*70,1),
             FGPer = round(FGM/FGA*100,1),
             eFG = round((FGM+ThreeM*0.5)/FGA*100,1)) %>%
      select(Date, Points, Reb, Ast, TwoM, TwoA, TwoPer, ThreeM, ThreeA, ThreePer, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects)
    
  }
  
  # totals <- c("Total", sum(team_stats$Points), sum(team_stats$Reb), sum(team_stats$Ast), sum(team_stats$TwoM), sum(team_stats$TwoA), round(sum(team_stats$TwoM)/sum(team_stats$TwoA)*100,1),
  #             sum(team_stats$ThreeM), sum(team_stats$ThreeA), round(sum(team_stats$ThreeM)/sum(team_stats$ThreeA)*100,1), sum(team_stats$Stl), sum(team_stats$Blk), sum(team_stats$Tov), sum(team_stats$Oreb),
  #             sum(team_stats$Dreb), sum(team_stats$Foul), sum(team_stats$DrawFoul), sum(team_stats$Deflects))
  # team_stats[nrow(team_stats) + 1,] = totals
  # team_stats[, 2:18] <- lapply(team_stats[, 2:18], as.numeric)
  
  colnames(stats) <- c("Date", "Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections")
  return(stats)
}

get_proj_stats <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  PPP_Mult = 1
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1.2
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  stats[nrow(stats) + 1,] = totals
  
  colnames(stats) <- c("", "Player", "Minutes", "Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Fouls")
  
  return(stats)
}

get_proj_team <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  PPP_Mult = 1#1.07/as.numeric(totals[2])
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1.2
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
    
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  
  projections <- read.csv("2023 Opponent Projections.csv")
  
  Team_Points = as.numeric(totals[4])/69.1*100
  Def = 100.05
  Poss = 69.1
  
  projections$HC <- 0
  
  for (i in 1:length(projections$Opponent)) {
    if (projections$Location[i] == "Home") {
      projections$HC[i] = 1.5
    }
    if (projections$Location[i] == "Away") {
      projections$HC[i] = -1.5
    }
  }
  
  projections <- projections %>%
    mutate(Points = (Defense+Team_Points)/2/100*(Pace+Poss)/2+HC,
           Opp_Points = (Offense+Def)/2/100*(Pace+Poss)/2-HC,
           Pythag = round(Points^10.25/(Points^10.25+Opp_Points^10.25)*100,1),
           Points = round(Points,1),
           Opp_Points = round(Opp_Points,1)) %>%
    select(Logo, Opponent, Location, Points, Opp_Points, Pythag)
  
  colnames(projections) <- c("","Opponent", "Location", "Points", "Opp. Points", "% Chance to Win")
  
  return(projections)
}

get_wins <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  # PPP_Mult = 1.07/as.numeric(totals[2])
  PPP_Mult = 1
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
    
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  
  projections <- read.csv("2023 Opponent Projections.csv")
  
  Team_Points = as.numeric(totals[4])/69.1*100
  Def = 100.05
  Poss = 69.1
  
  projections$HC <- 0
  
  for (i in 1:length(projections$Opponent)) {
    if (projections$Location[i] == "Home") {
      projections$HC[i] = 1.5
    }
    if (projections$Location[i] == "Away") {
      projections$HC[i] = -1.5
    }
  }
  
  projections <- projections %>%
    mutate(Points = (Defense+Team_Points)/2/100*(Pace+Poss)/2+HC,
           Opp_Points = (Offense+Def)/2/100*(Pace+Poss)/2-HC,
           Pythag = round(Points^10.25/(Points^10.25+Opp_Points^10.25)*100,1),
           Points = round(Points,1),
           Opp_Points = round(Opp_Points,1)) %>%
    select(Logo, Opponent, Location, Points, Opp_Points, Pythag)
  
  Wins = round(mean(projections$Pythag)/100*30,0)
  Record = paste0(Wins, "-", 30-Wins, " (", round(Wins/30*100,1), "%)")
  
  return(Record)
}

get_conf <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  PPP_Mult = 1#1.07/as.numeric(totals[2])
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
    
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  
  projections <- read.csv("2023 Opponent Projections.csv")
  
  Team_Points = as.numeric(totals[4])/69.1*100
  Def = 100.05
  Poss = 69.1
  
  projections$HC <- 0
  
  for (i in 1:length(projections$Opponent)) {
    if (projections$Location[i] == "Home") {
      projections$HC[i] = 1.5
    }
    if (projections$Location[i] == "Away") {
      projections$HC[i] = -1.5
    }
  }
  
  projections <- projections %>%
    mutate(Points = (Defense+Team_Points)/2/100*(Pace+Poss)/2+HC,
           Opp_Points = (Offense+Def)/2/100*(Pace+Poss)/2-HC,
           Pythag = round(Points^10.25/(Points^10.25+Opp_Points^10.25)*100,1),
           Points = round(Points,1),
           Opp_Points = round(Opp_Points,1)) %>%
    select(Logo, Opponent, Location, Points, Opp_Points, Pythag)
  
  Wins = round(mean(projections$Pythag[13:31],projections$Pythag[9])/100*20,0)
  
  Conf = floor(rank(c(-14,-13,-12,-12,-11,-10,-10,-9,-9,-4,-3,-Wins)))
  
  if (Conf[12] == 1) {
    Conf = "1st"
  }
  else if (Conf[12] == 2) {
    Conf = "2nd"
  }
  else if (Conf[12] == 3) {
    Conf = "3rd"
  }
  else {
    Conf = paste0(Conf[12],"th")
  }
  
  Record = paste0(Wins, "-", 20-Wins, " (", round(Wins/20*100,1), "%, ", Conf, " in MVC)")
  
  return(Record)
}

get_bracket <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  PPP_Mult = 1#1.07/as.numeric(totals[2])
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
    
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  
  projections <- read.csv("2023 Opponent Projections.csv")
  
  Team_Points = as.numeric(totals[4])/69.1*100
  Def = 100.05
  Poss = 69.1
  
  projections$HC <- 0
  
  for (i in 1:length(projections$Opponent)) {
    if (projections$Location[i] == "Home") {
      projections$HC[i] = 1.5
    }
    if (projections$Location[i] == "Away") {
      projections$HC[i] = -1.5
    }
  }
  
  projections <- projections %>%
    mutate(Points = (Defense+Team_Points)/2/100*(Pace+Poss)/2+HC,
           Opp_Points = (Offense+Def)/2/100*(Pace+Poss)/2-HC,
           Pythag = round(Points^10.25/(Points^10.25+Opp_Points^10.25)*100,1),
           Points = round(Points,1),
           Opp_Points = round(Opp_Points,1)) %>%
    select(Logo, Opponent, Location, Points, Opp_Points, Pythag)
  
  Wins = mean(projections$Pythag[11:30])/100*20
  
  # Read the image file
  image_path <- "Arch Madness Bracket.PNG"  # Replace with the actual path to your image
  image <- image_read(image_path)
  
  standings <- read.csv("2023-24 MVC Projections.csv")
  standings[nrow(standings)+1,] <- c("Belmont", Team_Points, 101, Wins, 69.1, "#002469", "white")
  
  standings <- standings %>%
    mutate_at(vars(Offense, Defense, Wins, Poss), as.numeric) %>%
    arrange(desc(Wins))
  
  image <- image_ggplot(image)
  image <- image + annotate("text", label = standings$Team[1], size = 12, x = 260, y = 735, color = standings$Color[1], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[2], size = 12, x = 260, y = 355, color = standings$Color[2], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[3], size = 12, x = 260, y = 155, color = standings$Color[3], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[4], size = 12, x = 260, y = 540, color = standings$Color[4], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[5], size = 12, x = 110, y = 470, color = standings$Color[5], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[6], size = 12, x = 110, y = 85, color = standings$Color[6], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[7], size = 12, x = 110, y = 280, color = standings$Color[7], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[8], size = 12, x = 110, y = 665, color = standings$Color[8], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[9], size = 12, x = 110, y = 615, color = standings$Color[9], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[10], size = 12, x = 110, y = 235, color = standings$Color[10], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[11], size = 12, x = 110, y = 35, color = standings$Color[11], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[12], size = 12, x = 110, y = 425, color = standings$Color[12], fontface = "bold")
  
  
  # image <- image_annotate(image, standings$Team[1], size = 18, color = standings$Text[1], boxcolor = standings$Color[1], location = "+220+2")
  # image <- image_annotate(image, standings$Team[2], size = 18, color = standings$Text[2], boxcolor = standings$Color[2], location = "+220+383")
  # image <- image_annotate(image, standings$Team[3], size = 18, color = standings$Text[3], boxcolor = standings$Color[3], location = "+220+580")
  # image <- image_annotate(image, standings$Team[4], size = 18, color = standings$Text[4], boxcolor = standings$Color[4], location = "+220+195")
  # image <- image_annotate(image, standings$Team[5], size = 18, color = standings$Text[5], boxcolor = standings$Color[5], location = "+50+265")
  # image <- image_annotate(image, standings$Team[6], size = 18, color = standings$Text[6], boxcolor = standings$Color[6], location = "+50+646")
  # image <- image_annotate(image, standings$Team[7], size = 18, color = standings$Text[7], boxcolor = standings$Color[7], location = "+50+457")
  # image <- image_annotate(image, standings$Team[8], size = 18, color = standings$Text[8], boxcolor = standings$Color[8], location = "+50+74")
  # image <- image_annotate(image, standings$Team[9], size = 18, color = standings$Text[9], boxcolor = standings$Color[9], location = "+50+121")
  # image <- image_annotate(image, standings$Team[10], size = 18, color = standings$Text[10], boxcolor = standings$Color[10], location = "+50+504")
  # image <- image_annotate(image, standings$Team[11], size = 18, color = standings$Text[11], boxcolor = standings$Color[11], location = "+50+693")
  # image <- image_annotate(image, standings$Team[12], size = 18, color = standings$Text[12], boxcolor = standings$Color[12], location = "+50+312")
  # print("Works")
  poss <- (standings$Poss[8]+standings$Poss[9])/2
  home_points <- (standings$Offense[8]+standings$Defense[9])/2/100*poss
  away_points <- (standings$Defense[8]+standings$Offense[9])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner89 <- standings$Team[8]
  } else if (num >= home_chance * 1000) {
    winner89 <- standings$Team[9]
  }
  
  image <- image + annotate("text", label = winner89, size = 12, x = 280, y = 640, color = standings$Color[which(standings$Team == winner89)], fontface = "bold")
  
  # 
  # image <- image_annotate(image, winner89, size = 18, color = standings$Text[which(standings$Team == winner89)], boxcolor = standings$Color[which(standings$Team == winner89)], location = "+220+99")
  
  poss <- (standings$Poss[5]+standings$Poss[12])/2
  home_points <- (standings$Offense[5]+standings$Defense[12])/2/100*poss
  away_points <- (standings$Defense[5]+standings$Offense[12])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner512 <- standings$Team[5]
  } else if (num >= home_chance * 1000) {
    winner512 <- standings$Team[12]
  }
  
  image <- image + annotate("text", label = winner512, size = 12, x = 280, y = 450, color = standings$Color[which(standings$Team == winner512)], fontface = "bold")
  
  # image <- image_annotate(image, winner512, size = 18, color = standings$Text[which(standings$Team == winner512)], boxcolor = standings$Color[which(standings$Team == winner512)], location = "+220+290")
  
  poss <- (standings$Poss[7]+standings$Poss[10])/2
  home_points <- (standings$Offense[7]+standings$Defense[10])/2/100*poss
  away_points <- (standings$Defense[7]+standings$Offense[10])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner710 <- standings$Team[7]
  } else if (num >= home_chance * 1000) {
    winner710 <- standings$Team[10]
  }
  
  image <- image + annotate("text", label = winner710, size = 12, x = 280, y = 255, color = standings$Color[which(standings$Team == winner710)], fontface = "bold")
  
  # image <- image_annotate(image, winner710, size = 18, color = standings$Text[which(standings$Team == winner710)], boxcolor = standings$Color[which(standings$Team == winner710)], location = "+220+482")
  
  poss <- (standings$Poss[6]+standings$Poss[11])/2
  home_points <- (standings$Offense[6]+standings$Defense[11])/2/100*poss
  away_points <- (standings$Defense[6]+standings$Offense[11])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner611 <- standings$Team[6]
  } else if (num >= home_chance * 1000) {
    winner611 <- standings$Team[11]
  }
  
  image <- image + annotate("text", label = winner611, size = 12, x = 280, y = 60, color = standings$Color[which(standings$Team == winner611)], fontface = "bold")
  
  # image <- image_annotate(image, winner611, size = 18, color = standings$Text[which(standings$Team == winner611)], boxcolor = standings$Color[which(standings$Team == winner611)], location = "+220+671")
  
  poss <- (standings$Poss[1]+standings$Poss[which(standings$Team == winner89)])/2
  home_points <- (standings$Offense[1]+standings$Defense[which(standings$Team == winner89)])/2/100*poss
  away_points <- (standings$Defense[1]+standings$Offense[which(standings$Team == winner89)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi1 <- standings$Team[1]
  } else if (num >= home_chance * 1000) {
    winnersemi1 <- standings$Team[which(standings$Team == winner89)]
  }
  
  image <- image + annotate("text", label = winnersemi1, size = 12, x = 440, y = 690, color = standings$Color[which(standings$Team == winnersemi1)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi1, size = 18, color = standings$Text[which(standings$Team == winnersemi1)], boxcolor = standings$Color[which(standings$Team == winnersemi1)], location = "+420+46")
  
  poss <- (standings$Poss[4]+standings$Poss[which(standings$Team == winner512)])/2
  home_points <- (standings$Offense[4]+standings$Defense[which(standings$Team == winner512)])/2/100*poss
  away_points <- (standings$Defense[4]+standings$Offense[which(standings$Team == winner512)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi2 <- standings$Team[4]
  } else if (num >= home_chance * 1000) {
    winnersemi2 <- standings$Team[which(standings$Team == winner512)]
  }
  
  image <- image + annotate("text", label = winnersemi2, size = 12, x = 440, y = 490, color = standings$Color[which(standings$Team == winnersemi2)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi2, size = 18, color = standings$Text[which(standings$Team == winnersemi2)], boxcolor = standings$Color[which(standings$Team == winnersemi2)], location = "+420+243")
  
  poss <- (standings$Poss[2]+standings$Poss[which(standings$Team == winner710)])/2
  home_points <- (standings$Offense[2]+standings$Defense[which(standings$Team == winner710)])/2/100*poss
  away_points <- (standings$Defense[2]+standings$Offense[which(standings$Team == winner710)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi3 <- standings$Team[2]
  } else if (num >= home_chance * 1000) {
    winnersemi3 <- standings$Team[which(standings$Team == winner710)]
  }
  
  image <- image + annotate("text", label = winnersemi3, size = 12, x = 440, y = 310, color = standings$Color[which(standings$Team == winnersemi3)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi3, size = 18, color = standings$Text[which(standings$Team == winnersemi3)], boxcolor = standings$Color[which(standings$Team == winnersemi3)], location = "+420+431")
  
  poss <- (standings$Poss[3]+standings$Poss[which(standings$Team == winner611)])/2
  home_points <- (standings$Offense[3]+standings$Defense[which(standings$Team == winner611)])/2/100*poss
  away_points <- (standings$Defense[3]+standings$Offense[which(standings$Team == winner611)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi4 <- standings$Team[3]
  } else if (num >= home_chance * 1000) {
    winnersemi4 <- standings$Team[which(standings$Team == winner611)]
  }
  
  image <- image + annotate("text", label = winnersemi4, size = 12, x = 440, y = 110, color = standings$Color[which(standings$Team == winnersemi4)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi4, size = 18, color = standings$Text[which(standings$Team == winnersemi4)], boxcolor = standings$Color[which(standings$Team == winnersemi4)], location = "+420+623")
  
  
  poss <- (standings$Poss[which(standings$Team == winnersemi1)]+standings$Poss[which(standings$Team == winnersemi2)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemi1)]+standings$Defense[which(standings$Team == winnersemi2)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemi1)]+standings$Offense[which(standings$Team == winnersemi2)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemifinal1 <- standings$Team[which(standings$Team == winnersemi1)]
  } else if (num >= home_chance * 1000) {
    winnersemifinal1 <- standings$Team[which(standings$Team == winnersemi2)]
  }
  
  image <- image + annotate("text", label = winnersemifinal1, size = 12, x = 700, y = 595, color = standings$Color[which(standings$Team == winnersemifinal1)], fontface = "bold")
  
  
  # image <- image_annotate(image, winnersemifinal1, size = 18, color = standings$Text[which(standings$Team == winnersemifinal1)], boxcolor = standings$Color[which(standings$Team == winnersemifinal1)], location = "+660+141")
  
  poss <- (standings$Poss[which(standings$Team == winnersemi3)]+standings$Poss[which(standings$Team == winnersemi4)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemi3)]+standings$Defense[which(standings$Team == winnersemi4)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemi3)]+standings$Offense[which(standings$Team == winnersemi4)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemifinal2 <- standings$Team[which(standings$Team == winnersemi3)]
  } else if (num >= home_chance * 1000) {
    winnersemifinal2 <- standings$Team[which(standings$Team == winnersemi4)]
  }
  
  image <- image + annotate("text", label = winnersemifinal2, size = 12, x = 700, y = 210, color = standings$Color[which(standings$Team == winnersemifinal2)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemifinal2, size = 18, color = standings$Text[which(standings$Team == winnersemifinal2)], boxcolor = standings$Color[which(standings$Team == winnersemifinal2)], location = "+660+527")
  
  
  
  poss <- (standings$Poss[which(standings$Team == winnersemifinal1)]+standings$Poss[which(standings$Team == winnersemifinal2)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemifinal1)]+standings$Defense[which(standings$Team == winnersemifinal2)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemifinal1)]+standings$Offense[which(standings$Team == winnersemifinal2)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner <- standings$Team[which(standings$Team == winnersemifinal1)]
  } else if (num >= home_chance * 1000) {
    winner <- standings$Team[which(standings$Team == winnersemifinal2)]
  }
  
  image <- image + annotate("text", label = winner, size = 12, x = 880, y = 420, color = standings$Color[which(standings$Team == winner)], fontface = "bold")
  
  
  # image <- image_annotate(image, winner, size = 18, color = standings$Text[which(standings$Team == winner)], boxcolor = standings$Color[which(standings$Team == winner)], location = "+860+315")
  # 
  # raster_obj <- as.raster(image)
  # 
  # # Create a ggplot object and add the raster image as an annotation
  # image <- ggplot() +
  #   annotation_raster(raster_obj, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  #   theme_void()
  # 
  return(image)
  
}

get_mvc_projections <- function(Brody, JP, Carter, Brigham, Bez, Isaiah, Cooper, Sam, Win, Aidan, Keith, Jake, Noyes, Drew, Eoin, Tyler) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2),
           dPPP = PPP,
           deFG = eFG, 
           DRebRate = ORebRate, 
           dTovRate = TovRate, 
           dFTr = FTr, 
           dThreeRate = ThreeRate, 
           dBlkRate = BlkRate, 
           dAstRate = AstRate
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
    arrange(desc(Date))
  totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))  
  
  PPP_Mult = 1#1.07/as.numeric(totals[2])
  Reb_Mult = (as.numeric(totals[3])/25.4+30/as.numeric(totals[3]))/2
  #Ast_Mult = 54.2/as.numeric(totals[9])*PPP_Mult
  Ast_Mult = 1
  Stl_Mult = 17.7/as.numeric(totals[4])
  Blk_Mult = 7.9/as.numeric(totals[8])
  #Tov_Mult = as.numeric(totals[4])/11.6
  Tov_Mult = 14/11.6
  Foul_Mult = 1.4
  Draw_Mult = as.numeric(totals[5])/29.3
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum)
  
  stats <- stats %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1))
  
  stats <- stats %>%
    mutate(Points = round(Points/Poss*70,1),
           Reb = round((Reb)/Poss*70,1),
           Ast = round(Ast/Poss*70,1),
           TwoM = round(TwoM/Poss*70,1),
           TwoA = round(TwoA/Poss*70,1),
           ThreeM = round(ThreeM/Poss*70,1),
           ThreeA = round(ThreeA/Poss*70,1),
           Stl = round(Stl/Poss*70,1),
           Blk = round(Blk/Poss*70,1),
           Tov = round(Tov/Poss*70,1),
           Oreb = round(Oreb/Poss*70,1),
           Dreb = round(Dreb/Poss*70,1),
           Foul = round(Foul/Poss*70,1),
           DrawFoul = round(DrawFoul/Poss*70,1),
           Deflects = round(Deflects/Poss*70,1),
           FGM = round((TwoM+ThreeM)/Poss*70,1),
           FGA = round((TwoA+ThreeA)/Poss*70,1),
           FGPer = round(FGM/FGA*100,1),
           eFG = round((FGM+ThreeM*0.5)/FGA*100,1))
  
  stats <- stats %>%
    arrange(Num) %>%
    select(url, Player, Points, Reb, Ast, Stl, Blk, Tov, Foul, DrawFoul)
  
  
  stats$Min <- 0
  
  for (i in 1:length(stats$Player)) {
    stats$Min[which(stats$Player == "Brody Peebles")] = Brody
    stats$Min[which(stats$Player == "Jonathan Pierre")] = JP
    stats$Min[which(stats$Player == "Carter Whitt")] = Carter
    stats$Min[which(stats$Player == "Brigham Rogers")] = Brigham
    stats$Min[which(stats$Player == "Aidan Noyes")] = Noyes
    stats$Min[which(stats$Player == "Isaiah Walker")] = Isaiah
    stats$Min[which(stats$Player == "Cooper Haynes")] = Cooper
    stats$Min[which(stats$Player == "Sam Orme")] = Sam
    stats$Min[which(stats$Player == "Win Miller")] = Win
    stats$Min[which(stats$Player == "Aidan Braccia")] = Aidan
    stats$Min[which(stats$Player == "Keith Robbins")] = Keith
    stats$Min[which(stats$Player == "Jake Dykstra")] = Jake
    stats$Min[which(stats$Player == "Jabez Jenkins")] = Bez
    stats$Min[which(stats$Player == "Drew Scharnowski")] = Drew
    stats$Min[which(stats$Player == "Eoin Dillon")] = Eoin
    stats$Min[which(stats$Player == "Tyler Lundblade")] = Tyler
    
  }
  
  #print(typeof(stats$Min[14]))
  stats[, 3:11] <- lapply(stats[, 3:11], as.numeric)
  
  stats <- stats %>%
    mutate(Poss = Min/40*69.1,
           Points = round((Poss/70*Points*PPP_Mult+(DrawFoul*Poss/70))*abs(0.995^Min),1),
           Reb = round(Poss/70*Reb*Reb_Mult,1), 
           Ast = round(Poss/70*Ast*Ast_Mult*abs(0.995^Min),1), 
           Stl = round(Poss/70*Stl*Stl_Mult*abs(0.995^Min),1), 
           Blk = round(Poss/70*Blk*Blk_Mult*abs(0.995^Min),1), 
           Tov = round(Poss/70*Tov*Tov_Mult,1),
           Foul = round(Poss/70*Foul*Foul_Mult,1),
    ) %>%
    select(url, Player, Min, Points, Reb, Ast, Stl, Blk, Tov, Foul) %>%
    arrange(desc(Points))
  
  stats <- stats %>%
    mutate(Reb = round(33.1/sum(stats$Reb)*Reb,1))
  
  if (sum(stats$Points) > 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  
  if (sum(stats$Points) < 76.5) {
    Pts_Bayes <- abs(((sum(stats$Points)-76.5)/2+76.5)/76.5-2)
    
    stats <- stats %>%
      mutate(Points = round(Points*Pts_Bayes,1))
  }
  
  
  
  totals <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
              "Total", sum(stats$Min), sum(stats$Points), sum(stats$Reb), sum(stats$Ast), sum(stats$Stl), sum(stats$Blk), sum(stats$Tov), sum(stats$Foul))
  
  projections <- read.csv("2023 Opponent Projections.csv")
  
  Team_Points = as.numeric(totals[4])/69.1*100
  Team_Points = mean(c(110.2,Team_Points))
  Def = 101.05
  Poss = 69.1
  
  projections$HC <- 0
  
  for (i in 1:length(projections$Opponent)) {
    if (projections$Location[i] == "Home") {
      projections$HC[i] = 1.5
    }
    if (projections$Location[i] == "Away") {
      projections$HC[i] = -1.5
    }
  }
  
  projections <- projections %>%
    mutate(Points = (Defense+Team_Points)/2/100*(Pace+Poss)/2+HC,
           Opp_Points = (Offense+Def)/2/100*(Pace+Poss)/2-HC,
           Pythag = round(Points^10.25/(Points^10.25+Opp_Points^10.25)*100,1),
           Points = round(Points,1),
           Opp_Points = round(Opp_Points,1)) %>%
    select(Logo, Opponent, Location, Points, Opp_Points, Pythag)
  
  Wins = mean(projections$Pythag[11:30])/100*20
  
  image_path <- "Arch Madness Bracket.PNG"  # Replace with the actual path to your image
  image <- image_read(image_path)
  
  standings <- read.csv("2023 MVC Regular Season.csv")
  standings[nrow(standings)+1,] <- c('<img src="https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/1200px-Belmont_Bruins_logo.svg.png" height="52"></img>',
                                     "Belmont", Team_Points, 101, 69.1, 69.1, "#002469", "white")
  standings <- standings %>%
    mutate_at(c('Offense', 'Defense', 'Pace', 'Poss'), as.numeric)
  
  schedule <- read.csv("Conference Schedule.csv") %>%
    select(Home, Away)
  
  schedule$Winner <- "None"
  
  for (i in 1:length(schedule$Home)) {
    poss <- (standings$Pace[which(standings$Team == schedule$Home[i])]+standings$Pace[which(standings$Team == schedule$Away[i])])/2
    home_points <- (standings$Offense[which(standings$Team == schedule$Home[i])]+standings$Defense[which(standings$Team == schedule$Away[i])])/2/100*poss
    away_points <- (standings$Defense[which(standings$Team == schedule$Home[i])]+standings$Offense[which(standings$Team == schedule$Away[i])])/2/100*poss
    home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
    
    num <- sample(1:1000,1)
    
    if (num <= home_chance * 1000) {
      winner <- schedule$Home[i]
    } else if (num >= home_chance * 1000) {
      winner <- schedule$Away[i]
    }
    
    schedule$Winner[i] = winner
  }
  
  standings$Wins <- 0
  standings$Record <- "0-0"
  
  for (i in 1:length(standings$Team)) {
    wins <- sum(schedule$Winner == standings$Team[i])
    standings$Wins[i] = wins
    
    record <- paste0(wins, " - ", 20-wins)  
    standings$Record[i] = record
  }
  
  standings <- standings %>%
    arrange(desc(Wins))
  
  image <- image_ggplot(image)
  image <- image + annotate("text", label = standings$Team[1], size = 12, x = 260, y = 735, color = standings$Color[1], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[2], size = 12, x = 260, y = 355, color = standings$Color[2], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[3], size = 12, x = 260, y = 155, color = standings$Color[3], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[4], size = 12, x = 260, y = 540, color = standings$Color[4], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[5], size = 12, x = 110, y = 470, color = standings$Color[5], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[6], size = 12, x = 110, y = 85, color = standings$Color[6], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[7], size = 12, x = 110, y = 280, color = standings$Color[7], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[8], size = 12, x = 110, y = 665, color = standings$Color[8], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[9], size = 12, x = 110, y = 615, color = standings$Color[9], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[10], size = 12, x = 110, y = 235, color = standings$Color[10], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[11], size = 12, x = 110, y = 35, color = standings$Color[11], fontface = "bold")
  image <- image + annotate("text", label = standings$Team[12], size = 12, x = 110, y = 425, color = standings$Color[12], fontface = "bold")
  
  poss <- (standings$Poss[8]+standings$Poss[9])/2
  home_points <- (standings$Offense[8]+standings$Defense[9])/2/100*poss
  away_points <- (standings$Defense[8]+standings$Offense[9])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner89 <- standings$Team[8]
  } else if (num >= home_chance * 1000) {
    winner89 <- standings$Team[9]
  }
  
  image <- image + annotate("text", label = winner89, size = 12, x = 280, y = 640, color = standings$Color[which(standings$Team == winner89)], fontface = "bold")
  
  poss <- (standings$Poss[5]+standings$Poss[12])/2
  home_points <- (standings$Offense[5]+standings$Defense[12])/2/100*poss
  away_points <- (standings$Defense[5]+standings$Offense[12])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner512 <- standings$Team[5]
  } else if (num >= home_chance * 1000) {
    winner512 <- standings$Team[12]
  }
  
  image <- image + annotate("text", label = winner512, size = 12, x = 280, y = 450, color = standings$Color[which(standings$Team == winner512)], fontface = "bold")
  
  # image <- image_annotate(image, winner512, size = 18, color = standings$Text[which(standings$Team == winner512)], boxcolor = standings$Color[which(standings$Team == winner512)], location = "+220+290")
  
  poss <- (standings$Poss[7]+standings$Poss[10])/2
  home_points <- (standings$Offense[7]+standings$Defense[10])/2/100*poss
  away_points <- (standings$Defense[7]+standings$Offense[10])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner710 <- standings$Team[7]
  } else if (num >= home_chance * 1000) {
    winner710 <- standings$Team[10]
  }
  
  image <- image + annotate("text", label = winner710, size = 12, x = 280, y = 255, color = standings$Color[which(standings$Team == winner710)], fontface = "bold")
  
  # image <- image_annotate(image, winner710, size = 18, color = standings$Text[which(standings$Team == winner710)], boxcolor = standings$Color[which(standings$Team == winner710)], location = "+220+482")
  
  poss <- (standings$Poss[6]+standings$Poss[11])/2
  home_points <- (standings$Offense[6]+standings$Defense[11])/2/100*poss
  away_points <- (standings$Defense[6]+standings$Offense[11])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner611 <- standings$Team[6]
  } else if (num >= home_chance * 1000) {
    winner611 <- standings$Team[11]
  }
  
  image <- image + annotate("text", label = winner611, size = 12, x = 280, y = 60, color = standings$Color[which(standings$Team == winner611)], fontface = "bold")
  
  # image <- image_annotate(image, winner611, size = 18, color = standings$Text[which(standings$Team == winner611)], boxcolor = standings$Color[which(standings$Team == winner611)], location = "+220+671")
  
  poss <- (standings$Poss[1]+standings$Poss[which(standings$Team == winner89)])/2
  home_points <- (standings$Offense[1]+standings$Defense[which(standings$Team == winner89)])/2/100*poss
  away_points <- (standings$Defense[1]+standings$Offense[which(standings$Team == winner89)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi1 <- standings$Team[1]
  } else if (num >= home_chance * 1000) {
    winnersemi1 <- standings$Team[which(standings$Team == winner89)]
  }
  
  image <- image + annotate("text", label = winnersemi1, size = 12, x = 440, y = 690, color = standings$Color[which(standings$Team == winnersemi1)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi1, size = 18, color = standings$Text[which(standings$Team == winnersemi1)], boxcolor = standings$Color[which(standings$Team == winnersemi1)], location = "+420+46")
  
  poss <- (standings$Poss[4]+standings$Poss[which(standings$Team == winner512)])/2
  home_points <- (standings$Offense[4]+standings$Defense[which(standings$Team == winner512)])/2/100*poss
  away_points <- (standings$Defense[4]+standings$Offense[which(standings$Team == winner512)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi2 <- standings$Team[4]
  } else if (num >= home_chance * 1000) {
    winnersemi2 <- standings$Team[which(standings$Team == winner512)]
  }
  
  image <- image + annotate("text", label = winnersemi2, size = 12, x = 440, y = 490, color = standings$Color[which(standings$Team == winnersemi2)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi2, size = 18, color = standings$Text[which(standings$Team == winnersemi2)], boxcolor = standings$Color[which(standings$Team == winnersemi2)], location = "+420+243")
  
  poss <- (standings$Poss[2]+standings$Poss[which(standings$Team == winner710)])/2
  home_points <- (standings$Offense[2]+standings$Defense[which(standings$Team == winner710)])/2/100*poss
  away_points <- (standings$Defense[2]+standings$Offense[which(standings$Team == winner710)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi3 <- standings$Team[2]
  } else if (num >= home_chance * 1000) {
    winnersemi3 <- standings$Team[which(standings$Team == winner710)]
  }
  
  image <- image + annotate("text", label = winnersemi3, size = 12, x = 440, y = 310, color = standings$Color[which(standings$Team == winnersemi3)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi3, size = 18, color = standings$Text[which(standings$Team == winnersemi3)], boxcolor = standings$Color[which(standings$Team == winnersemi3)], location = "+420+431")
  
  poss <- (standings$Poss[3]+standings$Poss[which(standings$Team == winner611)])/2
  home_points <- (standings$Offense[3]+standings$Defense[which(standings$Team == winner611)])/2/100*poss
  away_points <- (standings$Defense[3]+standings$Offense[which(standings$Team == winner611)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemi4 <- standings$Team[3]
  } else if (num >= home_chance * 1000) {
    winnersemi4 <- standings$Team[which(standings$Team == winner611)]
  }
  
  image <- image + annotate("text", label = winnersemi4, size = 12, x = 440, y = 110, color = standings$Color[which(standings$Team == winnersemi4)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemi4, size = 18, color = standings$Text[which(standings$Team == winnersemi4)], boxcolor = standings$Color[which(standings$Team == winnersemi4)], location = "+420+623")
  
  
  poss <- (standings$Poss[which(standings$Team == winnersemi1)]+standings$Poss[which(standings$Team == winnersemi2)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemi1)]+standings$Defense[which(standings$Team == winnersemi2)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemi1)]+standings$Offense[which(standings$Team == winnersemi2)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemifinal1 <- standings$Team[which(standings$Team == winnersemi1)]
  } else if (num >= home_chance * 1000) {
    winnersemifinal1 <- standings$Team[which(standings$Team == winnersemi2)]
  }
  
  image <- image + annotate("text", label = winnersemifinal1, size = 12, x = 700, y = 595, color = standings$Color[which(standings$Team == winnersemifinal1)], fontface = "bold")
  
  
  # image <- image_annotate(image, winnersemifinal1, size = 18, color = standings$Text[which(standings$Team == winnersemifinal1)], boxcolor = standings$Color[which(standings$Team == winnersemifinal1)], location = "+660+141")
  
  poss <- (standings$Poss[which(standings$Team == winnersemi3)]+standings$Poss[which(standings$Team == winnersemi4)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemi3)]+standings$Defense[which(standings$Team == winnersemi4)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemi3)]+standings$Offense[which(standings$Team == winnersemi4)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winnersemifinal2 <- standings$Team[which(standings$Team == winnersemi3)]
  } else if (num >= home_chance * 1000) {
    winnersemifinal2 <- standings$Team[which(standings$Team == winnersemi4)]
  }
  
  image <- image + annotate("text", label = winnersemifinal2, size = 12, x = 700, y = 210, color = standings$Color[which(standings$Team == winnersemifinal2)], fontface = "bold")
  
  # image <- image_annotate(image, winnersemifinal2, size = 18, color = standings$Text[which(standings$Team == winnersemifinal2)], boxcolor = standings$Color[which(standings$Team == winnersemifinal2)], location = "+660+527")
  
  
  
  poss <- (standings$Poss[which(standings$Team == winnersemifinal1)]+standings$Poss[which(standings$Team == winnersemifinal2)])/2
  home_points <- (standings$Offense[which(standings$Team == winnersemifinal1)]+standings$Defense[which(standings$Team == winnersemifinal2)])/2/100*poss
  away_points <- (standings$Defense[which(standings$Team == winnersemifinal1)]+standings$Offense[which(standings$Team == winnersemifinal2)])/2/100*poss
  home_chance <- home_points^10.25/(home_points^10.25+away_points^10.25)
  
  num <- sample(1:1000,1)
  
  if (num <= home_chance * 1000) {
    winner <- standings$Team[which(standings$Team == winnersemifinal1)]
  } else if (num >= home_chance * 1000) {
    winner <- standings$Team[which(standings$Team == winnersemifinal2)]
  }
  
  image <- image + annotate("text", label = winner, size = 12, x = 880, y = 420, color = standings$Color[which(standings$Team == winner)], fontface = "bold")
  
  
  standings <- standings %>%
    arrange(desc(Wins)) %>%
    mutate(Rank = c(1:12)) %>%
    select(Rank, Logo, Team, Record)
  colnames(standings) <- c('Rank', '', 'Team', 'Record')
  
  return(list(first = standings, second = image))
}

get_team_factors <- function(Type, Week, Day, Show, Side, Adj) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  if (Adj == "Unadjusted") {
    team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             PPP = round((Points+DrawFoul*1.25)/Poss,2),
             eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
             ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
             TovRate = round(Tov/Poss*100,1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
             BlkRate = round(Blk/(TwoA)*100,1),
             AstRate = round(Ast/(TwoM+ThreeM)*100,1),
             AstTo = round(Ast/Tov,2),
             dPPP = PPP,
             deFG = eFG, 
             DRebRate = ORebRate, 
             dTovRate = TovRate, 
             dFTr = FTr, 
             dThreeRate = ThreeRate, 
             dBlkRate = BlkRate, 
             dAstRate = AstRate
      ) %>%
      select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
      arrange(desc(Date))
    totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
                round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
    team_advanced[nrow(team_advanced) + 1,] = c("Total", totals[2:10], totals[2:9])
    team_advanced[, 2:10] <- lapply(team_advanced[, 2:10], as.numeric)
  }
  
  else if (Adj == "Adjusted") {
    team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             PPP = round((Points+DrawFoul*1.25)/Poss-(1.075-1.048),2),
             eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100-(51.1-50.4),1),
             ORebRate = round(Oreb/(Oreb+Dreb)*100-(30-28.5),1),
             TovRate = round(Tov/Poss*100-(17.7-18.2),1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100-(29.2-31.5),1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100-(33.5-37.3),1),
             BlkRate = round(Blk/(TwoA)*100-(7.9-8.8),1),
             AstRate = round(Ast/(TwoM+ThreeM)*100-(49-50.9),1),
             AstTo = round(Ast/Tov,2),
             dPPP = round((Points+DrawFoul*1.25)/Poss-(1.112-1.048),2),
             deFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100-(55.3-50.4),1),
             DRebRate = round(Oreb/(Oreb+Dreb)*100-(25.4-28.5),1),
             dTovRate = round(Tov/Poss*100-(16.8-18.2),1),
             dFTr = round(DrawFoul*2/(TwoA+ThreeA)*100-(24.3-31.5),1),
             dThreeRate = round(ThreeA/(TwoA+ThreeA)*100-(41.5-37.3),1),
             dBlkRate = round(Blk/(TwoA)*100-(10.2-8.8),1),
             dAstRate = round(Ast/(TwoM+ThreeM)*100-(54.2-50.9),1),
      ) %>%
      select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
      arrange(desc(Date))
    totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
                round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
    
    team_advanced[nrow(team_advanced) + 1,] = c("Total", as.numeric(totals[2])-(1.075-1.048), as.numeric(totals[3])-(51.1-50.4), as.numeric(totals[4])-(30-28.5), as.numeric(totals[5])-(17.7-18.2),
                                                as.numeric(totals[6])-(29.2-31.5), as.numeric(totals[7])-(33.5-37.3), as.numeric(totals[8])-(7.9-8.8), as.numeric(totals[9])-(49-50.9), as.numeric(totals[10]),
                                                as.numeric(totals[2])-(1.112-1.048), as.numeric(totals[3])-(55.3-50.4), as.numeric(totals[4])-(25.4-28.5), as.numeric(totals[5])-(16.8-18.2),
                                                as.numeric(totals[6])-(24.3-31.5), as.numeric(totals[7])-(41.5-37.3), as.numeric(totals[8])-(10.2-8.8), as.numeric(totals[9])-(54.2-50.9))
    team_advanced[, 2:10] <- lapply(team_advanced[, 2:10], as.numeric)
  }
  
  colnames(team_advanced) <- c(colnames(total_team_advanced))
  
  for (i in 1:length(team_advanced$Team)) {
    total_stats <- total_team_advanced
    
    total_stats[nrow(total_stats) + 1,] = team_advanced[i,]
    
    total_stats[, 2:18] <- lapply(total_stats[, 2:18], as.numeric)
    
    total_stats <- total_stats %>%
      mutate(PPP_Per = round(pnorm((PPP-mean(total_stats$PPP))/sd(total_stats$PPP))*100,0),
             eFG_Per = round(pnorm((eFG-mean(total_stats$eFG))/sd(total_stats$eFG))*100,0),
             TovRate_Per = abs(round(pnorm((TovRate-mean(total_stats$TovRate))/sd(total_stats$TovRate))*100,0)-100),
             ORebRate_Per = round(pnorm((ORebRate-mean(total_stats$ORebRate))/sd(total_stats$ORebRate))*100,0),
             FTr_Per = round(pnorm((FTr-mean(total_stats$FTr))/sd(total_stats$FTr))*100,0),
             ThreeRate_Per = round(pnorm((ThreeRate-mean(total_stats$ThreeRate))/sd(total_stats$ThreeRate))*100,0),
             BlkRate_Per = abs(round(pnorm((BlkRate-mean(total_stats$BlkRate))/sd(total_stats$BlkRate))*100,0)-100),
             AstRate_Per = round(pnorm((AstRate-mean(total_stats$AstRate))/sd(total_stats$AstRate))*100,0),
             AstTo_Per = round(pnorm((AstTo-mean(total_stats$AstTo))/sd(total_stats$AstTo))*100,0),
             dPPP_Per = abs(round(pnorm((dPPP-mean(total_stats$dPPP))/sd(total_stats$dPPP))*100,0)-100),
             deFG_Per = abs(round(pnorm((deFG-mean(total_stats$deFG))/sd(total_stats$deFG))*100,0)-100),
             dTovRate_Per = round(pnorm((dTovRate-mean(total_stats$dTovRate))/sd(total_stats$dTovRate))*100,0),
             DRebRate_Per = abs(round(pnorm((DRebRate-mean(total_stats$DRebRate))/sd(total_stats$DRebRate))*100,0)-100),
             dFTr_Per = abs(round(pnorm((dFTr-mean(total_stats$dFTr))/sd(total_stats$dFTr))*100,0)-100),
             dThreeRate_Per = abs(round(pnorm((dThreeRate-mean(total_stats$dThreeRate))/sd(total_stats$dThreeRate))*100,0)-100),
             dBlkRate_Per = round(pnorm((dBlkRate-mean(total_stats$dBlkRate))/sd(total_stats$dBlkRate))*100,0),
             dAstRate_Per = abs(round(pnorm((dAstRate-mean(total_stats$dAstRate))/sd(total_stats$dAstRate))*100,0)-100),
             PPP_Rank = round(floor(rank(-PPP)),0),
             eFG_Rank = round(floor(rank(-eFG)),0),
             TovRate_Rank = round(floor(rank(TovRate)),0),
             ORebRate_Rank = round(floor(rank(-ORebRate)),0),
             FTr_Rank = round(floor(rank(-FTr)),0),
             ThreeRate_Rank = round(floor(rank(-ThreeRate)),0),
             BlkRate_Rank = round(floor(rank(BlkRate)),0),
             AstRate_Rank = round(floor(rank(-AstRate)),0),
             AstTo_Rank = round(floor(rank(-AstTo)),0),
             dPPP_Rank = round(floor(rank(dPPP)),0),
             deFG_Rank = round(floor(rank(deFG)),0),
             dTovRate_Rank = round(floor(rank(-dTovRate)),0),
             DRebRate_Rank = round(floor(rank(DRebRate)),0),
             dFTr_Rank = round(floor(rank(dFTr)),0),
             dThreeRate_Rank = round(floor(rank(dThreeRate)),0),
             dBlkRate_Rank = round(floor(rank(dBlkRate)),0),
             dAstRate_Rank = round(floor(rank(dAstRate)),0),) %>%
      filter(!(Team %in% c(total_team_advanced$Team)))
    
    if (i == 1) {
      team_advanced_output <- total_stats
    }
    else {
      team_advanced_output <- rbind(team_advanced_output,total_stats)
    }
  }
  
  if (Side == "Offense") {
    team_advanced_output <- team_advanced_output %>%
      select(Team, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, 
             PPP_Per, eFG_Per, ORebRate_Per, TovRate_Per, FTr_Per, ThreeRate_Per, BlkRate_Per, AstRate_Per, AstTo_Per, 
             PPP_Rank, eFG_Rank, ORebRate_Rank, TovRate_Rank, FTr_Rank, ThreeRate_Rank, BlkRate_Rank, AstRate_Rank, AstTo_Rank, )
  }
  else if (Side == "Defense") {
    team_advanced_output <- team_advanced_output %>%
      select(Team, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate,
             dPPP_Per, deFG_Per, DRebRate_Per, dTovRate_Per, dFTr_Per, dThreeRate_Per, dBlkRate_Per, dAstRate_Per, 
             dPPP_Rank, deFG_Rank, DRebRate_Rank, dTovRate_Rank, dFTr_Rank, dThreeRate_Rank, dBlkRate_Rank, dAstRate_Rank,)
  }
  
  if (Show == "Only Stats") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[1:10]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Effective FG%", "Off. Reb. %", "Tov. %", "Free Throw Rate", "3pt Rate",
                                          "% of Shots Blocked", "Ast Rate", "Ast/Tov Ratio")
    }
    else {
      team_advanced_output <- team_advanced_output[1:9]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Effective FG%", "Off. Reb. %", "Tov. %", "Free Throw Rate", "3pt Rate",
                                          "% of Shots Blocked", "Ast Rate")
    }
    
  }
  else if (Show == "Percentiles") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "%ile", "Effective FG%", "%ile", "Off. Reb. %", "%ile", "Tov. %", "%ile", "Free Throw Rate", "%ile", "3pt Rate","%ile", 
                                          "% of Shots Blocked", "%ile", "Ast Rate", "%ile", "Ast/Tov Ratio", "%ile")
    }
    else {
      team_advanced_output <- team_advanced_output[,c(1,2,10,3,11,4,12,5,13,6,14,7,15,8,16,9,17)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "%ile", "Effective FG%", "%ile", "Off. Reb. %", "%ile", "Tov. %", "%ile", "Free Throw Rate", "%ile", "3pt Rate","%ile", 
                                          "% of Shots Blocked", "%ile", "Ast Rate", "%ile")
    }
    
  }
  else if (Show == "Rankings") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[,c(1,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Rank", "Effective FG%", "Rank", "Off. Reb. %", "Rank", "Tov. %", "Rank", "Free Throw Rate", "Rank", "3pt Rate","Rank", 
                                          "% of Shots Blocked", "Rank", "Ast Rate", "Rank", "Ast/Tov Ratio", "Rank")
    }
    else {
      team_advanced_output <- team_advanced_output[,c(1,2,18,3,19,4,20,5,21,6,22,7,23,8,24,9,25)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Rank", "Effective FG%", "Rank", "Off. Reb. %", "Rank", "Tov. %", "Rank", "Free Throw Rate", "Rank", "3pt Rate","Rank", 
                                          "% of Shots Blocked", "Rank", "Ast Rate", "Rank")
    }
    
  }
  
  return(team_advanced_output)
}

get_team_factors_totals <- function(Type, Week, Day, Show, Side, Adj) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  if (Adj == "Unadjusted") {
    team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             PPP = round((Points+DrawFoul*1.25)/Poss,2),
             eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
             ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
             TovRate = round(Tov/Poss*100,1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
             BlkRate = round(Blk/(TwoA)*100,1),
             AstRate = round(Ast/(TwoM+ThreeM)*100,1),
             AstTo = round(Ast/Tov,2),
             dPPP = PPP,
             deFG = eFG, 
             DRebRate = ORebRate, 
             dTovRate = TovRate, 
             dFTr = FTr, 
             dThreeRate = ThreeRate, 
             dBlkRate = BlkRate, 
             dAstRate = AstRate
      ) %>%
      select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
      arrange(desc(Date))
    totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
                round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
    team_advanced[nrow(team_advanced) + 1,] = c("Total", totals[2:10], totals[2:9])
    team_advanced[, 2:10] <- lapply(team_advanced[, 2:10], as.numeric)
  }
  
  else if (Adj == "Adjusted") {
    team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
      mutate(Reb = Oreb+Dreb,
             TwoPer = round(TwoM/TwoA*100,1),
             ThreePer = round(ThreeM/ThreeA*100,1),
             Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
             PPP = round((Points+DrawFoul*1.25)/Poss-(1.075-1.048),2),
             eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100-(51.1-50.4),1),
             ORebRate = round(Oreb/(Oreb+Dreb)*100-(30-28.5),1),
             TovRate = round(Tov/Poss*100-(17.7-18.2),1),
             FTr = round(DrawFoul*2/(TwoA+ThreeA)*100-(29.2-31.5),1),
             ThreeRate = round(ThreeA/(TwoA+ThreeA)*100-(33.5-37.3),1),
             BlkRate = round(Blk/(TwoA)*100-(7.9-8.8),1),
             AstRate = round(Ast/(TwoM+ThreeM)*100-(49-50.9),1),
             AstTo = round(Ast/Tov,2),
             dPPP = round((Points+DrawFoul*1.25)/Poss-(1.112-1.048),2),
             deFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100-(55.3-50.4),1),
             DRebRate = round(Oreb/(Oreb+Dreb)*100-(25.4-28.5),1),
             dTovRate = round(Tov/Poss*100-(16.8-18.2),1),
             dFTr = round(DrawFoul*2/(TwoA+ThreeA)*100-(24.3-31.5),1),
             dThreeRate = round(ThreeA/(TwoA+ThreeA)*100-(41.5-37.3),1),
             dBlkRate = round(Blk/(TwoA)*100-(10.2-8.8),1),
             dAstRate = round(Ast/(TwoM+ThreeM)*100-(54.2-50.9),1),
      ) %>%
      select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate) %>%
      arrange(desc(Date))
    totals <- c("Total", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
                round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
                round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
    
    team_advanced[nrow(team_advanced) + 1,] = c("Total", as.numeric(totals[2])-(1.075-1.048), as.numeric(totals[3])-(51.1-50.4), as.numeric(totals[4])-(30-28.5), as.numeric(totals[5])-(17.7-18.2),
                                                as.numeric(totals[6])-(29.2-31.5), as.numeric(totals[7])-(33.5-37.3), as.numeric(totals[8])-(7.9-8.8), as.numeric(totals[9])-(49-50.9), as.numeric(totals[10]),
                                                as.numeric(totals[2])-(1.112-1.048), as.numeric(totals[3])-(55.3-50.4), as.numeric(totals[4])-(25.4-28.5), as.numeric(totals[5])-(16.8-18.2),
                                                as.numeric(totals[6])-(24.3-31.5), as.numeric(totals[7])-(41.5-37.3), as.numeric(totals[8])-(10.2-8.8), as.numeric(totals[9])-(54.2-50.9))
    team_advanced[, 2:10] <- lapply(team_advanced[, 2:10], as.numeric)
  }
  
  colnames(team_advanced) <- c(colnames(total_team_advanced))
  
  for (i in 1:length(team_advanced$Team)) {
    total_stats <- total_team_advanced
    
    total_stats[nrow(total_stats) + 1,] = team_advanced[i,]
    
    total_stats[, 2:18] <- lapply(total_stats[, 2:18], as.numeric)
    
    total_stats <- total_stats %>%
      mutate(PPP_Per = round(pnorm((PPP-mean(total_stats$PPP))/sd(total_stats$PPP))*100,0),
             eFG_Per = round(pnorm((eFG-mean(total_stats$eFG))/sd(total_stats$eFG))*100,0),
             TovRate_Per = abs(round(pnorm((TovRate-mean(total_stats$TovRate))/sd(total_stats$TovRate))*100,0)-100),
             ORebRate_Per = round(pnorm((ORebRate-mean(total_stats$ORebRate))/sd(total_stats$ORebRate))*100,0),
             FTr_Per = round(pnorm((FTr-mean(total_stats$FTr))/sd(total_stats$FTr))*100,0),
             ThreeRate_Per = round(pnorm((ThreeRate-mean(total_stats$ThreeRate))/sd(total_stats$ThreeRate))*100,0),
             BlkRate_Per = abs(round(pnorm((BlkRate-mean(total_stats$BlkRate))/sd(total_stats$BlkRate))*100,0)-100),
             AstRate_Per = round(pnorm((AstRate-mean(total_stats$AstRate))/sd(total_stats$AstRate))*100,0),
             AstTo_Per = round(pnorm((AstTo-mean(total_stats$AstTo))/sd(total_stats$AstTo))*100,0),
             dPPP_Per = abs(round(pnorm((dPPP-mean(total_stats$dPPP))/sd(total_stats$dPPP))*100,0)-100),
             deFG_Per = abs(round(pnorm((deFG-mean(total_stats$deFG))/sd(total_stats$deFG))*100,0)-100),
             dTovRate_Per = round(pnorm((dTovRate-mean(total_stats$dTovRate))/sd(total_stats$dTovRate))*100,0),
             DRebRate_Per = abs(round(pnorm((DRebRate-mean(total_stats$DRebRate))/sd(total_stats$DRebRate))*100,0)-100),
             dFTr_Per = abs(round(pnorm((dFTr-mean(total_stats$dFTr))/sd(total_stats$dFTr))*100,0)-100),
             dThreeRate_Per = abs(round(pnorm((dThreeRate-mean(total_stats$dThreeRate))/sd(total_stats$dThreeRate))*100,0)-100),
             dBlkRate_Per = round(pnorm((dBlkRate-mean(total_stats$dBlkRate))/sd(total_stats$dBlkRate))*100,0),
             dAstRate_Per = abs(round(pnorm((dAstRate-mean(total_stats$dAstRate))/sd(total_stats$dAstRate))*100,0)-100),
             PPP_Rank = round(floor(rank(-PPP)),0),
             eFG_Rank = round(floor(rank(-eFG)),0),
             TovRate_Rank = round(floor(rank(TovRate)),0),
             ORebRate_Rank = round(floor(rank(-ORebRate)),0),
             FTr_Rank = round(floor(rank(-FTr)),0),
             ThreeRate_Rank = round(floor(rank(-ThreeRate)),0),
             BlkRate_Rank = round(floor(rank(BlkRate)),0),
             AstRate_Rank = round(floor(rank(-AstRate)),0),
             AstTo_Rank = round(floor(rank(-AstTo)),0),
             dPPP_Rank = round(floor(rank(dPPP)),0),
             deFG_Rank = round(floor(rank(deFG)),0),
             dTovRate_Rank = round(floor(rank(-dTovRate)),0),
             DRebRate_Rank = round(floor(rank(DRebRate)),0),
             dFTr_Rank = round(floor(rank(dFTr)),0),
             dThreeRate_Rank = round(floor(rank(dThreeRate)),0),
             dBlkRate_Rank = round(floor(rank(dBlkRate)),0),
             dAstRate_Rank = round(floor(rank(dAstRate)),0),) %>%
      filter(!(Team %in% c(total_team_advanced$Team)))
    
    if (i == 1) {
      team_advanced_output <- total_stats
    }
    else {
      team_advanced_output <- rbind(team_advanced_output,total_stats)
    }
  }
  
  if (Side == "Offense") {
    team_advanced_output <- team_advanced_output %>%
      select(Team, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo, 
             PPP_Per, eFG_Per, ORebRate_Per, TovRate_Per, FTr_Per, ThreeRate_Per, BlkRate_Per, AstRate_Per, AstTo_Per, 
             PPP_Rank, eFG_Rank, ORebRate_Rank, TovRate_Rank, FTr_Rank, ThreeRate_Rank, BlkRate_Rank, AstRate_Rank, AstTo_Rank, )
  }
  else if (Side == "Defense") {
    team_advanced_output <- team_advanced_output %>%
      select(Team, dPPP, deFG, DRebRate, dTovRate, dFTr, dThreeRate, dBlkRate, dAstRate,
             dPPP_Per, deFG_Per, DRebRate_Per, dTovRate_Per, dFTr_Per, dThreeRate_Per, dBlkRate_Per, dAstRate_Per, 
             dPPP_Rank, deFG_Rank, DRebRate_Rank, dTovRate_Rank, dFTr_Rank, dThreeRate_Rank, dBlkRate_Rank, dAstRate_Rank,)
  }
  
  if (Show == "Only Stats") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[1:10]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Effective FG%", "Off. Reb. %", "Tov. %", "Free Throw Rate", "3pt Rate",
                                          "% of Shots Blocked", "Ast Rate", "Ast/Tov Ratio")
    }
    else {
      team_advanced_output <- team_advanced_output[1:9]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Effective FG%", "Off. Reb. %", "Tov. %", "Free Throw Rate", "3pt Rate",
                                          "% of Shots Blocked", "Ast Rate")
    }
    
  }
  else if (Show == "Percentiles") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "%ile", "Effective FG%", "%ile", "Off. Reb. %", "%ile", "Tov. %", "%ile", "Free Throw Rate", "%ile", "3pt Rate","%ile", 
                                          "% of Shots Blocked", "%ile", "Ast Rate", "%ile", "Ast/Tov Ratio", "%ile")
    }
    else {
      team_advanced_output <- team_advanced_output[,c(1,2,10,3,11,4,12,5,13,6,14,7,15,8,16,9,17)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "%ile", "Effective FG%", "%ile", "Off. Reb. %", "%ile", "Tov. %", "%ile", "Free Throw Rate", "%ile", "3pt Rate","%ile", 
                                          "% of Shots Blocked", "%ile", "Ast Rate", "%ile")
    }
    
  }
  else if (Show == "Rankings") {
    if (Side == "Offense") {
      team_advanced_output <- team_advanced_output[,c(1,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Rank", "Effective FG%", "Rank", "Off. Reb. %", "Rank", "Tov. %", "Rank", "Free Throw Rate", "Rank", "3pt Rate","Rank", 
                                          "% of Shots Blocked", "Rank", "Ast Rate", "Rank", "Ast/Tov Ratio", "Rank")
    }
    else {
      team_advanced_output <- team_advanced_output[,c(1,2,18,3,19,4,20,5,21,6,22,7,23,8,24,9,25)]
      colnames(team_advanced_output) <- c("Date", "Points Per Poss.", "Rank", "Effective FG%", "Rank", "Off. Reb. %", "Rank", "Tov. %", "Rank", "Free Throw Rate", "Rank", "3pt Rate","Rank", 
                                          "% of Shots Blocked", "Rank", "Ast Rate", "Rank")
    }
    
  }
  
  return(team_advanced_output[which(team_advanced_output$Date == "Total"),])
}

create_team_scatter <- function(X, Y) {
  database$Date <- format(as.Date(database$Date, format = "%m/%d/%y"), "%Y-%m-%d")
  
  database <- database %>%
    filter(Type == "Practice")
  
  total_poss = sum(database$TwoA)+sum(database$ThreeA)+sum(database$DrawFoul)+sum(database$Tov)-sum(database$Oreb)
  
  team_advanced <- aggregate(cbind(Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ Date, data = database, FUN = sum) %>%
    mutate(Reb = Oreb+Dreb,
           TwoPer = round(TwoM/TwoA*100,1),
           ThreePer = round(ThreeM/ThreeA*100,1),
           Poss = TwoA+ThreeA+DrawFoul+Tov-Oreb,
           PPP = round((Points+DrawFoul*1.25)/Poss,2),
           eFG = round((TwoM+ThreeM*1.5)/(TwoA+ThreeA)*100,1),
           ORebRate = round(Oreb/(Oreb+Dreb)*100,1),
           TovRate = round(Tov/Poss*100,1),
           FTr = round(DrawFoul*2/(TwoA+ThreeA)*100,1),
           ThreeRate = round(ThreeA/(TwoA+ThreeA)*100,1),
           BlkRate = round(Blk/(TwoA)*100,1),
           AstRate = round(Ast/(TwoM+ThreeM)*100,1),
           AstTo = round(Ast/Tov,2)
           #FG = paste0(TwoM+ThreeM,"/",TwoA+ThreeA)
    ) %>%
    select(Date, PPP, eFG, ORebRate, TovRate, FTr, ThreeRate, BlkRate, AstRate, AstTo) %>%
    arrange(desc(Date))
  totals <- c("Belmont", round((sum(database$Points)+sum(database$DrawFoul)*1.25)/total_poss,2), round((sum(database$TwoM)+sum(database$ThreeM*1.5))/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$Oreb)/(sum(database$Oreb)+sum(database$Dreb))*100,1), round(sum(database$Tov)/total_poss*100,1), round(sum(database$DrawFoul)*2/(sum(database$TwoA)+sum(database$ThreeA))*100,1),
              round(sum(database$ThreeA)/(sum(database$TwoA)+sum(database$ThreeA))*100,1), round(sum(database$Blk)/(sum(database$TwoA))*100,1), 
              round(sum(database$Ast)/(sum(database$TwoM)+sum(database$ThreeM))*100,1), round(sum(database$Ast)/sum(database$Tov),2))
  totals <- c(totals, totals[2:9])
  
  stats <- total_team_advanced
  
  stats[nrow(stats) + 1,] = totals
  stats[, 2:18] <- lapply(stats[, 2:18], as.numeric)
  
  colnames(stats) <- c("Team", "Points per Possession", "Effective FG%", "OReb Rate", "Tov Rate", "Free Throw Rate", "3PA Rate", "Block Rate",
                       "Assist Rate", "Ast to Tov", "Defensive PPP", "Defensive eFG%", "DReb Rate", "Defensive Tov Rate", "Defensive FTr",
                       "Defensive Three Rate", "Defensive Block Rate", "Defensive Assist Rate")
  
  
  # Create ggplot object with PPP and dPPP as x and y variables
  p <- ggplotly(
    ggplot(stats, aes(x = .data[[X]], y = .data[[Y]], text = paste("Team: ", Team))) +
      geom_point() +
      geom_text(aes(label = Team), nudge_y = mean(stats[[Y]])*0.01) +
      geom_point(data = subset(stats, Team == "Belmont"), color = "red", size = 3) +
      geom_text(data = subset(stats, Team == "Belmont"), aes(label = Team), color = "red", nudge_y = mean(stats[[Y]])*0.01) +
      theme_bryce() +
      labs(title = paste0(X, " vs. ", Y), x = X, y = Y) +
      theme(strip.text.x = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            plot.title.position = 'plot',
            plot.title = element_text(face =  'bold', size = 15),
            plot.subtitle = element_text(size = 12),
            plot.margin = unit(c(.5, .5, 1, .5), "lines"),
            legend.position = "none")
  )
  
  # Convert ggplot object to plotly object
  return(p)
}

get_plus_minus_bar <- function(Type = "Plus/Minus") {
  lineups <- read.csv("Practice Score Tracker.csv")
  
  PlusMinus <- data_frame(Player = c("Carter Whitt"), Points = 0, Opp_Points = 0, Diff = 0, Wins = 0, Losses = 0, WinPer = 0)
  
  names <- unique(c(lineups$PG, lineups$SG, lineups$SF, lineups$PF, lineups$C, lineups$B1, lineups$B2, lineups$B3))
  names <- na.omit(names)
  
  for (i in 1:length(names)) {
    if (names[i] != "") {
      IndividualPlusMinus <- lineups %>%
        filter(PG %in% names[i] | SG %in% names[i] | SF %in% names[i] | PF %in% names[i] | C %in% names[i] | B1 %in% names[i] | B2 %in% names[i] | B3 %in% names[i])
      
      player_row <- data.frame(Player = names[i], 
                               Points = sum(IndividualPlusMinus$Score), 
                               Opp_Points = sum(IndividualPlusMinus$OppScore), 
                               Diff = sum(IndividualPlusMinus$Score)-sum(IndividualPlusMinus$OppScore),
                               Wins = sum(IndividualPlusMinus$Win), 
                               Losses = length(IndividualPlusMinus$Win)-sum(IndividualPlusMinus$Win),
                               WinPer = 0)
      
      PlusMinus <- rbind(PlusMinus, player_row)
    }
  }
  
  PlusMinus <- PlusMinus[2:nrow(PlusMinus),] %>%
    mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
           Poss = 100/112*(Points+Opp_Points)/2,
           Off_Rtg = Points/Poss*100,
           Def_Rtg = Opp_Points/Poss*100,
           Net_Rtg = Off_Rtg-Def_Rtg) %>%
    mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1)))
  
  stats <- aggregate(cbind(Points) ~ url + Player, data = database, FUN = sum) %>% select(url, Player)
  
  PlusMinus <- merge(PlusMinus, stats, by = "Player")
  
  PlusMinus <- PlusMinus %>%
    select(url, Player, Points, Opp_Points, Diff, Off_Rtg, Def_Rtg, Net_Rtg, Wins, Losses, WinPer) %>%
    arrange(desc(WinPer))
  
  if (Type == "Plus/Minus") {
    p <-
      ggplot(PlusMinus, aes(x = reorder(Player, Diff), y = Diff, fill = Diff, text = paste("Player: ", Player, "<br>Diff: ", Diff))) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "red", high = "green") + 
      labs(x = "Player", y = "Plus/Minus", title = "Practice Plus/Minus") +
      theme_bryce() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
            plot.title = element_text(size = 20),  # Change title font size
            axis.title.x = element_text(size = 16),  # Change x-axis label font size
            axis.title.y = element_text(size = 16),
            axis.text = element_text(size = 14, face = "bold"))
    
  }
  
  if (Type == "Net Rating") {
    p <- ggplot(PlusMinus, aes(x = reorder(Player, Net_Rtg), y = Net_Rtg, fill = Net_Rtg)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "red", high = "green") + 
      labs(x = "Player", y = "Net Rating", title = "Practice Net Rating") +
      theme_bryce() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
            plot.title = element_text(size = 20),  # Change title font size
            axis.title.x = element_text(size = 16),  # Change x-axis label font size
            axis.title.y = element_text(size = 16),
            axis.text = element_text(size = 14, face = "bold"))  }
  
  if (Type == "Win Percentage") {
    p <- ggplot(PlusMinus, aes(x = reorder(Player, WinPer), y = WinPer-50, fill = WinPer)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "red", high = "green") + 
      labs(x = "Player", y = "Win Percentage", title = "Practice Win Percentage") +
      scale_y_continuous(limits = c(-20, 20), 
                         breaks = seq(-20,20,10),
                         labels = seq(30,70,10)) +
      theme_bryce() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
            plot.title = element_text(size = 20),  # Change title font size
            axis.title.x = element_text(size = 16),  # Change x-axis label font size
            axis.title.y = element_text(size = 16),
            axis.text = element_text(size = 14, face = "bold"))  }
  
  return(p)
}

get_brapm <- function() {
  lineups <- read.csv("Practice Score Tracker.csv")
  database <- read.csv("Practice Stat Database.csv")
  database$Player <- gsub("<a0>", " ", database$Player)
  
  evanmiya <- read.csv("EvanMiyaData.csv")
  
  lineups <- lineups %>%
    mutate(Wins = ifelse(Win == TRUE, 1, 0),
           Losses = ifelse(Win == FALSE, 1, 0))
  
  Total_Lineups <- lineups %>%
    mutate(Poss = 100/112*(Score+OppScore)/2)
  
  possesion_data <- Total_Lineups %>%
    mutate(offensePlayer1Id = "", offensePlayer2Id = "", offensePlayer3Id = "", offensePlayer4Id = "", offensePlayer5Id = "", defensePlayer1Id = "", 
           defensePlayer2Id = "", defensePlayer3Id = "", defensePlayer4Id = "", defensePlayer5Id = "", points = 0, possessions = 0)
  
  for (i in 1:length(possesion_data$PG)) {
    if (i%%2 != 0) {
      possesion_data$defensePlayer1Id[i] = possesion_data$PG[i+1]
      possesion_data$defensePlayer2Id[i] = possesion_data$SG[i+1]
      possesion_data$defensePlayer3Id[i] = possesion_data$SF[i+1]
      possesion_data$defensePlayer4Id[i] = possesion_data$PF[i+1]
      possesion_data$defensePlayer5Id[i] = possesion_data$C[i+1]
      
      possesion_data$offensePlayer1Id[i] = possesion_data$PG[i]
      possesion_data$offensePlayer2Id[i] = possesion_data$SG[i]
      possesion_data$offensePlayer3Id[i] = possesion_data$SF[i]
      possesion_data$offensePlayer4Id[i] = possesion_data$PF[i]
      possesion_data$offensePlayer5Id[i] = possesion_data$C[i]
      
      possesion_data$points[i] = possesion_data$Score[i]
      possesion_data$possessions[i] = possesion_data$Poss[i]
    }
    else if (i%%2 == 0) {
      possesion_data$defensePlayer1Id[i] = possesion_data$PG[i-1]
      possesion_data$defensePlayer2Id[i] = possesion_data$SG[i-1]
      possesion_data$defensePlayer3Id[i] = possesion_data$SF[i-1]
      possesion_data$defensePlayer4Id[i] = possesion_data$PF[i-1]
      possesion_data$defensePlayer5Id[i] = possesion_data$C[i-1]
      
      possesion_data$offensePlayer1Id[i] = possesion_data$PG[i]
      possesion_data$offensePlayer2Id[i] = possesion_data$SG[i]
      possesion_data$offensePlayer3Id[i] = possesion_data$SF[i]
      possesion_data$offensePlayer4Id[i] = possesion_data$PF[i]
      possesion_data$offensePlayer5Id[i] = possesion_data$C[i]
      
      possesion_data$points[i] = possesion_data$Score[i]
      possesion_data$possessions[i] = possesion_data$Poss[i]
    }
  }
  
  possesion_data <- possesion_data[16:27] %>%
    filter(possessions >= 5)
  
  for (i in 1:100) {
    get_players <- function(possesions) {
      
      possesions <- distinct(possesions)
      players <- unique(c(unique(possesions$offensePlayer1Id),
                          unique(possesions$offensePlayer2Id),
                          unique(possesions$offensePlayer3Id),
                          unique(possesions$offensePlayer4Id),
                          unique(possesions$offensePlayer5Id),
                          unique(possesions$defensePlayer1Id),
                          unique(possesions$defensePlayer2Id),
                          unique(possesions$defensePlayer3Id),
                          unique(possesions$defensePlayer4Id),
                          unique(possesions$defensePlayer5Id)))
      return(players)
      
    }
    
    players <- sort(get_players(possesions = possesion_data))
    
    #print(paste0("There are ", length(players), " unique players in the dataset."))
    possesion_data <- possesion_data %>% 
      mutate(ppp100 = 100 * points/possessions)
    
    make_matrix_rows <- function(lineup, players_in) {
      
      player1 <- lineup[1]
      player2 <- lineup[2]
      player3 <- lineup[3]
      player4 <- lineup[4]
      player5 <- lineup[5]
      player6 <- lineup[6]
      player7 <- lineup[7]
      player8 <- lineup[8]
      player9 <- lineup[9]
      player10 <- lineup[10]
      
      zeroRow <- rep(0, length(players_in) * 2)
      
      # OFFENSE #
      zeroRow[which(players_in == player1)] <- 1
      zeroRow[which(players_in == player2)] <- 1
      zeroRow[which(players_in == player3)] <- 1
      zeroRow[which(players_in == player4)] <- 1
      zeroRow[which(players_in == player5)] <- 1
      
      # DEFENSE #
      zeroRow[which(players_in == player6) + length(players_in)] <- -1
      zeroRow[which(players_in == player7) + length(players_in)] <- -1
      zeroRow[which(players_in == player8) + length(players_in)] <- -1
      zeroRow[which(players_in == player9) + length(players_in)] <- -1
      zeroRow[which(players_in == player10) + length(players_in)] <- -1
      
      return(zeroRow)
      
    }
    
    
    player_matrix <- t(apply(possesion_data[, 1:10], 1, 
                             function(x) make_matrix_rows(lineup = x, players_in = players)))
    
    player_matrix <- as(player_matrix, "dgCMatrix")
    
    target <- possesion_data$ppp100
    
    #print(dim(player_matrix))
    
    ## this is a cross validated model to pick the lambda
    cv_model <- glmnet::cv.glmnet(x = player_matrix, ##ncol = 1058
                                  y = target,
                                  alpha = 0, 
                                  standardize = FALSE)
    
    lam <- cv_model$lambda.min ## best lambda
    
    ## this is the model refit using that lambda
    coef_model <- glmnet::glmnet(x = player_matrix, 
                                 y = target,
                                 alpha = 0, 
                                 standardize = FALSE,
                                 lambda = lam)
    
    player_coefficients <- coef_model$beta ## length = 1058
    
    o_rapm <- player_coefficients[1:length(players)]
    
    d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]
    
    o_rapm_frame <- data.frame("player_id" = players,
                               "o_rapm" = o_rapm)
    
    d_rapm_frame <- data.frame("player_id" = players,
                               "d_rapm" = d_rapm)
    
    RAPM <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
      #left_join(player_data, by = c("player_id" = "playerId")) %>% 
      mutate(rapm = o_rapm + d_rapm) %>% 
      # select(Player = playerName,
      #        RAPM = rapm,
      #        O-RAPM = o_rapm,
      #        D-RAPM = d_rapm) %>% 
      arrange(-rapm)
    
    if (i == 1) {
      rapm_avg <- RAPM
    }
    else {
      rapm_avg <- rbind(rapm_avg, RAPM)
    }
    
  }
  
  
  rapm <- aggregate(cbind(o_rapm, d_rapm, rapm) ~ player_id, data = rapm_avg, FUN = mean)
  
  database <- database %>%
    filter(Type == "Practice")
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum) %>%
    mutate(OBPM = 1*(Points/Poss*100 + 0.4*ThreeM/Poss*100 + 0.7*Ast/Poss*100 + 0.4*Oreb/Poss*100 + 1*DrawFoul/Poss*100 - 1*Tov/Poss*100 - 0.5*(TwoA+ThreeA)/Poss*100),
           DBPM = 1.2*(0.3*Dreb/Poss*100 + 1*Stl/Poss*100 + 1.0*Blk/Poss*100 + 0.15*Deflects/Poss*100 - 0.4*Foul/Poss*100))
  
  stats <- stats %>%
    mutate(OBPM = OBPM-mean(stats$OBPM),
           DBPM = DBPM-mean(stats$DBPM),
           BPM = OBPM+DBPM) %>%
    dplyr::select(url, Player, OBPM, DBPM, BPM)
  
  stats <- merge(stats, rapm, by.x="Player", by.y="player_id") 
  stats <- merge(stats, evanmiya, by="Player") %>%
    mutate(o_rapm = 0.34*o_rapm+0.33*OBPR+0.33*BROBPM,
           d_rapm = 0.5*d_rapm+0.5*DBPR+0.33*BRDBPM,
           rapm = o_rapm+d_rapm,
           BRAPM = BPM*0.5+rapm*0.5) %>%
    mutate(across(c(OBPM, DBPM, BPM, o_rapm, d_rapm, rapm, BRAPM), ~ round(., 1))) %>%
    select(url, Player, OBPM, DBPM, BPM, o_rapm, d_rapm, rapm, BRAPM) %>%
    arrange(desc(BRAPM))
  
  colnames(stats) <- c("", "Player", "Offensive BPM", "Defensive BPM", "Total BPM", "Offensive RAPM", "Defensive RAPM", "Total RAPM", "Total Adjusted Impact")
  
  return(stats)
}

get_plus_minus <- function() {
  lineups <- read.csv("Practice Score Tracker.csv")
  
  PlusMinus <- data_frame(Player = c("Carter Whitt"), Points = 0, Opp_Points = 0, Diff = 0, Wins = 0, Losses = 0, WinPer = 0)
  
  names <- unique(c(lineups$PG, lineups$SG, lineups$SF, lineups$PF, lineups$C, lineups$B1, lineups$B2, lineups$B3))
  names <- na.omit(names)
  
  for (i in 1:length(names)) {
    if (names[i] != "") {
      IndividualPlusMinus <- lineups %>%
        filter(PG %in% names[i] | SG %in% names[i] | SF %in% names[i] | PF %in% names[i] | C %in% names[i] | B1 %in% names[i] | B2 %in% names[i] | B3 %in% names[i])
      
      player_row <- data.frame(Player = names[i], 
                               Points = sum(IndividualPlusMinus$Score), 
                               Opp_Points = sum(IndividualPlusMinus$OppScore), 
                               Diff = sum(IndividualPlusMinus$Score)-sum(IndividualPlusMinus$OppScore),
                               Wins = sum(IndividualPlusMinus$Win), 
                               Losses = length(IndividualPlusMinus$Win)-sum(IndividualPlusMinus$Win),
                               WinPer = 0)
      
      PlusMinus <- rbind(PlusMinus, player_row)
    }
  }
  
  PlusMinus <- PlusMinus[2:nrow(PlusMinus),] %>%
    mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
           Poss = 100/112*(Points+Opp_Points)/2,
           Off_Rtg = Points/Poss*100,
           Def_Rtg = Opp_Points/Poss*100,
           Net_Rtg = Off_Rtg-Def_Rtg) %>%
    mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1)))
  
  stats <- aggregate(cbind(Points) ~ url + Player, data = database, FUN = sum) %>% select(url, Player)
  
  PlusMinus <- merge(PlusMinus, stats, by = "Player")
  
  PlusMinus <- PlusMinus %>%
    select(url, Player, Points, Opp_Points, Diff, Off_Rtg, Def_Rtg, Net_Rtg, Wins, Losses, WinPer) %>%
    arrange(desc(WinPer)) %>%
    arrange(desc(Diff))
  
  colnames(PlusMinus) <- c("", "Player", "Team Points", "Opponent Points", "+/-", "Off. Rtg.", "Def. Rtg", "Net Rtg.", "Wins", "Losses", "Win %")
  
  return(PlusMinus)
}

get_lineups <- function(Players, NotPlayers, PG1, SG1, SF1, PF1, C1) {
  lineups <- read.csv("Practice Score Tracker.csv")
  
  lineups[is.na(lineups)] <- ""
  
  lineups <- lineups %>%
    mutate(Wins = ifelse(Win == TRUE, 1, 0),
           Losses = ifelse(Win == FALSE, 1, 0)) %>%
    rowwise() %>%
    filter(all(Players %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
    filter(all(PG1 %in% c(PG))) %>%
    filter(all(SG1 %in% c(SG))) %>%
    filter(all(SF1 %in% c(SF))) %>%
    filter(all(PF1 %in% c(PF))) %>%
    filter(all(C1 %in% c(C))) %>%
    filter(!any(NotPlayers %in% c(PG, SG, SF, PF, C, B1, B2, B3)))
  
  Total_Lineups <- aggregate(cbind(Score, OppScore, Diff, Wins, Losses) ~ PG + SG + SF + PF + C + B1 + B2 + B3, data = lineups, FUN = sum) %>%
    mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
           Poss = 100/112*(Score+OppScore)/2,
           Off_Rtg = Score/Poss*100,
           Def_Rtg = OppScore/Poss*100,
           Net_Rtg = Off_Rtg-Def_Rtg) %>%
    mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1))) %>%
    arrange(desc(Poss)) %>%
    select(PG, SG, SF, PF, C, B1, B2, B3, Score, OppScore, Diff, Off_Rtg, Def_Rtg, Net_Rtg)
  
  colnames(Total_Lineups) <- c("PG", "SG", "SF", "PF", "C", "B1", "B2", "B3", "Points", "Opp. Points", "+/-", "Off. Rtg.", "Def. Rtg.", "Net Rtg.")
  
  return(Total_Lineups)
}

get_lineup_totals <- function(Players, NotPlayers, PG1, SG1, SF1, PF1, C1) {
  lineups <- read.csv("Practice Score Tracker.csv")
  
  lineups[is.na(lineups)] <- ""
  
  lineups <- lineups %>%
    mutate(Wins = ifelse(Win == TRUE, 1, 0),
           Losses = ifelse(Win == FALSE, 1, 0)) %>%
    rowwise() %>%
    filter(all(Players %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
    filter(all(PG1 %in% c(PG))) %>%
    filter(all(SG1 %in% c(SG))) %>%
    filter(all(SF1 %in% c(SF))) %>%
    filter(all(PF1 %in% c(PF))) %>%
    filter(all(C1 %in% c(C))) %>%
    filter(!any(NotPlayers %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
    mutate(Name = "Total")
  
  Total_Lineups <- aggregate(cbind(Score, OppScore, Diff, Wins, Losses) ~ Name, data = lineups, FUN = sum) %>%
    mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
           Poss = 100/112*(Score+OppScore)/2,
           Off_Rtg = Score/Poss*100,
           Def_Rtg = OppScore/Poss*100,
           Net_Rtg = Off_Rtg-Def_Rtg,
           Diff = Score-OppScore) %>%
    mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1))) %>%
    select(Name, Score, OppScore, Diff, Wins, Losses, WinPer, Off_Rtg, Def_Rtg, Net_Rtg)
  
  colnames(Total_Lineups) <- c("Name", "Points", "Opp. Points", "+/-", "Wins", "Losses", "Win %", "Off. Rtg.", "Def. Rtg.", "Net Rtg.")
  
  return(Total_Lineups)
}

get_lineup_combos <- function(Type) {
  names <- unique(c(lineups$PG, lineups$SG, lineups$SF, lineups$PF, lineups$C, lineups$B1, lineups$B2, lineups$B3))
  
  names <- subset(names, names != "")
  
  if (Type == "Two-Man") {
    player_combos <- data.frame(t(combn(names, 2))) %>%
      mutate(Score = 0, OppScore = 0, Diff = 0, Wins = 0, Losses = 0)
    
    for (i in 1:length(player_combos$X1)) {
      combo <- lineups %>%
        rowwise() %>%
        filter(all(player_combos$X1[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X2[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3)))
      
      player_combos$Score[i] = sum(combo$Score)
      player_combos$OppScore[i] = sum(combo$OppScore)
      player_combos$Diff[i] = sum(combo$Score)-sum(combo$OppScore)
      player_combos$Wins[i] = sum(combo$Win)
      player_combos$Losses[i] = length(combo$PG)-sum(combo$Win)
    }
    
    player_combos <- player_combos %>%
      mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
             Poss = 100/112*(Score+OppScore)/2,
             Off_Rtg = Score/Poss*100,
             Def_Rtg = OppScore/Poss*100,
             Net_Rtg = Off_Rtg-Def_Rtg) %>%
      mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1))) %>%
      arrange(desc(Poss)) %>%
      filter(Poss > 0) %>%
      select(-Poss)
    
    colnames(player_combos) <- c("Player 1", "Player 2", "Points", "Opp. Points", "+/-", "Wins", "Losses", "Win %", "Off. Rtg.", "Def. Rtg.", "Net Rtg.")
  }
  
  if (Type == "Three-Man") {
    player_combos <- data.frame(t(combn(names, 3))) %>%
      mutate(Score = 0, OppScore = 0, Diff = 0, Wins = 0, Losses = 0)
    
    for (i in 1:length(player_combos$X1)) {
      combo <- lineups %>%
        rowwise() %>%
        filter(all(player_combos$X1[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X2[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X3[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3)))
      
      player_combos$Score[i] = sum(combo$Score)
      player_combos$OppScore[i] = sum(combo$OppScore)
      player_combos$Diff[i] = sum(combo$Score)-sum(combo$OppScore)
      player_combos$Wins[i] = sum(combo$Win)
      player_combos$Losses[i] = length(combo$PG)-sum(combo$Win)
    }
    
    player_combos <- player_combos %>%
      mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
             Poss = 100/112*(Score+OppScore)/2,
             Off_Rtg = Score/Poss*100,
             Def_Rtg = OppScore/Poss*100,
             Net_Rtg = Off_Rtg-Def_Rtg) %>%
      mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1))) %>%
      arrange(desc(Poss)) %>%
      filter(Poss > 0) %>%
      select(-Poss)
    
    colnames(player_combos) <- c("Player 1", "Player 2", "Player 3", "Points", "Opp. Points", "+/-", "Wins", "Losses", "Win %", "Off. Rtg.", "Def. Rtg.", "Net Rtg.")
    
  }
  
  if (Type == "Four-Man") {
    player_combos <- data.frame(t(combn(names, 4))) %>%
      mutate(Score = 0, OppScore = 0, Diff = 0, Wins = 0, Losses = 0)
    
    for (i in 1:length(player_combos$X1)) {
      combo <- lineups %>%
        rowwise() %>%
        filter(all(player_combos$X1[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X2[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X3[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3))) %>%
        filter(all(player_combos$X4[i] %in% c(PG, SG, SF, PF, C, B1, B2, B3)))
      
      player_combos$Score[i] = sum(combo$Score)
      player_combos$OppScore[i] = sum(combo$OppScore)
      player_combos$Diff[i] = sum(combo$Score)-sum(combo$OppScore)
      player_combos$Wins[i] = sum(combo$Win)
      player_combos$Losses[i] = length(combo$PG)-sum(combo$Win)
    }
    
    player_combos <- player_combos %>%
      mutate(WinPer = round(Wins/(Wins+Losses)*100,1),
             Poss = 100/112*(Score+OppScore)/2,
             Off_Rtg = Score/Poss*100,
             Def_Rtg = OppScore/Poss*100,
             Net_Rtg = Off_Rtg-Def_Rtg) %>%
      mutate(across(c(Poss, Off_Rtg, Def_Rtg, Net_Rtg), ~ round(., 1))) %>%
      arrange(desc(Poss)) %>%
      filter(Poss > 0) %>%
      select(-Poss)
    
    colnames(player_combos) <- c("Player 1", "Player 2", "Player 3", "Player 4", "Points", "Opp. Points", "+/-", "Wins", "Losses", "Win %", "Off. Rtg.", "Def. Rtg.", "Net Rtg.")
    
  }
  
  return(player_combos)
  
}

get_teammates <- function(Player) {
  lineups <- read.csv("Practice Score Tracker.csv")
  
  Teammate <- data_frame(Player = c("Win Miller"), Points = 0, Opp_Points = 0, Wins = 0, Losses = 0, Vs_Points = 0, Vs_Opp_Points = 0, Vs_Wins = 0, Vs_Losses = 0)
  
  names <- unique(c(lineups$PG, lineups$SG, lineups$SF, lineups$PF, lineups$C, lineups$B1, lineups$B2, lineups$B3))
  
  names <- subset(names, names != "")
  
  #Player = "Ja'Kobi Gillespie"
  
  lineups <- lineups %>%
    mutate(Wins = ifelse(Win == TRUE, 1, 0),
           Losses = ifelse(Win == FALSE, 1, 0))
  
  possesion_data <- lineups %>%
    mutate(off1Id = "", off2Id = "", off3Id = "", off4Id = "", off5Id = "", off6Id = "", off7Id = "", off8Id = "", def1Id = "", 
           def2Id = "", def3Id = "", def4Id = "", def5Id = "", def6Id = "", def7Id = "", def8Id = "", points = 0, opp_points = 0, wins = 0, losses = 0)
  
  for (i in 1:length(possesion_data$PG)) {
    if (i%%2 != 0) {
      possesion_data$def1Id[i] = possesion_data$PG[i+1]
      possesion_data$def2Id[i] = possesion_data$SG[i+1]
      possesion_data$def3Id[i] = possesion_data$SF[i+1]
      possesion_data$def4Id[i] = possesion_data$PF[i+1]
      possesion_data$def5Id[i] = possesion_data$C[i+1]
      possesion_data$def6Id[i] = possesion_data$B1[i+1]
      possesion_data$def7Id[i] = possesion_data$B2[i+1]
      possesion_data$def8Id[i] = possesion_data$B3[i+1]
      
      possesion_data$off1Id[i] = possesion_data$PG[i]
      possesion_data$off2Id[i] = possesion_data$SG[i]
      possesion_data$off3Id[i] = possesion_data$SF[i]
      possesion_data$off4Id[i] = possesion_data$PF[i]
      possesion_data$off5Id[i] = possesion_data$C[i]
      possesion_data$off6Id[i] = possesion_data$B1[i]
      possesion_data$off7Id[i] = possesion_data$B2[i]
      possesion_data$off8Id[i] = possesion_data$B3[i]
      
      possesion_data$points[i] = possesion_data$Score[i]
      possesion_data$opp_points[i] = possesion_data$OppScore[i]
      possesion_data$wins[i] = possesion_data$Wins[i]
      possesion_data$losses[i] = possesion_data$Losses[i]
    }
    
  }
  
  possesion_data <- possesion_data[16:35] %>%
    filter(points > 0 | opp_points > 0)
  
  for (i in names) {
    Points = 0
    Opp_Points = 0
    Wins = 0
    Losses = 0
    Vs_Points = 0
    Vs_Opp_Points = 0
    Vs_Wins = 0
    Vs_Losses = 0
    
    for (j in 1:length(possesion_data$off1Id)) {
      if (i != Player) {
        if (Player %in% c(possesion_data$off1Id[j], possesion_data$off2Id[j], possesion_data$off3Id[j], possesion_data$off4Id[j],
                          possesion_data$off5Id[j], possesion_data$off6Id[j], possesion_data$off7Id[j], possesion_data$off8Id[j])){
          if (i %in% c(possesion_data$def1Id[j], possesion_data$def2Id[j], possesion_data$def3Id[j], possesion_data$def4Id[j],
                       possesion_data$def5Id[j], possesion_data$def6Id[j], possesion_data$def7Id[j], possesion_data$def8Id[j])) {
            Vs_Points = Vs_Points + possesion_data$points[j]
            Vs_Opp_Points = Vs_Opp_Points + possesion_data$opp_points[j]
            Vs_Wins = Vs_Wins + possesion_data$wins[j]
            Vs_Losses = Vs_Losses + possesion_data$losses[j]
          }
        }
        
        if (i %in% c(possesion_data$off1Id[j], possesion_data$off2Id[j], possesion_data$off3Id[j], possesion_data$off4Id[j],
                     possesion_data$off5Id[j], possesion_data$off6Id[j], possesion_data$off7Id[j], possesion_data$off8Id[j])) {
          if (Player %in% c(possesion_data$def1Id[j], possesion_data$def2Id[j], possesion_data$def3Id[j], possesion_data$def4Id[j],
                            possesion_data$def5Id[j], possesion_data$def6Id[j], possesion_data$def7Id[j], possesion_data$def8Id[j])) {
            Vs_Points = Vs_Points + possesion_data$opp_points[j]
            Vs_Opp_Points = Vs_Opp_Points + possesion_data$points[j]
            Vs_Wins = Vs_Wins + possesion_data$losses[j]
            Vs_Losses = Vs_Losses + possesion_data$wins[j]
          }
        }
        
        if (i %in% c(possesion_data$off1Id[j], possesion_data$off2Id[j], possesion_data$off3Id[j], possesion_data$off4Id[j],
                     possesion_data$off5Id[j], possesion_data$off6Id[j], possesion_data$off7Id[j], possesion_data$off8Id[j])) {
          if (Player %in% c(possesion_data$off1Id[j], possesion_data$off2Id[j], possesion_data$off3Id[j], possesion_data$off4Id[j],
                            possesion_data$off5Id[j], possesion_data$off6Id[j], possesion_data$off7Id[j], possesion_data$def8Id[j])) {
            Points = Points + possesion_data$points[j]
            Opp_Points = Opp_Points + possesion_data$opp_points[j]
            Wins = Wins + possesion_data$wins[j]
            Losses = Losses + possesion_data$losses[j]
          }
        }
        
        if (i %in% c(possesion_data$def1Id[j], possesion_data$def2Id[j], possesion_data$def3Id[j], possesion_data$def4Id[j],
                     possesion_data$def5Id[j], possesion_data$def6Id[j], possesion_data$def7Id[j], possesion_data$off8Id[j])) {
          if (Player %in% c(possesion_data$def1Id[j], possesion_data$def2Id[j], possesion_data$def3Id[j], possesion_data$def4Id[j],
                            possesion_data$def5Id[j], possesion_data$def6Id[j], possesion_data$def7Id[j], possesion_data$def8Id[j])) {
            Points = Points + possesion_data$opp_points[j]
            Opp_Points = Opp_Points + possesion_data$points[j]
            Wins = Wins + possesion_data$losses[j]
            Losses = Losses + possesion_data$wins[j]
          }
        }
        
      }
    }
    
    player_row <- data.frame(Player = i, 
                             Points = Points, 
                             Opp_Points = Opp_Points, 
                             Wins = Wins, 
                             Losses = Losses, 
                             Vs_Points = Vs_Points, 
                             Vs_Opp_Points = Vs_Opp_Points, 
                             Vs_Wins = Vs_Wins, 
                             Vs_Losses = Vs_Losses)
    
    Teammate <- rbind(Teammate, player_row)
    
  }
  
  Teammate <- Teammate %>%
    filter(Wins > 0 | Losses > 0 | Vs_Wins > 0 | Vs_Losses > 0) %>%
    mutate(Diff = Points - Opp_Points,
           Vs_Diff = Vs_Points - Vs_Opp_Points,
           Win_Per = round(Wins/(Wins+Losses)*100,1),
           Vs_Win_Per = round(Vs_Wins/(Vs_Wins+Vs_Losses)*100,1),
           TeammatePer = round((Wins+Losses)/(Wins+Losses+Vs_Wins+Vs_Losses)*100,1)) %>%
    select(Player, TeammatePer, Points, Opp_Points, Diff, Wins, Losses, Win_Per, Vs_Points, Vs_Opp_Points, Vs_Diff, Vs_Wins, Vs_Losses, Vs_Win_Per) %>%
    arrange(desc(TeammatePer))
  
  colnames(Teammate) <- c("Player", "% of Time on Team", "Team Points when Teammates", "Opponent Points when Teammates", "Point Diff when Teammates", "Wins when Teammates", "Losses when Teammates", "Win % when Teammates",
                          "Team Points when Opponents", "Opponent Points when Opponents", "Point Diff when Opponents", "Wins when Opponents", "Losses when Opponents", "Win % when Opponents")
  
  return(Teammate)
}  

predict_scrimmage <- function(team1, team2, Target_Score) {
  lineups <- read.csv("Practice Score Tracker.csv")
  database <- read.csv("Practice Stat Database.csv")
  database$Player <- gsub("<a0>", " ", database$Player)
  
  evanmiya <- read.csv("EvanMiyaData.csv")
  
  lineups <- lineups %>%
    mutate(Wins = ifelse(Win == TRUE, 1, 0),
           Losses = ifelse(Win == FALSE, 1, 0))
  
  Total_Lineups <- lineups %>%
    mutate(Poss = 100/112*(Score+OppScore)/2)
  
  possesion_data <- Total_Lineups %>%
    mutate(offensePlayer1Id = "", offensePlayer2Id = "", offensePlayer3Id = "", offensePlayer4Id = "", offensePlayer5Id = "", defensePlayer1Id = "", 
           defensePlayer2Id = "", defensePlayer3Id = "", defensePlayer4Id = "", defensePlayer5Id = "", points = 0, possessions = 0)
  
  for (i in 1:length(possesion_data$PG)) {
    if (i%%2 != 0) {
      possesion_data$defensePlayer1Id[i] = possesion_data$PG[i+1]
      possesion_data$defensePlayer2Id[i] = possesion_data$SG[i+1]
      possesion_data$defensePlayer3Id[i] = possesion_data$SF[i+1]
      possesion_data$defensePlayer4Id[i] = possesion_data$PF[i+1]
      possesion_data$defensePlayer5Id[i] = possesion_data$C[i+1]
      
      possesion_data$offensePlayer1Id[i] = possesion_data$PG[i]
      possesion_data$offensePlayer2Id[i] = possesion_data$SG[i]
      possesion_data$offensePlayer3Id[i] = possesion_data$SF[i]
      possesion_data$offensePlayer4Id[i] = possesion_data$PF[i]
      possesion_data$offensePlayer5Id[i] = possesion_data$C[i]
      
      possesion_data$points[i] = possesion_data$Score[i]
      possesion_data$possessions[i] = possesion_data$Poss[i]
    }
    else if (i%%2 == 0) {
      possesion_data$defensePlayer1Id[i] = possesion_data$PG[i-1]
      possesion_data$defensePlayer2Id[i] = possesion_data$SG[i-1]
      possesion_data$defensePlayer3Id[i] = possesion_data$SF[i-1]
      possesion_data$defensePlayer4Id[i] = possesion_data$PF[i-1]
      possesion_data$defensePlayer5Id[i] = possesion_data$C[i-1]
      
      possesion_data$offensePlayer1Id[i] = possesion_data$PG[i]
      possesion_data$offensePlayer2Id[i] = possesion_data$SG[i]
      possesion_data$offensePlayer3Id[i] = possesion_data$SF[i]
      possesion_data$offensePlayer4Id[i] = possesion_data$PF[i]
      possesion_data$offensePlayer5Id[i] = possesion_data$C[i]
      
      possesion_data$points[i] = possesion_data$Score[i]
      possesion_data$possessions[i] = possesion_data$Poss[i]
    }
  }
  
  possesion_data <- possesion_data[16:27] %>%
    filter(possessions >= 5)
  
  for (i in 1:100) {
    get_players <- function(possesions) {
      
      possesions <- distinct(possesions)
      players <- unique(c(unique(possesions$offensePlayer1Id),
                          unique(possesions$offensePlayer2Id),
                          unique(possesions$offensePlayer3Id),
                          unique(possesions$offensePlayer4Id),
                          unique(possesions$offensePlayer5Id),
                          unique(possesions$defensePlayer1Id),
                          unique(possesions$defensePlayer2Id),
                          unique(possesions$defensePlayer3Id),
                          unique(possesions$defensePlayer4Id),
                          unique(possesions$defensePlayer5Id)))
      return(players)
      
    }
    
    players <- sort(get_players(possesions = possesion_data))
    
    #print(paste0("There are ", length(players), " unique players in the dataset."))
    possesion_data <- possesion_data %>% 
      mutate(ppp100 = 100 * points/possessions)
    
    make_matrix_rows <- function(lineup, players_in) {
      
      player1 <- lineup[1]
      player2 <- lineup[2]
      player3 <- lineup[3]
      player4 <- lineup[4]
      player5 <- lineup[5]
      player6 <- lineup[6]
      player7 <- lineup[7]
      player8 <- lineup[8]
      player9 <- lineup[9]
      player10 <- lineup[10]
      
      zeroRow <- rep(0, length(players_in) * 2)
      
      # OFFENSE #
      zeroRow[which(players_in == player1)] <- 1
      zeroRow[which(players_in == player2)] <- 1
      zeroRow[which(players_in == player3)] <- 1
      zeroRow[which(players_in == player4)] <- 1
      zeroRow[which(players_in == player5)] <- 1
      
      # DEFENSE #
      zeroRow[which(players_in == player6) + length(players_in)] <- -1
      zeroRow[which(players_in == player7) + length(players_in)] <- -1
      zeroRow[which(players_in == player8) + length(players_in)] <- -1
      zeroRow[which(players_in == player9) + length(players_in)] <- -1
      zeroRow[which(players_in == player10) + length(players_in)] <- -1
      
      return(zeroRow)
      
    }
    
    
    player_matrix <- t(apply(possesion_data[, 1:10], 1, 
                             function(x) make_matrix_rows(lineup = x, players_in = players)))
    
    player_matrix <- as(player_matrix, "dgCMatrix")
    
    target <- possesion_data$ppp100
    
    #print(dim(player_matrix))
    
    ## this is a cross validated model to pick the lambda
    cv_model <- glmnet::cv.glmnet(x = player_matrix, ##ncol = 1058
                                  y = target,
                                  alpha = 0, 
                                  standardize = FALSE)
    
    lam <- cv_model$lambda.min ## best lambda
    
    ## this is the model refit using that lambda
    coef_model <- glmnet::glmnet(x = player_matrix, 
                                 y = target,
                                 alpha = 0, 
                                 standardize = FALSE,
                                 lambda = lam)
    
    player_coefficients <- coef_model$beta ## length = 1058
    
    o_rapm <- player_coefficients[1:length(players)]
    
    d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]
    
    o_rapm_frame <- data.frame("player_id" = players,
                               "o_rapm" = o_rapm)
    
    d_rapm_frame <- data.frame("player_id" = players,
                               "d_rapm" = d_rapm)
    
    RAPM <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
      #left_join(player_data, by = c("player_id" = "playerId")) %>% 
      mutate(rapm = o_rapm + d_rapm) %>% 
      # select(Player = playerName,
      #        RAPM = rapm,
      #        O-RAPM = o_rapm,
      #        D-RAPM = d_rapm) %>% 
      arrange(-rapm)
    
    if (i == 1) {
      rapm_avg <- RAPM
    }
    else {
      rapm_avg <- rbind(rapm_avg, RAPM)
    }
    
  }
  
  
  rapm <- aggregate(cbind(o_rapm, d_rapm, rapm) ~ player_id, data = rapm_avg, FUN = mean)
  
  database <- database %>%
    filter(Type == "Practice")
  
  stats <- aggregate(cbind(Poss, Points, Ast, TwoM, TwoA, ThreeM, ThreeA, Stl, Blk, Tov, Oreb, Dreb, Foul, DrawFoul, Deflects) ~ url + Player + Num, data = database, FUN = sum) %>%
    mutate(OBPM = 1*(Points/Poss*100 + 0.4*ThreeM/Poss*100 + 0.7*Ast/Poss*100 + 0.4*Oreb/Poss*100 + 1*DrawFoul/Poss*100 - 1*Tov/Poss*100 - 0.5*(TwoA+ThreeA)/Poss*100),
           DBPM = 1.2*(0.3*Dreb/Poss*100 + 1*Stl/Poss*100 + 1.0*Blk/Poss*100 + 0.15*Deflects/Poss*100 - 0.4*Foul/Poss*100))
  
  stats <- stats %>%
    mutate(OBPM = OBPM-mean(stats$OBPM),
           DBPM = DBPM-mean(stats$DBPM),
           BPM = OBPM+DBPM) %>%
    dplyr::select(url, Player, OBPM, DBPM, BPM)
  
  stats <- merge(stats, rapm, by.x="Player", by.y="player_id") 
  stats <- merge(stats, evanmiya, by="Player") %>%
    mutate(o_rapm = 0.34*o_rapm+0.33*OBPR+0.33*BROBPM,
           d_rapm = 0.5*d_rapm+0.5*DBPR+0.33*BRDBPM,
           rapm = o_rapm+d_rapm,
           BRAPM = BPM*0.5+rapm*0.5) %>%
    mutate(across(c(OBPM, DBPM, BPM, o_rapm, d_rapm, rapm, BRAPM), ~ round(., 1))) %>%
    select(url, Player, OBPM, DBPM, BPM, o_rapm, d_rapm, rapm, BRAPM) %>%
    arrange(desc(BRAPM))
  
  #colnames(stats) <- c("", "Player", "Offensive BPM", "Defensive BPM", "Total BPM", "Offensive RAPM", "Defensive RAPM", "Total RAPM", "Total Adjusted Impact")
  
  #team1 <- c("Ja'Kobi Gillespie", "Win Miller", "Kyler Vanderjagt", "Jayce Willingham", "Malik Dia")
  
  #team2 <- c("Keishawn Davidson", "Isaiah Walker", "Cade Tyson", "Sam Orme", "Brigham Rogers")
  
  team1points = 0
  team2points = 0
  
  team1score = 0
  team2score = 0
  
  for (i in team1) {
    BRAPM = stats$BRAPM[stats$Player == i]
    
    team1points = team1points + BRAPM
  }
  
  # print(team2)
  # print(stats)
  for (i in team2) {
    BRAPM = stats$BRAPM[stats$Player == i]
    
    team2points = team2points + BRAPM
  }
  
  #Target_Score = 25
  
  
  if (team1points > team2points) {
    team1score = round(Target_Score,0)
    team2score = round(Target_Score*(1-((team1points-team2points)/100)),1)
  }
  
  if (team2points > team1points) {
    team2score = round(Target_Score,0)
    team1score = round(Target_Score*(1-((team2points-team1points)/100)),1)
  }
  
  team1chance = round(team1score^16.1/(team1score^16.1+team2score^16.1)*100,1)^(Target_Score/100)
  team2chance = round(team2score^16.1/(team2score^16.1+team1score^16.1)*100,1)^(Target_Score/100)
  
  team1chances = round(team1chance/(team1chance+team2chance)*100,1)
  team2chances = round(team2chance/(team2chance+team1chance)*100,1)
  
  
  scoretable <- data.frame(Team = c("Team 1", "Team 2"), Lineup = c(paste(team1, collapse = " - "), paste(team2, collapse = " - ")), Score = c(team1score, team2score), Odds = c(team1chances, team2chances))
  
  return(scoretable)
}

create_shot_chart <- function(Player, Type, Contested, Assisted) {
  
  player = Player
  
  curve <- data.frame(x = c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),
                      y = c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3))
  
  right_curve <- curve %>% 
    filter(x >= 10 & x <= 22)
  
  left_curve <- curve %>% 
    filter(x <= -10 & x >= -22)
  
  mid_curve <- curve %>% 
    filter(x <= 10 & x >= -10)
  
  small_curve <- data.frame(x=c(-4000:(-1)/1000,1:4000/1000),
                            y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2)))
  
  # Define the box
  right_box <- data.frame(x = c(right_curve$x, rev(right_curve$x)),
                          y = c(right_curve$y, rep(34.9, length(right_curve$x))))
  
  left_box <- data.frame(x = c(left_curve$x, rev(left_curve$x)),
                         y = c(left_curve$y, rep(34.9, length(left_curve$x))))
  
  mid_box <- data.frame(x = c(mid_curve$x, rev(mid_curve$x)),
                        y = c(mid_curve$y, rep(34.9, length(mid_curve$x))))
  
  low_box <- data.frame(x = c(curve$x, rev(curve$x)),
                        y = c(curve$y, rep(3, length(curve$x))))
  
  box <- data.frame(x = c(curve$x, rev(curve$x)),
                    y = c(curve$y, rep(34.9, length(curve$x))))
  
  rim_box <- data.frame(x = c(small_curve$x, rev(small_curve$x)),
                        y = c(small_curve$y, rep(2, length(small_curve$x))))
  
  shots <- read.csv("Practice Shot Locations.csv") %>%
    mutate(W = X,
           X = Y*-1,
           Y = W+48.5)
  
  shots <- shots %>%
    mutate(Rim = point.in.polygon(shots$X, shots$Y, rim_box$x, rim_box$y+1.5),
           NonPaintRA = ifelse(X >= -6 & X <= 6 & Y >= 3 & Y <= 19 & Rim == 0, 1, 0),
           Midrange = ifelse(point.in.polygon(shots$X, shots$Y, low_box$x, low_box$y) & Rim == 0 & NonPaintRA == 0, 1, 0),
           RightCorner = ifelse(X >= -26.95 & X <= -22 & Y >= 3 & Y <= 14, 1, 0),
           RightWing = ifelse(point.in.polygon(shots$X, shots$Y, left_box$x, left_box$y) | (X >= -26.95 & X<= -22 & Y >= 14 & Y <= 34.9), 1, 0),
           TopOfKey = point.in.polygon(shots$X, shots$Y, mid_box$x, mid_box$y),
           LeftWing = ifelse(point.in.polygon(shots$X, shots$Y, right_box$x, right_box$y) | (X >= 26.95 & X<= 22 & Y >= 14 & Y <= 34.9), 1, 0),
           LeftCorner = ifelse(X <= 26.95 & X >= 22 & Y >= 3 & Y <= 14, 1, 0))
  
  colors <- c("red", "#FF685E", "#FF765A", "#FF845E", "orange", "yellow", "#CCE05E", "#B3E65E", "#9AEC5E", "#81F25E", "#6BF85E")
  
  RimAvg = 62.5
  NonPaintAvg = 41.2
  MidrangeAvg = 35.4
  LeftCornerAvg = 35.7
  RightCornerAvg = 36.2
  LeftWingAvg = 33.6
  RightWingAvg = 34.0
  TopOfKeyAvg = 33.1
  
  if (player != "Belmont Total") {
    shots <- shots %>%
      filter(Player == player)
  }
  
  if (Contested == "Contested") {
    shots <- shots %>%
      filter(Contest == "Contested")
  }
  else if (Contested == "Uncontested") {
    shots <- shots %>%
      filter(Contest == "Uncontested")
  }
  
  if (Assisted == "Assisted") {
    shots <- shots %>%
      filter(Ast == "Assisted")
  }
  else if (Assisted == "Unassisted") {
    shots <- shots %>%
      filter(Ast == "Unassisted")
  }
  
  RimM = sum(shots$Rim[which(shots$Rim == 1 & shots$Outcome == "Make")])
  RimA = sum(shots$Rim[which(shots$Rim == 1)])
  NonPaintRAM = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1 & shots$Outcome == "Make")])
  NonPaintRAA = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1)])
  MidrangeM = sum(shots$Midrange[which(shots$Midrange == 1 & shots$Outcome == "Make")])
  MidrangeA = sum(shots$Midrange[which(shots$Midrange == 1)])
  RightCornerM = sum(shots$RightCorner[which(shots$RightCorner == 1 & shots$Outcome == "Make")])
  RightCornerA = sum(shots$RightCorner[which(shots$RightCorner == 1)])
  TopOfKeyM = sum(shots$TopOfKey[which(shots$TopOfKey == 1 & shots$Outcome == "Make")])
  TopOfKeyA = sum(shots$TopOfKey[which(shots$TopOfKey == 1)])
  RightWingM = sum(shots$RightWing[which(shots$RightWing == 1 & shots$Outcome == "Make")])
  RightWingA = sum(shots$RightWing[which(shots$RightWing == 1)])
  LeftWingM = sum(shots$LeftWing[which(shots$LeftWing == 1 & shots$Outcome == "Make")])
  LeftWingA = sum(shots$LeftWing[which(shots$LeftWing == 1)])
  LeftCornerM = sum(shots$LeftCorner[which(shots$LeftCorner == 1 & shots$Outcome == "Make")])
  LeftCornerA = sum(shots$LeftCorner[which(shots$LeftCorner == 1)])
  
  theme_bryce <- function () {
    theme_minimal(base_size=12, base_family="Consolas") %+replace%
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
      )
  }
  
  two_m <- RimM+NonPaintRAM+MidrangeM
  two_a <- RimA+NonPaintRAA+MidrangeA
  
  three_m <- RightCornerM+TopOfKeyM+RightWingM+LeftWingM+LeftCornerM
  three_a <- RightCornerA+TopOfKeyA+RightWingA+LeftWingA+LeftCornerA
  
  if (Type == "Zone") {
    p <- ggplot(data=data.frame(x=1,y=1),aes(x,y))+
      #midrange
      geom_polygon(data = low_box, aes(x = x, y = y), fill = colors[min(max(round(((MidrangeM/MidrangeA*100-MidrangeAvg)/5)+6, 0),1),11)])+
      geom_path(data = curve, aes(x = x, y = y)) +
      #top of key
      geom_polygon(data = mid_box, aes(x = x, y = y), fill = colors[min(max(round(((TopOfKeyM/TopOfKeyA*100-TopOfKeyAvg)/2)+6, 0),1),11)])+
      #right corner
      geom_rect(xmin = -26.95, xmax = -22, ymin = 3, ymax = 14, fill = colors[min(max(round(((RightCornerM/RightCornerA*100-RightCornerAvg)/2)+6, 0),1),11)])+
      #left corner
      geom_rect(xmin = 26.95, xmax = 22, ymin = 3, ymax = 14, fill = colors[min(max(round(((LeftCornerM/LeftCornerA*100-LeftCornerAvg)/2)+6, 0),1),11)])+
      #Non Rim Paint
      geom_rect(xmin = -6, xmax = 6, ymin = 3, ymax = 19, fill = colors[min(max(round(((NonPaintRAM/NonPaintRAA*100-NonPaintAvg)/2)+6, 0),1),11)])+
      #restricted area
      geom_polygon(data = rim_box, aes(x = x, y = y+1.5), fill = colors[min(max(round(((RimM/RimA*100-RimAvg)/2)+6, 0),1),11)])+
      #right wing
      geom_polygon(data = left_box, aes(x = x, y = y), fill = colors[min(max(round(((RightWingM/RightWingA*100-RightWingAvg)/2)+6, 0),1),11)])+
      geom_rect(xmin = -26.9, xmax = -22, ymin = 14, ymax = 34.9, fill = colors[min(max(round(((RightWingM/RightWingA*100-RightWingAvg)/2)+6, 0),1),11)])+
      #left wing
      geom_polygon(data = right_box, aes(x = x, y = y), fill = colors[min(max(round(((LeftWingM/LeftWingA*100-LeftWingAvg)/2)+6, 0),1),11)])+
      geom_rect(xmin = 26.9, xmax = 22, ymin = 14, ymax = 34.9, fill = colors[min(max(round(((LeftWingM/LeftWingA*100-LeftWingAvg)/2)+6, 0),1),11)])+
      ###outside box:
      geom_path(data=data.frame(x=c(-27,-27,27,27,-27),y=c(3,35,35,3,3)))+
      ###three-point line:
      geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3)),aes(x=x,y=y))+
      ###fix aspect ratio to 1:1
      ##solid FT semicircle above FT line:
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      ###box inside the key:
      geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(3,19,19,3,3)))+
      ###restricted area semicircle:
      geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y+1.5))+
      ###rim:
      geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y+1.5))+
      ###backboard:
      geom_path(data=data.frame(x=c(-3,3),y=c(5.5,5.5)),lineend='butt')+
      geom_text(data = data.frame(x = 0, y = 32), aes(x = x, y = y, label = paste0(TopOfKeyM,"/", TopOfKeyA,"\n", ifelse(TopOfKeyA > 0, round(TopOfKeyM/TopOfKeyA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = 20, y = 28), aes(x = x, y = y, label = paste0(LeftWingM,"/", LeftWingA,"\n", ifelse(LeftWingA > 0, round(LeftWingM/LeftWingA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = -20, y = 28), aes(x = x, y = y, label = paste0(RightWingM,"/", RightWingA,"\n", ifelse(RightWingA > 0, round(RightWingM/RightWingA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = 24.5, y = 7), aes(x = x, y = y, label = paste0(LeftCornerM,"/", LeftCornerA,"\n", ifelse(LeftCornerA > 0, round(LeftCornerM/LeftCornerA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = -24.5, y = 7), aes(x = x, y = y, label = paste0(RightCornerM,"/", RightCornerA,"\n", ifelse(RightCornerA > 0, round(RightCornerM/RightCornerA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = 0, y = 8.7), aes(x = x, y = y, label = paste0(RimM,"/", RimA,"\n", ifelse(RimA > 0, round(RimM/RimA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = 0, y = 15), aes(x = x, y = y, label = paste0(NonPaintRAM,"/", NonPaintRAA,"\n", ifelse(NonPaintRAA > 0, round(NonPaintRAM/NonPaintRAA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      geom_text(data = data.frame(x = -12.5, y = 12.5), aes(x = x, y = y, label = paste0(MidrangeM,"/", MidrangeA,"\n", ifelse(MidrangeA > 0, round(MidrangeM/MidrangeA*100,1), 0), "%")),
                hjust = 0.5, vjust = 0.5, size = 6, fontface = "bold",
                color = "black", bg.color = "white", alpha = 0.8,
                box.padding = unit(0.5, "lines"), lineheight = 0.8,
                angle = 0)+
      theme_bryce()+
      theme(strip.text.x = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            plot.title.position = 'plot',
            plot.title = element_text(face =  'bold', size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            plot.margin = unit(c(.5, .5, 1, .5), "lines"),
            legend.position = "none",
            axis.ticks.x = element_blank()) +
      labs(title = paste0(player, " Shot Chart"),
           x = "",
           y = "",
           subtitle = paste0(two_m, "/", two_a, "(", round(two_m/two_a*100, 1), "%) from 2 | ", three_m, "/", three_a, "(", round(three_m/three_a*100, 1), "%) from 3")
      )+
      scale_x_continuous(breaks = NULL, name = NULL)+
      scale_y_continuous(breaks = NULL, name = NULL)+
      coord_fixed()
  }
  
  if (Type == "Dot") {
    p <- ggplot(data=data.frame(x=1,y=1),aes(x,y))+
      ###outside box:
      geom_path(data=data.frame(x=c(-27,-27,27,27,-27),y=c(3,35,35,3,3)))+
      ###three-point line:
      geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3)),aes(x=x,y=y))+
      ###fix aspect ratio to 1:1
      ##solid FT semicircle above FT line:
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      ###box inside the key:
      geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(3,19,19,3,3)))+
      ###restricted area semicircle:
      geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y+1.5))+
      ###rim:
      geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y+1.5))+
      ###backboard:
      geom_path(data=data.frame(x=c(-3,3),y=c(5.5,5.5)),lineend='butt')+
      theme_bryce()+
      geom_point(shots, mapping = aes(x=X, y=Y, color = Outcome), size = 3, stroke = 0.5) +
      scale_color_manual(aesthetics = "color", values = c("green3","red2"), labels=c("Made", "Missed")) +
      # scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks=c("Made", "Missed")) +
      # scale_fill_manual(values = c("green2","gray20"), aesthetics = "fill", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
      theme(strip.text.x = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            plot.title.position = 'plot',
            plot.title = element_text(face =  'bold', size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            plot.margin = unit(c(.5, .5, 1, .5), "lines"),
            legend.position = "none",
            axis.ticks.x = element_blank()) +
      labs(title = paste0(player, " Shot Chart"),
           x = "",
           y = "",
           subtitle = paste0(two_m, "/", two_a, "(", round(two_m/two_a*100, 1), "%) from 2 | ", three_m, "/", three_a, "(", round(three_m/three_a*100, 1), "%) from 3")
      )+
      scale_x_continuous(breaks = NULL, name = NULL)+
      scale_y_continuous(breaks = NULL, name = NULL)+
      coord_fixed()
  }
  
  if (Type == "Heatmap") {
    p <- ggplot(data=data.frame(x=1,y=1),aes(x,y))+
      theme_bryce()+  
      stat_density_2d(
        data = shots,
        aes(x = X, y = Y, fill = stat(density / max(density))),
        geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
      ) +
      scale_fill_viridis_c(
        "Shot Frequency",
        option = "inferno",
        guide = guide_colorbar(barwidth = 15)
      ) +
      ###outside box:
      geom_path(data=data.frame(x=c(-27,-27,27,27,-27),y=c(3,35,35,3,3)), color="white")+
      ###three-point line:
      geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3)),aes(x=x,y=y), color="white")+
      ###fix aspect ratio to 1:1
      ##solid FT semicircle above FT line:
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y), color="white")+
      ###box inside the key:
      geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(3,19,19,3,3)), color="white")+
      ###restricted area semicircle:
      geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y+1.5), color="white")+
      ###rim:
      geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y+1.5), color="white")+
      ###backboard:
      geom_path(data=data.frame(x=c(-3,3),y=c(5.5,5.5)),lineend='butt', color="white")+
      theme(strip.text.x = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            plot.title.position = 'plot',
            plot.title = element_text(face =  'bold', size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            plot.margin = unit(c(.5, .5, 1, .5), "lines"),
            legend.position = "none",
            axis.ticks.x = element_blank()) +
      labs(title = paste0(player, " Shot Chart"),
           x = "",
           y = "",           
           subtitle = paste0(two_m, "/", two_a, "(", round(two_m/two_a*100, 1), "%) from 2 | ", three_m, "/", three_a, "(", round(three_m/three_a*100, 1), "%) from 3",
                             "\nHeatmap Shows Frequency of Where Shots are Taken")
      )+
      scale_x_continuous(breaks = NULL, name = NULL)+
      scale_y_continuous(breaks = NULL, name = NULL)+
      coord_fixed()
  }
  
  return(p)
}

create_shot_table <- function(Player, Type, Contested, Assisted) {
  
  player = Player
  
  curve <- data.frame(x = c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),
                      y = c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3))
  
  right_curve <- curve %>%
    filter(x >= 10 & x <= 22)
  
  left_curve <- curve %>%
    filter(x <= -10 & x >= -22)
  
  mid_curve <- curve %>%
    filter(x <= 10 & x >= -10)
  
  small_curve <- data.frame(x=c(-4000:(-1)/1000,1:4000/1000),
                            y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2)))
  
  # Define the box
  right_box <- data.frame(x = c(right_curve$x, rev(right_curve$x)),
                          y = c(right_curve$y, rep(34.9, length(right_curve$x))))
  
  left_box <- data.frame(x = c(left_curve$x, rev(left_curve$x)),
                         y = c(left_curve$y, rep(34.9, length(left_curve$x))))
  
  mid_box <- data.frame(x = c(mid_curve$x, rev(mid_curve$x)),
                        y = c(mid_curve$y, rep(34.9, length(mid_curve$x))))
  
  low_box <- data.frame(x = c(curve$x, rev(curve$x)),
                        y = c(curve$y, rep(3, length(curve$x))))
  
  box <- data.frame(x = c(curve$x, rev(curve$x)),
                    y = c(curve$y, rep(34.9, length(curve$x))))
  
  rim_box <- data.frame(x = c(small_curve$x, rev(small_curve$x)),
                        y = c(small_curve$y, rep(2, length(small_curve$x))))
  
  shots <- read.csv("Practice Shot Locations.csv") %>%
    mutate(W = X,
           X = Y*-1,
           Y = W+48.5)
  
  shots <- shots %>%
    mutate(Rim = point.in.polygon(shots$X, shots$Y, rim_box$x, rim_box$y+1.5),
           NonPaintRA = ifelse(X >= -6 & X <= 6 & Y >= 3 & Y <= 19 & Rim == 0, 1, 0),
           Midrange = ifelse(point.in.polygon(shots$X, shots$Y, low_box$x, low_box$y) & Rim == 0 & NonPaintRA == 0, 1, 0),
           RightCorner = ifelse(X >= -26.95 & X <= -22 & Y >= 3 & Y <= 14, 1, 0),
           RightWing = ifelse(point.in.polygon(shots$X, shots$Y, left_box$x, left_box$y) | (X >= -26.95 & X<= -22 & Y >= 14 & Y <= 34.9), 1, 0),
           TopOfKey = point.in.polygon(shots$X, shots$Y, mid_box$x, mid_box$y),
           LeftWing = ifelse(point.in.polygon(shots$X, shots$Y, right_box$x, right_box$y) | (X >= 26.95 & X<= 22 & Y >= 14 & Y <= 34.9), 1, 0),
           LeftCorner = ifelse(X <= 26.95 & X >= 22 & Y >= 3 & Y <= 14, 1, 0))
  
  RimAvg = 62.5
  NonPaintAvg = 41.2
  MidrangeAvg = 35.4
  LeftCornerAvg = 35.7
  RightCornerAvg = 36.2
  LeftWingAvg = 33.6
  RightWingAvg = 34.0
  TopOfKeyAvg = 33.1
  
  if (player != "Belmont Total") {
    shots <- shots %>%
      filter(Player == player)
  }
  
  if (Contested == "Contested") {
    shots <- shots %>%
      filter(Contest == "Contested")
  }
  else if (Contested == "Uncontested") {
    shots <- shots %>%
      filter(Contest == "Uncontested")
  }
  
  if (Assisted == "Assisted") {
    shots <- shots %>%
      filter(Ast == "Assisted")
  }
  else if (Assisted == "Unassisted") {
    shots <- shots %>%
      filter(Ast == "Unassisted")
  }
  
  RimM = sum(shots$Rim[which(shots$Rim == 1 & shots$Outcome == "Make")])
  RimA = sum(shots$Rim[which(shots$Rim == 1)])
  NonPaintRAM = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1 & shots$Outcome == "Make")])
  NonPaintRAA = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1)])
  MidrangeM = sum(shots$Midrange[which(shots$Midrange == 1 & shots$Outcome == "Make")])
  MidrangeA = sum(shots$Midrange[which(shots$Midrange == 1)])
  RightCornerM = sum(shots$RightCorner[which(shots$RightCorner == 1 & shots$Outcome == "Make")])
  RightCornerA = sum(shots$RightCorner[which(shots$RightCorner == 1)])
  TopOfKeyM = sum(shots$TopOfKey[which(shots$TopOfKey == 1 & shots$Outcome == "Make")])
  TopOfKeyA = sum(shots$TopOfKey[which(shots$TopOfKey == 1)])
  RightWingM = sum(shots$RightWing[which(shots$RightWing == 1 & shots$Outcome == "Make")])
  RightWingA = sum(shots$RightWing[which(shots$RightWing == 1)])
  LeftWingM = sum(shots$LeftWing[which(shots$LeftWing == 1 & shots$Outcome == "Make")])
  LeftWingA = sum(shots$LeftWing[which(shots$LeftWing == 1)])
  LeftCornerM = sum(shots$LeftCorner[which(shots$LeftCorner == 1 & shots$Outcome == "Make")])
  LeftCornerA = sum(shots$LeftCorner[which(shots$LeftCorner == 1)])
  
  RimDist = round(mean(shots$Distance[which(shots$Rim == 1)]),1)
  NonPaintRADist = round(mean(shots$Distance[which(shots$NonPaintRA == 1)]),1)
  MidrangeDist = round(mean(shots$Distance[which(shots$Midrange == 1)]),1)
  RightCornerDist = round(mean(shots$Distance[which(shots$RightCorner == 1)]),1)
  TopOfKeyDist = round(mean(shots$Distance[which(shots$TopOfKey == 1)]),1)
  RightWingDist = round(mean(shots$Distance[which(shots$RightWing == 1)]),1)
  LeftWingDist = round(mean(shots$Distance[which(shots$LeftWing == 1)]),1)
  LeftCornerDist = round(mean(shots$Distance[which(shots$LeftCorner == 1)]),1)
  
  total_shots = RimA+NonPaintRAA+MidrangeA+RightCornerA+TopOfKeyA+RightWingA+LeftWingA+LeftCornerA
  
  shotsTable <- data.frame(Location = c("Rim", "Non-Rim Paint", "Midrange", "Top of Key", "Left Wing", "Right Wing", "Left Corner", "Right Corner"),
                           Makes = c(RimM, NonPaintRAM, MidrangeM, TopOfKeyM, LeftWingM, RightWingM, LeftCornerM, RightCornerM),
                           Attempts = c(RimA, NonPaintRAA, MidrangeA, TopOfKeyA, LeftWingA, RightWingA, LeftCornerA, RightCornerA),
                           Distance = c(RimDist, NonPaintRADist, MidrangeDist, TopOfKeyDist, LeftWingDist, RightWingDist, LeftCornerDist, RightCornerDist)) %>%
    mutate(Percentage = round(Makes/Attempts*100,1),
           PointsPerShot = ifelse(Location %in% c("Rim", "Non-Rim Paint", "Midrange"), round(Makes*2/Attempts,2), round(Makes*3/Attempts,2)),
           Frequency = round(Attempts/total_shots*100,1)
    ) %>%
    select(Location, Makes, Attempts, Percentage, PointsPerShot, Frequency, Distance)
  
  colnames(shotsTable) <- c("Location", "Makes", "Attempts", "Percentage", "Points Per Shot", "Frequency", "Avg. Distance")
  return(shotsTable)
}

create_shot_bar <- function(Player, Type, Contested, Assisted) {
  
  player = Player
  
  curve <- data.frame(x = c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),
                      y = c(3,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,3))
  
  right_curve <- curve %>%
    filter(x >= 10 & x <= 22)
  
  left_curve <- curve %>%
    filter(x <= -10 & x >= -22)
  
  mid_curve <- curve %>%
    filter(x <= 10 & x >= -10)
  
  small_curve <- data.frame(x=c(-4000:(-1)/1000,1:4000/1000),
                            y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2)))
  
  # Define the box
  right_box <- data.frame(x = c(right_curve$x, rev(right_curve$x)),
                          y = c(right_curve$y, rep(34.9, length(right_curve$x))))
  
  left_box <- data.frame(x = c(left_curve$x, rev(left_curve$x)),
                         y = c(left_curve$y, rep(34.9, length(left_curve$x))))
  
  mid_box <- data.frame(x = c(mid_curve$x, rev(mid_curve$x)),
                        y = c(mid_curve$y, rep(34.9, length(mid_curve$x))))
  
  low_box <- data.frame(x = c(curve$x, rev(curve$x)),
                        y = c(curve$y, rep(3, length(curve$x))))
  
  box <- data.frame(x = c(curve$x, rev(curve$x)),
                    y = c(curve$y, rep(34.9, length(curve$x))))
  
  rim_box <- data.frame(x = c(small_curve$x, rev(small_curve$x)),
                        y = c(small_curve$y, rep(2, length(small_curve$x))))
  
  shots <- read.csv("Practice Shot Locations.csv") %>%
    mutate(W = X,
           X = Y*-1,
           Y = W+48.5)
  
  shots <- shots %>%
    mutate(Rim = point.in.polygon(shots$X, shots$Y, rim_box$x, rim_box$y+1.5),
           NonPaintRA = ifelse(X >= -6 & X <= 6 & Y >= 3 & Y <= 19 & Rim == 0, 1, 0),
           Midrange = ifelse(point.in.polygon(shots$X, shots$Y, low_box$x, low_box$y) & Rim == 0 & NonPaintRA == 0, 1, 0),
           RightCorner = ifelse(X >= -26.95 & X <= -22 & Y >= 3 & Y <= 14, 1, 0),
           RightWing = ifelse(point.in.polygon(shots$X, shots$Y, left_box$x, left_box$y) | (X >= -26.95 & X<= -22 & Y >= 14 & Y <= 34.9), 1, 0),
           TopOfKey = point.in.polygon(shots$X, shots$Y, mid_box$x, mid_box$y),
           LeftWing = ifelse(point.in.polygon(shots$X, shots$Y, right_box$x, right_box$y) | (X >= 26.95 & X<= 22 & Y >= 14 & Y <= 34.9), 1, 0),
           LeftCorner = ifelse(X <= 26.95 & X >= 22 & Y >= 3 & Y <= 14, 1, 0))
  
  RimAvg = 62.5
  NonPaintAvg = 41.2
  MidrangeAvg = 35.4
  LeftCornerAvg = 35.7
  RightCornerAvg = 36.2
  LeftWingAvg = 33.6
  RightWingAvg = 34.0
  TopOfKeyAvg = 33.1
  
  if (player != "Belmont Total") {
    shots <- shots %>%
      filter(Player == player)
  }
  
  if (Contested == "Contested") {
    shots <- shots %>%
      filter(Contest == "Contested")
  }
  else if (Contested == "Uncontested") {
    shots <- shots %>%
      filter(Contest == "Uncontested")
  }
  
  if (Assisted == "Assisted") {
    shots <- shots %>%
      filter(Ast == "Assisted")
  }
  else if (Assisted == "Unassisted") {
    shots <- shots %>%
      filter(Ast == "Unassisted")
  }
  
  RimM = sum(shots$Rim[which(shots$Rim == 1 & shots$Outcome == "Make")])
  RimA = sum(shots$Rim[which(shots$Rim == 1)])
  NonPaintRAM = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1 & shots$Outcome == "Make")])
  NonPaintRAA = sum(shots$NonPaintRA[which(shots$NonPaintRA == 1)])
  MidrangeM = sum(shots$Midrange[which(shots$Midrange == 1 & shots$Outcome == "Make")])
  MidrangeA = sum(shots$Midrange[which(shots$Midrange == 1)])
  RightCornerM = sum(shots$RightCorner[which(shots$RightCorner == 1 & shots$Outcome == "Make")])
  RightCornerA = sum(shots$RightCorner[which(shots$RightCorner == 1)])
  TopOfKeyM = sum(shots$TopOfKey[which(shots$TopOfKey == 1 & shots$Outcome == "Make")])
  TopOfKeyA = sum(shots$TopOfKey[which(shots$TopOfKey == 1)])
  RightWingM = sum(shots$RightWing[which(shots$RightWing == 1 & shots$Outcome == "Make")])
  RightWingA = sum(shots$RightWing[which(shots$RightWing == 1)])
  LeftWingM = sum(shots$LeftWing[which(shots$LeftWing == 1 & shots$Outcome == "Make")])
  LeftWingA = sum(shots$LeftWing[which(shots$LeftWing == 1)])
  LeftCornerM = sum(shots$LeftCorner[which(shots$LeftCorner == 1 & shots$Outcome == "Make")])
  LeftCornerA = sum(shots$LeftCorner[which(shots$LeftCorner == 1)])
  
  RimDist = round(mean(shots$Distance[which(shots$Rim == 1)]),1)
  NonPaintRADist = round(mean(shots$Distance[which(shots$NonPaintRA == 1)]),1)
  MidrangeDist = round(mean(shots$Distance[which(shots$Midrange == 1)]),1)
  RightCornerDist = round(mean(shots$Distance[which(shots$RightCorner == 1)]),1)
  TopOfKeyDist = round(mean(shots$Distance[which(shots$TopOfKey == 1)]),1)
  RightWingDist = round(mean(shots$Distance[which(shots$RightWing == 1)]),1)
  LeftWingDist = round(mean(shots$Distance[which(shots$LeftWing == 1)]),1)
  LeftCornerDist = round(mean(shots$Distance[which(shots$LeftCorner == 1)]),1)
  
  shotsTable <- data.frame(Location = c("Rim", "Non-Rim Paint", "Midrange", "Top of Key", "Left Wing", "Right Wing", "Left Corner", "Right Corner"),
                           Makes = c(RimM, NonPaintRAM, MidrangeM, TopOfKeyM, LeftWingM, RightWingM, LeftCornerM, RightCornerM),
                           Attempts = c(RimA, NonPaintRAA, MidrangeA, TopOfKeyA, LeftWingA, RightWingA, LeftCornerA, RightCornerA),
                           Distance = c(RimDist, NonPaintRADist, MidrangeDist, TopOfKeyDist, LeftWingDist, RightWingDist, LeftCornerDist, RightCornerDist)) %>%
    mutate(Percentage = round(Makes/Attempts*100,1),
           PointsPerShot = ifelse(Location %in% c("Rim", "Non-Rim Paint", "Midrange"), round(Makes*2/Attempts,2), round(Makes*3/Attempts,2))
    ) %>%
    select(Location, Makes, Attempts, Percentage, PointsPerShot, Distance)
  
  # colnames(shotsTable) <- c("Location", "Makes", "Attempts", "Percentage", "Points Per Shot", "Avg. Distance")
  
  shotsTable$Location <- factor(shotsTable$Location, levels = c("Rim", "Non-Rim Paint", "Midrange", "Top of Key", "Left Wing", "Right Wing", "Left Corner", "Right Corner"))
  
  p <- ggplot(shotsTable, aes(x = Location, y = PointsPerShot, fill = PointsPerShot)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(Makes, "/", Attempts)), vjust = -0.3, size = 5) +
    scale_fill_gradient(low = "red", high = "green") + 
    labs(x = "Location", y = "Points Per Shot", title = "Points per Shot by Location", subtitle = "Dotted Line Represents NCAA Average") +
    theme_bryce() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
          plot.title = element_text(face =  'bold', size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 16),  # Change x-axis label font size
          axis.title.y = element_text(size = 16),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.text = element_text(size = 14, face = "bold")) +
    geom_hline(yintercept = 1.06, linetype = "dotted", color = "black", size = 3)
  
  return(p)
}

get_playlist_id <- function(player) {
  links <- links %>%
    filter(Player == player)
  
  return(links$ID[1])
}

get_view_counts <- function() {
  data <- read.csv("Practice Clip Links.csv") %>%
    mutate(Views = 0,
           PerVideo = 0)
  
  # Define your API key and the video ID
  api_key <- "AIzaSyBe0pfj6Q3EWilBQnqMdJ5-ZcjI3ynjJG0"
  
  for (i in 1:length(data$Player)) {
    playlist_id <- data$ID[i]
    # playlist_id <- "PL0dUi_n3TCkiUv_5y47gnydNY_bPnEjJj"
    
    # Create the API request URL
    api_url <- paste0("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=50&playlistId=", playlist_id, "&key=", api_key)
    # Make the API request
    response <- GET(api_url)
    
    # Parse the JSON response
    playlist_data <- content(response, "parsed")
    
    # Extract the view count
    # view_count <- video_data[["items"]][[1]][["statistics"]][["viewCount"]]
    
    get_video_view_count <- function(video_id, api_key) {
      api_url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=", video_id, "&key=", api_key)
      response <- GET(api_url)
      
      video_data <- content(response, "parsed")
      
      if (video_data[["pageInfo"]][["resultsPerPage"]] == 0) {
        return("Nope")
      }
      else {
        return(as.numeric(video_data[["items"]][[1]][["statistics"]][["viewCount"]]))
      }
      
    }
    
    video_ids <- sapply(playlist_data$items, function(item) item$snippet$resourceId$videoId)
    
    view_counts <- sapply(video_ids, function(video_id) get_video_view_count(video_id, api_key))
    
    view_count <- sum(as.numeric(view_counts[view_counts != "Nope"]))
    
    per_video <- round(view_count/length(view_counts[view_counts != "Nope"]),1)
    
    data$Views[i] <- view_count
    data$PerVideo[i] <- per_video
  }
  
  database <- aggregate(cbind(Points) ~ url + Player, data = database, FUN = sum)
  
  data <- merge(database, data, by="Player") %>%
    select(url, Link, Views, PerVideo) %>%
    arrange(desc(PerVideo))
  
  colnames(data) <- c("", "Player", "Total Views", "Views per Video")
  
  return(data)
}

get_play_sheet <- function() {
  
  colnames(plays) <- c("Play", "Drawing", "Clip", "Description")
  
  return(plays)
}

get_term_sheet <- function() {
  
  colnames(terms) <- c("Term", "Description")
  
  return(terms)
}

get_coach_score <- function() {
  PlusMinus <- data_frame(Coach = c("Luke Smith"), Points = 0, Opp_Points = 0, Diff = 0, Wins = 0, Losses = 0, WinPer = 0)
  
  names <- unique(c(Coach_Score$Coach))
  names <- na.omit(names)
  
  for (i in 1:length(names)) {
    if (names[i] != "") {
      IndividualPlusMinus <- Coach_Score %>%
        filter(Coach %in% names[i])
      
      player_row <- data.frame(Coach = names[i], 
                               Points = sum(IndividualPlusMinus$Score), 
                               Opp_Points = sum(IndividualPlusMinus$OppScore), 
                               Diff = sum(IndividualPlusMinus$Score)-sum(IndividualPlusMinus$OppScore),
                               Wins = sum(IndividualPlusMinus$Win), 
                               Losses = length(IndividualPlusMinus$Win)-sum(IndividualPlusMinus$Win),
                               WinPer = 0)
      
      PlusMinus <- rbind(PlusMinus, player_row)
    }
  }
  
  PlusMinus <- PlusMinus[2:nrow(PlusMinus),] %>%
    mutate(WinPer = round(Wins/(Wins+Losses)*100,1))
  
  PlusMinus <- merge(PlusMinus, Coach_Images, by = "Coach")
  
  PlusMinus <- PlusMinus %>%
    select(url, Coach, Points, Opp_Points, Diff, Wins, Losses, WinPer) %>%
    arrange(desc(WinPer)) %>%
    arrange(desc(Diff))
  
  colnames(PlusMinus) <- c("", "Coach", "Team Points", "Opponent Points", "+/-", "Wins", "Losses", "Win %")
  
  return(PlusMinus)
}

ui <- navbarPage(
  
  title = "Belmont Practice Stats",
  
  #Stylized title
  tags$head(
    tags$style(
      HTML(
        "table.dataTable tr td {
          vertical-align: middle !important;
          font-size: 22px;
          font-weight: bold;
        }
            .my-sidebar {
      width: 900px;
    }"
      )
    )
  ),
  
  tabPanel("Individual Stats",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Individual Practice Stats"
             ),
             # tags$p(
             #   style = "font-size: 20px; color: gray",
             #   "Some words about something"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),     
           # sidebarLayout(
           
           #        div(class = "my-sidebar",
           #            sidebarPanel(
           #              tags$head(
           #                tags$style(HTML("
           #           .my-sidebar {
           #       width: 0px;
           #     }
           #   
           #   .sidebar-header {
           #     font-size: 18px;
           #     font-weight: bold;
           #     margin-top: 10px;
           #     margin-bottom: 5px;
           #   }
           # "))
           #              ),
           #            )
           #        ),
           
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Overall Stats",
                                  # style = "justify-content: center;",
                                  fluidRow(
                                    column(width = 3, radioButtons("radioInput1", "Filter By:",
                                                                   choices = c("Week", "Day"),
                                                                   selected = "Week")),
                                    column(width = 3, selectInput("myWeek", "Select a week:", choices = c("All", database$Week), selected = "All")),
                                    column(width = 3, dateInput("myDate", "Select a date:", max(format(as.Date(database$Date[which(database$Type == "Practice")], format = "%m/%d/%y"), "%Y-%m-%d")))),
                                    column(width = 3, radioButtons("radioInput4", "Type:",
                                                                   choices = c("Totals", "Per 70 Possessions"),
                                                                   selected = "Totals"))),
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>***Click a player name to go to their practice clips***</b>")),
                                  DT::dataTableOutput("daily"),
                                  DT::dataTableOutput("daily_totals")),
                         tabPanel("Practice Log",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Scroll/Filter/Sort to see player stats from each practice</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View how a player has performed across the past few practices")),
                                  fluidRow(
                                    column(width = 2, selectInput("logPlayer", "Select Player Log:", choices = c(unique(database$Player)), selected = "Malik Dia")),
                                    column(width = 2, radioButtons("logradioInput1", "Filter By:",
                                                                   choices = c("Week", "Day"),
                                                                   selected = "Week")),
                                    column(width = 2, selectInput("logmyWeek", "Select a week:", choices = c("All", database$Week), selected = "All")),
                                    column(width = 2, dateInput("logmyDate", "Select a date:", max(format(as.Date(database$Date[which(database$Type == "Practice")], format = "%m/%d/%y"), "%Y-%m-%d")))),
                                    column(width = 2, radioButtons("logradioInput4", "Type:",
                                                                   choices = c("Totals", "Per 70 Possessions"),
                                                                   selected = "Totals"))),
                                  DT::dataTableOutput("log")),
                         #tabPanel("Leaderboard", plotOutput("leaders", height = "800px", width = "1000px")),
                         #tabPanel("Leaderboard", DT::dataTableOutput("pts_leaders", width = 2), DT::dataTableOutput("reb_leaders", width = 2)),
                         tabPanel("Leaderboard", 
                                  fluidRow(
                                    column(width = 3, radioButtons("LradioInput1", "Filter By:",
                                                                   choices = c("Week", "Day"),
                                                                   selected = "Week")),
                                    column(width = 3, selectInput("LmyWeek", "Select a week:", choices = c("All", database$Week), selected = "All")),
                                    column(width = 3, dateInput("LmyDate", "Select a date:", max(format(as.Date(database$Date[which(database$Type == "Practice")], format = "%m/%d/%y"), "%Y-%m-%d")))),
                                    column(width = 3, radioButtons("LradioInput4", "Type:",
                                                                   choices = c("Totals", "Per 70 Possessions"),
                                                                   selected = "Totals"))),
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Top 3 leaders in each stat category</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: Select the 'Day' button above to view leaders from the last practice")),
                                  fluidRow(
                                    column(width = 4, DT::dataTableOutput("pts_leaders")),
                                    column(width = 4, DT::dataTableOutput("reb_leaders")),
                                    column(width = 4, DT::dataTableOutput("ast_leaders")),
                                  ),
                                  # fluidRow(
                                  #   column(width = 4, DT::dataTableOutput("two_leaders", width = "33%")),
                                  #   column(width = 4, DT::dataTableOutput("three_leaders", width = "33%")),
                                  #   column(width = 4, DT::dataTableOutput("efg_leaders", width = "33%")),
                                  # ),
                                  fluidRow(style = "display: flex; flex-wrap: nowrap",
                                           div(style = "width: 33%",
                                               DT::dataTableOutput("two_leaders")),
                                           div(style = "width: 33%",
                                               DT::dataTableOutput("three_leaders")),
                                           div(style = "width: 33%",
                                               DT::dataTableOutput("efg_leaders"))
                                  ),
                                  fluidRow(
                                    column(width = 4, DT::dataTableOutput("block_leaders")),
                                    column(width = 4, DT::dataTableOutput("steal_leaders")),
                                    column(width = 4, DT::dataTableOutput("deflection_leaders")),
                                  ),
                                  fluidRow(
                                    column(width = 4, DT::dataTableOutput("tov_leaders")),
                                    column(width = 4, DT::dataTableOutput("foul_leaders")),
                                    column(width = 4, DT::dataTableOutput("drawfoul_leaders")),
                                  )
                         ),
                         tabPanel("Shot Chart", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Practice Shot Charts</b>")),
                                  # tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                  #          HTML("Example Use Case: View how Drew has performaned since coming back from injury")),
                                  fluidRow(
                                    column(width = 3, selectInput("shotChartPlayer", "Select Player:", choices = c("Belmont Total",unique(database$Player)), selected = "Belmont Total")),
                                    column(width = 3, radioButtons("shotChartType", "Type:",
                                                                   choices = c("Dot", "Zone", "Heatmap"),
                                                                   selected = "Zone")),
                                    column(width = 3, radioButtons("shotChartContested", "Contest Level:",
                                                                   choices = c("All", "Contested", "Uncontested"),
                                                                   selected = "All")),
                                    column(width = 3, radioButtons("shotChartAssisted", "Assisted:",
                                                                   choices = c("All", "Assisted", "Unassisted"),
                                                                   selected = "All")),
                                  ),
                                  plotOutput("shotChart", height = "1000px", width = "1000px"),
                                  DT::dataTableOutput("shotChartTable"),
                                  plotOutput("shotChartBar", height = "1000px", width = "1000px")
                         ),
                         tabPanel("Scatter Plot", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Select different values for x and y axis for scatter plots using practice stats</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: Select 'Deflections' for the x-axis and 'Stl' for the y-axis")),
                                  fluidRow(
                                    column(width = 4, radioButtons("SradioInput1", "Filter By:",
                                                                   choices = c("Week", "Day"),
                                                                   selected = "Week")),
                                    column(width = 4, selectInput("SmyWeek", "Select a week:", choices = c("All", database$Week), selected = "All")),
                                    column(width = 4, dateInput("SmyDate", "Select a date:", max(format(as.Date(database$Date[which(database$Type == "Practice")], format = "%m/%d/%y"), "%Y-%m-%d"))))),
                                  fluidRow(
                                    column(width = 4, selectInput("X", "Scatter X-Axis", choices = c("Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections"), selected = "Pts")),
                                    column(width = 4, selectInput("Y", "Scatter Y-Axis", choices = c("Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections"), selected = "Reb")),
                                    column(width = 4, radioButtons("radioInput3", "Scatter Type:",
                                                                   choices = c("Cumulative Total", "Cumulative Per 70 Possessions"),
                                                                   selected = "Cumulative Total")),
                                  ),
                                  plotlyOutput("scatter", height = "1000px", width = "1000px")),
                         tabPanel("Line Chart",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Select different players and y-axis values to view player practice performance over time</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View Aidan Braccia's 3pt% over time")),
                                  fluidRow(
                                    column(width = 4, selectInput("LineY", "Line Y-Axis", choices = c("Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%",
                                                                                                      "FGM", "FGA", "FG%", "eFG%", "Stl", "Blk", "Tov", "OReb", "DReb",
                                                                                                      "Foul", "Drawn", "Deflections"), selected = "3P%")),
                                    column(width = 4, pickerInput("selectedPlayers", "Line Graph Players:",
                                                                  choices = c(unique(database$Player)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  selected = c("Malik Dia","Cade Tyson", "Ja'Kobi Gillespie"))),
                                    column(width = 4, radioButtons("radioInput2", "Line Graph Type:",
                                                                   choices = c("Cumulative Total", "Cumulative Per 70 Possessions", "Per Practice"),
                                                                   selected = "Cumulative Total"))
                                  ),
                                  plotlyOutput("line", height = "1000px", width = "1600px")),
                         tabPanel("Individual Rankings", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Select different players to view where they rank in different stat categories</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View where a player ranks across different categories")),
                                  fluidRow(
                                    column(width = 8, selectInput("radarPlayer", "Select Player:", choices = c(unique(database$Player)), selected = "Carter Whitt")),
                                  ),
                                  plotOutput("radarChart", height = "1000px", width = "1000px")),
                         tabPanel("Timed Shooting & FT's", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Overall Timed Shooting and Free Throw numbers</b>")),
                                  DT::dataTableOutput("shooting"), DT::dataTableOutput("shooting_totals")),
             )
           )
           
           # )
           
  ),
  tabPanel("Lineups",
           id = "Lineups",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Player Lineups & Advanced Lineup Stats"
             ),
             # tags$p(
             #   style = "font-size: 15px; color: gray",
             #   "Projections based on practice stats and are meant for fun"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Individual +/-", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Player +/-, Win Percentage, and Net Rating</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("These numbers don't REALLY mean anything but they also don't not mean anything. 
                                                <br>Numbers change every day and will gain more meaning as samples get bigger.
                                                <br>I'd recommed being very careful when looking at these numbers, games played to small target scores have weird results")),
                                  DT::dataTableOutput("plusminus"),
                                  pickerInput("barType", "Type:",
                                              choices = c("Plus/Minus", "Net Rating", "Win Percentage"),
                                              selected = "Plus/Minus"),
                                  plotOutput("barChart", height = "800px", width = "1000px")
                         ),
                         tabPanel("Lineups",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Filter/Sort to view lineup performance</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View how Win and Brody teams have performed<br>
                                                This page is NOT for looking at full lineups, rather for getting a general sense of how certain looks have performed at practice<br>
                                                No matter how you splice the data samples will likely be too small to gain 100% useful insights")),
                                  fluidRow(
                                    column(width = 6, pickerInput("lineupPlayers", "Select Players to Include:",
                                                                  choices = c(unique(database$Player)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                    column(width = 6, pickerInput("notlineupPlayers", "Select Players to Exclude:",
                                                                  choices = c(unique(database$Player)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    ))),
                                  fluidRow(
                                    column(width = 2, pickerInput("PG", "Select PG:",
                                                                  choices = c(unique(lineups$PG)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                    column(width = 2, pickerInput("SG", "Select SG:",
                                                                  choices = c(unique(lineups$SG)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                    column(width = 2, pickerInput("SF", "Select SF:",
                                                                  choices = c(unique(lineups$SF)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                    column(width = 2, pickerInput("PF", "Select PF:",
                                                                  choices = c(unique(lineups$PF)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                    column(width = 2, pickerInput("C", "Select C:",
                                                                  choices = c(unique(lineups$C)),
                                                                  options = list(`actions-box` = TRUE),
                                                                  multiple = TRUE,
                                                                  #selected = c("")
                                    )),
                                  ),
                                  DT::dataTableOutput("lineup_totals"),
                                  DT::dataTableOutput("lineups")),
                         tabPanel("Advanced Player Data", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>These numbers are meant to give a look into overall performance but are far from perfect indicators of total impact</b>")),
                                  tags$div(style = "text-align: left; margin-top: 5px; font-size: 22px;", 
                                           HTML("RAPM: Regularized Adjusted Plus Minus, a projection of a players impact independent of teammates or opponents<br>
                                   BPM: Box Plus Minus, estiamte of a players quality and contribution based on the box score<br>
                                   Total Adjusted Impact: Estimated value of a player weighing BPM and RAPM")),
                                  DT::dataTableOutput("brapm"),
                                  tags$div(style = "text-align: left; margin-top: 5px; font-size: 22px;", 
                                           HTML("This is the most Artificial Intelligence, nerd heavy part of the site
                                                <br>Numbers are more meant to give a different insight rather than try to say anything definitive
                                                <br>RAPM and BPM are each weighted with bayesian priors that will have a lower impact as the practice data sample gets larger")),
                         ),
                         tabPanel("Lineup Combinations", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Shows team performance when players share the floor together</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View JP's team performance when he's paired with different players<br>
                                                Be careful when looking at this data, plenty goes on to determine what happens outside of 2-4 players<br>
                                                Page is meant to get a general overview of combos, not to gain 100% useful insights")),
                                  radioButtons("comboType", "Combo Type:",
                                               choices = c("Two-Man", "Three-Man", "Four-Man"),
                                               selected = "Two-Man"),
                                  DT::dataTableOutput("lineup_combos")),
                         tabPanel("Teammate Data", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Shows team performance when a player is teammates/opponents with certain players</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View who Carter has been teammates with<br>
                                                Be careful when looking at this data, plenty goes on to determine what happens outside of 2 players<br>
                                                Page is meant to get an ineteresting overview of who plays with who and how that plays out, not to gain groundbreaking insights")),
                                  pickerInput("teammatePlayer", "Player:",
                                              choices = c(unique(database$Player)),
                                              selected = "Aidan Braccia"),
                                  DT::dataTableOutput("teammate")),
                         tabPanel("Coach Tracker", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Shows team performance based on which coach is coaching the team</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("We're having fun this year")),
                                  DT::dataTableOutput("coach_tracker")),
                         tabPanel("Scrimmage Projection",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>This page is just for fun</b>")),
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 17px;", 
                                           HTML("<b>Make sure to select 5 players for each team</b>
                                                <br>Make sure you're patient with this page, it needs time to think")),
                                  fluidRow(
                                    column(width = 4, 
                                           pickerInput("teamOne", "Select Team 1:",
                                                       choices = c(sort(unique(database$Player))),
                                                       options = list(`actions-box` = TRUE),
                                                       multiple = TRUE,
                                                       selected = c("Brody Peebles", "Carter Whitt", "Jonathan Pierre", "Aidan Noyes", "Drew Scharnowski")
                                           )),
                                    column(width = 4, 
                                           pickerInput("teamTwo", "Select Team 2:",
                                                       choices = c(sort(unique(database$Player))),
                                                       options = list(`actions-box` = TRUE),
                                                       multiple = TRUE,
                                                       selected = c("Jake Dykstra", "Keith Robbins", "Jabez Jenkins", "Brigham Rogers", "Cooper Haynes")
                                           )),
                                    
                                    column(width = 4, sliderInput("Target", "Target Score:", min = 10, max = 100, value = 25, step = 1)),
                                  ),
                                  # pickerInput("teamOne", "Select Team 1:",
                                  #             choices = c(unique(database$Player)),
                                  #             options = list(`actions-box` = TRUE),
                                  #             multiple = TRUE,
                                  #             selected = c("Ja'Kobi Gillespie", "Win Miller", "Kyler Vanderjagt", "Jayce Willingham", "Malik Dia")
                                  # ),
                                  # pickerInput("teamTwo", "Select Team 2:",
                                  #             choices = c(unique(database$Player)),
                                  #             options = list(`actions-box` = TRUE),
                                  #             multiple = TRUE,
                                  #             selected = c("Keishawn Davidson", "Isaiah Walker", "Cade Tyson", "Sam Orme", "Brigham Rogers")
                                  # ),
                                  # sliderInput("Target", "Target Score:", min = 10, max = 100, value = 25, step = 1),
                                  DT::dataTableOutput("scrimmage"),
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 17px;", 
                                           HTML("Projections use the Regularized Adjusted Plus/Minus model and the Box Plus/Minus model from the 'Advanced Player Data' page to project score for each team<br>
                                                Pythagorean Win Expectancy along with an exponential adjustment for target score is used to predict % likelihood of a team winning"))),
             )
           )
           
           # )
           
  ),
  
  tabPanel("Team Stats",
           id = "Team Stats",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Team Practice Stats"
             ),
             # tags$p(
             #   style = "font-size: 20px; color: gray",
             #   "Some words about something"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),     
           # sidebarLayout(
           
           #        div(class = "my-sidebar",
           #            sidebarPanel(
           #              tags$head(
           #                tags$style(HTML("
           #   .sidebar-header {
           #     font-size: 18px;
           #     font-weight: bold;
           #     margin-top: 10px;
           #     margin-bottom: 5px;
           #   }
           # "))
           #              ),
           #            #   wellPanel(
           #            #     h4(class = "sidebar-header", "Advanced Stat Filters"),
           #            #     radioButtons("radioInputOffDef", "Side of the Ball:",
           #            #                  choices = c("Offense", "Defense"),
           #            #                  selected = "Offense"),
           #            #     radioButtons("radioInputFilter", "Show:",
           #            #                  choices = c("Only Stats", "Percentiles", "Rankings"),
           #            #                  selected = "Only Stats"),
           #            #     helpText("Percentiles and Rankings compare practice stats to 2022-23 college stats from every team"),
           #            # radioButtons("radioInputAdj", "Adjusted?:",
           #            #              choices = c("Unadjusted", "Adjusted"),
           #            #              selected = "Unadjusted"),
           #            # helpText("Adjusts for Belmont performance from last season. Adjusts for expectations against an average opponent."),
           #            #   ),
           #            )
           #        )
           #        ,
           
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Team Totals", 
                                  fluidRow(
                                    column(width = 3, radioButtons("TeamradioInput1", "Filter By:",
                                                                   choices = c("Week", "Day"),
                                                                   selected = "Week")),
                                    column(width = 3, selectInput("TeammyWeek", "Select a week:", choices = c("All", database$Week), selected = "All")),
                                    column(width = 3, dateInput("TeammyDate", "Select a date:", max(format(as.Date(database$Date[which(database$Type == "Practice")], format = "%m/%d/%y"), "%Y-%m-%d")))),
                                    column(width = 3, radioButtons("TeamradioInput4", "Type:",
                                                                   choices = c("Totals", "Per 70 Possessions"),
                                                                   selected = "Totals"))),
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Basic and Advanced total team stats</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: Select 'Day' to view team stats from last practice")),
                                  fluidRow(
                                    column(width = 6, DT::dataTableOutput("teamTotals")), 
                                    column(width = 6, DT::dataTableOutput("teamTotalsAdvanced"))
                                  )
                         ),
                         tabPanel("Basic Log", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Practice log of basic team stats</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View how our 3pt% compares across the past few practices")),
                                  radioButtons("TeamLogradioInput4", "Type:",
                                               choices = c("Totals", "Per 70 Possessions"),
                                               selected = "Totals"),
                                  DT::dataTableOutput("team")),
                         tabPanel("Advanced Log",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Practice log of advanced team stats</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View how our Points per Possession compares across the past few practices")),
                                  fluidRow(
                                    column(width = 4, radioButtons("radioInputOffDef", "Side of the Ball:",
                                                                   choices = c("Offense", "Defense"),
                                                                   selected = "Offense")),
                                    column(width = 4, radioButtons("radioInputFilter", "Show:",
                                                                   choices = c("Only Stats", "Percentiles", "Rankings"),
                                                                   selected = "Only Stats"),
                                           helpText("Percentiles and Rankings compare practice stats to 2022-23 college stats from every team")),
                                    column(width = 4, radioButtons("radioInputAdj", "Adjusted?:",
                                                                   choices = c("Unadjusted", "Adjusted"),
                                                                   selected = "Unadjusted"),
                                           helpText("Adjusts for Belmont performance from last season. Adjusts for expectations against an average opponent.")),
                                  ),
                                  DT::dataTableOutput("advancedTotal"),
                                  DT::dataTableOutput("advanced")),
                         tabPanel("Line Chart", 
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Select different y-axis values to view team performance over time in different stat categories</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Example Use Case: View how our Assist Rate (assists per field goal made) has changed over time")),
                                  fluidRow(
                                    column(width = 4, selectInput("LineYTeam", "Line Y-Axis", choices = c("Pts", "Reb", "Ast", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", 
                                                                                                          "PPP", "eFG%", "ORebRate", "TovRate", "FTr", "ThreeRate", "BlkRate", "AstRate", "AstTo", 
                                                                                                          "Stl", "Blk", "Tov", "OReb", "DReb", "Foul", "Drawn", "Deflections"), selected = "3P%")),
                                    column(width = 4, radioButtons("radioInput2Team", "Line Graph Type:",
                                                                   choices = c("Cumulative Total", "Cumulative Per 70 Possessions", "Per Practice"),
                                                                   selected = "Cumulative Total"))
                                  ),
                                  plotlyOutput("lineTeam", height = "1000px", width = "1600px")),
                         tabPanel("Scatter",                                    fluidRow(
                           column(width = 6, selectInput("TeamX", "Scatter X-Axis", choices = c("Points per Possession", "Effective FG%", "OReb Rate", "Tov Rate", "Free Throw Rate", "3PA Rate", "Block Rate",
                                                                                                "Assist Rate", "Ast to Tov", "Defensive PPP", "Defensive eFG%", "DReb Rate", "Defensive Tov Rate", "Defensive FTr",
                                                                                                "Defensive Three Rate", "Defensive Block Rate", "Defensive Assist Rate"), selected = "Points per Possession")),
                           column(width = 6, selectInput("TeamY", "Scatter Y-Axis", choices = c("Points per Possession", "Effective FG%", "OReb Rate", "Tov Rate", "Free Throw Rate", "3PA Rate", "Block Rate",
                                                                                                "Assist Rate", "Ast to Tov", "Defensive PPP", "Defensive eFG%", "DReb Rate", "Defensive Tov Rate", "Defensive FTr",
                                                                                                "Defensive Three Rate", "Defensive Block Rate", "Defensive Assist Rate"), selected = "Defensive PPP"))),
                           tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                    HTML("<b>Compares team practice stats to teams stats from last season</b>")),
                           tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                    HTML("Example Use Case: View how our Defensive Points per Possession and Defensive Effective FG% compare to other teams from last season")),
                           plotlyOutput("team_scatter", height = "1000px", width = "1000px")),
             )
           )
           
           # )
           
  ),
  tabPanel("Projections",
           id = "Projections",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Player Stat Projections"
             ),
             # tags$p(
             #   style = "font-size: 15px; color: gray",
             #   "Projections based on practice stats and are meant for fun"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),
           sidebarLayout(
             
             div(class = "my-sidebar",
                 sidebarPanel(
                   tags$head(
                     tags$style(HTML("
        .my-sidebar {
            width: 1200px;
          }
        
        .sidebar-header {
          font-size: 18px;
          font-weight: bold;
          margin-top: 10px;
          margin-bottom: 5px;
        }
      "))
                   ),
                   # wellPanel(
                   #   h4(class = "sidebar-header", "Arch Madness Simulation"),
                   #   actionButton("refreshButton", "Refresh Season Simulation", value = 0)
                   # ),
                   wellPanel(
                     h4(class = "sidebar-header", "Player Minutes"),
                     helpText("Minutes should add up to 200"),
                     sliderInput("PeeblesMin", "Brody Peebles Minutes:", min = 0, max = 40, value = 30, step = 1),
                     sliderInput("PierreMin", "Jonathan Pierre Minutes:", min = 0, max = 40, value = 30, step = 1),
                     sliderInput("WhittMin", "Carter Whitt Minutes:", min = 0, max = 40, value = 20, step = 1),
                     sliderInput("NoyesMin", "Aidan Noyes Minutes:", min = 0, max = 40, value = 30, step = 1),
                     sliderInput("WalkerMin", "Isaiah Walker Minutes:", min = 0, max = 40, value = 20, step = 1),
                     sliderInput("RogersMin", "Brigham Rogers Minutes:", min = 0, max = 40, value = 30, step = 1),
                     sliderInput("MillerMin", "Win Miller Minutes:", min = 0, max = 40, value = 20, step = 1),
                     sliderInput("OrmeMin", "Sam Orme Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("ScharnowskiMin", "Drew Scharnowski Minutes:", min = 0, max = 40, value = 20, step = 1),
                     sliderInput("DykstraMin", "Jake Dykstra Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("RobbinsMin", "Keith Robbins Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("BracciaMin", "Aidan Braccia Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("HaynesMin", "Cooper Haynes Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("JenkinsMin", "Jabez Jenkins Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("DillonMin", "Eoin Dillon Minutes:", min = 0, max = 40, value = 0, step = 1),
                     sliderInput("LundbladeMin", "Tyler Lundblade Minutes:", min = 0, max = 40, value = 0, step = 1)
                     
                   ),
                   
                   
                 )
             )
             ,
             
             mainPanel(
               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("Basic Stats", 
                                    tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                             HTML("<b>Projections are based on practice stats and are only meant for fun</b>
                                                   <br>Projections will be terrible early in the offseason but will get better as the season goes on")),
                                    DT::dataTableOutput("projections"),
                                    tags$div(style = "text-align: left; margin-bottom: 8px; font-size: 17px;", 
                                             HTML("Player minutes are roughly set to maximize team points scored. If a player has too many/few minutes, that can be changed with the sliders above<br>
                                                   <br>How the projections are made:<br>
                                                   1. The input with the most weight is Points per 70 Possessions during practice<br>
                                                   2. The points are adjusted for what they might look like against an 'average' opponent<br>
                                                   3. There is Bayesian modeling for players that were here last season using last seasons points as a prior<br>
                                                   4. There is a bit of Bayesian modeling for total team points as well<br>
                                                   5. Finally, there are exponential fatigue adjustments for minutes played<br><br>
                                                   If you aren't happy with a player's projected points, remember:<br>
                                                   1. These are just silly projections<br>
                                                   2. These are mostly based on silly practice stats<br>
                                                   3. Most players don't score nearly as many points as you think in college basketball<br>
                                                   4. Not everyone can score as much as they would like (the semi-optimized lineup is going to be at almost 75 points!)"))),
                           tabPanel("Team Schedule", 
                                    tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                             HTML("<b>Projections are based on practice stats and are only meant for fun</b>")),
                                    tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                             HTML("Projections and Records will change as player minutes are changed")),
                                    DT::dataTableOutput("team_proj"),
                                    tags$div(style = "text-align: left; margin-bottom: 8px; font-size: 17px;", 
                                             HTML("How the projections are made:<br>
                                                    1. Uses the projected points from the 'Basic Stats' page<br>
                                                    2. Roughly projects adjusted defensive efficiency using practice stats and the Bayesian prior of Belmont's adjusted defense the past 3 seasons<br>
                                                    3. Uses Barttorvik's 2024-25 projections as a very strong Bayesian prior for projections of other teams<br>
                                                    4. If you have projected adjusted offensive efficiency, adjusted defensive efficiency, and tempo data from last season, you can roughly predict the score of each game using some basic formulas<br>
                                                    5. Using Pythagorean Win Expectancy along with the predicted score of each game you can get the projected % chance to win each game<br>
                                                    6. Finally, adding up the projected % chance to win each game gets you your projected record
                                                   "))),
                           tabPanel("Season Simulator",
                                    wellPanel(
                                      h4(class = "sidebar-header", "Arch Madness Simulation"),
                                      actionButton("refreshButton", "Refresh Season Simulation", value = 0)
                                    ),
                                    tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                             HTML("<b>Projections are based on practice stats and are only meant for fun</b>")),
                                    tags$div(style = "text-align: center; margin-top: 5px; font-size: 17px;", 
                                             HTML("<b>This page simulates the entire regular season and Arch Madness</b>")),
                                    tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                             HTML("Belmont's odds to win conference games and Arch Madness will change as minutes are changed")),
                                    plotOutput("tourney_sim", height = "1400px", width = "1800px"), DT::dataTableOutput("mvc_proj"))
               )
             )
             
           )
           
  ),
  tabPanel("Play Sheet",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Individual Practice Stats"
             ),
             # tags$p(
             #   style = "font-size: 20px; color: gray",
             #   "Some words about something"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),     
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Play Sheet",
                                  # tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                  #          HTML("<b>***Click a player name to go to their practice clips***</b>")),
                                  DT::dataTableOutput("playsheet")),
                         tabPanel("Term Sheet",
                                  # tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                  #          HTML("<b>***Click a player name to go to their practice clips***</b>")),
                                  DT::dataTableOutput("termsheet")),
             )
           )
  ),
  tabPanel("Practice Clips",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             tags$h1(
               style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
               "Belmont Individual Practice Stats"
             ),
             # tags$p(
             #   style = "font-size: 20px; color: gray",
             #   "Some words about something"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),     
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Clips",
                                  # style = "justify-content: center;",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;", 
                                           HTML("<b>Select a Player to Watch Their Clips Below</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("Select the icon in the top right corner of the video player to select a different video")),
                                  selectInput("playerClips", "Select Player to Watch:", choices = c(unique(links$Player)), selected = "Win Miller"),
                                  uiOutput("youtube_playlist")
                         ),
                         tabPanel("Who's Being Watched",
                                  # style = "justify-content: center;",
                                  tags$div(style = "text-align: center; margin-top: 15px; font-size: 22px;",
                                           HTML("<b>Table Shows Who's Clips are Being Watched</b>")),
                                  tags$div(style = "text-align: center; margin-bottom: 8px; font-size: 17px;", 
                                           HTML("This page has zero meaning and was just made for the purposes of my curiosity<br>
                                                Page will take ~10 seconds to load")),
                                  DT::dataTableOutput("view_count")
                         )
             )
           )
  ),
  tabPanel("Feedback",
           id = "Feedback",
           tags$div(
             style = "display:flex; flex-direction:column; align-items:center; justify-content:center; height:150px",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d6/Belmont_Bruins_logo.svg/300px-Belmont_Bruins_logo.svg.png", width = 100),
             # tags$h1(
             #   style = "text-align: center; font-size: 36px; margin-top: 20px; margin-bottom: 20px; color: black",
             #   "Belmont Team Practice Stats"
             # ),
             # tags$p(
             #   style = "font-size: 20px; color: gray",
             #   "Some words about something"
             # ),
           ),
           tags$style(
             "
      table.dataTable tbody tr:hover {
        background-color: pink !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .text-class {
        z-index: 1000; /* add a higher z-index */
        position: relative; /* add a relative position */
      }
      "
           ),     
           mainPanel(
             # h4("Welcome to my app!"),
             
             p("If you're seeing this page, thank you for using the website! A lot of work went into it so it's been great to hear 
               people have been using and enjoying it.",br(),br(),
               "While people seem to enjoy the project, I definitely don't think it's perfect. For that reason, I'd love to get
               some anonymous feedback about what you like about the site and how it can be improved.",br(),br(),
               "Use the following link to complete an anonymous Google Form: ", 
               a(href="https://docs.google.com/forms/d/e/1FAIpQLSdbH6Wv5yANuz44G8cZGLLlIdrxXaNImm49JHOyVtfC7olKAA/viewform?usp=sf_link", "CLICK HERE TO GO TO THE FORM"),
               br(), br(), "Thank you for your feedback!", style = "font-size: 1.4em;"),
             
           )
           
           # )
           
  ),
  
  inverse = F,
  theme = shinythemes::shinytheme("readable")
  
)

server <- function(input, output, session) {
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
  
  observeEvent(
    input$refreshButton, {
      image <- get_mvc_projections(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                   Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                   Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin)
      
      output$tourney_sim <- renderPlot({
        image$second
      })
      output$mvc_proj = DT::renderDataTable({
        DT::datatable(image$first, 
                      escape = FALSE,
                      rownames = FALSE,
                      options = list(
                        pageLength = 16, # Set the number of rows per page based on input
                        lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                        searching = TRUE, # Enable searching
                        ordering = TRUE, # Enable sorting
                        bordered = TRUE, 
                        sanitize.text.function=identity,
                        dom = 'Bfrtip', # Add options for exporting the table
                        buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                        language = list(
                          search = 'Filter Table:',
                          paginate = list(
                            first = '&laquo;',
                            last = '&raquo;',
                            previous = '&lsaquo;',
                            more = '&rsaquo;'
                          )
                        )
                      ),
                      class = "display",
                      style = "bootstrap",
                      extensions = "Buttons")
      })
    }, ignoreNULL = FALSE
  )
  
  observeEvent(input$teamOne, {
    # Update choices in teamTwo based on selections in teamOne
    updateCheckboxGroupInput(session, "teamTwo",
                             choices = setdiff(unique(database$Player), input$teamOne),
                             selected = input$teamTwo)
  })
  
  observeEvent(input$teamTwo, {
    # Update choices in teamOne based on selections in teamTwo
    updateCheckboxGroupInput(session, "teamOne",
                             choices = setdiff(unique(database$Player), input$teamTwo),
                             selected = input$teamOne)
  })
  
  output$daily = DT::renderDataTable({
    data <- get_daily_stats(date = input$myDate, week = input$myWeek, choice = input$radioInput1, type = input$radioInput4)
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    # Calculate the color indices for each value in the "3P%" column
    colorIndicesThree <- cut(data$`3P%`, breaks = numColors, include.lowest = TRUE)
    
    # Map the color indices to the corresponding colors in the palette
    colorsThree <- colorPalette[colorIndicesThree]
    
    # Calculate the color indices for each value in the "3P%" column
    colorIndicesTwo <- cut(data$`2P%`, breaks = numColors, include.lowest = TRUE)
    
    # Map the color indices to the corresponding colors in the palette
    colorsTwo <- colorPalette[colorIndicesTwo]
    
    # Render the datatable with the formatted "3P%" column
    DT::datatable(data,
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 16,
                    lengthChange = FALSE,
                    searching = TRUE,
                    ordering = TRUE,
                    bordered = TRUE,
                    sanitize.text.function = identity,
                    dom = 'Bfrtip',
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons") %>%
      formatStyle(
        "3P%",
        backgroundColor = styleEqual(data$`3P%`, colorsThree)
      ) %>%
      formatStyle(
        "2P%",
        backgroundColor = styleEqual(data$`2P%`, colorsTwo)
      )
    
  })
  
  output$daily_totals = DT::renderDataTable({
    DT::datatable(get_daily_totals(date = input$myDate, week = input$myWeek, choice = input$radioInput1, type = input$radioInput4), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$shooting = DT::renderDataTable({
    data <- get_shooting()
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    # Calculate the color indices for each value in the "3P%" column
    colorIndicesFT <- cut(data$`Free Throw %`, breaks = numColors, include.lowest = TRUE)
    
    # Map the color indices to the corresponding colors in the palette
    colorsFT <- colorPalette[colorIndicesFT]
    
    # Calculate the color indices for each value in the "3P%" column
    colorIndicesTS <- cut(data$`Timed Shooting %`, breaks = numColors, include.lowest = TRUE)
    
    # Map the color indices to the corresponding colors in the palette
    colorsTS <- colorPalette[colorIndicesTS]
    
    # Render the datatable with the formatted "3P%" column
    DT::datatable(data,
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 16,
                    lengthChange = FALSE,
                    searching = TRUE,
                    ordering = TRUE,
                    bordered = TRUE,
                    sanitize.text.function = identity,
                    dom = 'Bfrtip',
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons") %>%
      formatStyle(
        "Free Throw %",
        backgroundColor = styleEqual(data$`Free Throw %`, colorsFT)
      ) %>%
      formatStyle(
        "Timed Shooting %",
        backgroundColor = styleEqual(data$`Timed Shooting %`, colorsTS)
      )
    
  })
  
  output$shooting_totals = DT::renderDataTable({
    DT::datatable(get_shooting_totals(), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$log = DT::renderDataTable({
    DT::datatable(get_log_stats(date = input$logmyDate, week = input$logmyWeek, choice = input$logradioInput1, player = input$logPlayer, type = input$logradioInput4), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 16, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons")
    
  })
  
  output$pts_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Points", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$reb_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Reb", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$ast_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Ast", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$two_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "TwoPer", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$three_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "ThreePer", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$efg_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "eFG", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$block_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Blk", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$steal_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Stl", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$deflection_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Deflects", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$tov_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Tov", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$foul_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "Foul", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$drawfoul_leaders = DT::renderDataTable({
    DT::datatable(get_leaders(Stat = "DrawFoul", type = input$LradioInput4, date = input$LmyDate, week = input$LmyWeek, choice = input$LradioInput1), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  )
    )
    
  })
  
  output$team = DT::renderDataTable({
    DT::datatable(get_team_stats(Type = input$TeamLogradioInput4), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons")
    
  })
  
  output$teamTotals = DT::renderDataTable({
    DT::datatable(get_team_overall(date = input$TeammyDate, week = input$TeammyWeek, choice = input$TeamradioInput1, type = input$TeamradioInput4), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$teamTotalsAdvanced = DT::renderDataTable({
    DT::datatable(get_team_overall_adv(date = input$TeammyDate, week = input$TeammyWeek, choice = input$TeamradioInput1, type = input$TeamradioInput4), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$advanced = DT::renderDataTable({
    DT::datatable(get_team_factors(Type = input$radioInputDate, Week = input$myWeekTeam, Day = input$myDateTeam, Show = input$radioInputFilter,
                                   Side = input$radioInputOffDef, Adj = input$radioInputAdj), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons")
    
  })
  
  output$advancedTotal = DT::renderDataTable({
    DT::datatable(get_team_factors_totals(Type = input$radioInputDate, Week = input$myWeekTeam, Day = input$myDateTeam, Show = input$radioInputFilter,
                                          Side = input$radioInputOffDef, Adj = input$radioInputAdj),  
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$scatter <- renderPlotly({
    create_scatter_plot(input$X, input$Y, date = input$SmyDate, week = input$SmyWeek, choice = input$SradioInput1, graphType = input$radioInput3)
  })
  
  output$barChart <- renderPlot({
    get_plus_minus_bar(Type = input$barType)
  })
  
  output$shotChart <- renderPlot({
    create_shot_chart(Type = input$shotChartType, Player = input$shotChartPlayer, Contested = input$shotChartContested, Assisted = input$shotChartAssisted)
  })
  
  output$shotChartBar <- renderPlot({
    create_shot_bar(Type = input$shotChartType, Player = input$shotChartPlayer, Contested = input$shotChartContested, Assisted = input$shotChartAssisted)
  })
  
  output$team_scatter <- renderPlotly({
    create_team_scatter(X = input$TeamX, Y = input$TeamY)
  })
  
  output$line <- renderPlotly({
    create_line(Y = input$LineY, players = input$selectedPlayers, graphType = input$radioInput2)
  })
  
  output$lineTeam <- renderPlotly({
    create_team_line(Y = input$LineYTeam, graphType = input$radioInput2Team)
  })
  
  output$leaders <- renderPlot({
    create_leaderboard(date = input$myDate, week = input$myWeek, choice = input$radioInput1)
  })
  
  output$radarChart <- renderPlot({
    create_radar_chart(selected_player = input$radarPlayer)
  })
  
  output$tourney_sim <- renderPlot({
    image <- get_mvc_projections(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                 Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                 Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin)
    image$second
  })
  
  output$mvc_proj = DT::renderDataTable({
    table <- get_mvc_projections(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                 Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                 Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin)
    DT::datatable(table$first, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 16, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons")
    
  })
  
  output$projections = DT::renderDataTable({
    DT::datatable(get_proj_stats(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                 Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                 Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin), 
                  escape = FALSE,
                  rownames = FALSE,
                  #caption = "Projections",
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons",
                  caption = tags$caption(
                    HTML(paste0("Total Minutes: ", sum(input$PeeblesMin, input$PierreMin, input$WhittMin, input$RogersMin, input$JenkinsMin,
                                                       input$WalkerMin, input$HaynesMin, input$OrmeMin, input$MillerMin, input$BracciaMin,
                                                       input$RobbinsMin, input$DykstraMin, input$NoyesMin, input$ScharnowskiMin, input$DillonMin),"<br>","Remaining Minutes: ",200-sum(input$PeeblesMin, input$PierreMin, input$WhittMin, input$RogersMin, input$JenkinsMin,
                                                                                                                                                                                       input$WalkerMin, input$HaynesMin, input$OrmeMin, input$MillerMin, input$BracciaMin,
                                                                                                                                                                                       input$RobbinsMin, input$DykstraMin, input$NoyesMin, input$ScharnowskiMin, input$DillonMin))),
                    style = "font-size: 20px; font-weight: bold; text-align: center; padding: 10px;"
                  )
    )
    
  })
  
  output$team_proj = DT::renderDataTable({
    DT::datatable(get_proj_team(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin), 
                  escape = FALSE,
                  rownames = FALSE,
                  #caption = "Projections",
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "display",
                  style = "bootstrap",
                  extensions = "Buttons",
                  caption = tags$caption(
                    HTML(paste0("Projected Record: ",get_wins(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                                              Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                                              Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin),
                                "<br>","Projected Conf. Record: ",get_conf(Brody = input$PeeblesMin, JP = input$PierreMin, Carter = input$WhittMin, Brigham = input$RogersMin, Bez = input$JenkinsMin,
                                                                           Isaiah = input$WalkerMin, Cooper = input$HaynesMin, Sam = input$OrmeMin, Win = input$MillerMin, Aidan = input$BracciaMin,
                                                                           Keith = input$RobbinsMin, Jake = input$DykstraMin, Noyes = input$NoyesMin, Drew = input$ScharnowskiMin, Eoin = input$DillonMin, Tyler = input$LundbladeMin))),
                    style = "font-size: 24px; font-weight: bold; text-align: center; padding: 10px; color: black;"
                  )
    )
    
  })
  
  output$shotChartTable = DT::renderDataTable({
    DT::datatable(create_shot_table(Type = input$shotChartType, Player = input$shotChartPlayer, Contested = input$shotChartContested, Assisted = input$shotChartAssisted), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$view_count = DT::renderDataTable({
    DT::datatable(get_view_counts(), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$brapm = DT::renderDataTable({
    data <- get_brapm()
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    # Calculate the color indices for each value in the "3P%" column
    colorIndicesBRAPM <- cut(data$`Total Adjusted Impact`, breaks = numColors, include.lowest = TRUE)
    
    # Map the color indices to the corresponding colors in the palette
    colorsBRAPM <- colorPalette[colorIndicesBRAPM]
    
    DT::datatable(data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    ) %>%
      formatStyle(
        "Total Adjusted Impact",
        backgroundColor = styleEqual(data$`Total Adjusted Impact`, colorsBRAPM)
      ) 
    
  })
  
  output$plusminus = DT::renderDataTable({
    data <- get_plus_minus()
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    colorIndicesPM <- cut(data$`+/-`, breaks = numColors, include.lowest = TRUE)
    colorsPM <- colorPalette[colorIndicesPM]
    
    colorIndicesNet <- cut(data$`Net Rtg.`, breaks = numColors, include.lowest = TRUE)
    colorsNet <- colorPalette[colorIndicesNet]
    
    colorIndicesWin <- cut(data$`Win %`, breaks = numColors, include.lowest = TRUE)
    colorsWin <- colorPalette[colorIndicesWin]
    
    DT::datatable(data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    ) %>%
      formatStyle(
        "+/-",
        backgroundColor = styleEqual(data$`+/-`, colorsPM)
      )  %>%
      formatStyle(
        "Net Rtg.",
        backgroundColor = styleEqual(data$`Net Rtg.`, colorsNet)
      )  %>%
      formatStyle(
        "Win %",
        backgroundColor = styleEqual(data$`Win %`, colorsWin)
      ) 
    
  })
  
  output$lineups = DT::renderDataTable({
    DT::datatable(get_lineups(Players = input$lineupPlayers, NotPlayers = input$notlineupPlayers, PG1 = input$PG,
                              SG1 = input$SG, SF1 = input$SF, PF1 = input$PF, C1 = input$C), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$lineup_totals = DT::renderDataTable({
    DT::datatable(get_lineup_totals(Players = input$lineupPlayers, NotPlayers = input$notlineupPlayers, PG1 = input$PG,
                                    SG1 = input$SG, SF1 = input$SF, PF1 = input$PF, C1 = input$C), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$lineup_combos = DT::renderDataTable({
    data <- get_lineup_combos(Type = input$comboType)
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    colorIndicesPM <- cut(data$`+/-`, breaks = numColors, include.lowest = TRUE)
    colorsPM <- colorPalette[colorIndicesPM]
    
    colorIndicesNet <- cut(data$`Net Rtg.`, breaks = numColors, include.lowest = TRUE)
    colorsNet <- colorPalette[colorIndicesNet]
    
    colorIndicesWin <- cut(data$`Win %`, breaks = numColors, include.lowest = TRUE)
    colorsWin <- colorPalette[colorIndicesWin]
    
    DT::datatable(data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    ) %>%
      formatStyle(
        "+/-",
        backgroundColor = styleEqual(data$`+/-`, colorsPM)
      )  %>%
      formatStyle(
        "Net Rtg.",
        backgroundColor = styleEqual(data$`Net Rtg.`, colorsNet)
      )  %>%
      formatStyle(
        "Win %",
        backgroundColor = styleEqual(data$`Win %`, colorsWin)
      ) 
    
  })
  
  output$teammate = DT::renderDataTable({
    data <- get_teammates(Player = input$teammatePlayer)
    
    # Define the number of colors in the gradient
    numColors <- 10
    
    # Create a color palette with a gradient from red to yellow to green
    colorPalette <- colorRampPalette(c("#FAA0A0", "yellow", "#009E60"))(numColors)
    
    colorIndicesWin <- cut(data$`Win % when Teammates`, breaks = numColors, include.lowest = TRUE)
    colorsWin <- colorPalette[colorIndicesWin]
    
    colorIndicesPD <- cut(data$`Point Diff when Teammates`, breaks = numColors, include.lowest = TRUE)
    colorsPD <- colorPalette[colorIndicesPD]
    
    colorIndicesOppWin <- cut(data$`Win % when Opponents`, breaks = numColors, include.lowest = TRUE)
    colorsOppWin <- colorPalette[colorIndicesOppWin]
    
    colorIndicesOppPD <- cut(data$`Point Diff when Opponents`, breaks = numColors, include.lowest = TRUE)
    colorsOppPD <- colorPalette[colorIndicesOppPD]
    
    DT::datatable(data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    ) %>%
      formatStyle(
        "Win % when Teammates",
        backgroundColor = styleEqual(data$`Win % when Teammates`, colorsWin)
      )  %>%
      formatStyle(
        "Point Diff when Teammates",
        backgroundColor = styleEqual(data$`Point Diff when Teammates`, colorsPD)
      ) %>%
      formatStyle(
        "Win % when Opponents",
        backgroundColor = styleEqual(data$`Win % when Opponents`, colorsOppWin)
      )  %>%
      formatStyle(
        "Point Diff when Opponents",
        backgroundColor = styleEqual(data$`Point Diff when Opponents`, colorsOppPD)
      ) 
    
  })
  
  output$scrimmage = DT::renderDataTable({
    DT::datatable(predict_scrimmage(team1 = input$teamOne, team2 = input$teamTwo, Target_Score = input$Target), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    pageLength = 160, # Set the number of rows per page based on input
                    lengthChange = FALSE, # Hide the dropdown menu to change number of rows
                    searching = TRUE, # Enable searching
                    ordering = TRUE, # Enable sorting
                    bordered = TRUE, 
                    sanitize.text.function=identity,
                    dom = 'Bfrtip', # Add options for exporting the table
                    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      search = 'Filter Table:',
                      paginate = list(
                        first = '&laquo;',
                        last = '&raquo;',
                        previous = '&lsaquo;',
                        more = '&rsaquo;'
                      )
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$youtube_playlist <- renderUI({
    selected_player <- input$playerClips
    playlist_id <- get_playlist_id(selected_player)
    
    iframe_url <- paste0("https://www.youtube.com/embed/videoseries?list=", playlist_id)
    
    tags$iframe(src = iframe_url, height = "600", width = "1000")
  })
  
  addResourcePath("images", ".")
  
  # Modify the data to include HTML for images
  play_data <- get_play_sheet()
  play_data$Drawing <- sprintf('<img src="images/%s" height="250">', play_data$Drawing)
  
  output$playsheet = DT::renderDataTable({
    DT::datatable(play_data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  term_data <- get_term_sheet()
  
  output$termsheet = DT::renderDataTable({
    DT::datatable(term_data, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  output$coach_tracker = DT::renderDataTable({
    DT::datatable(get_coach_score(), 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE,
                    language = list(
                      info = ""
                    )
                  ),
                  class = "compact",
                  style = "bootstrap",
                  extensions = "Buttons",
    )
    
  })
  
  
}

shinyApp(ui, server)


#man this shit is complicated good lord