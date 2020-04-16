#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(ggplot2)
library(formattable)
library(FNN)
library(tidyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  explanations <- reactive({
    explanations <- read.csv("explanation.csv", stringsAsFactors = FALSE)
    return(explanations)
  })
  
  loadBadges <- reactive({
    cur_badges <- read.csv("badges.csv", stringsAsFactors = FALSE)
    return(cur_badges)
  })
  
  check_badges <- function(player, log = TRUE){
    # for each player calculate which badges they have, if they logged save the difference.
    cur_badges <- loadBadges()
    dat <- read.csv(paste("./data/", player, ".csv", sep = ""), stringsAsFactors = FALSE)
    
    playernames <- c("Coco", "Dianne", "Hilco", "Iris", "Jessica", 
                     "Joji", "Joost", "Joram", "Justine", "Marieke",
                     "Nena", "Rogier", "Sanne", "Tajji", "Tim", "Timo")
    
    gender <- cur_badges[cur_badges$name == player, "sex"]
    
    totals <- dat %>% group_by(name, date) %>%
      summarise(dailies = max(daily, na.rm=TRUE),
                workouts = sum(workout, na.rm = TRUE),
                conditioning = sum(conditioning, na.rm = TRUE),
                trainings = sum(training, na.rm = TRUE),
                pods = sum(pod, na.rm = TRUE),
                throwing = sum(throwing, na.rm = TRUE),
                other = sum(other, na.rm = TRUE)
                )
    
    totals <- totals %>% group_by(name) %>%
      summarise(
        dailies = sum(dailies),
        workouts = sum(workouts),
        conditioning = sum(conditioning),
        trainings = sum(trainings),
        pods = sum(pods),
        throwing = sum(throwing),
        other = sum(other)
      )
    
    total_dailies <- sum(dat$daily, na.rm = TRUE)
    
    
    # 23 badges, 23 badge conditions
    d25 <- totals$dailies >= 25
    d50 <- totals$dailies >= 50
    d75 <- totals$dailies >= 75
    d100 <- totals$dailies >= 100
    d125 <- totals$dailies >= 125
    heartlove <- totals$conditioning >= 10
    firstman <- totals$conditioning >= 20
    podfish <- totals$pods >= 10
    othersmother <- totals$other >= 35
    throwingninja <- totals$throwing >= 15
    strongsoul <- totals$workouts >= 15
    workoutmonkey <- totals$workouts >= 45
    zen <- sum(grepl("meditat.*|yoga.*", dat$note)) >= 15
    balanced <- all(totals[,-1] >= 10)
    workethic <- totals$workouts > 20 &
      totals$conditioning > 15 &
      totals$trainings > 12 &
      totals$pods > 10 &
      totals$throwing > 15
    
    nightowl <- sum((as.Date(dat$date) == as.Date(dat$savedate) & hour(dat$savedate) > 23) | 
                      (as.Date(dat$date) == (as.Date(dat$savedate) - 1) & hour(dat$savedate) < 3)) > 10 # marieke/DINO escape clause
    earlybird <- sum(as.Date(dat$date) == as.Date(dat$savedate) & hour(dat$savedate) > 4 & hour(dat$savedate) < 8) > 10 # and log on the same date!
    
    socialglue <- sum(grepl(paste(playernames, collapse = "|"), dat$note)) >= 10
    womenstrong <- gender == "f" & sum(dat$workout, na.rm = TRUE) > 25
    
    dailies_dat <- dat %>% 
      mutate(date = as.Date(date)) %>% 
      complete(date = seq.Date(min(date), Sys.Date(), by = "day")) %>%
      group_by(date) %>%
      summarise(dailies = max(daily))
    
    runs <- rle(dailies_dat$dailies >= 1)
    
    consistency <- any(runs$values == TRUE & runs$lengths >= 30)
    unstoppable <- any(runs$values == TRUE & runs$lengths >= 60)
    
    L7 <- Sys.Date() - 7
    
    this_week <- dailies_dat %>% filter(date >= L7 & date != Sys.Date())
    this_week <- sum(this_week$dailies, na.rm = TRUE)
    
    queen <- gender == "f" & this_week == 7
    king <- gender == "m" & this_week == 7
    
    # get players current badges
    cur_badges_player <- cur_badges[cur_badges$name == player,]
    updated_badges_player <- data.frame(timestamp = Sys.Date(),
                                        name = player,
                                        sex = gender,
                                        d25 = d25, d50 = d50, d75 = d75, d100 = d100, d125 = d125,
                                        balanced = balanced, consistency = consistency, earlybird = earlybird,
                                        nightowl = nightowl, firstmanout = firstman, heartlove = heartlove,
                                        othersmother = othersmother, podfish = podfish, queen = queen, king = king, 
                                        socialglue = socialglue, strongsoul = strongsoul, throwingninja = throwingninja,
                                        unstoppable = unstoppable, womenstrong = womenstrong, workethic = workethic,
                                        workoutmonkey = workoutmonkey, zen = zen, stringsAsFactors = FALSE
    )
    
    mutations <- updated_badges_player[,-c(1:3)] - cur_badges_player[,-c(1:3)]
    gotten <- names(mutations[,mutations == 1])
    lost <- names(mutations[,mutations == -1])
    
    if(log == TRUE){
      cur_badges[cur_badges$name == player,] <- updated_badges_player
      write.csv(cur_badges, "badges.csv", row.names=FALSE)
    }
    
    returned_data <- list(current = cur_badges_player, 
                          updated = updated_badges_player, 
                          gotten = gotten, 
                          lost = lost)
    
    return(returned_data)
  }
  

  # save new line to data
  observeEvent(input$submit,{
    name <- input$name
    date <- as.character(input$date)
    workouttypes <-  c("Daily", "Workout", "Conditioning", "Training", "Pod", "Throwing", "Other") %in% input$workoutType
    verklaring <- input$fill_in_field

    invoer_data <- data.frame(savedate = as.character(Sys.time()),
                              name = name,
                              date = date,
                              daily = workouttypes[1],
                              workout = workouttypes[2],
                              conditioning = workouttypes[3],
                              training = workouttypes[4],
                              pod = workouttypes[5],
                              throwing = workouttypes[6],
                              other = workouttypes[7],
                              note = verklaring,
                              stringsAsFactors = FALSE)
    saveData(invoer_data, name)
    
    returned_data <- check_badges(playername(), log = TRUE)
    
    explanations <- explanations()
    
    html_badges <- ""
    if(length(returned_data$gotten) > 0){
      html_gotten <- "<div><h3>Congrats! You've gained the following achievements!</h3>"
      for(entry in returned_data$gotten){
        link <- paste("./badges/", entry, ".png", sep = "")
        badge <- img(src = link, id = "badge")
        explanation <- explanations[which(explanations$badge == entry),"explanation"]
        html_new <- 
          div(class = "content",
              badge, 
              h3(explanation)
          )
        html_gotten <- paste(html_gotten, html_new)
      }
      html_badges <- paste(html_badges, html_gotten)
    }
    if(length(returned_data$lost) > 0){
      html_lost <- "<div><h3>Unfortunately you lost one of your badges! :(</h3>"
      for(entry in returned_data$lost){
        link <- paste("./badges/", entry, ".png", sep = "")
        badge <- img(src = link, id = "badge")
        explanation <- explanations()[which(explanations$badge == entry),"explanation"]
        html_new <- 
          div(class = "content",
              badge, 
              h3(explanation)
          )
        html_lost <- paste(html_lost, html_new)
      }
      html_badges <- paste(html_badges, html_lost)
    }
    
    vec <- c('<img src="arnold.jpg" alt="Arnold" style="max-width: 100%;">',
             '<img src="twotrophies.jpg" alt="Arnold" style="max-width: 100%;">',
             '<img src="ryan.jpg" alt="Ryan" style="max-width: 100%;">',
             '<img src="pipe.jpg" alt="Ron Burgundy" style="max-width: 100%;">',
             '<img src="anna.gif" alt="Maria Sharapova" style="max-width: 100%;">',
             '<img src="biles.gif" alt="Simone Biles" style="max-width: 100%;">',
             '<img src="blowitup.gif" alt="Schmidt" style="max-width: 100%;">',
             '<img src="fistbump.gif" alt="Overexcited fist bump" style="max-width: 100%;">',
             '<img src="giphy.gif" alt="Peeles pull-up" style="max-width: 100%;">',
             '<img src="roger.gif" alt="Roger Federer" style="max-width: 100%;">',
             '<img src="woowoo.gif" alt="Wooohooo" style="max-width: 100%;">',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="sanne.mp4" type="video/mp4" />
             </video>',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="tajji.mp4" type="video/mp4" /></video>',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="iris.mp4" type="video/mp4" /></video>',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="rogier.mp4" type="video/mp4" /></video>',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="coco.mp4" type="video/mp4" /></video>',
             '<video autoplay="autoplay" loop="loop" max-width= "100%" height="300">
             <source src="nena.mp4" type="video/mp4" /></video>')
    
    positive_reinforcement<- sample(vec, 1)
    
    textinmodal <- paste(html_badges, positive_reinforcement)
    
    showModal(modalDialog(
      title = "Thanks for logging your one percent!",
      HTML(textinmodal),
      easyClose = TRUE,
      footer = NULL
    ))

  })

  
  
# displaybox #1 - total dailies

  dailies_per_person <- reactive({
    input$submit
    data <- loadData()

    data <- data %>% group_by(name, date) %>%
      summarise(dailies = max(daily, na.rm = TRUE)) %>%
      group_by(name) %>% summarise(dailies = sum(dailies, na.rm = TRUE))

    data
   })
  
    observe({
      data <- dailies_per_person()
      total <- sum(data$dailies)
      
      html("total_dailies", total)
    })
    
    observe({
      now <- Sys.Date()
      ECBU <- mdy("may 6th 2019")
      days_left <- as.numeric(ECBU - now)
      
      html("days_until_ECBU", days_left)
    })
    
    observe({
      input$submit
      data <- loadData()
      
      data <- data %>% group_by(name, date) %>%
        summarise(dailies = max(daily, na.rm = TRUE)) %>%
        group_by(date) %>%
        summarise(dailies = sum(dailies, na.rm = TRUE))
      
      team_day_count <- sum(data$dailies == 16)
      
      html("team_days", team_day_count)
    })
    
    observe({
      input$submit
      data <- loadData()
      
      today <- Sys.Date()
      
      data <- data %>% group_by(name, date) %>%
        summarise(dailies = max(daily, na.rm = TRUE)) %>%
        group_by(date) %>%
        summarise(dailies = sum(dailies, na.rm = TRUE))
      
      today_count <- data %>% filter(date == today) %>% summarise(dailies = sum(dailies))
      yesterday_count <- data %>% filter(date == today-1) %>% summarise(dailies = sum(dailies))
      
      html("dailies_today", as.numeric(today_count))
      html("dailies_yesterday", as.numeric(yesterday_count))
    })
    
    observe({
      input$submit
      data <- loadData()
      total <- sum(data$workout) + sum(data$conditioning) + sum(data$training) + sum(data$pod) + sum(data$throwing) + sum(data$daily) + sum(data$other)
      
      
      
      html("total_events", total)
    })
    
    dailies_per_day <- reactive({
      input$submit
      data <- loadData()
      
      data <- data %>% group_by(name, date) %>%
        summarise(dailies = max(daily, na.rm = TRUE)) %>%
        group_by(date) %>% summarise(dailies = sum(dailies, na.rm = TRUE))
      
      data
    })

    observe({
      data <- dailies_per_day()
      avg <- round(mean(data$dailies),1)
      
      html("avg_day", avg)
    })
    
    observe({
      now <- Sys.Date()
      ECBU <- mdy("may 6th 2019")
      days_left <- as.numeric(ECBU - now)
      data <- dailies_per_day()
      
      if(length(unique(data$date)) > 1){
        data <- data %>% filter(data$date < now)
      } 
      avg <- round(mean(data$dailies),0)
      current_total <- sum(data$dailies)
      
      proj <- current_total + avg*days_left
        
      html("proj_total", proj)
    })
    
    
    
    rotate_labels <- function(angle) {
      theme(axis.text.x = element_text(angle = angle, hjust = 0))
    }
    
    output$overal_history <- renderPlot({
      input$submit
      data <- loadData()
      
      data <- data %>% select(date, name) %>% group_by(date) %>%
        summarise(unique_el = n_distinct(name))
        
      
      data$team_day <- ifelse(data$unique_el == 16, TRUE, FALSE)
      
      data %>% ggplot(aes(x = date, y = unique_el)) + 
        geom_bar(aes(fill = data$team_day), stat = "identity") + 
        geom_text(aes(y = unique_el, label = unique_el), vjust= 1.2) +
        labs(x = "Date",
             y = "Loggers") +
        guides(fill=FALSE) +
        theme(axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_line(color = "black"),
              panel.background = element_blank()
        ) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
        scale_fill_manual(values = c("gray", "#ffa500"), aesthetics = "fill")
    })
    
    output$dailies_history <- renderPlot({
      input$submit
      data <- dailies_per_day()
      
      data$team_day <- ifelse(data$dailies == 16, TRUE, FALSE)
      
      data %>% ggplot(aes(x = date, y = dailies)) + 
        geom_bar(aes(fill = data$team_day), stat = "identity") + 
        geom_text(aes(y = dailies, label = dailies), vjust= 1.2) +
        labs(x = "Date",
             y = "Dailies") +
        guides(fill=FALSE) +
        theme(axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_line(color = "black"),
              panel.background = element_blank()
              ) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
        scale_fill_manual(values = c("gray", "#ffa500"), aesthetics = "fill")
    })
    
  output$totals <- renderFormattable({
    input$submit
    data <- loadData()
    if(nrow(data) < 2){
      return(NULL)
    }

    if(input$total_or_last7 == "L7"){
      start_date <- Sys.Date() - 6
      data <- data %>% filter(date >= start_date)
    }

    dailies <- data %>% group_by(name, date) %>%
      summarise(dailies = max(daily, na.rm = TRUE)) %>%
      group_by(name) %>% summarise(dailies = sum(dailies, na.rm = TRUE))

    workouts <- data %>% group_by(name) %>%
      summarise(workouts = sum(workout),
                conditioning = sum(conditioning),
                training = sum(training),
                POD = sum(pod),
                throwing = sum(throwing),
                other = sum(other)
                )

    tabledat <- merge(dailies, workouts)

    tabledat$most_similar_index <- get.knn(tabledat[2:6],3)$nn.index[,2]
    tabledat$most_similar <- tabledat[tabledat$most_similar_index, "name"]

    tabledat <- tabledat %>% select(-most_similar_index)
    names(tabledat) <- c("Name", "Dailies", "Workouts", "Conditioning", "Training", "POD", "Throwing", "Other", "Most_similar")

      formattable(tabledat, align = c(rep("l", 8), "c"),
                  col.names = c("Name", "Dailies", "Workouts", "Conditioning", "Training", "POD", "Throwing", "Other", "Most similar player"),
                  list(
        area(col = c(Dailies, Workouts, Conditioning, Training, POD, Throwing, Other)) ~ normalize_bar("pink", 0.2)

      ))



  })

  output$last_logs <- renderFormattable({
    input$submit
    data <- loadData()

    filter_date <- input$which_day

    data <- data %>% filter(date == filter_date)
    
    data <- data %>% group_by(name) %>% summarise(
      daily = max(daily, na.rm = TRUE),
      workout = sum(workout, na.rm = TRUE),
      conditioning = sum(conditioning, na.rm = TRUE),
      training = sum(training, na.rm = TRUE),
      pod = sum(pod, na.rm = TRUE),
      throwing = sum(throwing, na.rm = TRUE),
      other = sum(other, na.rm = TRUE),
      note = paste(ifelse(note == "NA" | is.na(note), "", note), ifelse(note == "" | is.na(note)| note == "NA", "" , "<br>"), collapse='')
    )

    color_fm <- formatter("span",
                          style = x ~ style(color = ifelse(x, "green", "red")),
                          x ~ icontext(ifelse(x, "ok", "remove")),
                          x ~ icontext(ifelse(x > 1, "ok",NA)),
                          x ~ icontext(ifelse(x > 2, "ok",NA)))
    



    tabledat <- data %>%
      select(name, daily, workout, conditioning, training, pod, throwing, other, note)

    names(tabledat) <- c("Name", "Daily", "Workout", "Cond", "Train", "POD", "Throw", "Other", "Note")

    tabledat$Note <- ifelse(is.na(tabledat$Note),"", tabledat$Note)
    
      formattable(tabledat,
                  align = "l",
                  list(
        Daily = color_fm, Workout = color_fm, Cond = color_fm,
        Train = color_fm, POD = color_fm, Throw = color_fm, Other = color_fm


    ))
      
  })
     
playername <- reactive({
  if(is.null(input$name) | input$name == "Select_name"){
    playernames <- c("Coco", "Dianne", "Hilco", "Iris", "Jessica", 
                     "Joji", "Joost", "Joram", "Justine", "Marieke", 
                     "Nena", "Rogier", "Sanne", "Tajji", "Tim", "Timo")
    playername <- sample(playernames, 1)
  } else {
    playername <- input$name
  }
  return(playername)
})


# disclaimer
  output$disclaimer <- renderUI({ 
    input$submit
    
    current_badges <- check_badges(playername(), log = FALSE)$current
    
    html_badges <- ""
    for(col in names(current_badges[,-c(1:3)])){
      if(current_badges[,col] == TRUE){
        link <- paste("./badges/", col, ".png", sep = "")
        html_badges <- paste(html_badges, img(src = link, id = "badge"))
      }
    }
    
    div(class = "panel panel-default",
        div(class = "panel-heading", tags$b("Player spotlight"),
            style = "color: #fff; background-color: #E95420;"),
        div(class = "panel-body",
            fluidRow(
              column(3,
              img(src = paste("./Personal/", playername(),".jpg", sep = ""), style = "max-width: 100%;")
              ),
              column(4,
                     h4(playername(), style = "font-weight: 700; font-size: 24px;"),
                     tableOutput("personal")
              ),
              column(5,
                     h4("Badges", style = "font-weight: 700; font-size: 24px;"),
                     div(
                       HTML(html_badges)
                     )
                     )
            )
        )
)
  })
  
  
  output$personal <- renderTable({
    input$submit
    data <- loadData()
    
    L7 <- Sys.Date() - 6
    
    tabledat_L7 <- data %>% filter(date >= L7 & name == playername()) %>% 
      summarise(dailies = sum(daily),
                workouts = sum(workout),
                conditioning = sum(conditioning),
                training = sum(training),
                POD = sum(pod),
                throwing = sum(throwing),
                other = sum(other))
    
    data %>% group_by(name, date) %>%
      summarise(dailies = max(daily, na.rm = TRUE)) %>%
      group_by(name) %>% summarise(dailies = sum(dailies, na.rm = TRUE))
    
    tabledat_OV <- data %>% filter(name == playername()) %>%
      group_by(date) %>% 
      summarise(daily = max(daily, na.rm = TRUE),
                workouts = sum(workout),
                conditioning = sum(conditioning),
                training = sum(training),
                pod = sum(pod),
                throwing = sum(throwing),
                other = sum(other))
    
    tabledat_OV <- tabledat_OV %>% 
      summarise(dailies = sum(daily),
                workouts = sum(workouts),
                conditioning = sum(conditioning),
                training = sum(training),
                POD = sum(pod),
                throwing = sum(throwing),
                other = sum(other))
    
    tabledat <- rbind(tabledat_OV, tabledat_L7)
    
    tabledat <- as.data.frame(t(tabledat))
    
    names(tabledat) <- c("Overall", "Last 7 days")
    
    tabledat
  }, rownames = TRUE, bordered = TRUE, hover = TRUE
  )




  # data backlog
  output$raw_csv <- renderFormattable({
    input$submit
    data <- loadData()

    tabledat <- data %>% filter(name == input$personal.name & (date >= input$personal.range[1] & date <= input$personal.range[2])) 
      
    
    
    color_fm <- formatter("span",
                          style = x ~ style(color = ifelse(x, "green", "red")),
                          x ~ icontext(ifelse(x, "ok", "remove")),
                          x ~ icontext(ifelse(x > 1, "ok",NA)),
                          x ~ icontext(ifelse(x > 2, "ok",NA)))
    
    tabledat$note <- ifelse(is.na(tabledat$note),"", tabledat$note)
    formattable(tabledat,
                align = "l",
                list(
                  daily = color_fm, workout = color_fm, conditioning = color_fm,
                  training = color_fm, pod = color_fm, throwing = color_fm, other = color_fm
                  
                  
                ))

    })
  

  
  
  
  
  
  
 
  
})
