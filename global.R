
displayBox <- function(value, text, color, id){
  if(is.na(value)){
    value <- NA
  }
  html <- fluidRow(
    div( 
        p(text, style = "font-size: 1em; font-weight: 500; color:white; top: 10%; position: relative; text-transform: uppercase;"),
        span(value, class = "odometer", id = id, style = "font-size: 2em; color:white; position: relative; margin-bottom: 0;"),
        style = paste0("text-align: center; width: 95%; height: auto; border-radius: 4px; background-color:",color,";display: inline-block; margin: 10px;")
      )
    )
  return(html)
}




outputDir <- "./data/"

# saveData <- function(data) {
#   data <- data
#   # Create a unique file name
#   fileName <-"test.csv"
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, path = outputDir)
# }

saveData <- function(invoer_data, player) {
  # Create a unique file name
  filePath <- paste0("./data/",player, ".csv", sep ="")
  # load the data for this player
  data <- read.csv(filePath, stringsAsFactors = FALSE)
  # Upload the file to Dropbox
  new_data <- bind_rows(data, invoer_data)
  write.csv(new_data, filePath, row.names = FALSE, quote = TRUE)
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(path="./data/")
  data <- do.call(rbind, lapply(files, function(x) read.csv(paste0("data/",x,sep=""), stringsAsFactors = FALSE)))
  # Concatenate all data together into one data.frame
  data$date <- as.Date(data$date, "%Y-%m-%d")
  data
}

colors <- c("#E50028",
            "#D70B28",
            "#CA1629",
            "#BC2129",
            "#AF2C2A",
            "#A1382B",
            "#94432B",
            "#864E2C",
            "#79592D",
            "#6B652D",
            "#5E702E",
            "#507B2F",
            "#43862F",
            "#359230",
            "#289D31",
            "#1AA831",
            "#0DB332",
            "#00BF33")

# initialize player files
# test <- data.frame(savedate = character(),
#                    name = character(),
#                    date = character(),
#                    daily = logical(),
#                    workout = logical(),
#                    conditioning = logical(),
#                    training = logical(),
#                    pod = logical(),
#                    throwing = logical(),
#                    other = logical(),
#                    note = character(),
#                    stringsAsFactors = FALSE)
# 
# playerNames <- c("Coco", "Dianne", "Hilco", "Iris", "Jessica",
#                  "Joji", "Joost", "Joram", "Justine", "Marieke",
#                  "Nena", "Rogier", "Sanne", "Tajji", "Tim","Timo")
# 
# for(player in playerNames){
#   write.csv(test, paste0("data/",player,".csv", sep = ""), row.names=FALSE)
# }


