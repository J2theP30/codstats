library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured')
img <- readPNG('cwl-data/maps/ww2/ardennes_forest.png')
#make df to put output in
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'data', pattern = "structured")
for(j in 1:length(location)) {
  
  filenames <- list.files(path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & data_json$map == "Ardennes Forest" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      
      start_time=5000
      
      hp<-rep(seq(1,4),4,each=60*1000)
      time<-seq(start_time,start_time+length(hp)-1)
      set<-rep(seq(1,4),each=60*1000*4)
      df<-data.frame(time,hp,set)
      data=merge(data,df,by.x='time_ms',by.y='time')
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      output<-rbind(output,data)
    }}}