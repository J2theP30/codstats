library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gridExtra)
library(gganimate)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured')
img <- readPNG('cwl-data/maps/ww2/london_docks.png')
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
    if(data_json$mode == "Hardpoint" & data_json$map == "Gibraltar" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon,map=data_json$map)
      
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #start_time=45000
      start_time=5000
      
      # Split by hp and set
      if (data_json$mode=="Hardpoint"){
        rot <- rep(seq(1,3),4,each = 20*1000)
        time <- seq(start_time, start_time+length(rot)-1) 
        df <- data.frame(time,rot)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      }
      # Split by hp and set
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')}
      
      for(row in 1:nrow(data)){
        if(hp==1){
          xdist1=(data$data.attacker.pos.x-500)^2
          ydist1=(data$data.attacker.pos.y-500)^2
          dist1=sqrt(xdist1+ydist1)
          xdist2=(data$data.attacker.pos.x-750)^2
          ydist2=(data$data.attacker.pos.y-500)^2
          dist2=sqrt(xdist2+ydist2)
          dist<-ifelse(dist1>dist2,0,1)
          cbind(data,dist)
        }
        
        else if(hp==2){
          xdist2=(data$data.attacker.pos.x-750)^2
          ydist2=(data$data.attacker.pos.y-500)^2
          dist2=sqrt(xdist2+ydist2)
          xdist3=(data$data.attacker.pos.x-300)^2
          ydist3=(data$data.attacker.pos.y-375)^2
          dist3=sqrt(xdist3+ydist3)
          dist<-ifelse(dist2>dist3,0,1)
          cbind(data,dist)
        }
        
        else if(hp==3){
          xdist3=(data$data.attacker.pos.x-300)^2
          ydist3=(data$data.attacker.pos.y-375)^2
          dist3=sqrt(xdist3+ydist3)
          xdist4=(data$data.attacker.pos.x-550)^2
          ydist4=(data$data.attacker.pos.y-700)^2
          dist4=sqrt(xdist4+ydist4)
          dist<-ifelse(dist3>dist4,0,1)
          cbind(data,dist)
        }
        else if(hp==4){
          xdist4=(data$data.attacker.pos.x-550)^2
          ydist4=(data$data.attacker.pos.y-700)^2
          dist4=sqrt(xdist4+ydist4)
          xdist1=(data$data.attacker.pos.x-500)^2
          ydist1=(data$data.attacker.pos.y-500)^2
          dist1=sqrt(xdist1+ydist1)
          dist<-ifelse(dist4>dist1,0,1)
          cbind(data,dist)
        }
      }
      
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      output<-rbind(output,data)
    }}}

h=dim(img)[1]
w=dim(img)[2]


kills<-output %>%
  group_by(map,data.attacker.id,rot) %>%
  summarise(kills=n())
deaths<-output %>%
  group_by(map,data.id,rot) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id",'map','rot'),by.y=c('data.id','map','rot'))
KD$KD<-KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n>60,map=="London Docks",rot==3)%>%
  ungroup() %>%
  group_by(data.attacker.id,map,n) %>%
  summarise(m=KD)%>%
  arrange(desc(m))%>%
  head()

