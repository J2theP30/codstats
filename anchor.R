library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gganimate)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured/cwl-data/')
img <- readPNG('maps/ww2/valkyrie.png')
#make df to put output in
output <- data.frame()

location <- list.files(path = 'data', pattern = "structured")
for(j in 8:length(location)) {
  
  filenames <- list.files(path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type=='spawn'| events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(id=data_json$id,name = data_json$players$name, player.team = data_json$players$team, 
                                 gun = data_json$players$fave_weapon,map=data_json$map,
                                 player.opponent=ifelse(data_json$players$team==data_json$teams$name[1],data_json$teams$name[2],data_json$teams$name[1]))
      
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #start_time=45000
      start_time=5000
      
      # # Split by hp and set
      # if (data_json$mode=="Hardpoint"){
      #   rot <- rep(seq(1,3),4,each = 20*1000)
      #   time <- seq(start_time, start_time+length(rot)-1) 
      #   df <- data.frame(time,rot)
      #   data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      # }
      # Split by hp and set
      data<-merge(data,team_players,by.x='data.id',by.y='name')
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        dur <- (time-start_time)%%60000
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time, dur,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
        data1 = subset(data,data$dur < 10000)
        data2 = subset(data,data$dur > 50000)
        # only works for this hardpoint example with 4 hps
        # you can add others tho
        if(data_json$map == "Gibraltar") {
          #gibraltar
          hill1 = c(500,500)
          hill2 = c(750, 500)
          hill3 = c(300, 375)
          hill4 = c(550, 700)
        } else if (data_json$map == "Ardennes Forest") {
          # ardennes
          hill1 = c(525,220)
          hill2 = c(500,750)
          hill3 <- c(375,375)
          hill4 <- c(600, 500)
        } else if (data_json$map == "Sainte Marie du Mont") {
          # st marie
          hill1 = c(500,525)
          hill2 = c(350,125)
          hill3 <- c(650,800)
          hill4 <- c(250, 525)
        }
        
        data1$dist1 <- sqrt(((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2))
        data1$dist2 <- sqrt(((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2))
        data1$dist3 <- sqrt(((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2))
        data1$dist4 <- sqrt(((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2))
        data1$dist5 <- 0
        data1$closerto <- ifelse(data1$hp == 1 & data1$dist1 == 1, 1, 
                                 ifelse(data1$hp == 2 & data1$dist2 == 1, 1,
                                        ifelse(data1$hp == 3 & data1$dist3 == 1, 1,
                                               ifelse(data1$hp == 4 & data1$dist4 == 1, 1,0))))
        
        data1$dist2nexthp <- ifelse(data1$hp == 1 , data1$dist2, 
                                    ifelse(data1$hp == 2 , data1$dist3,
                                           ifelse(data1$hp == 3 , data1$dist4,
                                                  ifelse(data1$hp == 4 , data1$dist1, data1$dist5))))
        
        data1$sdist1 <- (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2) <= (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2)))*1
        data1$sdist2 <- (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2) <= (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2)))*1
        data1$sdist3 <- (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2) <= (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2)))*1
        data1$sdist4 <- (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2) <= (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2)))*1
        data1$sdist5 <- 0
        
        data1$ssdist1 <- (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2) <= (((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2)))*1
        data1$ssdist2 <- (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2) <= (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2)))*1
        data1$ssdist3 <- (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2) <= (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2)))*1
        data1$ssdist4 <- (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2) <= (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2)))*1
        data1$sdist5 <- 0
        
        data1$scloserto <- ifelse(data1$type=="death" | (data1$hp == 1 & data1$sdist1 == 1), 1, 
                                  ifelse(data1$type=="death" | (data1$hp == 2 & data1$sdist2 == 1), 1,
                                         ifelse(data1$type=="death" | (data1$hp == 3 & data1$sdist3 == 1), 1,
                                                ifelse(data1$type=="death" | (data1$hp == 4 & data1$sdist4 == 1), 1, 0))))
        
        
        data2$dist1 <- sqrt(((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2))
        data2$dist2 <- sqrt(((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2))
        data2$dist3 <- sqrt(((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2))
        data2$dist4 <- sqrt(((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2))
        data2$dist5 <- 0
        data2$closerto <- ifelse(data2$hp == 1 & data2$dist1 == 1, 1, 
                                 ifelse(data2$hp == 2 & data2$dist2 == 1, 1,
                                        ifelse(data2$hp == 3 & data2$dist3 == 1, 1,
                                               ifelse(data2$hp == 4 & data2$dist4 == 1, 1,0))))
        
        data2$dist2nexthp <- ifelse(data2$hp == 1 , data2$dist2, 
                                    ifelse(data2$hp == 2 , data2$dist3,
                                           ifelse(data2$hp == 3 , data2$dist4,
                                                  ifelse(data2$hp == 4 , data2$dist1, data2$dist5))))
        
        data2$sdist1 <- (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2) <= (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2)))*1
        data2$sdist2 <- (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2) <= (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2)))*1
        data2$sdist3 <- (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2) <= (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2)))*1
        data2$sdist4 <- (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2) <= (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2)))*1
        data2$sdist5 <- 0
        
        data2$ssdist1 <- (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2) <= (((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2)))*1
        data2$ssdist2 <- (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2) <= (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2)))*1
        data2$ssdist3 <- (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2) <= (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2)))*1
        data2$ssdist4 <- (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2) <= (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2)))*1
        data2$sdist5 <- 0
        
        data2$scloserto <- ifelse(data2$type=="death" | (data2$hp == 1 & data2$ssdist1 == 1), 1, 
                                  ifelse(data2$type=="death" | (data2$hp == 2 & data2$ssdist2 == 1), 1,
                                         ifelse(data2$type=="death" | (data2$hp == 3 & data2$ssdist3 == 1), 1,
                                                ifelse(data2$type=="death" | (data2$hp == 4 & data2$ssdist4 == 1), 1, 0))))}
      
      else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        dur <- (time-start_time)%%60000
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,dur,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
        data1 = subset(data,data$dur < 10000)
        data2 = subset(data,data$dur > 50000)
        if(data_json$map == "London Docks") {
          #london docks
          hill1 = c(500,550)
          hill2 = c(700,650)
          hill3 <- c(250,550)
          hill4 <- c(550, 350)
          hill5 <- c(425, 900)
        } else {
          # valk
          hill1 = c(525,475)
          hill2 = c(475,275)
          hill3 <- c(475,740)
          hill4 <- c(300, 425)
          hill5 <- c(750, 625)
        }
        data1$dist1 <- sqrt(((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2))
        data1$dist2 <- sqrt(((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2))
        data1$dist3 <- sqrt(((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2))
        data1$dist4 <- sqrt(((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2))
        data1$dist5 <- sqrt(((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2))
        data1$closerto <- ifelse(data1$hp == 1 & data1$dist1 == 1, 1, 
                                 ifelse(data1$hp == 2 & data1$dist2 == 1, 1,
                                        ifelse(data1$hp == 3 & data1$dist3 == 1, 1,
                                               ifelse(data1$hp == 4 & data1$dist4 == 1, 1,
                                                      ifelse(data1$hp == 5 & data1$dist5 == 1, 1, 0)))))
        
        data1$dist2nexthp <- ifelse(data1$hp == 1 , data1$dist2, 
                                    ifelse(data1$hp == 2 , data1$dist3,
                                           ifelse(data1$hp == 3 , data1$dist4,
                                                  ifelse(data1$hp == 4 , data1$dist5, data1$dist1))))
        
        data1$sdist1 <- (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2) <= (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2)))*1
        data1$sdist2 <- (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2) <= (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2)))*1
        data1$sdist3 <- (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2) <= (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2)))*1
        data1$sdist4 <- (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2) <= (((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2)))*1
        data1$sdist5 <- (((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2) <= (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2)))*1
        
        data1$ssdist1 <- (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2) <= (((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2)))*1
        data1$ssdist2 <- (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2) <= (((data1$data.pos.x-hill1[1])^2)+((data1$data.pos.y-hill1[2])^2)))*1
        data1$ssdist3 <- (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2) <= (((data1$data.pos.x-hill2[1])^2)+((data1$data.pos.y-hill2[2])^2)))*1
        data1$ssdist4 <- (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2) <= (((data1$data.pos.x-hill3[1])^2)+((data1$data.pos.y-hill3[2])^2)))*1
        data1$ssdist5 <- (((data1$data.pos.x-hill5[1])^2)+((data1$data.pos.y-hill5[2])^2) <= (((data1$data.pos.x-hill4[1])^2)+((data1$data.pos.y-hill4[2])^2)))*1
        
        data1$scloserto <- ifelse(data1$type=="death" | (data1$hp == 1 & data1$sdist1 == 1), 1, 
                                  ifelse(data1$type=="death" | (data1$hp == 2 & data1$sdist2 == 1), 1,
                                         ifelse(data1$type=="death" | (data1$hp == 3 & data1$sdist3 == 1), 1,
                                                ifelse(data1$type=="death" | (data1$hp == 4 & data1$sdist4 == 1), 1,
                                                       ifelse(data1$type=="death" | (data1$hp == 5 & data1$sdist5 == 1), 1, 0)))))
        
        data2$dist1 <- sqrt(((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2))
        data2$dist2 <- sqrt(((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2))
        data2$dist3 <- sqrt(((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2))
        data2$dist4 <- sqrt(((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2))
        data2$dist5 <- sqrt(((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2))
        data2$closerto <- ifelse(data2$hp == 1 & data2$dist1 == 1, 1, 
                                 ifelse(data2$hp == 2 & data2$dist2 == 1, 1,
                                        ifelse(data2$hp == 3 & data2$dist3 == 1, 1,
                                               ifelse(data2$hp == 4 & data2$dist4 == 1, 1,
                                                      ifelse(data2$hp == 5 & data2$dist5 == 1, 1, 0)))))
        
        data2$dist2nexthp <- ifelse(data2$hp == 1 , data2$dist2, 
                                    ifelse(data2$hp == 2 , data2$dist3,
                                           ifelse(data2$hp == 3 , data2$dist4,
                                                  ifelse(data2$hp == 4 , data2$dist5, data2$dist1))))
        
        data2$sdist1 <- (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2) <= (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2)))*1
        data2$sdist2 <- (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2) <= (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2)))*1
        data2$sdist3 <- (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2) <= (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2)))*1
        data2$sdist4 <- (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2) <= (((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2)))*1
        data2$sdist5 <- (((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2) <= (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2)))*1
        
        data2$ssdist1 <- (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2) <= (((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2)))*1
        data2$ssdist2 <- (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2) <= (((data2$data.pos.x-hill1[1])^2)+((data2$data.pos.y-hill1[2])^2)))*1
        data2$ssdist3 <- (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2) <= (((data2$data.pos.x-hill2[1])^2)+((data2$data.pos.y-hill2[2])^2)))*1
        data2$ssdist4 <- (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2) <= (((data2$data.pos.x-hill3[1])^2)+((data2$data.pos.y-hill3[2])^2)))*1
        data2$ssdist5 <- (((data2$data.pos.x-hill5[1])^2)+((data2$data.pos.y-hill5[2])^2) <= (((data2$data.pos.x-hill4[1])^2)+((data2$data.pos.y-hill4[2])^2)))*1
        
        
        data2$scloserto <-ifelse(data2$type=="death" | (data2$hp == 1 & data2$ssdist1 == 1), 1, 
                                 ifelse(data2$type=="death" | (data2$hp == 2 & data2$ssdist2 == 1), 1,
                                        ifelse(data2$type=="death" | (data2$hp == 3 & data2$ssdist3 == 1), 1,
                                               ifelse(data2$type=="death" | (data2$hp == 4 & data2$ssdist4 == 1), 1,
                                                      ifelse(data2$type=="death" | (data2$hp == 5 & data2$ssdist5 == 1), 1, 0)))))} 
      
      
      
      
      #  data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      #data<-merge(data,team_players,by.x='data.id',by.y='name')
      output<-rbind(data1,data2)
      output<-rbind(output,data1)
    }}}


## Make a new dataframe with each kill combined with each instance
# of teammate spawn in the next 5 seconds
anchorKills <- data.frame()
kills <- output %>%
  filter(type == 'death', dur > 40000)
spawn = output %>%
  filter(type == 'spawn', ifelse((set==1 & hp==1),dur > 50000,(dur > 50000 | dur< 10000)))
for(i in 1:nrow(kills)) {
  team = kills[i, "player.team"]
  game = kills[i, "id"]
  time = kills[i, 'time_ms']
  spawns = subset(spawn, spawn$id == game & spawn$player.team == team & 
                    spawn$time_ms %in% (time+1):(time+5000))
  if(nrow(spawns) > 0){
    anchorKills <- rbind(anchorKills, 
                         data.frame(killer = rep(kills[i,'data.id'], nrow(spawns)),
                                    dist = spawns$dist2nexthp,
                                    map = spawns$map, hp = spawns$hp))
  }}

spawn %>%
  group_by(player.team,map,hp) %>%
  summarise(spawnpct=mean(scloserto),rot=n()) %>%
  filter(rot>100) %>%
  arrange(desc(spawnpct))

spawn<-spawn %>%
  group_by(map,hp) %>%
  summarise(spawnpct=mean(),rot=n())
ggplot(spawn, aes(spawnpct, color = as.factor(hp)))+geom_density()


anchordist<-anchorKills %>%
  group_by(killer,map,hp) %>%
  summarise(kills=n(),dist=mean(dist)) %>%
  arrange((dist))

timehp<-output %>%
  group_by(data.id,map,hp) %>%
  summarise(rot=n())

anchor<-merge(anchordist,timehp,by.x=c("killer","map","hp"),by.y=c("data.id","map","hp"))
anchor%>%
  group_by(killer,map,hp) %>%
  summarise(kpr=kills/rot,rot=rot)%>%
  filter(rot>200)%>%
  arrange(desc(kpr))%>%
  data.frame








anchordist<-anchorKills %>%
  group_by(killer) %>%
  summarise(kills=n(),dist=mean(dist)) %>%
  arrange((dist))

timehp<-output %>%
  group_by(data.id) %>%
  summarise(rot=n())

anchor<-merge(anchordist,timehp,by.x=c("killer"),by.y=c("data.id"))
anchor%>%
  group_by(killer,map) %>%
  summarise(kpr=kills/rot,rot=rot)%>%
  filter(rot>750)%>%
  arrange(desc(kpr))%>%
  data.frame



#function to get last death
anchor_function1<-function(data,x){
  d1<-data[1:x,]
  d1$time_ms[max(which(d1$type=='death'))]
}
anchor_function2<-function(data,x){
  d1<-data[1:x,]
  c(as.character(d1$player.opponent[max(which(d1$type=='death'))]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-1]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-2]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-3]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-4]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-5]))
}
anchor_function3<-function(data,x){
  d1<-data[1:x,]
  c(as.character(d1$data.attacker.id[max(which(d1$type=='death'))]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-1]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-2]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-3]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-4]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-5]))
}


h=dim(img)[1]
w=dim(img)[2]

gib1 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 1 & output$closerto==0)
gib2 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 2 & output$closerto==0)
gib3 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 3 & output$closerto==0)
gib4 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 4 & output$closerto==0)
gib5 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 5 & output$closerto==0)
one<-ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.pos.x,y=data.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('purple','blue',"light blue"),name = "Frequency") +
  guides(alpha="none")

two<-ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('yellow','orange',"red"),name = "Frequency") +
  guides(alpha="none")





two<-ggplot(data=gib2) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
three<-ggplot(data=gib3) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
four<-ggplot(data=gib4) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
five<-ggplot(data=gib5) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
grid.arrange(one,two, ncol=2)




kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id) %>%
  #group_by(data.attacker.id) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id) %>%
  #group_by(data.id) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
#KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 300)%>%
  arrange(desc(KD))%>%
  head()


kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id, map.x) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id, map.x) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id", 'map.x'),by.y=c('data.id', 'map.x'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 150)%>%
  arrange(desc(KD))%>%
  head()

kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.x) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.y) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("player.team.x"),by.y=c('player.team.y'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 100)%>%
  arrange(desc(KD))%>%
  head()

# 
# cum <- output %>%
#   mutate(rot = ifelse(dur > 40000,1,0)) %>%
#   filter(rot == 1) %>%
#   group_by(map, hp, dur) %>% 
#   summarise(n = n()) %>%
#   ungroup() %>%
#   group_by(map, hp) %>%
#   mutate(cs = cumsum(n))
# ggplot(cum[cum$map == "London Docks",] , aes(dur, cs, color = as.factor(hp)))+geom_line(size = 2)+
#   scale_color_brewer(palette = 'Set1')