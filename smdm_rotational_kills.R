library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gganimate)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')
#make df to put output in
output <- data.frame()

location <- list.files(path = 'data', pattern = "structured")
for(j in 1:length(location)) {
  
  filenames <- list.files(path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & data_json$map == "Sainte Marie du Mont" & !is.null(nrow(data_json$events))){
      
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
      
      # # Split by hp and set
      # if (data_json$mode=="Hardpoint"){
      #   rot <- rep(seq(1,3),4,each = 20*1000)
      #   time <- seq(start_time, start_time+length(rot)-1) 
      #   df <- data.frame(time,rot)
      #   data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      # }
      # Split by hp and set
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        dur <- (time-start_time)%%60*1000
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time, dur,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
        # only works for this hardpoint example with 4 hps
        # you can add others tho
        
        data$dist1 <- sqrt(((data$data.attacker.pos.x-500)^2)+((data$data.attacker.pos.y-500)^2))
        data$dist2 <- sqrt(((data$data.attacker.pos.x-300)^2)+((data$data.attacker.pos.y-225)^2))
        data$dist3 <- sqrt(((data$data.attacker.pos.x-700)^2)+((data$data.attacker.pos.y-750)^2))
        data$dist4 <- sqrt(((data$data.attacker.pos.x-250)^2)+((data$data.attacker.pos.y-500)^2))
        data$closerto <- ifelse(data$hp == 1, ifelse(data$dist1 < data$dist2,1,2), 
                                ifelse(data$hp == 2, ifelse(data$dist2 < data$dist3, 2,3),
                                       ifelse(data$hp == 3, ifelse(data$dist3 < data$dist4,3,4),
                                              ifelse(data$dist4< data$dist1, 4,1))))
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        dur <- (time-start_time)%%60*1000
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,dur,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')}
      
      
      
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      output<-rbind(output,data)
    }}}

h=dim(img)[1]
w=dim(img)[2]

#gib1 <- subset(output, output$closerto != output$hp & output$dur >= 40)

ggplot(data=output) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none')+#,axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  geom_point(aes(data.attacker.pos.x,data.attacker.pos.y,color = closerto)) +
  geom_point(aes(x = 500, y = 500), shape = 17, size = 5, color = 'yellow') +
  geom_point(aes(x = 750, y = 500), shape = 17, size = 5, color = 'yellow') +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  # scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  # guides(alpha="none") +
  # ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "Kills on hp 1")


kills<-output %>%
  mutate(rot = ifelse(closerto != hp & dur > 40*1000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto != hp & dur > 40*1000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 15)%>%
  arrange(desc(KD))%>%
  head()