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
for(j in 7:length(location)) {
  
  filenames <- list.files(path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Capture The Flag" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, 
                                 gun = data_json$players$fave_weapon,map=data_json$map)
      
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      
      
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      data<-merge(data,team_players,by.x='data.id',by.y='name')
      output<-rbind(output,data)
    }}}

h=dim(img)[1]
w=dim(img)[2]

gib1 <- subset(output,output$map == "Sainte Marie du Mont" &  output$dur > 40000 & output$hp == 3 & output$closerto==0)
ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  # geom_point(aes(data.attacker.pos.x,data.attacker.pos.y,color="red")) +
  #geom_point(aes(data.pos.x,data.pos.y,color="blue"))
  
  #geom_point(aes(data.attacker.pos.x,data.attacker.pos.y,color = as.factor(closerto))) +
  #geom_point(aes(data.pos.x,data.pos.y,color = as.factor(closerto)))
  #scale_color_manual(values=c("#CC6666", "#9999CC")) +
  #geom_point(aes(x = hill3[1], y = hill3[2]), shape = 17, size = 5, color = 'yellow') +
  #geom_point(aes(x = hill2[1], y = hill2[2]), shape = 17, size = 5, color = 'yellow') +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  # scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
# guides(alpha="none") +
# ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))

stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")+
  ggtitle(label = "Last 20 seconds  Parking Lot - Sainte Marie du Mont")


kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id, map) %>%
  #group_by(data.attacker.id) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id, map) %>%
  #group_by(data.id) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id", 'map'),by.y=c('data.id', 'map'))
#KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 50)%>%
  arrange(desc(KD))%>%
  head()


kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.x, map.x) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.y, map.x) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("player.team.x", 'map.x'),by.y=c('player.team.y', 'map.x'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 100)%>%
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
