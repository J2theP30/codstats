library(jsonlite)
library(dplyr)
library(ggplot2)
library(png)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured/cwl-data/')
img <- readPNG('maps/ww2/sainte_marie_du_mont.png')
#make df to put output in
output <- data.frame()

# For each event folder
location <- list.files(path = 'data', pattern = "structured")
for(j in 2:length(location)) {
  filenames <- list.files( path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  # for each game in each event folder
  for (i in 1:length(filenames)) {
    #read json
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # filter out by mode if desired
    if(data_json$mode == "Search & Destroy" & !is.null(nrow(data_json$events))){
      events <- (data_json$events)
      data <- subset(events, events$type == 'death' | events$type == "roundend" | events$type == "roundstart")
    
      team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                                round = rep(seq(1,data_json$rounds),2),
                                offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                                win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      
      # make df of players and teams to match 
      team_players <- data.frame(id=data_json$id,name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      

      # this breaks down list of lists into good data frame
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      
      #merges with player list to name each attacker
      # I DONT have it name the person who died, but could be added
      
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      data <- merge(data, team_players, by.x = 'data.id', by.y = 'name')
      
      data$p1<-ifelse(data$type=="death" & data$data.id==data_json$players$name[1],0,data_json$players$name[1])
      data$p2<-ifelse(data$type=="death" & data$data.id==data_json$players$name[2],0,data_json$players$name[2])
      data$p3<-ifelse(data$type=="death" & data$data.id==data_json$players$name[3],0,data_json$players$name[3])
      data$p4<-ifelse(data$type=="death" & data$data.id==data_json$players$name[4],0,data_json$players$name[4])
      data$p5<-ifelse(data$type=="death" & data$data.id==data_json$players$name[5],0,data_json$players$name[5])
      data$p6<-ifelse(data$type=="death" & data$data.id==data_json$players$name[6],0,data_json$players$name[6])
      data$p7<-ifelse(data$type=="death" & data$data.id==data_json$players$name[7],0,data_json$players$name[7])
      data$p8<-ifelse(data$type=="death" & data$data.id==data_json$players$name[8],0,data_json$players$name[8])
      
      # this binds any data
      # I usually filter out more stuff that I want before this part just to 
      # limit the complexity of the resulting output, but whatever
      output <- rbind(output, data)
    }}}
output<-output[order(output$time_ms),]
output<-output %>%
  filter(data.attacker.means_of_death!="suicide")%>%
  group_by(id.x,round) %>%
  mutate(p1 = ifelse(p1 == '0','0',ifelse(cummean((p1!='0')*1)<1,'0',as.character(p1)))) %>%
  mutate(p2 = ifelse(p2 == '0','0',ifelse(cummean((p2!='0')*1)<1,'0',as.character(p2)))) %>%
  mutate(p3 = ifelse(p3 == '0','0',ifelse(cummean((p3!='0')*1)<1,'0',as.character(p3)))) %>%
  mutate(p4 = ifelse(p4 == '0','0',ifelse(cummean((p4!='0')*1)<1,'0',as.character(p4)))) %>%
  mutate(p5 = ifelse(p5 == '0','0',ifelse(cummean((p5!='0')*1)<1,'0',as.character(p5)))) %>%
  mutate(p6 = ifelse(p6 == '0','0',ifelse(cummean((p6!='0')*1)<1,'0',as.character(p6)))) %>%
  mutate(p7 = ifelse(p7 == '0','0',ifelse(cummean((p7!='0')*1)<1,'0',as.character(p7)))) %>%
  mutate(p8 = ifelse(p8 == '0','0',ifelse(cummean((p8!='0')*1)<1,'0',as.character(p8)))) 

W<-'SCUMP'
T<-'OPTIC GAMING'

#with
kills<-output %>%
  filter(p1==W |p2==W |p3==W |p4==W |p5==W |p6==W |p7==W | p8==W )%>%
   group_by(data.attacker.id,player.team.x) %>%
   summarise(kills=n())
 deaths<-output %>%
   filter(p1==W |p2==W |p3==W |p4==W |p5==W |p6==W |p7==W | p8==W )%>%
   group_by(data.id,player.team.y) %>%
   summarise(deaths=n())
 KD<-merge(kills,deaths, by.x=c("data.attacker.id","player.team.x"),by.y=c('data.id','player.team.y'))
 KD$KD <- KD$kills/KD$deaths
 KD$n<-KD$kills+KD$deaths
 KD %>%
   filter(player.team.x==T)%>%
   filter(data.attacker.id!=W)%>%
   arrange(desc(KD))

#without
 kills<-output %>%
   filter(p1!=W & p2!=W & p3!=W & p4!=W & p5!=W & p6!=W & p7!=W & p8!=W )%>%
   group_by(data.attacker.id,player.team.x) %>%
   summarise(kills=n())
 deaths<-output %>%
   filter(p1!=W & p2!=W & p3!=W & p4!=W & p5!=W & p6!=W & p7!=W & p8!=W )%>%
   group_by(data.id,player.team.y) %>%
   summarise(deaths=n())
 KD<-merge(kills,deaths, by.x=c("data.attacker.id","player.team.x"),by.y=c('data.id','player.team.y'))
 KD$KD <- KD$kills/KD$deaths
 KD$n<-KD$kills+KD$deaths
 KD %>%
   filter(player.team.x==T)%>%
   filter(data.attacker.id!=W)%>%
   arrange(desc(KD))
