simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

#library(tidyverse)
#library(gganimate)
#library(magick)
#library(ggthemes)
#library(extrafont)
#library("extrafontdb")
#library(ggrepel)
# add Simpsons font extrafont::font_import("C:/Users/reynoldsk/AppData/Local/Microsoft/Windows/Fonts/", pattern = "akbar", prompt = F)


#############################################

#Explore dataset
simpsons$guest_star<-as.factor(simpsons$guest_star)
simpsons$role<-as.factor(simpsons$role)
levels(simpsons$guest_star)
levels(simpsons$role)

#count guest star appearances
gs_count<-simpsons%>%
  group_by(season, guest_star)%>%
  summarise (n = n())
str(gs_count)

#transform to data frame
gs_count<-as.data.frame(gs_count)
gs_count$season<-as.numeric(gs_count$season)


#select only guest stars who appeared more than 3 times per season
fiveo<-gs_count%>%
  filter(n >= 3)


# animated lollipop plot
p<-ggplot(fiveo, aes(x=guest_star, y=n, label = guest_star)) + 
  geom_point(size=15, stat = "identity", color= "white" ) + 
  geom_segment(aes(x=guest_star, xend=guest_star, y=0, yend=n), color= "white", lwd = 3)+
  coord_flip()+
  ylab("Number of Guest Appearances")+
  geom_text(data = fiveo, aes(label = guest_star,  size = 26, family = 'Akbar'), color="black")+
  theme(panel.background = element_rect(fill = "#87ceeb"))+
  theme(legend.position = "none")+
  theme(
    panel.grid.major = element_line(colour = "#87ceeb"),
    panel.grid.minor = element_line(colour = "#87ceeb")
  )+theme(axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())+ 
  theme(axis.text.x = element_text(face="bold", color="white", family = 'Akbar'))+
  theme(axis.title.x = element_text(face="bold", color="white", family = 'Akbar', size = 20))+
  transition_states(season)+ 
  ggtitle('Season {closest_state}')+ 
  theme(plot.title = element_text(size = 50, face = "bold",  color="#FFD300", family = 'Akbar', hjust = 0.5))+
  theme(plot.background = element_rect(fill = "#87ceeb"))+
  ylim(0,20)

#animate plot, change height, width and frame per second and add end pause
tf<- animate(p, fps = 5, height = 500, width =550, end_pause = 5)

#save plot
#anim_save("simpsons_guest_stars.gif", animation = tf)
