#Load libraries I need
library(tidyverse)
library(readr)
library(lubridate)
library(xml2)
library(stringi)
library(ggplot2)


# Read in data (players, 2022 matches, and rankings)
Matches_2022 <- data.frame(read.csv(url('https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_2022.csv')))



#let's filter to just Wimbledon? Test it out and see how it looks
# Might also do Top 50 players for the year 2022, depending on which is more interesting

usOpen <- Matches_2022 %>% 
  filter(tourney_name == 'Us Open')

#retrieve list of players so we can iterate to retrieve their point differences
#best to loop through losers list, cuz everyone lost besides Rybakina
open_players <- rep(NA,128)
for (i in 1:length(usOpen$loser_name)) {
    open_players[i] <- usOpen$loser_name[i]
}

#add Iga Swiatek :)
open_players[128] <- 'Iga Swiatek'

#I'll keep every match the same weight, even if one went 3 sets and the other went... cuz a longer match shouldn't be weighted more
# If I just summed up all points, that wouldn't be as effective
# Are all points equal the same or should all matches be equal? I think all points actually should be the same

#build function to loop through player list... retrieve 1st serve points had, and 1st serve points won
# same for second serve... just need a dataframe to of these numbers, then I can do the math in tje dataframe
# do it for one player first

ons <- usOpen %>% 
  filter(winner_name == 'Iga Swiatek' | loser_name == 'Iga Swiatek')

ons_first_serves <- 0
ons_first_win <- 0

for (i in 1:nrow(ons)) {
  if (ons$winner_name[i] == 'Dalma Galfi'){
    ons_first_serves <- ons_first_serves + ons$w_1stIn[i]
    ons_first_win <- ons_first_win + ons$w_1stWon[i]
    
  }else{
    ons_first_serves <- ons_first_serves + ons$l_1stIn[i]
  }
}

#it works!!!

blehhh <- wimbledon %>% 
  filter(winner_name == serves$Player[2] | loser_name == serves$Player[2])

first_in <- 0
first_won <- 0
sec_in <- 0
sec_won <- 0
for (i in 1:nrow(blehhh)) {
  if(blehhh$winner_name == serves$Player[i]){
    first_in = first_in + blehhh$w_1stIn[i]
    # first_won = first_won + player$w_1stWon[i]
    #sec_in = sec_in + (player$w_svpt[i] - player$w_1stIn[i] - player$w_df[i])
    #sec_won = sec_won + player$w_2ndWon[i]
  }
  else{
    first_in = first_in + blehhh$l_1stIn[i]
    #  first_won = first_won + player$l_1stWon[i]
    #  sec_in = sec_in + player$l_svpt[i] - player$l_1stIn[i] - player$l_df[i]
    #  sec_won = sec_won + player$l_2ndWon[i]
  }
}

#now build function for all players
serves  <- data.frame(matrix(ncol = 6, nrow = 128))
colnames(serves) <- c('Player','Matches','First_In','First_Won','Second_In','Second_Won')
serves$Player <- open_players


for (p in 1:nrow(serves)) {
  player <- usOpen %>% 
    filter(winner_name == serves$Player[p] | loser_name == serves$Player[p])
  
  first_in <- 0
  first_won <- 0
  sec_in <- 0
  sec_won <- 0
  for (i in 1:nrow(player)) {
    if(player$winner_name[i] == serves$Player[p]){
      first_in = first_in + player$w_1stIn[i]
      first_won = first_won + player$w_1stWon[i]
      sec_in = sec_in + (player$w_svpt[i] - player$w_1stIn[i] - player$w_df[i])
      sec_won = sec_won + player$w_2ndWon[i]
    }
    else{
      first_in = first_in + player$l_1stIn[i]
      first_won = first_won + player$l_1stWon[i]
      sec_in = sec_in + player$l_svpt[i] - player$l_1stIn[i] - player$l_df[i]
      sec_won = sec_won + player$l_2ndWon[i]
    }
  }
  serves$First_In[p] = first_in
  serves$First_Won[p] = first_won
  serves$Second_In[p] = sec_in
  serves$Second_Won[p] = sec_won
  serves$Matches[p] = nrow(player)
}


## Add 1st Serve win pct, 2nd serve win pct, and difference between the two ##

serves <- serves %>% 
  mutate(First_WinPct = First_Won/First_In,
         Second_WinPct = Second_Won/Second_In,
         Serve_Diff = First_WinPct - Second_WinPct)

r3_serves <- serves %>% 
  filter(Matches>2) %>% 
  mutate(Highlight = ifelse(Player== 'Iga Swiatek','iga','gray'))

# ok going to make two visualizations and figure out which i like better

#first do bar chart for the 8 quarterfinalists
qrt_finalists <- serves %>% 
  filter(Matches > 4) %>% 
  mutate(Highlight = ifelse(Player== 'Iga Swiatek','iga',
                            ifelse(Player %in% c('Jessica Pegula','Ons Jabeur','Caroline Garcia'),'iga_l','gray'))
         ) %>% 
  arrange(Serve_Diff)


ggplot(qrt_finalists) +
  geom_col(aes(x= reorder(Player,-Serve_Diff),y = Serve_Diff, fill = Highlight))+
  labs(title = 'Which US Open qurterfinalists had the biggest \n dropoff between 1st and 2nd serve points won?',
       y= 'Pct Difference Between 1st \n and 2nd Serve Points Won',
       caption = 'Data Source: JeffSackmann WTA Github Repo')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=7),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = 'none',
        plot.caption = element_text(size = 6))+
  scale_fill_manual(values = c( "iga"="#202A44", "gray"="gray","iga_l"="tomato"))+
  annotate("text", x = 4, y = .24, label = "All three players that lost to Iga had \na lot more pressure to make first serve",
           family = "serif", fontface = "italic", colour = "tomato", size = 2)+
  annotate("text", x = 7, y = .15, label = "Iga's small pct change took \npresure off her first serve",
           family = "serif", fontface = "italic", colour = "#202A44", size = 2)+
  annotate("segment", x = 7, xend = 8, y = .128, yend = .07,
           colour = "#202A44", size = .6)

ggplot(r3_serves) +
  geom_point(aes(x=First_WinPct, y = Second_WinPct, color = Highlight))+
  scale_color_manual(values = c('iga'= 'tomato','gray'='black'))+
  labs(title = "US Open Women's first\nserve vs. second serve win %",
       y = 'Second Serve Win %',
       x = 'First Serve Win %',
       caption = 'Data Source: JeffSackmann WTA Github Repo')+
  theme(legend.position = 'none',
        plot.caption = element_text(size = 6))+
  annotate("text", x = .62, y = .54, label = "Iga",
           family = "serif", fontface = "italic", colour = "tomato", size = 3)+
  annotate("text", x = .69, y = .463, label = "Ons",
           family = "serif", fontface = "italic", colour = "Black", size = 3)+
  annotate("text", x = .725, y = .43, label = "Serena",
           family = "serif", fontface = "italic", colour = "Black", size = 3)+
  annotate("text", x = .655, y = .655, label = "Muguruza led second\n serves with 66%!",
           family = "serif", fontface = "italic", colour = "Black", size = 2)+
  annotate("text", x = .806, y = .526, label = "Kudermetova led first\n serves with 82%!",
           family = "serif", fontface = "italic", colour = "Black", size = 2)+
  annotate("text", x = .747, y = .535, label = "Zheng",
           family = "serif", fontface = "italic", colour = "Black", size = 3)+
  annotate("text", x = .775, y = .6, label = "Collins",
           family = "serif", fontface = "italic", colour = "Black", size = 3)+
  annotate("text", x = .711, y = .625, label = "Coco",
           family = "serif", fontface = "italic", colour = "Black", size = 3)

ggplot(r3_serves) +
  geom_label(aes(x=First_WinPct, y = Second_WinPct, label=Player))+
  annotate("text", x = .8, y = .3, label = "Coco",
           family = "serif", fontface = "italic", colour = "Black", size = 3)

ggsave('first_v_sec_usopenwta.png', antialias = 'none')

