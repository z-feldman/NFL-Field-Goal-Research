
# Load Libraries ----------------------------------------------------------


library(tidyverse)
library(magrittr)
library(stringi)
library(stringr)
library(ggbeeswarm)
library(cowplot)
library(gtable)
library(grid)
library(lemon)

# Load Data ---------------------------------------------------------------


pbp2009 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2009.csv")
pbp2010 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2010.csv")
pbp2011 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2011.csv")
pbp2012 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2012.csv")
pbp2013 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2013.csv")
pbp2014 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2014.csv")
pbp2015 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2015.csv")
pbp2016 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2016.csv")
pbp2017 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2017.csv")
pbp2018 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2018.csv")
pbp2019 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2019.csv")





pbp <- rbind(pbp2009, pbp2010, pbp2011, pbp2012, pbp2013, pbp2014, pbp2015, pbp2016, pbp2017, pbp2018, pbp2019)

# define possessions based on binned score differential, with possession defined as 8 points (being conservative with 2 point conversions)----

pbp_poss <- pbp %>% drop_na(score_differential, score_differential_post) %>% 
  mutate(num_poss = case_when(score_differential < 0 & score_differential >= -8 ~ -1, # losing between 1 and 8 before going into the play
                              score_differential <= -9 & score_differential >= -16 ~ -2, # losing between 9 and 16
                              score_differential <= -17 & score_differential >= -24 ~ -3, # losing between 17 and 24
                              score_differential < -24 ~ -4, # everything more than 24 lump together as the same
                              
                              score_differential == 0 ~ 0, # tie game
                                            
                              score_differential > 0 & score_differential <= 8 ~ 1, # winning between 1 and 8
                              score_differential >= 9 & score_differential <= 16 ~ 2, # winning between 9 and 16
                              score_differential >= 17 & score_differential <= 24 ~ 3, # winning between 17 and 24
                              score_differential > 24 ~ 4), # everything more than 24 lump together as the same
                
         num_poss_post = case_when(score_differential_post < 0 & score_differential_post >= -8 ~ -1, # same as above, but after the play is over
                              score_differential_post <= -9 & score_differential_post >= -16 ~ -2,
                              score_differential_post <= -17 & score_differential_post >= -24 ~ -3,
                              score_differential_post < -24 ~ -4,
                              
                              score_differential_post == 0 ~ 0,
                                     
                              score_differential_post > 0 & score_differential_post <= 8 ~ 1,
                              score_differential_post >= 9 & score_differential_post <= 16 ~ 2,
                              score_differential_post >= 17 & score_differential_post <= 24 ~ 3,
                              score_differential_post > 24 ~ 4),
         
         num_poss_change = num_poss_post - num_poss # change in possessions on that play
  )

# A few basic averages, can change to different data frames (fg, fg_account_miss, fg_seven_poss created below) if interested

pbp_poss %>% filter(play_type == "field_goal") %>% group_by(num_poss) %>% summarise(mean_wpa = mean(wpa, na.rm = T)) 
pbp_poss %>% filter(play_type == "field_goal") %>% group_by(num_poss_post) %>% summarise(mean_wpa = mean(wpa, na.rm = T)) 
pbp_poss %>% filter(play_type == "field_goal") %>% group_by(num_poss_change) %>% summarise(mean_wpa = mean(wpa, na.rm = T)) 

##### Start testing scenarios #####
# can't see a ton, too much data
fg <- pbp_poss %>% 
  filter(play_type == "field_goal", num_poss >= -2 & num_poss <= 2) %>% drop_na(wpa)
# account for misses ----
fg_account_miss <- fg %>% 
  mutate(num_poss_post = if_else((field_goal_result == "missed" | field_goal_result == "blocked"), 
                                 true = case_when(
                                   score_differential +3 < 0 & score_differential +3 >= -8 ~ -1,
                                   score_differential +3 <= -9 & score_differential +3 >= -16 ~ -2,
                                   score_differential +3 <= -17 & score_differential +3 >= -24 ~ -3,
                                   score_differential +3 < -24 ~ -4,
                                   
                                   score_differential +3 == 0 ~ 0,
                                   
                                   score_differential +3 > 0 & score_differential +3 <= 8 ~ 1,
                                   score_differential +3 >= 9 & score_differential +3 <= 16 ~ 2,
                                   score_differential +3 >= 17 & score_differential +3 <= 24 ~ 3,
                                   score_differential +3 > 24 ~ 4
                                 ), 
                                 false = num_poss_post),
         num_poss_change = num_poss_post - num_poss) 
# possessions are 7 points not 8----
fg_seven_poss <- fg %>% 
  mutate(num_poss = case_when(score_differential < 0 & score_differential >= -7 ~ -1,
                              score_differential <= -8 & score_differential >= -14 ~ -2,
                              score_differential <= -15 & score_differential >= -21 ~ -3,
                              score_differential < -21 ~ -4,
                              
                              score_differential == 0 ~ 0,
                              
                              score_differential > 0 & score_differential <= 7 ~ 1,
                              score_differential >= 8 & score_differential <= 14 ~ 2,
                              score_differential >= 15 & score_differential <= 21 ~ 3,
                              score_differential > 21 ~ 4),
         
         num_poss_post = case_when(score_differential_post < 0 & score_differential_post >= -7 ~ -1,
                                   score_differential_post <= -8 & score_differential_post >= -14 ~ -2,
                                   score_differential_post <= -15 & score_differential_post >= -21 ~ -3,
                                   score_differential_post < -21 ~ -4,
                                   
                                   score_differential_post == 0 ~ 0,
                                   
                                   score_differential_post > 0 & score_differential_post <= 1 ~ 1,
                                   score_differential_post >= 8 & score_differential_post <= 14 ~ 2,
                                   score_differential_post >= 15 & score_differential_post <= 21 ~ 3,
                                   score_differential_post > 21 ~ 4),
         
         num_poss_change = num_poss_post - num_poss) 



# plotting function taken from stackoverflow user Artem Sokolov----
shift_legend <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

######################################## Plots #####################################


# all FG, accounting for what change in possession would've been if made

all_account_p <- ggplot(data = fg_account_miss, aes(x = as.factor(num_poss_change), y = wpa, color = if_else(wpa >= 0.01, "wpa >= 0.01", if_else(wpa <= -0.01, "wpa <= -0.01", "-0.01 < wpa < 0.01")))) +
  geom_quasirandom(alpha = 0.5) +
  scale_colour_manual(name = "WPA", values = c("wpa >= 0.01" = "green", "wpa <= -0.01" = "red", "-0.01 < wpa < 0.01" = "black")) +
  geom_hline(yintercept = 0, color = "gray") +
  facet_wrap(~num_poss, scales = "free_x") +
  ylim(-0.1, 0.1) +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1), hjust = 0),
        axis.text = element_text(family = "mono"),
        strip.text = element_text(family = "mono", hjust = 0.5),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(family = "mono", face = "italic", size = rel(0.8), hjust = 1, vjust = -10),
        legend.text = element_text(family = "mono", face = "italic", size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        plot.background = element_rect(fill = alpha(colour = "ivory1", alpha = 1))) +
  labs(title = "Win Probability Added on Field Goal Attempts", 
       subtitle = "Faceted by Number of Score Differential Possessions Before Play (2009-2019, regular season)", 
       caption = "Data from: @weightroomshoe, @nflscrapR \nGraphic by: @ZachFeldman3",
       x = "Number of Score Differential Possessions Changed on Play (if field goal attempt is made)", 
       y = "Win Probability Added")
 
  
shift_legend(all_account_p)



# other various things to test out of curiosity

fg %>% 
  group_by(yardline_100) %>% 
  mutate(percent = sum(field_goal_result == "made") / n() , Attempts = n()) %>% 
  ggplot(aes(x = yardline_100 + 17, y = percent)) + 
  geom_point(aes(size = Attempts / 600), alpha = 0.5) +
  geom_smooth(size = 1.5) +
  labs(title = "Field Goal Percentage By Distance", x = "Distance", y = "Field Goal Percent", caption = "Data from nflscrapR") +
  theme_bw() +
  theme(legend.position = "none")


fg %>% 
  group_by(yardline_100) %>% 
  mutate(mean_wpa = mean(wpa, na.rm = T), Attempts = n()) %>% 
  ggplot(aes(x = yardline_100 + 17, y = mean_wpa)) + 
  geom_point(aes(size = Attempts / 600), alpha = 0.5) +
  facet_wrap(~num_poss) +
  geom_smooth(size = 1.5) +
  labs(title = "WPA By Field Goal Distance", x = "Distance", y = "Field Goal Percent", caption = "Data from nflscrapR") +
  theme_bw() +
  theme(legend.position = "none")


fg_seven_poss %>% 
  group_by(yardline_100) %>% 
  mutate(mean_wpa = mean(wpa, na.rm = T), Attempts = n()) %>% 
  ggplot(aes(x = yardline_100 + 17, y = mean_wpa)) + 
  geom_point(aes(size = Attempts / 600), alpha = 0.5) +
  facet_wrap(~num_poss) +
  geom_smooth(size = 1.5) +
  labs(title = "WPA By Field Goal Distance", x = "Distance", y = "Field Goal Percent", caption = "Data from nflscrapR") +
  theme_bw() +
  theme(legend.position = "none")

fg_account_miss %>% filter(between(num_poss, -1, 1)) %>% summarize(count = n(), mean_wpa = mean(wpa))

fg %>% filter(qtr == 4, score_differential == 3, score_differential_post == 6) %>% group_by(half_seconds_remaining/60) %>% mutate(count = n(), mean_wpa = mean(wpa), percent_neg = sum(wpa < 0) / count) %>%
  ggplot(aes(x = half_seconds_remaining / 60, y = percent_neg)) +
  geom_point() +
  geom_smooth() +
  theme_bw()



fg %>% 
  filter(qtr == 4, num_poss_post == 1 | num_poss_post == 0, num_poss_change > 0) %>% 
  mutate(minutes_remaining = cut(half_seconds_remaining / 60, breaks = c(0,0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))) %>%
  group_by(minutes_remaining) %>% 
  summarize(count = n(), mean_wpa = mean(wpa), percent_neg = sum(wpa < 0) / count) 


fg %>% filter(score_differential == 3, score_differential_post == 3) %>% summarize(count = n(), mean_wpa = mean(wpa), percent_neg = sum(wpa < 0) / count) 

pbp %>% filter(score_differential == 13, field_goal_result == "made", qtr == 4) %>% 
  #mutate(minutes_remaining = cut(half_seconds_remaining / 60, breaks = c(0,0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))) %>%
  #group_by(minutes_remaining) %>% 
  summarize(count = n(), mean_wpa = mean(wpa, na.rm = T), percent_neg = sum(wpa < 0, na.rm = T) / count) 


















