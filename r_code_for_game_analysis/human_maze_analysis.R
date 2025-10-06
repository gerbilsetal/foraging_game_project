#load packages & data####    
library("dplyr") 
library("ggplot2")
library("lmerTest")

#load data
df_unfiltered <- read.csv('human_maze_data.csv', fileEncoding="UTF-8-BOM" )

#filter players that didn't finished the trial
df <- df_unfiltered[which(df_unfiltered$finished == 1),1:3]

#defining coordinates of corridors between rooms####

#start location
start_room <- matrix(range(-5,5), nrow = 2, ncol = 2)

#first row of rooms
f_1 <- matrix(c(range(18,22), range(14,20)), nrow = 2, ncol = 2)
f_2 <- matrix(c(range(-22,-18), range(14,20)), nrow = 2, ncol = 2)

#second row of rooms
s_1 <- matrix(c(range(28,32), range(32,40)), nrow = 2, ncol = 2)
s_2 <- matrix(c(range(8,12), range(32,40)), nrow = 2, ncol = 2)
s_3 <- matrix(c(range(-12,-8), range(32,40)), nrow = 2, ncol = 2)
s_4 <- matrix(c(range(-32,-28), range(32,40)), nrow = 2, ncol = 2)

#third row of rooms
th_1 <- matrix(c(range(32,36), range(52,58)), nrow = 2, ncol = 2)
th_2 <- matrix(c(range(23,27), range(52,58)), nrow = 2, ncol = 2)
th_3 <- matrix(c(range(13,18), range(52,58)), nrow = 2, ncol = 2)
th_4 <- matrix(c(range(3,7), range(52,58)), nrow = 2, ncol = 2)
th_5 <- matrix(c(range(-7,-3), range(52,58)), nrow = 2, ncol = 2)
th_6 <- matrix(c(range(-18,-13), range(52,58)), nrow = 2, ncol = 2)
th_7 <- matrix(c(range(-27,-23), range(52,58)), nrow = 2, ncol = 2)
th_8 <- matrix(c(range(-36,-32), range(52,58)), nrow = 2, ncol = 2)

#list of all rooms
rooms <- list(start_room,f_1,f_2,s_1,s_2,s_3,s_4,th_1,th_2,th_3,th_4,th_5,th_6,th_7,th_8)


#function for choosing trial number####

choose_trial <- function(dataframe, trial_num) { 
  new_data <- dataframe[which(dataframe$trial == trial_num),]
  return(new_data)
}


#function for checking if a set of coordinates is within a corridor####
check_room <- function(row) {
  room_index = 1
  for (room in rooms) {
    if (between(row$X, room[1,1], room[2,1]) & between(row$Z, room[1,2], room[2,2])) {
      row$corridor <- room_index
      room_index <- room_index + 1
    } else {
      room_index <- room_index + 1
    }
  }
  room_index = 1
  return(row)
}


#adding time####

#create data frame containing times
df1 <- data.frame()
trial_list <- (unique(df$trial))

#the game saved a coordinate each 0.5sec. this code accounts for this
for (i in trial_list) {
  trial <- choose_trial(df,i)
  time <- seq(from = 0, to = nrow(trial)/2, length.out = nrow(trial))
  x<- cbind(trial,time)
  df1 <- rbind(df1,x)
}


#getting corridor passages####

#create data frame
passages <- data.frame()

#flag corridor passages
for (i in trial_list) {
  trial <- choose_trial(df1,i)
  for(j in 1:nrow(trial)) {
    row <- trial[j,]
    x <- check_room(row)
    if(length(x) == 5) {
      passages <- rbind(passages,x)
    } 
  }
}

#filter only the entry and exit for each corridor
passages_clean <- data.frame()
for(j in 2:(nrow(passages)-1)) {
  row_p <- passages[j-1,]
  row <- passages[j,]
  row_n <- passages[j+1,]
  if(row$corridor != row_n$corridor | row$corridor != row_p$corridor) {
    passages_clean <- rbind(passages_clean,row)
  }
}


#getting the room with the food####
prize_rooms <- list()

#the prize room (room with the food) is the third room 
#from the end to which the players entered
for (i in (unique(passages_clean$trial))) {
  trial <- choose_trial(passages_clean,i)
  prize_rooms <- append(prize_rooms, trial[nrow(trial) - 6, 5])
}

#getting times of arrival to  prize rooms####

times_to_arrive <- data.frame()
for (i in trial_list) {
  trial <- choose_trial(passages_clean,i)
  prize_room <- prize_rooms[which(trial_list == i)]
  trial_filtered <- trial[trial$corridor == prize_room, ] 
  times_to_arrive <- rbind(times_to_arrive,trial_filtered)
}

# Select every odd row to filter out the exits
times_to_arrive <- times_to_arrive[seq(1, nrow(times_to_arrive), by = 2), ]  

#creating data frame for time for each round trip to the prize rooms####

round_trips <- times_to_arrive %>% 
  group_by(trial) %>%
  mutate(
    round_trip_time = time - lag(time)
  ) %>%
  mutate(
    round_trip_time = ifelse(is.na(round_trip_time), time, round_trip_time)) %>%
  mutate(trip_num = row_number()) %>%
  ungroup()

#sum round trips for all trials (for plotting)
round_trips_sum <- round_trips %>% group_by(trip_num) %>%
  summarise(mean = mean(round_trip_time),sd = sd(round_trip_time, na.rm=TRUE)) %>%
  filter(trip_num <6)


#plotting####

ggplot(round_trips_sum, aes(trip_num, mean)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.1) +
  labs(x="round trip number", 
       y="round trip time (sec)") +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ exp(-x)) +
  ggtitle("Maze learning curve") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

#fitting exponential model to calculate learning curve####


time_model <- lmer(log(round_trip_time) ~ trip_num + (1|trial), data = round_trips)

#view the output of the model
summary(time_model)

# Extract random effects (deviations from fixed effects)
individual_effects <- ranef(time_model)$trial
