#load packages####    

library("dplyr") #data wrangling
library('tidyr') #data wrangling
library("ggplot2") #plotting
library('lmerTest') #mixed-effect models
library('emmeans') #marginal mean comparisons


#load data####

df <- read.csv('open_field_data.csv', fileEncoding="UTF-8-BOM")


#turn columns into factors
df$safe_first <- as.factor(df$safe_first)
df$n <- as.factor(df$n)
#remove pilot runs
df <- df[which(!df$n %in% c(1,2,3,4,5)),]
#load background data
background <- read.csv('general_data.csv', fileEncoding="UTF-8-BOM")
background$n <- as.factor(background$n)


#preparing data frames #########################################################

#define places of interest####

#bush coordinates
bush1_coor <-  matrix(c(range(831,1021), range(3361,3232)), nrow = 2, ncol = 2)
bush2_coor <-  matrix(c(range(895,960), range(2590,2655)), nrow = 2, ncol = 2)
bush3_coor <-  matrix(c(range(800,960), range(1470,1790)), nrow = 2, ncol = 2)
bush4_coor <-  matrix(c(range(900,1215), range(860,1025)), nrow = 2, ncol = 2)
bush5_coor <-  matrix(c(range(1722,1880), range(2815,2975)), nrow = 2, ncol = 2)
bush6_coor <-  matrix(c(range(1630,1690), range(2300,2360)), nrow = 2, ncol = 2)
bush7_coor <-  matrix(c(range(1560,1665), range(1860,1945)), nrow = 2, ncol = 2)
bush8_coor <-  matrix(c(range(2145,2230), range(2620,2751)), nrow = 2, ncol = 2)
bush9_coor <-  matrix(c(range(2240,2330), range(2305,2365)), nrow = 2, ncol = 2)
bush10_coor <-  matrix(c(range(1920,2075), range(1987,2140)), nrow = 2, ncol = 2)
bush11_coor <-  matrix(c(range(1950,2055), range(1635,1730)), nrow = 2, ncol = 2)
bush12_coor <-  matrix(c(range(2240,2420), range(990,1180)), nrow = 2, ncol = 2)
bush13_coor <-  matrix(c(range(2880,3200), range(2940,3230)), nrow = 2, ncol = 2)
bush14_coor <-  matrix(c(range(2870,3130), range(2370,2495)), nrow = 2, ncol = 2)
bush15_coor <-  matrix(c(range(2625,2690), range(1730,1790)), nrow = 2, ncol = 2)
bush16_coor <-  matrix(c(range(2880,2940), range(1570,1630)), nrow = 2, ncol = 2)
bush17_coor <-  matrix(c(range(3070,3290), range(765,985)), nrow = 2, ncol = 2)
bushes_coor <- list(bush1_coor, bush2_coor,bush3_coor,bush4_coor,
                    bush5_coor,bush6_coor,bush7_coor,bush8_coor,
                    bush9_coor,bush10_coor,bush11_coor,bush12_coor,
                    bush13_coor,bush14_coor,bush15_coor,bush16_coor,
                    bush17_coor)

#food patch coordinates
food1_coor_risk <- matrix(c(range(1080,1277), range(1377,1535)), nrow = 2, ncol = 2)
food2_coor_risk <- matrix(c(range(2015,2210), range(1120,1280)), nrow = 2, ncol = 2)

food1_coor_safe <- matrix(c(range(1920,2080), range(1080,1280)), nrow = 2, ncol = 2)
food2_coor_safe <- matrix(c(range(2848,3040), range(1370,1530)), nrow = 2, ncol = 2)

food_coor_risk <- list(food1_coor_risk,food2_coor_risk)
food_coor_safe <- list(food1_coor_safe,food2_coor_safe)

#home coordinates
cave_coor <- matrix(c(range(2015,2175), range(215,2175)), nrow = 2, ncol = 2)

#flag instances of hiding in bushes####

#function to check both columns against a matrix range
is_in_range <- function(row, matrices) {
  any(sapply(matrices, function(mat) {
    # Extract min/max for the first and second column of the matrix
    min_col1 <- min(mat[,1]); max_col1 <- max(mat[,1])
    min_col2 <- min(mat[,2]); max_col2 <- max(mat[,2])
    
    # Check if row's Col1 and Col2 fall within the respective ranges
    (row["x"] >= min_col1 & row["x"] <= max_col1) &
      (row["y"] >= min_col2 & row["y"] <= max_col2)
  }))
}


#flag
df$Flag <- apply(df, 1, is_in_range, matrices = bushes_coor)

#summarize hiding time####

hiding_sum<- df[which(df$Flag == TRUE),] %>%
  group_by(n,risk,safe_first) %>% 
  summarise(sum=n()) %>% group_by(risk) 
  
hiding_sum$risk[which(hiding_sum$risk == 1)] <- "risky"
hiding_sum$risk[which(hiding_sum$risk == 0)] <- "safe"
#IN N = 29 THE PLAYER DIDNT HIDE IN THHE SAFE LEVEL
#MANUALLY ADD DATA
trial29safe <- data.frame(n = 29, risk = 'safe', safe_first = "risky first", sum = 0)
trial29safe$safe_first <- as.factor(trial29safe$safe_first)
trial29safe$n <- as.factor(trial29safe$n)

hiding_sum <- rbind(hiding_sum,trial29safe)

#turn column into factor
hiding_sum$risk <- as.factor(hiding_sum$risk)

#append background data
hiding_sum <- left_join(hiding_sum, background)


#add times####

df <- df %>% group_by(n,risk) %>%  mutate(time=row_number())


#flag patch visits in a long format####

df$at_patch <- 0
df$at_patch[which(df$risk == 1)] <- apply(df[which(df$risk == 1),], 
                                          1, is_in_range, matrices = food_coor_risk)
df$at_patch[which(df$risk == 0)] <- apply(df[which(df$risk == 0),], 
                                          1, is_in_range, matrices = food_coor_safe)

#crate data frame for patch arrivals
patch_arrivals <- df[which(df$at_patch == TRUE & lead(df$at_patch == FALSE)),]

#sum patch arrivals
patch_arrivals_sum <- patch_arrivals %>% group_by(n,risk,safe_first) %>%
  summarise(sum = n())

#turn binary variables into string for plotting
patch_arrivals_sum$risk[which(patch_arrivals_sum$risk == 1)] <- "risky"
patch_arrivals_sum$risk[which(patch_arrivals_sum$risk == 0)] <- "safe" 

#append background data
patch_arrivals_sum <- left_join(patch_arrivals_sum, background)


#flag patch visits in a wide format####

#initialize lists
food_coor1_safe <- list(food1_coor_safe) 
food_coor2_safe <- list(food2_coor_safe) 
food_coor1_risk <- list(food1_coor_risk) 
food_coor2_risk <- list(food2_coor_risk) 

#initialize columns
df$patch1_safe <- 0
df$patch2_safe <- 0
df$patch1_risk <- 0
df$patch2_risk <- 0

#flag patch visits
df$patch1_safe[which(df$risk == 0)] <- apply(df[which(df$risk == 0),], 
                                             1, is_in_range, matrices = food_coor1_safe)

df$patch2_safe[which(df$risk == 0)] <- apply(df[which(df$risk == 0),], 
                                             1, is_in_range, matrices = food_coor2_safe)

df$patch1_risk[which(df$risk == 1)] <- apply(df[which(df$risk == 1),], 
                                             1, is_in_range, matrices = food_coor1_risk)

df$patch2_risk[which(df$risk == 1)] <- apply(df[which(df$risk == 1),], 
                                             1, is_in_range, matrices = food_coor2_risk)

#replace NA with zeroes
df <- df %>% replace(is.na(.), 0)

patch_visits <- df[which(df$at_patch == TRUE & lead(df$at_patch == FALSE)),]

patch_visits <- patch_visits %>% group_by(n) %>% summarise(patch1_safe = sum(patch1_safe),
                                                 patch2_safe = sum(patch2_safe),
                                                 patch1_risk = sum(patch1_risk),
                                                 patch2_risk = sum(patch2_risk))

#write.csv(patch_visits,"~/filename.csv", row.names = FALSE)

#preparing data frames for time to reach first and second patches####

#data from the data frames containing patch visits in a long and wide format
#were summarized manually in excel to create the data frames 'open_field_visits_detail'

#load data
patch_visits <- read.csv('open_field_visits_detail.csv', fileEncoding="UTF-8-BOM")

#turn columns into factors
patch_visits$risk <- as.factor(patch_visits$risk)
patch_visits$safe_first <- as.factor(patch_visits$safe_first)
patch_visits$n <- as.factor(patch_visits$n)

#remove pilot runs
patch_visits <- patch_visits[which(!patch_visits$n %in% c(1,2,3,4,5)),]

#filter data for first patch
patch_visits_filtered1 <- patch_visits %>% group_by(n,risk) %>%
  filter(patch == 1) %>%
  slice(1)

#filter data for second patch
patch_visits_filtered2 <- patch_visits %>% group_by(n,risk) %>%
  filter(patch == 2) %>%
  slice(1)

#MANUALLY ADD DATA FOR RUN WITH NO PATCH VISITS
missimgsecond <- data.frame(n = c(7,8,14,16,24,24,29,30,32,36,37,37), 
                            risk = c('safe','safe','risky','safe','risky','safe','risky','risky','risky','risky','risky','safe'), 
                            safe_first = c("risky first",'safe first','safe first','safe first','safe first','safe first','risky first','safe first','safe first','safe first','risky first','risky first'), 
                            time = c(300,300,300,300,300,300,300,300,300,300,300,300),
                                     patch = c(2,2,2,2,2,2,2,2,2,2,2,2))
missimgsecond$safe_first <- as.factor(missimgsecond$safe_first)
missimgsecond$n <- as.factor(missimgsecond$n)

#append data
patch_visits_filtered2 <- rbind(patch_visits_filtered2,missimgsecond)

#turn columns into factors
patch_visits_filtered2$risk <- as.factor(patch_visits_filtered2$risk)

#append background data
patch_visits_filtered1 <- left_join(patch_visits_filtered1, background)
patch_visits_filtered2 <- left_join(patch_visits_filtered2, background)

#order data frames
patch_visits_filtered2 <- patch_visits_filtered2[order(patch_visits_filtered2$n,
                                                       patch_visits_filtered2$risk),]
patch_visits_filtered1 <- patch_visits_filtered1[order(patch_visits_filtered1$n,
                                                       patch_visits_filtered1$risk),]

#calculate time to reach second patch after reaching the first
patch_visits_filtered2$clean_time <- patch_visits_filtered2$time - patch_visits_filtered1$time


#hiding time model####

#model
model <- lmer(data = hiding_sum, (sum)^0.5 ~ risk*safe_first + (1|n))
summary(model)

#check for normality of residuals
res <- residuals(model)
shapiro.test(res) #normal
 

#models#########################################################################

#time to reach first patch model####

#model
model <- lmer(data = patch_visits_filtered1, time ~ risk*safe_first + (1|n))
summary(model)

#check for normality of residuals
res <- residuals(model)
shapiro.test(res) #normal

#marginal means comparison
emm <- emmeans(model, ~ risk | safe_first)
pairs(emm)


#time to reach second patch model####
model <- lmer(data = patch_visits_filtered2, clean_time ~ risk*safe_first + (1|n))
summary(model)

#check for normality of residuals
res <- residuals(model)
shapiro.test(res)

#marginal means comparison
emm <- emmeans(model, ~ risk | safe_first)
pairs(emm) #normal


#patch visits model####

#model
model <- lmer(data = patch_arrivals_sum, sum ~ risk*safe_first + (1|n))
summary(model)

#check for normality of residuals
res <- residuals(model)
shapiro.test(res) #normal 

#marginal means comparison
emm <- emmeans(model, ~ risk | safe_first)
pairs(emm)



#plots##########################################################################

#plot hiding time####
ggplot(hiding_sum, aes(x = risk, y = sum, color = safe_first, fill = safe_first)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75), 
              size = 2, shape = 21) +
  labs(x="level", 
       y="time spent hiding (sec)") +
  scale_color_manual(values = c('risky first' = "firebrick4", 'safe first' = "olivedrab")) +
  scale_fill_manual(values = c('risky first' = "firebrick1", 'safe first' = "olivedrab1"))+
  theme_minimal(base_size = 14) +
  ggtitle("Hiding time")  + theme(legend.position = "none")

hiding_sum$sum_root <- hiding_sum$sum^0.5



#plot patch visits####
ggplot(patch_arrivals_sum, aes(x = risk, y = sum, color = safe_first, fill = safe_first)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75), 
              size = 2, shape = 21) +
  labs(x="level", 
       y="Patch visits (n)") +
  scale_color_manual(values = c('risky first' = "firebrick4", 'safe first' = "olivedrab")) +
  scale_fill_manual(values = c('risky first' = "firebrick1", 'safe first' = "olivedrab1"))+
  theme_minimal(base_size = 14) +
  ggtitle("Patch visits") + theme(legend.position = "none")



#plot time to reach first patch####
ggplot(patch_visits_filtered1, aes(x = risk, y = time, color = safe_first, fill = safe_first)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75), 
              size = 2, shape = 21) +
  labs(x="level", 
       y="time at first patch(sec)") +
  scale_color_manual(values = c('risky first' = "firebrick4", 'safe first' = "olivedrab")) +
  scale_fill_manual(values = c('risky first' = "firebrick1", 'safe first' = "olivedrab1"))+
  theme_minimal(base_size = 14) +
  ggtitle("Time at first patch") + theme(legend.position = "none")


#plot time to reach second patch####
ggplot(patch_visits_filtered2, aes(x = risk, y = clean_time, color = safe_first, fill = safe_first)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75), 
              size = 2, shape = 21) +
  labs(x="level", 
       y="time at second patch(sec)") +
  scale_color_manual(values = c('risky first' = "firebrick4", 'safe first' = "olivedrab")) +
  scale_fill_manual(values = c('risky first' = "firebrick1", 'safe first' = "olivedrab1"))+
  theme_minimal(base_size = 14) +
  ggtitle("Time at second patch after finding the first") + theme(legend.position = "none")
