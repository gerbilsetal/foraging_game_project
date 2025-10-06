#load packages & data####    

library("dplyr") 
library("ggplot2")
library("tidyr")

#load data
df <- read.csv('game_sum.csv', fileEncoding="UTF-8-BOM" )
#subset data by treatments
risk <- df[which(df$risk == 'risky'),]
safe <- df[which(df$risk == 'safe'),]

#filter unnecessary columns 
sub_df <- subset(df, select = c(risk,score,n))
sub_df2 <- subset(df, select = c(risk,time.patch1,n))

#wide data frames for plotting
df_wide <- pivot_wider(sub_df, names_from = `risk`, values_from = score)
df_wide2 <- pivot_wider(sub_df2, names_from = `risk`, values_from = time.patch1)


#correlations####

#maze learning vs. patch visits
cor.test(risk$score,risk$finish.time) #no
cor.test(safe$score,safe$finish.time) #no

#maze learning vs. time to reach first patch
cor.test(risk$time.patch1,risk$finish.time) #yes
cor.test(safe$time.patch1,safe$finish.time) #yes

#plotting####
ggplot(safe, aes(time.patch1, finish.time)) + 
  geom_point(size = 3) +
  labs(x="time at first patch (sec)", 
       y="maze finish time (sec)") +
  theme_classic(base_size = 16) + 
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x) +
  ggtitle("Maze finish time vs. time at first patch (safe level)") 

ggplot(risk, aes(time.patch1, finish.time)) + 
  geom_point(size = 3) +
  labs(x="time at first patch (sec)", 
       y="maze finish time (sec)") +
  theme_classic(base_size = 16) + 
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x) +
  ggtitle("Maze finish time vs. time at first patch (risky level)")
