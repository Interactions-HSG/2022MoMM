rm(list=ls())
library(tidyverse)
library(plotly)
library(lubridate)
library(Hmisc)
setwd("/Users/simeonpilz/Documents/Simeon/Masterarbeit/dataAnalysis")
getwd()
packageVersion('tidyverse')
packageVersion('plotly')
packageVersion('lubridate')
packageVersion('Hmisc')

#-------------------------------------------------------------------------------


#################
## SURVEY DATA ##
#################

survey <- read.csv("participantData.csv")
# View(survey)

colnames(survey)


s_complete <- survey %>% filter(end!="0")
demo_complete <- s_complete %>% select(c(ID, start, end, birthyear, education, gender, mainbuyer, nutrition, wage, workingstatus))
s1_complete <- s_complete %>% select(matches("^e.*\\d"))
s2_complete <- s_complete %>% select(matches("^p.*\\d"))

dim(demo_complete)
dim(s1_complete)
dim(s2_complete)
colnames(s1_complete) <- gsub("e","",colnames(s1_complete))
colnames(s2_complete) <- gsub("p","",colnames(s2_complete))

s1_complete$survey <- "s1"
s2_complete$survey <- "s2"


all(colnames(s1_complete)==colnames(s2_complete) )

s_complete <- rbind(s1_complete, s2_complete)

# aggregate individual answers to their class-level
s_complete <- s_complete %>%
  mutate(intrinsic=(B1+B2+B3+B4)/4,
         integrated=(B5+B6+B7+B8)/4,
         identified=(B9+B10+B11+B12)/4,
         introjected=(B13+B14+B15)/3,
         external=(B16+B17+B18+B19)/4,
         amotivation=(B20+B21+B22+B23)/4) %>%
  mutate(autonomy=(C1+C2+C3+C4)/4,
         competence=(C5+C6+C7+C8)/4 ,
         relatedness=(C9+C10+C11+C12)/4)



###################################################
## COMPARE FIRST AND SECOND SURVEY -- MOTIVATION ##
###################################################

## fist vs second in levels
gg_s1_s2_motivation <- s_complete %>% pivot_longer(cols=!survey, names_to="question", values_to="answer") %>%
  filter(question %in% c("intrinsic", "integrated", "identified", "introjected", "external", "amotivation")) %>%
  mutate(question=factor(question, levels=c("intrinsic", "integrated", "identified", "introjected", "external", "amotivation"))) %>%
  ggplot(aes(x=question, y=answer, color=survey)) +
  geom_boxplot() +
  ggtitle("Comparison motivation -- first/second survey") 

gg_s1_s2_motivation
ggsave("R scripts/plots/gg_s1_s2_motivation.pdf", gg_s1_s2_motivation, width=6, height=4)


## difference
s1_complete <- s_complete %>% filter(survey=="s1") %>% select(-survey)
s2_complete <- s_complete %>% filter(survey=="s2") %>% select(-survey)

s_diff <- s2_complete - s1_complete

mean(s1_complete$autonomy)
mean(s1_complete$competence)
mean(s1_complete$relatedness)
mean(s1_complete$intrinsic)
mean(s1_complete$integrated)
mean(s1_complete$identified)
mean(s1_complete$introjected)
mean(s1_complete$external)
mean(s1_complete$amotivation)

sd(s1_complete$autonomy)
sd(s1_complete$competence)
sd(s1_complete$relatedness)
sd(s1_complete$intrinsic)
sd(s1_complete$integrated)
sd(s1_complete$identified)
sd(s1_complete$introjected)
sd(s1_complete$external)
sd(s1_complete$amotivation)

mean(s2_complete$autonomy)
mean(s2_complete$competence)
mean(s2_complete$relatedness)
mean(s2_complete$intrinsic)
mean(s2_complete$integrated)
mean(s2_complete$identified)
mean(s2_complete$introjected)
mean(s2_complete$external)
mean(s2_complete$amotivation)

sd(s2_complete$autonomy)
sd(s2_complete$competence)
sd(s2_complete$relatedness)
sd(s2_complete$intrinsic)
sd(s2_complete$integrated)
sd(s2_complete$identified)
sd(s2_complete$introjected)
sd(s2_complete$external)
sd(s2_complete$amotivation)




gg_s_diff_motivation <- s_diff %>% pivot_longer(cols=everything(), names_to="question", values_to="answer") %>%
  filter(question %in% c("intrinsic", "integrated", "identified", "introjected", "external", "amotivation")) %>%
  mutate(question=factor(question, levels=c("intrinsic", "integrated", "identified", "introjected", "external", "amotivation"))) %>%
  ggplot(aes(x=question, y=answer)) +
  geom_boxplot() +
  ggtitle("Difference motivation -- second minus first survey") 

gg_s_diff_motivation
ggsave("R scripts/plots/gg_s_diff_motivation.pdf", gg_s_diff_motivation, width=6, height=4)





##############################################
## COMPARE FIRST AND SECOND SURVEY -- NEEDS ##
##############################################


## fist vs second in levels
gg_s1_s2_needs <- s_complete %>% pivot_longer(cols=!survey, names_to="question", values_to="answer") %>%
  filter(question %in% c("autonomy", "competence", "relatedness")) %>%
  mutate(question=factor(question, levels=c("autonomy", "competence", "relatedness"))) %>%
  ggplot(aes(x=question, y=answer, color=survey)) +
  geom_boxplot() +
  ggtitle("Comparison needs -- first/second survey") 

gg_s1_s2_needs
ggsave("R scripts/plots/gg_s1_s2_needs.pdf", gg_s1_s2_needs, width=6, height=4)


## difference

gg_s_diff_needs <- s_diff %>% pivot_longer(cols=everything(), names_to="question", values_to="answer") %>%
  filter(question %in% c("autonomy", "competence", "relatedness")) %>%
  mutate(question=factor(question, levels=c("autonomy", "competence", "relatedness"))) %>%
  ggplot(aes(x=question, y=answer)) +
  geom_boxplot() +
  ggtitle("Difference needs -- second minus first survey") 

gg_s_diff_needs
ggsave("R scripts/plots/gg_s_diff_needs.pdf", gg_s_diff_needs, width=6, height=4)






#-------------------------------------------------------------------------------


##############
## CO2 DATA ##
##############

fp <- read.csv("CO2Scores.csv")
head(fp)
# View(fp)

fp <- fp %>% mutate(Date=gsub("T.*","",Date)%>%ymd())


## overall evolution over time
gg_fp <- fp %>% filter(Date >= ymd("2021-06-01")) %>%
  ggplot(aes(x=Date, y=Score, color=ID)) +
  theme_minimal() +
  guides(colour="none") +
  labs(x=NULL) +
  geom_line() +
  geom_point(aes(size=kcal))

gg_fp



## effect of App ##

start <- survey %>% filter(start!="0") %>%
  mutate(start=gsub("T.*","",start)%>%ymd())
start
user_start <- start %>% pull(start)
names(user_start) <- start %>% pull(ID)
user_start

all(fp$ID %in% names(user_start))

## add starting date to dataframe
fp <- fp %>% mutate(start=user_start[ID]) %>%
  mutate(start_dist=difftime(Date,start,units="days")%>%as.numeric() ) %>%
  mutate(user=start_dist>=0)
fp


time <- c("week -3 to 0", "week 0 to 3", "week 3 to 6", "week 6 to 9")
CO2Score <- numeric()
sd <- numeric()
sum(fp$start_dist >= -21 & fp$start_dist < 0)
sum(fp$start_dist >= 0 & fp$start_dist < 21)
sum(fp$start_dist >= 21 & fp$start_dist < 42)
sum(fp$start_dist >= 42 & fp$start_dist < 63)


mean_before <- weighted.mean(fp[fp$start_dist >= -21 & fp$start_dist < 0, 'Score'],fp$kcal[fp$start_dist >= -21 & fp$start_dist < 0])
weighted_var <- wtd.var(fp[fp$start_dist >= -21 & fp$start_dist < 0, 'Score'],fp$kcal[fp$start_dist >= -21 & fp$start_dist < 0])
std_before <- sqrt(weighted_var)
CO2Score[1] <- mean_before
sd[1] <- std_before

mean_during <- weighted.mean(fp[fp$start_dist >= 0 & fp$start_dist < 21, 'Score'],fp$kcal[fp$start_dist >= 0 & fp$start_dist < 21])
weighted_var <- wtd.var(fp[fp$start_dist >= 0 & fp$start_dist < 21, 'Score'],fp$kcal[fp$start_dist >= 0 & fp$start_dist < 21])
std_during <- sqrt(weighted_var)
CO2Score[2] <- mean_during
sd[2] <- std_during

mean_after <- weighted.mean(fp[fp$start_dist >= 21 & fp$start_dist < 42, 'Score'],fp$kcal[fp$start_dist >= 21 & fp$start_dist < 42])
weighted_var <- wtd.var(fp[fp$start_dist >= 21 & fp$start_dist < 42, 'Score'],fp$kcal[fp$start_dist >= 21 & fp$start_dist < 42])
std_after <- sqrt(weighted_var)
CO2Score[3] <- mean_after
sd[3] <- std_after

mean_end <- weighted.mean(fp[fp$start_dist >= 42 & fp$start_dist < 63, 'Score'],fp$kcal[fp$start_dist >= 42 & fp$start_dist < 63])
weighted_var <- wtd.var(fp[fp$start_dist >= 42 & fp$start_dist < 63, 'Score'],fp$kcal[fp$start_dist >= 42 & fp$start_dist < 63])
std_end <- sqrt(weighted_var)
CO2Score[4] <- mean_end
sd[4] <- std_end

df <- data.frame(time, CO2Score, sd)

gg_barplot <- ggplot(df) +
  geom_bar( aes(x=time, y=CO2Score), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=time, ymin=CO2Score-sd, ymax=CO2Score+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)
gg_barplot

ggsave("R scripts/plots/gg_barplot.pdf", gg_barplot, width=5, height=6)

