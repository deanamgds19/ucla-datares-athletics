setwd("~/Desktop/DataRes")
library(tidyverse)
library(lubridate)
library(ggplot2)
Query.1 <- read.csv("~/Desktop/DataRes/Query 3.csv") # this is the FORMS2 table
df <- Query.1[, c(1,2)]
df$date <- substr(df$recorded_at,start = 1, stop = 10)# extract the dates
df <- df[,c(1,3)] # only need this two column
#<- week(df$date)# convert to the week in the year
# <- seq.Date(as.Date("2016-04-04"), as.Date("2016-04-14"), by = 1)
df$week <- floor_date(as.Date(df$date), "weeks", week_start = 1)
query2 <- read.csv("~/Desktop/DataRes/Query 2.csv")# The demographics query
query2 <- query2[,c(1,4)]
final_df <- left_join(x = df, y = query2)# joining
final_df$freq <- rep(1,nrow(final_df)) # each row represent one survey was filled out

junk <- final_df %>% group_by(org_id,team,week) %>% summarise(times = sum(freq))
junk$pass <- if_else(junk$times < 6, 0, 1)

junk2 <- junk[,c(2,3,4,5)]
junk3 <- junk2 %>% group_by(team,week) %>% summarise(freq = mean(pass))

reference <- final_df %>% filter(team == "Women's Basketball")


men_df <- junk3 %>% filter(team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country",
                                    "Men's Golf","Men's Soccer","Men's Tennis","Men's Track and Field",
                                    "Men's Volleyball","Men's Water Polo","Track and Field"))
women_df <- junk3 %>% filter(!team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country",
                                          "Men's Golf","Men's Soccer","Men's Tennis","Men's Track and Field",
                                          "Men's Volleyball","Men's Water Polo","Track and Field"))
men1_plot <- ggplot(data = men_df %>% filter(team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country","Men's Golf")), aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Men's teams compliance(1)")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
ggsave("men_com_1.png",plot = men1_plot,width = 10)
men2_plot <- ggplot(data = men_df %>% filter(!team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country","Men's Golf")), aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Men's teams compliance(2)")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
ggsave("men_com_2.png",plot = men2_plot,width = 10)

men1_plot <- ggplot(data = men_df %>% filter(team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country","Men's Golf")), aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Men's teams compliance(1)")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
ggsave("men_com_1.png",plot = men1_plot,width = 10)
men2_plot <- ggplot(data = men_df %>% filter(!team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country","Men's Golf")), aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Men's teams compliance(2)")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
ggsave("men_com_2.png",plot = men2_plot,width = 10)








men_plot <- ggplot(data = men_df, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Men's teams compliance")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
men_plot
ggsave("men_plot.png",plot = men_plot,width = 10)

women_plot <- ggplot(data = women_df, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021-22 Women's teams compliance")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in a year") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
women_plot
ggsave("women_plot.png",plot = women_plot, width = 10)

# SPARTA jumps
sparta_jumps <- read.csv("~/Desktop/DataRes/sparta_jumps.csv")
sparta_jumps <- sparta_jumps[,c(2,4)]
# Just keep the recorded column
sparta_jumps$recorded_at <- substr(sparta_jumps$recorded_at,start = 1, stop = 10)
sparta_jumps$week <- week(sparta_jumps$recorded_at)
sparta_jumps$week_1st <- floor_date(as.Date(sparta_jumps$recorded_at), "weeks", week_start = 1)
#sparta_jumps$jump_comp <- if_else(sparta_jumps$times < 6, 0, 1)
sparta_jumps <- sparta_jumps[,c(2,3,4)]
sparta_jumps <- unique(sparta_jumps) # just take the unique rows
# make column of 1s
sparta_jumps$pass <- rep(1,nrow(sparta_jumps))
# Creatinbg empty vector for the loop
sparta_33_week <- sparta_jumps %>% group_by(org_id) %>% summarise(passed = sum(pass))
table(sparta_33_week$passed)
week_vec <- c()
org_vec <- c()
i <- 1
while (i <= nrow(query2)){
  week_vec <- append(week_vec,1:52)
  org_vec <- append(org_vec,rep(query2[[1]][i],52))
  #week1st_vec <- append(week1sg_vec rep())
  i = i+1
}
# make the data frame 
join_df <- as.data.frame(cbind(org_vec,week_vec))
# rename for the join
names(join_df)[1] <- "org_id"
names(join_df)[2] <- "week"
join_df <- left_join(x = join_df, y = sparta_jumps,by = c("org_id","week")) # here

# replace NA's with zero
join_df$pass <- if_else(is.na(join_df$pass) == TRUE, 0, 1)

# join again with the demographic dataframe
final_spart_df <- left_join(x = join_df, y = query2, by = c("org_id"))
final_spart_df <- final_spart_df %>% group_by(team,week) %>% summarise(freq = mean(pass))
sparta_men_df <- final_spart_df %>% filter(team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country",
                                       "Men's Golf","Men's Soccer","Men's Tennis","Men's Track and Field",
                                       "Men's Volleyball","Men's Water Polo","Track and Field"))
sparta_women_df <- final_spart_df %>% filter(!team %in% c("Cross Country","Football","Men's Baseball","Men's Basketball","Men's Cross Country",
                                          "Men's Golf","Men's Soccer","Men's Tennis","Men's Track and Field",
                                          "Men's Volleyball","Men's Water Polo","Track and Field"))


sparta_men_2021 <- sparta_men_df %>% filter(week > 24 )
sparta_men_2022 <- sparta_men_df %>% filter(week <= 24 )
sparta_women_2021 <- sparta_women_df %>% filter(week > 24 )
sparta_women_2022 <- sparta_women_df %>% filter(week <= 24 )

# Making the plots
sparta_men_plot_2021 <- ggplot(data = sparta_men_2021, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021 Men's teams Sparta Jumps")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in 2021") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
sparta_men_plot_2021
ggsave("sparta_men_plot2021.png",plot = sparta_men_plot_2021,width = 10)
sparta_men_plot_2022 <- ggplot(data = sparta_men_2022, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2022 Men's teams Sparta Jumps")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Week in 2022") +
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
ggsave("sparta_men_plot2022.png",plot = sparta_men_plot_2022,width = 10)
sparta_men_plot_2022



sparta_women_plot_2021 <- ggplot(data = sparta_women_2021, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2021 Women's teams Sparta Jumps")+
  xlab("Week in 2021") +
  scale_y_continuous(labels = scales::percent)+
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
sparta_women_plot_2021
ggsave("sparta_women_plot_2021.png",plot = sparta_women_plot_2021,width = 10)
sparta_women_plot_2022 <- ggplot(data = sparta_women_2022, aes(x = week, y = freq, group = team, col = team))+
  ggtitle("2022 Women's teams Sparta Jumps")+
  xlab("Week in 2022") +
  scale_y_continuous(labels = scales::percent)+
  ylab("Rate of completion")+
  geom_line()+
  geom_point()
sparta_women_plot_2022
ggsave("sparta_women_plot_2022.png",plot = sparta_women_plot_2022,width = 10)
# Boxplot with number of unique jumps studentds did through out the year.
# graph them side by side


# RPE
rpe_df1 <- read.csv("~/Downloads/rpe_df1.csv")


# Forms 2 work on rpe
Query.1 <- read.csv("~/Desktop/DataRes/Query 3.csv") # this is the FORMS2 table
rpe_df <- Query.1[, c(1,2,3,5)]
rpe_df$date <- as.Date(substr(rpe_df$recorded_at,start = 1, stop = 10))# extract the dates
rpe_df <- rpe_df[rpe_df$date >= "2021-09-01",]

rpe_df <- rpe_df[,c(1,4,5)] # only need this two column
rpe_df$date <- floor_date(as.Date(rpe_df$date), "weeks", week_start = 1)
rpe_df$rpe <- ifelse(is.na(rpe_df$rpe) == TRUE, 0, 1 )
rpe_completion_total <- rpe_df %>% group_by(org_id,date) %>% summarise(times = sum(rpe))



