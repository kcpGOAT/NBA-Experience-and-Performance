library(tidyverse)
library(ggthemes)
## This project attempts to measure the association between 
## experience in the NBA and performance in terms of 
## offensive metrics. It only includes players with a average
## career RAPTOR WAR of 5 or greater. 


## I predict that their performance will peak in the middle of
## their time in the NBA, with their performance eventually 
## dropping as their career moves forward toward a conclusion. 


nba_experience <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  rename(raptorWAR = "Raptor WAR") %>%
  rename(raptorO = "Raptor O") %>%
  filter(type == "RS", MPG > 15) %>%
  group_by(name_common) %>%
  mutate(minYear = min(year_id)) %>%
  filter(mean(raptorWAR) > 5 && mean(TS) > 50) %>%
  group_by(name_common, year_id) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorWAR = mean(raptorWAR, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE), 
            experience = year_id - minYear + 1, 
            age = age) %>%
  filter(experience < 22) %>%
  group_by(name_common) %>%
  filter(max(experience) > 10)


## rplot_experienceTS.png
ggplot(data = nba_experience, 
       mapping = aes(x = experience, y = TS)) +
  geom_jitter(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  theme_stata() +
  labs(x = "Years of experience", 
       y = "True Shooting Percentage", 
       title = "The Association between NBA Experience and TS%", 
       caption = "Source: fivethirtyeight") +
  theme(axis.title.y = element_text(vjust = 2), 
        axis.title.x = element_text(vjust = -1))


## rplot_experienceWAR.png
ggplot(data = nba_experience, 
       mapping = aes(x = experience, y = raptorWAR)) +
  geom_jitter(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  theme_stata() +
  labs(x = "Years of experience", 
       y = "RAPTOR wins above replacement", 
       title = "The Association between NBA Experience and RAPTOR WAR", 
       caption = "Source: fivethirtyeight") +
  theme(axis.title.y = element_text(vjust = 2), 
        axis.title.x = element_text(vjust = -1))


## rplot_experienceRAPTORo.png
ggplot(data = nba_experience, 
       mapping = aes(x = experience, y = raptorO)) +
  geom_jitter(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  theme_stata() +
  labs(x = "Years of experience", 
       y = "Offensive RAPTOR Rating", 
       title = "The Association between NBA Experience and Offensive 
       RAPTOR Rating", 
       caption = "Source: fivethirtyeight") +
  theme(axis.title.y = element_text(vjust = 2), 
        axis.title.x = element_text(vjust = -1))


## The plots depict the NBA players generally peak with about
## five to seven years of experience. We can confirm this finding by 
## transforming the current data. 
nba_experience %>% 
  group_by(experience) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorWAR = mean(raptorWAR, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE))


## And given that the average age of a rookie among these data is
## 21.7 years old. 
mean(nba_experience$age[nba_experience$experience == 1])
## We can see that these players usually peak when they are around
## 26-29 years old. 
