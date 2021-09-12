library(tidyverse)
library(ggthemes)
## This project attempts to measure the association between 
## experience in the NBA and performance in terms of 
## offensive metrics. It only includes players with a average
## career RAPTOR WAR of 5 or greater. 


## I predict that their performance will peak in the middle of
## their time in the NBA, with their performance eventually 
## dropping as their career moves forward toward a conclusion. 


## We only include regular season performances in which the player played for more than 15 minutes. 
## We also only include players whose mean RAPTOR WAR is greater than 5 and whose mean TS is greater than 50%. 
## Once we calculate their experience, we eliminate players with duplicate names who played long periods of time apart from each other by only including 
## those who have less than 22 years of experience, thereby only including those who have had continuous experience.
## 
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


## We can also run a multivariate linear regression. 


summary(lm(cbind(TS, raptorWAR, raptorO) ~ experience, 
              data = nba_experience))

# Response TS :
#   
#   Call:
#   lm(formula = TS ~ experience, data = nba_experience)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.4580  -2.7865  -0.1864   2.9460  14.5875 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 55.99447    0.22727 246.382   <2e-16 ***
#   experience  -0.06137    0.02432  -2.523   0.0117 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.283 on 1446 degrees of freedom
# Multiple R-squared:  0.004384,	Adjusted R-squared:  0.003695 
# F-statistic: 6.367 on 1 and 1446 DF,  p-value: 0.01173
# 
# 
# Response raptorWAR :
#   
#   Call:
#   lm(formula = raptorWAR ~ experience, data = nba_experience)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.7783 -3.4261 -0.3544  2.7976 15.9976 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.33764    0.23708  39.385   <2e-16 ***
#   experience  -0.23932    0.02537  -9.432   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.468 on 1446 degrees of freedom
# Multiple R-squared:  0.05796,	Adjusted R-squared:  0.05731 
# F-statistic: 88.96 on 1 and 1446 DF,  p-value: < 2.2e-16
# 
# 
# Response raptorO :
#   
#   Call:
#   lm(formula = raptorO ~ experience, data = nba_experience)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.3031 -1.6757 -0.2085  1.4842  8.7606 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.95018    0.12900  22.870  < 2e-16 ***
#   experience  -0.08726    0.01381  -6.321 3.46e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.431 on 1446 degrees of freedom
# Multiple R-squared:  0.02689,	Adjusted R-squared:  0.02621 
# F-statistic: 39.95 on 1 and 1446 DF,  p-value: 3.46e-10
