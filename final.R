# BAE 495 Final Project

#load packages
library(tidyverse)
library(lubridate)
library(tidytext)
library(modelr)
library(corrr)
library(broom)


#import data
base_flow <- read_csv("data/base_flow.csv")
rain <- read_csv("data/rainfall_new.csv")
pop <- read_csv("data/pop_data.csv")


#fix date format for data sets 
base_flow %>%
  mutate(date = ymd(date)) -> base_flow

rain %>%
  mutate(date = mdy(date)) -> rain

pop %>%
  mutate(date = mdy(Date)) -> pop

#sort data by year
base_flow%>%
  mutate(year = year(date)) -> base_flow

rain %>%
  mutate(year = year(date)) -> rain
rain %>%
  mutate(precip = as.numeric(as.character(rain$daily_precip))) -> daily_precip
drop_na(daily_precip) -> daily_precip 

pop %>%
  mutate(year = year(date)) -> pop

pop %>%
  mutate(pop_div = (Population/1000000)) -> pop

#group each by year and average data
base_flow %>%
  group_by(year)%>%
  summarise(mean_flow = mean(flow.cfs)) -> avg_base

daily_precip %>% 
  group_by(year) %>%
  summarise(mean_rain = mean(precip)) -> avg_rain


#join data sets 
full_join(avg_base,avg_rain,  by = "year") -> avg_by_year

inner_join(pop, avg_base, by = "year") -> avg_base_pop

#Average Baseflow over previous years
avg_by_year%>%
  ggplot(mapping = aes(year, mean_flow))+
  geom_col()+
  labs( x= "Year", y = "Average Base Flow (cfs)", title = "Average Daily Base Flow (cfs) in Pigeon House Creek")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


#Population vs Baseflow 
avg_base_pop%>%
  ggplot(mapping = aes(pop_div, mean_flow))+
  geom_line()+
  labs(x = "Population (millions)", y = "Average Daily Base Flow (cfs)", title = "Population vs. Average Daily Base Flow (1987-2014)")+
  theme(plot.title = element_text(hjust = 0.5))



base_flow %>%
  mutate(date = ymd(date)) %>%
  mutate(year = year(date)) -> base_flow

mutate(base_flow$flow.cfs, flow_amount = if_else(base_flow$flow.cfs) >= 0.5, "Greater than 0.5", "Less than 0.5 ")


base_flow %>%
  mutate(flow_amount = if_else(flow.cfs >= 0.5, 0,1 )) -> base_flow 


base_flow %>%
  group_by(year)

inner_join(pop, base_flow, "year") -> flow_amount_year_pop




flow_amount_year_pop %>%
  ggplot(mapping = aes(year, pop_div, color = flow_amount))+
  geom_point()+
  labs(x = "Year", y = "Population (millions)")





inner_join(rain, base_flow, "year") -> flow_amount_year_rain


mod <- lm(flow.cfs ~ Population, data = flow_amount_year_pop)

mod$coefficients
mod$fitted.values

summary(mod)
flow_amount_year_pop%>%
  ggplot(mapping = aes(pop_div, flow.cfs))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0.0219,  colour = "red")
 


                  

mod2 <- lm(year ~ Population, data = flow_amount_year_pop)

mod2$coefficients
mod2$fitted.values

summary(mod2)
flow_amount_year_pop%>%
  ggplot(mapping = aes(year, pop_div))+
  geom_point()+
  geom_abline(intercept = 0, slope = ,  colour = "red")
