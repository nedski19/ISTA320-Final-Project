library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape)
library(viridis)

# read in data
homicide_reports_1980_2014 <- read.csv("data/database.csv")
# filter data: only Arizona
homicide_reports_az <- 
  homicide_reports_1980_2014[homicide_reports_1980_2014$State == "Arizona",]

# bar plot
# Which race is the most common among perpetrators in Arizona in the murders from 1980 - 2014? 
homicide_reports_az %>% 
  ggplot(aes(x = Perpetrator.Race, y = Perpetrator.Count, 
             fill = Perpetrator.Race)) + 
  geom_col() +
  labs(x = "Perpetrator's Race", 
       y = "Count", 
       title = "Race of Perpetrators in Arizona") +
  theme(legend.position = "off") +
  scale_fill_viridis_d()


# bar plot
# Which county in Arizona experienced the most murders from 1980 - 2014?
homicide_reports_az %>%
  ggplot(aes(x = City, y = Incident, group = `City`,
             fill = City)) +
  geom_col() + 
  labs(x = "County", 
       y = "Incidents", 
       title = "Number of Incidents Per County in Arizona") +
  theme(legend.position = "off") + 
  scale_fill_viridis_d()


# time series
# How has the number of murders changed from 1980 - 2014?
homicide_reports_az %>%
  group_by(Year) %>%
  summarise(mean_value = mean(Incident)) %>%
  ggplot(aes(x = Year, y = mean_value)) +
  geom_point() + 
  scale_x_continuous(breaks = seq(1980, 2014, by = 2)) +
  labs(x = "Year", 
       y = "Incidents", 
       title = "Average Number of Incidents Between 1980-2014 in Arizona") +
  theme(legend.position = "off")


# heat map
# How do the number of male victims compare to the number of female victims in Arizona from 1980 - 2014?
homicide_reports_az %>% filter(Victim.Age < 80 & Victim.Sex != "Unknown") %>% 
  count(Victim.Sex, Victim.Age) %>%
  ggplot(aes(x = Victim.Sex, y = Victim.Age, fill = n)) + 
  geom_tile() +
  labs(x = "Victim Sex",
       y = "Victim Age",
       title = "Number of Male Victims vs Female Victims by Age") +
  guides(fill = guide_legend(title = "Count")) +
  scale_fill_viridis_c()
