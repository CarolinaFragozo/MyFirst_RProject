$ X
?datasets
id <-insurance_with_date
View(id)
id$X
id$age
id$sex
id$bmi
id$children
id$smoker
id$region
id$charges
id$date
library(tidyverse)

#Make factors sex and region 
reformatte <- id 
mutate (across(c(sex, region), factor),
        #sex= factor(sex),
        #region= factor (region),
View(id)        
id <-id %>%
  mutate(new_sex = as.factor(sex),
         new_region = as.factor(region)
  )
id$new_sex
id$new_region

id %>% mutate(across(c(sex, region),factor),
              #sex=factor (sex)
              #region= (region),
gt2_children = children > 2,
smokes = smoker == "yes",
date_6m = date + months(6)
# date_6m = date + 30.4 * 6

#load library 
install.packages(ggplot)
installed.packages(ggplot)
install.packages(ggplot)
library(ggplot)

install.packages(ggplot2)
library(dplyr)
library(lubridate)
library(
library(readr)
View (covid_cantons_2020_06)

# filter data frame covid: 
# only keep confirmed cases in the cantons of Zurich, Bern and Vaud 
# in the first half of the year 2020

covid_cantons_2020_06 <- covid_cantons_2020_06 %>% filter(datum <= ymd("2020-06-30") 
                                                          & (geoRegion == "ZH" | geoRegion == "BE" | geoRegion == "VD"))


# write data frame covid_cantons_2020 to a csv file
write_csv (x=covid_cantons_2020_06, file = "data/processed/covid_cantons_2020_06.csv")



# load library
library(ggplot2)

plot_covid_point_v0 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
geom_point()
plot_covid_point_v0


plot_covid_point_v0 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_line(mapping = aes (group = geoRegion))
plot_covid_point_v0

plot_covid_point_v0 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_col(position = "stack")
plot_covid_point_v0



#Ebola 
# sort data_ebola by date
ebola<- ebola %>% arrange(Date)
View(ebola)

# filter data_ebola: cumulative number of confirmed cases in Guinea, 
# Liberia and Sierra Leone before 31 March 2015 
ebola_cum_cases <- ebola %>% 
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
  filter(date <= ymd("2015-03-31") & 
           (country == "Guinea" | country ==  "Liberia" | country == "Sierra Leone"))



plot_ebola_point_v0 <- ggplot(data = ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point()
plot_ebola_point_v0



plot_ebola_point_v0 <- ggplot(data = ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(aes (group = country))
plot_ebola_point_v0


plot_ebola_point_v0 <- ggplot(data = ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(position = "stack")
plot_ebola_point_v0


#COLORS 
plot_covid_point_v0 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_point(alpha = 0.7, colour = "black", fill = "blue") 
plot_covid_point_v0       



plot_covid_point_v1 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_point(alpha = 0.7, colour = "green", fill = "blue", 
             shape = 21, size = 1.5, stroke = 1.5) + geom_line(colour = "red")
plot_covid_point_v1






plot_covid_point_v1 <- ggplot(data = covid_cantons_2020_06, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_line(mapping = aes (group = geoRegion),
            alpha = 0.7, colour = "pink", linetype = "solid", linewidth = 1.5)
plot_covid_point_v1


plot_covid_col_v1 <- ggplot(data = covid_cantons_2020_06, 
                            mapping = aes(x = datum, y = entries)) + 
  geom_col(position = "stack", alpha = 0.7, fill = "orange", 
           linetype = "solid", linewidth = 0.5, width = 0.7)
plot_covid_col_v1


# create point plot
plot_ebola_point_v1 <- ggplot(data = ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point(alpha = 0.7, colour = "purple", fill = "pink", 
             shape = 22, size = 2.5, stroke = 1.5)
plot_ebola_point_v1


# create line plot
plot_ebola_line_v1 <- ggplot(data = ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, colour = "orange", linetype = "dashed", linewidth = 1.5)
plot_ebola_line_v1



# create column plot
plot_ebola_col_v1 <- ggplot(data = ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(alpha = 0.7, colour = "red", fill = "green", 
           linetype = "solid", linewidth = 0.2, position = "stack", width = 0.7)
plot_ebola_col_v1







