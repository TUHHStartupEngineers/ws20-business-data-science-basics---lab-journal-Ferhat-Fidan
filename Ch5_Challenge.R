library(tidyverse)
library(lubridate)
library(readxl)
getwd()
#Task 1 ----

#Had to download xlsx version since, the current csv version that is hosted on that webpage only contains weekly data since dec. 14th and not daily data anymore
covid_data_tbl <- read_xlsx("COVID-19.xlsx")
CountriesOfInterest <- c("Germany", "France", "Spain", "United_Kingdom", "United_States_of_America")

covid_data_tbl %>% class()
covid_data_tbl
#For all countries of interest

cov_19_tbl1 <- covid_data_tbl %>% filter(countriesAndTerritories %in% CountriesOfInterest) %>%  select(dateRep, day, month, year, cases, countriesAndTerritories) %>% 
  rename("Country" = "countriesAndTerritories") %>% mutate(dateRep = dateRep %>% date()) %>%rename("Date" = "dateRep") %>% 
  arrange(Date) %>% group_by(Country) %>% mutate(Cumulative_Cases = cumsum(cases)) %>% ungroup() %>% arrange(Country,Date) 

#For only Europe, and considering it a "country"

cov_19_tbl2 <- covid_data_tbl %>% filter(continentExp == "Europe") %>% select(dateRep, day, month, year, cases, countriesAndTerritories,continentExp) %>% 
  mutate(dateRep = dateRep %>% date()) %>% rename("Date" = "dateRep", "Country" ="countriesAndTerritories", "Continent" = "continentExp") %>% 
  arrange(Date) %>% group_by(Date) %>% summarise(cases = sum(cases)) %>% ungroup() %>%
  mutate(Cumulative_Cases = cumsum(cases)) %>% mutate(day = day(Date),month = month(Date),year = year(Date), Country = "Europe") %>% 
  relocate(Cumulative_Cases, .after = Country) %>% relocate(cases, .before = Country)

#Combining the Country of Interest Table with the Europe Table

cov_19_tbl <- cov_19_tbl1 %>% bind_rows(cov_19_tbl2) %>% mutate(Country = Country %>% str_replace_all("_"," "))  %>% arrange(Country,Date) 

label_data <- cov_19_tbl %>% filter(Country == "Europe"|Country == "United States of America") %>% 
  arrange(desc(Date)) %>% slice(1:2) %>% select(Date, Country, Cumulative_Cases)

cov_19_tbl %>% filter((Country == "Europe"|Country == "United States of America")&Date == "2020-12-14")


library(ggthemes)
install.packages("ggrepel")
library(ggrepel)

RColorBrewer::display.brewer.all() 
RColorBrewer::brewer.pal(n = 8, name = "Greys")[8]

#geom_line(aes(x = Date, y = Cumulative_Cases, group = Country, color = Country))

cov_19_tbl %>% ggplot(aes(Date, Cumulative_Cases)) + geom_line(aes(x = Date, y = Cumulative_Cases, group = Country, color = Country)) +  
  
  geom_label_repel( 
    #hjust = "inward"-0.5,
    data=cov_19_tbl %>% filter((Country == "Europe"|Country == "United States of America")&Date == "2020-12-14"), # Filter data first
    aes(label= scales::comma(Cumulative_Cases, big.mark = ".", decimal.mark = ",")),
    nudge_x = -50,
    box.padding   = 0.35, 
    point.padding = 0.5,
    #fill = cov_19_tbl$Country,
    #fill = c("red","blue"),
    segment.color = 'white'
    
  )+
  
  scale_x_date(date_breaks = "1 month", date_labels = "%B")+ theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, 
                                                    prefix = "",
                                                    decimal.mark = ",",
                                                    suffix = "M"))+ 
   labs(   title = "Cumulative Cases of the Covid-19 Pandemic",
          subtitle = "Numbers for USA, Europe and selected European countries tracked til December 14th, 2020",
          x = "Year 2020",
          y = "Cumulative Cases",
          color = "Continent/ Country",
          caption = "Source of Data: https://opendata.ecdc.europa.eu/covid19")+
  
  
theme(panel.grid.major  = element_line(color = "#D9D9D9"), 
      panel.grid.minor  = element_line(color = "#D9D9D9"),
      panel.background = element_rect(fill = "#737373"),
      panel.border = element_rect(color = "black", fill = NA),
      axis.ticks = element_line(color = "black", size = 1),
      axis.text = element_text(color = "#252525", size = 14),
      axis.title.x = element_text(color = "#737373", size = 18),
      axis.title.y = element_text(color = "#252525", size = 16),
     # plot.caption = element_text(hjust = 0.5, size = 20),
      legend.position="bottom",
      legend.title = element_text(color = "#252525", size = 14),
      legend.text =  element_text(color = "#252525", size = 12 )) 
  
  #lubridate::month(cov_19_tbl$Date, label = T, abbr = F, locale = "en-US") %>% unique()
  
  #p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
  #p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
  
#Task 2 ----
  #Goal: Visualize the distribution of the mortality rate (deaths / population) with geom_map()
# Step 1: Library, and load world data with latitude and longitude data----  
library(maps)
world <- map_data("world")

#Check feasibility of approach----

#The following shows the correct death numbers (21.975 for Gemany as of 14/12/2020 which is identical to the numbers claimed by RKI for the same day
#https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Dez_2020/2020-12-14-en.pdf?__blob=publicationFile
covid_data_tbl %>% select(dateRep, deaths, popData2019, countriesAndTerritories)%>% arrange(countriesAndTerritories, desc(dateRep)) %>%
  group_by(countriesAndTerritories) %>% summarise(total_deaths = sum(deaths)) %>% ungroup() %>% filter(countriesAndTerritories == "Germany")

#Create data set with total deaths, as sum over observation period, which is roughtly 1 year by default----
#For an actual calculation of mortality rate, the observation period would have been set to exactly 1 year
cov_total_death <- covid_data_tbl %>% select(dateRep, deaths, popData2019, countriesAndTerritories)%>% arrange(countriesAndTerritories, desc(dateRep)) %>%
  group_by(countriesAndTerritories) %>% summarise(total_deaths = sum(deaths)) %>% ungroup() 

#Since we lose some info by summarise (alternatively we could have just used mutate to not lose any columns), create a tibble which contains the popdata
pop_data <- covid_data_tbl %>% select(countriesAndTerritories, popData2019,) %>% arrange(countriesAndTerritories) %>% distinct()

#Join Popdata and tibble with total deaths back together, add column that calculates mortality rates, and change the way certain countries are named
#In order to make them match the values in the World dataset
cov_mortality <- cov_total_death %>%left_join(pop_data) %>% mutate(mortality_rate = total_deaths/popData2019) %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    )) 

#Join cov_mortal data and world data together
cov_mortal_world <-world %>% left_join(cov_mortality, by = c("region" = "countriesAndTerritories"))

#Create a variable which stores the maximum mortality rate. We will use it to configure the breaks of our scales.
Max_Mortality_rate <-cov_mortal_world %>% filter(!is.na(mortality_rate)) %>%pull(mortality_rate) %>% max()

#Final Step: Create the Plot----
ggplot(cov_mortal_world, aes(map_id = region, fill = mortality_rate))+
  geom_map(map = cov_mortal_world,  color = "#969696")+
  expand_limits(x = cov_mortal_world$long, y = cov_mortal_world$lat)+
  scale_fill_gradient(low = "red", high = "black", breaks = seq(from = 0, to = Max_Mortality_rate,length.out=6), labels = scales::percent_format(accuracy = 0.01L))+
  labs(
    title = "Confirmed COVID-19 deaths in relation to size of population",
    fill="Mortality Rate",
    subtitle = "Over 1.6M deaths worldwide as of  December, 14th 2020",
    x = NULL,
    y = NULL,
    caption = "Source of Data: https://opendata.ecdc.europa.eu/covid19"
    ) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major  = element_line(color = "#D9D9D9"), 
        panel.grid.minor  = element_line(color = "#D9D9D9"),
        panel.background = element_rect(fill = "#252525"))
cov_mortal_world %>% distinct(region,total_deaths) %>% filter(!is.na(total_deaths)) %>%summarise(total_death = sum(total_deaths))%>% glimpse()
