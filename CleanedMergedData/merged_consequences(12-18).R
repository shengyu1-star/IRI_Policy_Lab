library(tidyverse)
library(states)
library(readxl)
library(rworldmap)
library(tidyr)
library(ggplot2)
library(texreg)
library(stringr)
library(timeDate)
library(reshape2)
library(tidytext)
rm(list = ls())

# read data
merged_3 <- read.csv("merged_datasets3.csv")

african_countries <- unique(merged_3$country)
ACLED <- read_xlsx("Africa_1997-2022_May06.xlsx")
ACLED <- ACLED %>% filter(YEAR >= 2011 & COUNTRY %in% african_countries) %>% 
  select(EVENT_DATE, 
         YEAR, 
         COUNTRY, 
         EVENT_TYPE, 
         SUB_EVENT_TYPE, 
         INTERACTION, 
         FATALITIES)

elec_date <- read_csv("African Election Dates (2012-2018) - Sheet1.csv")
elec_date <- elec_date[1:95, ]

# correct data type
new_time <- strptime(as.character(elec_date$Electiondate), "%m/%d/%Y")
elec_date$Electiondate <- new_time
elec_date$year <- as.numeric(format(elec_date$Electiondate, format="%Y"))

ACLED_date <- strptime(as.character(ACLED$EVENT_DATE), "%Y-%m-%d")
ACLED$EVENT_DATE <- ACLED_date

# join election date with main data
merged_3 <- merged_3 %>% filter(year>=2011)
merged_3 <- left_join(merged_3, elec_date, by=c("year", "country"))

# join ACLED with main data
merged_3 <- right_join(merged_3, ACLED, 
                       by=c("country" = "COUNTRY", "year" = "YEAR"))

# calculate difference in date and mutate binary variables indicating before or after election date
merged_3 <- merged_3 %>% 
  mutate(days_off = 
           as.numeric(difftime(EVENT_DATE, Electiondate, units = "days"))) %>% 
  filter(days_off <= 30 & days_off >= -30) %>% 
  mutate(before_after = ifelse(days_off<0, -1, 1), 
         on_date = ifelse(days_off==0, 1, 0)) %>%
  mutate(before_after = before_after - on_date) %>%
  select(-on_date)
# here is a little bit messy, but finally in before_after -1 is before, 0 is on the election date and 1 is after.


# create summary variable of total unrest and total fatalities
violent_events <- c("Remote explosive/landmine/IED", 
                    "Protest with intervention", 
                    "Armed clash",
                    "Violent demonstration",
                    "Attack",
                    "Mob violence", 
                    "Shelling/artillery/missile attack",
                    "Excessive force against protesters",
                    "Air/drone strike",
                    "Abduction/forced disappearance",
                    "Grenade",
                    "Government regains territory",
                    "Suicide bomb",
                    "Non-state actor overtakes territory",
                    "Sexual violence")

non_violent_events <- c("Peaceful protest",
                        "Change to group/activity",
                        "Other",
                        "Looting/property destruction",
                        "Non-violent transfer of territory",
                        "Agreement",
                        "Headquarters or base established",
                        "Disrupted weapons use",
                        "Arrests")
merged_3 <- merged_3 %>% 
  mutate(violent = ifelse(SUB_EVENT_TYPE %in% violent_events, 1, 0))

merged_3 <- merged_3 %>% 
  group_by(country_year, Electiondate, violent, before_after) %>%
  mutate(number_of_events = n(), number_of_fatalities = sum(FATALITIES)) %>% ungroup()






merged_3 <- merged_3 %>% select(country_year, 
                               country, 
                               year, 
                               Electiondate, 
                               before_after, 
                               violent,  
                               number_of_events, 
                               number_of_fatalities) %>%
  distinct()

before_after <- list()
for (i in merged_3$before_after) {
  if(i==-1){
    i="before"
  } else if(i==0){
    i="on_date"
  } else {
    i="after"
  }
  before_after <- append(before_after, i)
}

merged_3$before_after <- c(unlist(before_after))
merged_3 <- merged_3 %>% mutate(violent=ifelse(violent==1, "violent", "non_violent"))
merged_3$Electiondate <- as.character(merged_3$Electiondate)

merged_3$combined <- str_c(merged_3$before_after, "_", merged_3$violent)

merged_3 <- merged_3 %>% select(-c(before_after, violent))




merged_3 <- melt(merged_3, 
                id.vars = c("country_year", 
                            "country", 
                            "year", 
                            "Electiondate", 
                            "combined"), 
                measure.vars = c("number_of_events", "number_of_fatalities"))

merged_3 <- select(merged_3, -c(Electiondate))


merged_consequences <- dcast(merged_3, country_year+country+year~combined+variable, fun.aggregate = sum)
write.csv(merged_consequences, "merged_consequences(12-18)")




