library(tidyverse)

setwd("~/Documents/School/Grad/Spring 2022/IRI Policy Lab/IRI_Policy_Lab")

# converts QED into csv, then load back in
load("Raw data/31461-0002-Data.rda")
da31461.0002 %>% write_csv("CleanedMergedData/QED.csv")
QED.da <- read_csv("CleanedMergedData/QED.csv")

# Loading IAEP data
IAEP <- read_csv("Raw data/IAEPv2_0_2015labels.csv")

# loading PEI data
PEI_election <- read_delim("Raw data/PEI election-level data (PEI_7.0) v2 09-05-2019.tab", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

# IRI countries
iri_countries <- c("Kenya", "Chad", "Somaliland", "Somalia", "Sudan", "Nigeria", 
                   "Sierra Leone", "Congo", "Zimbabwe")

# other country groups
all_africa <- c()
subsarahan_africa <- c()
south_america <- c()
all_developing_countries <- c()

# select IRI countries and relevant indicators
QED.mod <- QED.da %>% 
  select(COUNTRY, YEAR, EXELEC, LEGELEC, SELTRANS, SELRUNOFF, SF1, SF2, SF3, SA1, SA2, 
         SR0STR, SR11CHEAT, SR13VIOL, SR12CAP, SR21CHEAT, SR23VIOL, SR22CAP) %>% 
  mutate(EXELEC = case_when(EXELEC == "(1) Executive Election" ~ 1,
                            EXELEC == "(0) Otherwise" ~ 0),
         LEGELEC = case_when(LEGELEC == "(1) Legislative Election" ~ 1,
                             LEGELEC == "(0) Otherwise" ~ 0),
         SELTRANS = case_when(SELTRANS == "(1) Transition or Transitional Election" ~ 1,
                              SELTRANS == "(0) Otherwise" ~ 0),
         SELRUNOFF = case_when(SELRUNOFF == "(1) Run-off Election" ~ 1,
                               SELRUNOFF == "(0) Otherwise" ~ 0)) %>% 
  rename("Transitional election" = SELTRANS,
         "Runoff election" = SELRUNOFF,
         "Freedom to change government" = SF1,
         "Freedom of association" = SF2, 
         "Freedom of speech" = SF3,
         "Overall election quality" = SA1,
         "Extent of election problems" = SA2, 
         "Pre-election legal structural enviornment" = SR0STR, 
         "Pre-election political conditions" = SR11CHEAT,
         "Pre-election violence/unrest" = SR13VIOL,
         "Pre-election administrative capacity" = SR12CAP,
         "Election day explicit cheating" = SR21CHEAT,
         "Election day violence/unrest" = SR23VIOL,
         "Election day administrative capacity" = SR22CAP) %>% 
  filter(COUNTRY %in% iri_countries) %>% 
  mutate(country_year = paste(COUNTRY, YEAR, sep = "-"),
         dup = duplicated(country_year, fromLast = TRUE),
         LEGELEC = ifelse(dup | LEGELEC == "1", 1, 0),
         EXELEC = ifelse(dup | EXELEC == "1", 1, 0)) %>% 
  filter(!duplicated(country_year)) %>% 
  select(!dup)

#####--------------------------------------#####
#IAEP

IAEP_mod <- IAEP %>% 
  select(cname, year, election, electexec, electleg, electboth, amyear, constage, govstruct, 
         regstruct, electtime, elecperiod, lpartynom, epartynom, lelecsystem, eelect, eelectrules, parties,
         stateparty, legcompet, execcompet, electboy, electprot, protestpart, legelig, execelig) %>% 
  #select(!c(cabr, ccode), ) %>% 
  rename("Age of constitution (years)" = constage,
         "Constitution amended this year?" = amyear,
         "Regional government reps selected" = regstruct,
         "Election scheduled" = electtime,
         "Election period" = elecperiod,
         "Do party nominations field leg candidates?" = lpartynom,
         "Do party nominations field executive candidates?" = epartynom,
         "Leg elec sys" = lelecsystem,
         "Exec elec sys" = eelect,
         "Exec elec rules" = eelectrules,
         "Num of parties that hold >= 5% of legislature" = parties,
         "Official state party?" = stateparty,
         "Leg elec competetive?" = legcompet,
         "Exec elec competetive?" = execcompet,
         "Boycotted by major party?" = electboy,
         "Did election cause protest/violence?" = electprot,
         "% of pop voting in leg elec" = legelig,
         "% of pop voting in exec elec" = execelig) %>% 
  filter(cname %in% iri_countries, 
         election == "Yes") %>% 
  mutate(country_year = paste(cname, year, sep = "-"))

## join together IAEP and QED
IAEP_QED <- IAEP_mod %>% 
  full_join(QED.mod, by = "country_year") %>% 
  mutate(cname = ifelse(is.na(cname), COUNTRY, cname),
         year = ifelse(is.na(year), YEAR, year),
         electexec = ifelse(is.na(electexec), EXELEC, electexec),
         electleg = ifelse(is.na(electleg), LEGELEC, electleg)) %>% 
  select(!c(COUNTRY, YEAR, election)) %>% 
  mutate(electexec = case_when(electexec == 0 ~ "No",
                               electexec == 1 ~ "Yes",
                               electexec == "Yes" ~ "Yes",
                               electexec == "No" ~ "No"),
         electleg = case_when(electleg == 0 ~ "No",
                              electleg == 1 ~ "Yes",
                              electleg == "Yes" ~ "Yes",
                              electleg == "No" ~ "No"),
         electboth = case_when(electleg == "Yes" & electexec == "Yes" ~ "Yes",
                              TRUE ~ "No"))



####### ----------------------- #######
# PEI 2019 (7.0) election data
PEI_election_mod <- PEI_election %>% 
  filter(country %in% iri_countries) %>% 
  select(election, year, country, office, PR, CL, gdp_pc_ppp, polity2, durable, development, 
         PEIIndexi, rating, lawsi, proceduresi, boundariesi, voteregi, partyregi, mediai, 
         financei, votingi, counti, resultsi, EMBsi, Turnout, DatePrevious,) %>% 
  rename("FreedomHouse political rights scale (1-7)" = PR,
         "FreedomHouse civil liberities scale (1-7)" = CL,
         "polity combined score (-10-10)" = polity2,
         "# years since last regime transition" = durable,
         "WB income level" = development,
         "PEI index of electoral integrity, imputed" = PEIIndexi,
         "Rating of electoral integrity (1-10)" = rating,
         "Electoral laws index (0-100), imputed" = lawsi,
         "Electoral prodcedures index (0-100), imputed" = proceduresi,
         "Voting district boundaries index (0-100), imputed" = boundariesi,
         "Voter regisration index (0-100), imputed" = voteregi,
         "Party/candidate registration index (0-100), imputed" = partyregi,
         "Media coverage index (0-100), imputed" = mediai,
         "Campaign finance index (0-100), imputed" = financei,
         "Voting process index (0-100), imputed" = votingi,
         "Vote count index (0-100), imputed" = counti,
         "Voting results/reactions index (protests/disputes) (0-100), imputed" = resultsi,
         "Electoral authorities index (0-100), imputed" = EMBsi) %>% 
  mutate(country_year = paste(country, year, sep = "-"))

###-------------------------------#####
# combine IAEP_QED with PEI:
IAEP_QED_PEI <- IAEP_QED %>% 
  full_join(PEI_election_mod, by = "country_year") %>% 
  mutate(cname = ifelse(is.na(cname), country, cname),
         year.x = ifelse(is.na(year.x), year.y, year.x),
         electexec = case_when(!is.na(electexec) ~ electexec,
                               is.na(electexec) & office == 1 ~ "Yes",
                               is.na(electexec) & office == 0 ~ "No"),
         electleg = case_when(!is.na(electleg) ~ electleg,
                              is.na(electleg) & office == 0 ~ "Yes",
                              is.na(electleg) & office == 1 ~ "No"),
         electboth = case_when(electleg == "Yes" & electexec == "Yes" ~ "Yes",
                               TRUE ~ "No")) %>% 
  select(!c(election, year.y, country, office, EXELEC, LEGELEC)) %>% 
  select(country_year, everything()) %>% 
  arrange(country_year)

#exporting IAEP_QED_PEI
IAEP_QED_PEI %>% write_csv("CleanedMergedData/IAEP_QED_PEI_clean.csv")


