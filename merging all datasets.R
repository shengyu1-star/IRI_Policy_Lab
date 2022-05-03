library(tidyverse)
#library(remotes)
#remotes::install_github("xmarquez/democracyData") #need this package to download FH data
library(democracyData)
library(WDI)
library(countrycode)
library(readxl)
library(vistime)


#fh <- download_fh(verbose = FALSE) #uncomment these for first time running program
#polity5 <- download_polity_annual(verbose = FALSE)

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
iri_countries <- c("Kenya", "Chad", "Somalia", "Sudan", "Nigeria", 
                   "Sierra Leone", "Congo", "Zimbabwe", "Angola", "Libya", "Congo Kinshasa", "Congo (Kinshasa)")
iri_countries_ios2 <- countrycode(iri_countries, origin = 'country.name', destination = 'iso2c')

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

PEI_election_mod <- PEI_election %>% 
  filter(country %in% iri_countries) %>% 
  select(election, year, country, office, PR, CL, gdp_pc_ppp, polity2, durable, development, 
         PEIIndexi, rating, lawsi, proceduresi, boundariesi, voteregi, partyregi, mediai, 
         financei, votingi, counti, resultsi, EMBsi, Turnout, DatePrevious) %>% 
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

polity5_mod <- polity5 %>% 
  filter(polity_annual_country %in% iri_countries) %>% 
  mutate(polity_annual_country = case_when(polity_annual_country == "Congo Kinshasa" ~ "Congo",
                                           TRUE ~ polity_annual_country),
         country_year = paste(polity_annual_country, year, sep = "-")) %>%
  select(country_year, fragment, polity2, durable, parreg, parcomp, regtrans)

fh_mod <- fh %>% 
  filter(fh_country %in% iri_countries) %>% 
  mutate(fh_country = case_when(fh_country == "Congo (Kinshasa)" ~ "Congo",
                                TRUE ~ fh_country),
         country_year = paste(fh_country, year, sep = "-")) %>% 
  select(country_year, status, fh_total)

iri_WDI <- WDI(country = iri_countries_ios2, indicator = c("NY.GDP.PCAP.KD", 
                                                           "BX.KLT.DINV.CD.WD",
                                                           "FP.CPI.TOTL.ZG",
                                                           "SI.POV.GINI"))
iri_WDI <- iri_WDI %>% 
  rename("GDP per capita (constant 2015 US$)" = "NY.GDP.PCAP.KD",
         "Foreign direct investment, net inflows (BoP, current US$" = "BX.KLT.DINV.CD.WD",
         "Inflation, consumer prices (annual %)" = "FP.CPI.TOTL.ZG",
         "Gini index" = "SI.POV.GINI") %>%
  mutate(country = ifelse(country == "Congo, Dem. Rep.", "Congo", country),
         country_year = paste(country, year, sep = "-")) %>% 
  select(!c(iso2c, country, year))

NELDA <- read_excel("Raw data/NELDA.xls")

nelda_mod <- NELDA %>% 
  filter(country %in% iri_countries) %>% 
  select(country, year) %>%
  mutate(country_year = paste(country, year, sep = "-"))

ECAV <- read_excel("Raw data/ECAV datatset_Version 1.2.xls")

ecav_mod <- ECAV %>% 
  filter(country %in% iri_countries) %>% 
  mutate(year = substr(Electiondate, 1, 4)) %>% 
  mutate(country_year = paste(country, year, sep = "-"))

MGEP <- read_csv("Raw data/MGEP_S2016_Release.csv")

mgep_mod <- MGEP %>% 
  filter(election == 1) %>% 
  filter(targetstate %in% iri_countries) %>% 
  mutate(country_year = paste(targetstate, year, sep = "-")) 

DECO <- read_csv("Raw data/DECO_v.1.0.csv")

deco_mod <- DECO %>% 
  filter(country %in% iri_countries) %>% 
  mutate(country_year = paste(country, year, sep = "-"))

SCAD <- read_csv("Raw data/SCAD2018Africa_Final.csv")

scad_mod <- SCAD %>% 
  filter(countryname %in% iri_countries) %>% 
  filter(issue1 == 1) %>% 
  mutate(country_year = paste(countryname, eyr, sep = "-"))

CIRI <- read_excel("Raw data/CIRI Data 1981_2011 2014.04.14.xlsx")

ciri_mod <- CIRI %>% 
  filter(CTRY %in% iri_countries) %>% 
  mutate(country_year = paste(CTRY, YEAR, sep = "-"))

afrobarometer <- read_excel("Raw data/afrobarometer country_years.xlsx")

afrobarometer_mod <- afrobarometer %>% 
  mutate(country_year = paste(country, year, sep = "-"))

PFI <- read_csv("Raw data/press_freedom_index.csv")
pfi_mod <- PFI %>% 
  filter(`Country Name` %in% iri_countries) %>% 
  pivot_longer(`2001`:`2021`) %>% 
  mutate(country_year = paste(`Country Name`, name, sep = "-")) %>% 
  rename(`Press Freedom Index` = value) %>% 
  select(country_year, `Press Freedom Index`)


#### seeing country_year differences between all datasets ########
qed_country_year <- QED.mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>%
  mutate(qed_country_year = country_year) 

iaep_country_year <- IAEP_mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>% 
  mutate(iaep_country_year = country_year) 

pei_country_year <- PEI_election_mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>%
  mutate(pei_country_year = country_year)  

polity5_country_year <- polity5_mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>% 
  mutate(polity5_country_year = country_year)


fh_country_year <- fh_mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>% 
  mutate(fh_country_year = country_year) 

wdi_country_year <- iri_WDI %>% 
  select(country_year) %>% 
  distinct(country_year) %>% 
  mutate(wdi_country_year = country_year)


nelda_country_year <- nelda_mod %>% 
  select(country_year) %>% 
  distinct(country_year) %>% 
  mutate(nelda_country_year = country_year) 


ecav_country_year <- ecav_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(ecav_country_year = country_year) 

mgep_country_year <- mgep_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(mgep_country_year = country_year)

deco_country_year <- deco_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(deco_country_year = country_year)

scad_country_year <- scad_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(scad_country_year = country_year)

ciri_country_year <- ciri_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(ciri_country_year = country_year)

afrobarometer_country_year <- afrobarometer_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(afrobarometer_country_year = country_year)

pfi_country_year <- pfi_mod %>% 
  select(country_year) %>% 
  distinct(country_year)  %>% 
  mutate(pfi_country_year = country_year)



df_list <- list(qed_country_year, iaep_country_year, pei_country_year, nelda_country_year, 
                ecav_country_year, mgep_country_year, deco_country_year)

election_diff <- reduce(df_list, full_join) %>% 
  left_join(polity5_country_year) %>% left_join(fh_country_year) %>% left_join(wdi_country_year) %>% 
  left_join(scad_country_year) %>% left_join(ciri_country_year) %>% left_join(afrobarometer_country_year) %>% 
  left_join(pfi_country_year) %>% 
  arrange(country_year) 

election_diff_long <- election_diff %>% 
  select(!country_year) %>% 
  pivot_longer(cols = qed_country_year:pfi_country_year) %>% 
  drop_na() %>% 
  separate(value, into = c("country", "year"), sep = "-") %>% 
  mutate(dataset = str_sub(name, end = -14)) %>% 
  mutate(year = as.POSIXct(year, tryFormats = "%Y"),
         color = case_when(country == "Angola" ~ "red",
                           country == "Kenya" ~ "blue",
                           country == "Chad" ~ "yellow",
                           country == "Somalia" ~ "green",
                           country == "Sudan" ~ "salmon",
                           country == "Nigeria" ~ "pink",
                           country == "Sierra Leone" ~ "brown",
                           country == "Congo" ~ "orange",
                           country == "Zimbabwe" ~ "gray",
                           country == "Libya" ~ "tan",
                           TRUE ~ "black"))

gg_vistime(election_diff_long, col.color = "color", col.group = "dataset", 
           col.start = "year", col.end = "year", show_labels = FALSE, linewidth = 5)

# description of graph: 
# This graph visualizes the differences between each dataset. Each dot represent one country's specific national election for one year. This country-year data-type
# is standard in the literature and the key value that we use to merge the various datasets. All the relevant databases are listed on the y-axis and 
# each dot represents that the dataset has data for that country year's election. The dots are color-coded to country as listed in the legend. Taken as a whole, 
# this graph shows both the time and spatial limitations for the various election integrity datasets for the countries the IRI works in. 


# export just full list of iri country_years to be used for merging:
election_diff %>% 
  select(country_year) %>% 
  write_csv("CleanedMergedData/all_iri_country_year_elections.csv")
