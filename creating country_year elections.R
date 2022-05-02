library(tidyverse)
library(remotes)
#remotes::install_github("xmarquez/democracyData") #need this package to download FH data
library(democracyData)
library(WDI)
library(countrycode)

#fh <- download_fh(verbose = FALSE) #uncomment these for first time running program
#polity5 <- download_polity_annual(verbose = FALSE)

setwd("~/Documents/School/Grad/Spring 2022/IRI Policy Lab/IRI_Policy_Lab")

## Read in all datasets
# IRI countries
iri_countries <- c("Kenya", "Chad", "Somalia", "Sudan", "Nigeria", 
                   "Sierra Leone", "Congo", "Zimbabwe", "Angola", "Libya", "Congo Kinshasa", "Congo (Kinshasa)")
iri_countries_ios2 <- countrycode(iri_countries, origin = 'country.name', destination = 'iso2c')

# converts QED into csv, then load back in
load("Raw data/31461-0002-Data.rda")
da31461.0002 %>% write_csv("CleanedMergedData/QED.csv")
QED.da <- read_csv("CleanedMergedData/QED.csv")

IAEP <- read_csv("Raw data/IAEPv2_0_2015labels.csv")

PEI_election <- read_delim("Raw data/PEI election-level data (PEI_7.0) v2 09-05-2019.tab", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)
iri_WDI <- WDI(country = iri_countries_ios2, indicator = c("NY.GDP.PCAP.KD", 
                                                           "BX.KLT.DINV.CD.WD",
                                                           "FP.CPI.TOTL.ZG",
                                                           "SI.POV.GINI"))

NELDA <- read_excel("Raw data/NELDA.xls")
#ECAV <- read_excel("Raw data/ECAV datatset_Version 1.2.xls") #see ecav note below
MGEP <- read_csv("Raw data/MGEP_S2016_Release.csv")
DECO <- read_csv("Raw data/DECO_v.1.0.csv")
#SCAD <- read_csv("Raw data/SCAD2018Africa_Final.csv")

## Prepare all datasets for merge
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
  rename("QED.Transitional election" = SELTRANS,
         "QED.Runoff election" = SELRUNOFF,
         "QED.Freedom to change government" = SF1,
         "QED.Freedom of association" = SF2, 
         "QED.Freedom of speech" = SF3,
         "QED.Overall election quality" = SA1,
         "QED.Extent of election problems" = SA2, 
         "QED.Pre-election legal structural enviornment" = SR0STR, 
         "QED.Pre-election political conditions" = SR11CHEAT,
         "QED.Pre-election violence/unrest" = SR13VIOL,
         "QED.Pre-election administrative capacity" = SR12CAP,
         "QED.Election day explicit cheating" = SR21CHEAT,
         "QED.Election day violence/unrest" = SR23VIOL,
         "QED.Election day administrative capacity" = SR22CAP) %>% 
  filter(COUNTRY %in% iri_countries) %>% 
  mutate(country_year = paste(COUNTRY, YEAR, sep = "-"),
         dup = duplicated(country_year, fromLast = TRUE),
         LEGELEC = ifelse(dup | LEGELEC == "1", 1, 0),
         EXELEC = ifelse(dup | EXELEC == "1", 1, 0)) %>% 
  filter(!duplicated(country_year)) %>% 
  select(!dup)

IAEP_mod <- IAEP %>% 
  select(cname, year, election, electexec, electleg, electboth, amyear, constage, govstruct, 
         regstruct, electtime, elecperiod, lpartynom, epartynom, lelecsystem, eelect, eelectrules, parties,
         stateparty, legcompet, execcompet, electboy, electprot, protestpart, legelig, execelig) %>% 
  rename("IAEP.Age of constitution (years)" = constage,
         "IAEP.Constitution amended this year?" = amyear,
         "IAEP.Regional government reps selected" = regstruct,
         "IAEP.Election scheduled" = electtime,
         "IAEP.Election period" = elecperiod,
         "IAEP.Do party nominations field leg candidates?" = lpartynom,
         "IAEP.Do party nominations field executive candidates?" = epartynom,
         "IAEP.Leg elec sys" = lelecsystem,
         "IAEP.Exec elec sys" = eelect,
         "IAEP.Exec elec rules" = eelectrules,
         "IAEP.Num of parties that hold >= 5% of legislature" = parties,
         "IAEP.Official state party?" = stateparty,
         "IAEP.Leg elec competetive?" = legcompet,
         "IAEP.Exec elec competetive?" = execcompet,
         "IAEP.Boycotted by major party?" = electboy,
         "IAEP.Did election cause protest/violence?" = electprot,
         "IAEP.% of pop voting in leg elec" = legelig,
         "IAEP.% of pop voting in exec elec" = execelig) %>% 
  filter(cname %in% iri_countries, 
         election == "Yes") %>% 
  mutate(country_year = paste(cname, year, sep = "-")) 

IAEP_QED <- IAEP_mod %>% 
  full_join(QED.mod, by = "country_year") %>% 
  mutate(cname = ifelse(is.na(cname), COUNTRY, cname),
         year = ifelse(is.na(year), YEAR, year),
         electexec = ifelse(is.na(electexec), EXELEC, electexec),
         electleg = ifelse(is.na(electleg), LEGELEC, electleg)) %>% 
  select(!c(COUNTRY, YEAR, election, cname, year)) %>% 
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


PEI_election_mod <- PEI_election %>% 
  filter(country %in% iri_countries) %>% 
  select(election, year, country, office, durable, development, 
         PEIIndexi, rating, lawsi, proceduresi, boundariesi, voteregi, partyregi, mediai, 
         financei, votingi, counti, resultsi, EMBsi, Turnout, DatePrevious) %>% 
  rename("PEI.# years since last regime transition" = durable,
         "PEI.WB income level" = development,
         "PEI.PEI index of electoral integrity, imputed" = PEIIndexi,
         "PEI.Rating of electoral integrity (1-10)" = rating,
         "PEI.Electoral laws index (0-100), imputed" = lawsi,
         "PEI.Electoral prodcedures index (0-100), imputed" = proceduresi,
         "PEI.Voting district boundaries index (0-100), imputed" = boundariesi,
         "PEI.Voter regisration index (0-100), imputed" = voteregi,
         "PEI.Party/candidate registration index (0-100), imputed" = partyregi,
         "PEI.Media coverage index (0-100), imputed" = mediai,
         "PEI.Campaign finance index (0-100), imputed" = financei,
         "PEI.Voting process index (0-100), imputed" = votingi,
         "PEI.Vote count index (0-100), imputed" = counti,
         "PEI.Voting results/reactions index (protests/disputes) (0-100), imputed" = resultsi,
         "PEI.Electoral authorities index (0-100), imputed" = EMBsi,
         "PEI.turnout" = Turnout) %>% 
  mutate(country_year = paste(country, year, sep = "-")) %>% 
  select(!c(election, year, country, office, DatePrevious))

polity5_mod <- polity5 %>% 
  filter(polity_annual_country %in% iri_countries) %>% 
  mutate(polity_annual_country = case_when(polity_annual_country == "Congo Kinshasa" ~ "Congo",
                                           TRUE ~ polity_annual_country),
         country_year = paste(polity_annual_country, year, sep = "-")) %>%
  select(country_year, fragment, polity2, durable, parreg, parcomp, regtrans) %>% 
  rename_with(!country_year, .fn = ~ paste0("POLITY5.", .x))

fh_mod <- fh %>% 
  filter(fh_country %in% iri_countries) %>% 
  mutate(fh_country = case_when(fh_country == "Congo (Kinshasa)" ~ "Congo",
                                TRUE ~ fh_country),
         country_year = paste(fh_country, year, sep = "-")) %>% 
  select(country_year, status, fh_total) %>% 
  rename_with(!country_year, .fn = ~ paste0("FH.", .x))


iri_WDI <- iri_WDI %>% 
  rename("GDP per capita (constant 2015 US$)" = "NY.GDP.PCAP.KD",
         "Foreign direct investment, net inflows (BoP, current US$" = "BX.KLT.DINV.CD.WD",
         "Inflation, consumer prices (annual %)" = "FP.CPI.TOTL.ZG",
         "Gini index" = "SI.POV.GINI") %>%
  mutate(country = ifelse(country == "Congo, Dem. Rep.", "Congo", country),
         country_year = paste(country, year, sep = "-")) %>% 
  select(!c(iso2c, country, year)) %>% 
  rename_with(!country_year, .fn = ~ paste0("WB.", .x))


nelda_iri <- NELDA %>% 
  filter(country %in% iri_countries) %>% 
  mutate(country_year = paste(country, year, sep = "-")) %>% 
  select(country_year, nelda1, nelda3, nelda4, nelda5, nelda11, nelda17, nelda18, nelda29,
         nelda30, nelda32, nelda45, nelda46, nelda47, nelda48, nelda49)

# to incorporate ECAV would require a lot more work and it's not clear what use it would be, it's just more granualar
# violence data
# ecav_mod <- ECAV %>% 
#   filter(country %in% iri_countries) %>% 
#   mutate(year = substr(Electiondate, 1, 4)) %>% 
#   mutate(country_year = paste(country, year, sep = "-"))

mgep_mod <- MGEP %>% 
  filter(election == 1) %>% 
  filter(targetstate %in% iri_countries) %>% 
  mutate(country_year = paste(targetstate, year, sep = "-")) %>% 
  select(country_year, part, viop, active) %>% 
  rename_with(!country_year, .fn = ~ paste0("MGEP.", .x))

# similar to ecav, skipping scav because it's a lot of work and only gets more geolocated violence data
# deco_mod <- DECO %>% 
#   filter(country %in% iri_countries) %>% 
#   mutate(country_year = paste(country, year, sep = "-")) %>% 
#   select(country_year, type_of_violence, best, electoral_perpetrator, 
#          electoral_targets, electoral_type, electoral_timing) %>% 
#   rename_with(!country_year, .fn = ~ paste0("DECO.", .x))

# similar to ecav, deco,, skipping scav because it's a lot of work and only gets more geolocated violence data
# scad_mod <- SCAD %>% 
#   filter(countryname %in% iri_countries) %>% 
#   filter(issue1 == 1) %>% 
#   mutate(country_year = paste(countryname, eyr, sep = "-"))

## Merge all datasets
all_iri_country_year_elections <- read_csv("CleanedMergedData/all_iri_country_year_elections.csv")

merged_datasets <- all_iri_country_year_elections %>% 
  left_join(IAEP_QED) %>% 
  left_join(PEI_election_mod) %>% 
  left_join(polity5_mod) %>% 
  left_join(fh_mod) %>% 
  left_join(iri_WDI) %>% 
  left_join(nelda_iri) %>% 
  left_join(mgep_mod) 
  



