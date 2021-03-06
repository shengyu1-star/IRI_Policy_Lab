library(tidyverse)
library(remotes)
#remotes::install_github("xmarquez/democracyData") #need this package to download FH data
library(democracyData)
library(WDI)
library(countrycode)
library(naniar)
library(readxl)
#library(acled.api) # my key: hIZ0Z3n5e4IenWorw5we
library(gridExtra)
library(glmnet)
library(stargazer)
library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)

#fh <- download_fh(verbose = FALSE) #uncomment these for first time running program
#polity5 <- download_polity_annual(verbose = FALSE)

setwd("~/Documents/School/Grad/Spring 2022/IRI Policy Lab/IRI_Policy_Lab")

## Read in all datasets
# IRI countries
iri_countries <- c("Kenya", "Chad", "Somalia", "Sudan", "Nigeria", 
                   "Sierra Leone", "Congo", "Zimbabwe", "Angola", "Libya", 
                   "Congo Kinshasa", "Congo (Kinshasa)", "Congo, Dem. Rep.")
iri_countries_ios2 <- countrycode(iri_countries, origin = 'country.name', destination = 'iso2c')

all_africa_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Cabo Verde",
                          "Central African Republic", "Chad", "Comoros", "Republic of the Congo", "Congo-Brazzaville", "Congo-Kinshasa", 
                          "Cote d'Ivoire", "Ivory Coast", "Djibouti", "Democratic Republic of the Congo", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", 
                          "Eswatini", "Gambia", "Ghana", "Guinea", "Guinea Bissau", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", 
                          "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                          "Rwanda", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", 
                          "Sudan", "Swaziland", "São Tomé and Príncipe", "Tanzania", "The Gambia", "Togo", "Tunisia", "Uganda", 
                          "Western Sahara", "Zambia", "Zimbabwe")
all_africa_countries_ios2 <- countrycode(all_africa_countries, origin = 'country.name', destination = 'iso2c')

# converts QED into csv, then load back in
load("Raw data/31461-0002-Data.rda")
da31461.0002 %>% write_csv("Raw data/QED.csv")
QED.da <- read_csv("Raw data/QED.csv")

IAEP <- read_csv("Raw data/IAEPv2_0_2015labels.csv")

PEI_election <- read_delim("Raw data/PEI election-level data (PEI_7.0) v2 09-05-2019.tab", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)
iri_WDI <- WDI(country = iri_countries_ios2, indicator = c("NY.GDP.PCAP.KD", 
                                                           "BX.KLT.DINV.CD.WD",
                                                           "FP.CPI.TOTL.ZG",
                                                           "SI.POV.GINI",
                                                           "SP.URB.TOTL.IN.ZS"))
africa_WDI_raw <- WDI(country = all_africa_countries_ios2, indicator = c("NY.GDP.PCAP.KD", 
                                                              "BX.KLT.DINV.CD.WD",
                                                              "FP.CPI.TOTL.ZG",
                                                              "SI.POV.GINI",
                                                              "SP.URB.TOTL.IN.ZS", 
                                                              "SE.ADT.LITR.ZS"))

NELDA <- read_excel("Raw data/NELDA.xls")
ECAV <- read_excel("Raw data/ECAV datatset_Version 1.2.xls") #see ecav note below
#MGEP <- read_csv("Raw data/MGEP_S2016_Release.csv")
#DECO <- read_csv("Raw data/DECO_v.1.0.csv")
#SCAD <- read_csv("Raw data/SCAD2018Africa_Final.csv")
CIRI <- read_excel("Raw data/CIRI Data 1981_2011 2014.04.14.xlsx")
afrobarometer <- read_csv("Raw data/afrobarometer_free_and_fair.csv")
PFI <- read_csv("Raw data/press_freedom_index.csv")
vDEM <- read_csv("Raw data/V-Dem-CY-Full+Others-v12.csv")

#ECAV starting point
ECAV_specific_dates <- ECAV %>% 
  filter(country %in% all_africa_countries) %>% 
  select(country, Electiondate) %>% 
  mutate(country_elc = paste(country, Electiondate, sep = "-")) %>% 
  distinct(country_elc, .keep_all = TRUE) %>% 
  select(!country_elc) %>% 
  mutate(Electiondate = as.character(Electiondate),
         year = as.numeric(as.character(str_sub(Electiondate, 1, 4)))) %>% 
  filter(year > 2000)


write_csv(ECAV_specific_dates, "ECAV_specific_dates.csv")

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
  mutate(country_year = paste(COUNTRY, YEAR, sep = "-"),
         dup = duplicated(country_year, fromLast = TRUE),
         LEGELEC = ifelse(dup | LEGELEC == "1", 1, 0),
         EXELEC = ifelse(dup | EXELEC == "1", 1, 0)) %>% 
  filter(!duplicated(country_year)) %>% 
  select(!dup, "QED.Freedom of speech", "QED.Freedom of association", "QED.Freedom to change government",
         "QED.Runoff election") %>% 
  rename(`country` = COUNTRY)

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
         "IAEP.% of pop voting in exec elec" = execelig,
         "IAEP.protestpart" = protestpart) %>% 
  mutate(country_year = paste(cname, year, sep = "-")) %>% 
  filter(election == "Yes") %>% 
  rename(country = cname)

IAEP.QED <- IAEP_mod %>% 
  full_join(QED.mod, by = "country_year") %>% 
  mutate(year = ifelse(is.na(year), YEAR, year),
         electexec = ifelse(is.na(electexec), EXELEC, electexec),
         electleg = ifelse(is.na(electleg), LEGELEC, electleg)) %>% 
  select(!c(YEAR, country.x, country.y, election, year)) %>% 
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
  select(!c(election, country, year, office, DatePrevious))

polity5_mod <- polity5 %>% 
  mutate(polity_annual_country = case_when(polity_annual_country == "Congo Kinshasa" ~ "Congo",
                                           TRUE ~ polity_annual_country),
         country_year = paste(polity_annual_country, year, sep = "-")) %>%
  select(country_year, fragment, polity2, durable, parreg, parcomp, regtrans) %>% 
  rename_with(!country_year, .fn = ~ paste0("POLITY5.", .x))

fh_mod <- fh %>% 
  mutate(fh_country = case_when(fh_country == "Congo (Kinshasa)" ~ "Congo",
                                TRUE ~ fh_country),
         country_year = paste(fh_country, year, sep = "-")) %>% 
  select(country_year, status, fh_total) %>% 
  rename_with(!country_year, .fn = ~ paste0("FH.", .x))

iri_WDI <- iri_WDI %>% 
  rename("GDP per capita (constant 2015 US$)" = "NY.GDP.PCAP.KD",
         "Foreign direct investment, net inflows (BoP, current US$" = "BX.KLT.DINV.CD.WD",
         "Inflation, consumer prices (annual %)" = "FP.CPI.TOTL.ZG",
         "Gini index" = "SI.POV.GINI",
         "%pop living in urban" = "SP.URB.TOTL.IN.ZS") %>%
  mutate(country = ifelse(country == "Congo, Dem. Rep.", "Congo", country),
         country_year = paste(country, year, sep = "-")) %>% 
  select(!c(iso2c, country, year)) %>% 
  rename_with(!country_year, .fn = ~ paste0("WB.", .x))

africa_WDI <- africa_WDI_raw %>% 
  rename("GDP per capita (constant 2015 US$)" = "NY.GDP.PCAP.KD",
         "Foreign direct investment, net inflows (BoP, current US$" = "BX.KLT.DINV.CD.WD",
         "Inflation, consumer prices (annual %)" = "FP.CPI.TOTL.ZG",
         "Gini index" = "SI.POV.GINI",
         "%pop living in urban" = "SP.URB.TOTL.IN.ZS",
         "adult literacy rate" = "SE.ADT.LITR.ZS") %>%
  group_by(country) %>% 
  fill(`adult literacy rate`, .direction = "updown") %>% 
  ungroup() %>% 
  mutate(country = case_when(country == "Congo, Dem. Rep." ~ "Congo",
                             country == "Egypt, Arab Rep." ~ "Egypt",
                             country == "Cabo Verde" ~ "Cape Verde",
                             TRUE ~ country),
         country_year = paste(country, year, sep = "-")) %>% 
  select(!c(iso2c, country, year)) %>% 
  rename_with(!country_year, .fn = ~ paste0("WB.", .x))


nelda_mod <- NELDA %>% 
  mutate(country_year = paste(country, year, sep = "-")) %>% 
  select(country_year, nelda1, nelda3, nelda4, nelda5, nelda11, nelda17, nelda18, nelda29,
         nelda30, nelda32, nelda45, nelda46, nelda47, nelda48, nelda49) %>% 
  filter(!duplicated(country_year)) 


ciri_mod <- CIRI %>% 
  mutate(country_year = paste(CTRY, YEAR, sep = "-")) %>% 
  select(country_year, PHYSINT, NEW_EMPINX, ELECSD, WOPOL, INJUD) %>% 
  replace_with_na_all(condition = ~.x == -77) %>% 
  replace_with_na_all(condition = ~.x == -999) %>% 
  rename_with(!country_year, .fn = ~ paste0("CIRI.", .x)) 

afrobarometer_mod <- afrobarometer %>% 
  mutate(country_year = paste(Country, Year, sep = "-")) %>% 
  drop_na(`Not free and fair`) %>% 
  mutate(across("Not free and fair":"Completely free and fair", 
                ~ as.numeric(as.character(str_sub(.x, end = -2)))/100)) 

write_csv(afrobarometer_mod, "Raw Data/afrobarometer_mod.csv")
#need to make afrobarameter for all african countries, maybe...
afrobarometer_mod <- read_csv("Raw Data/afrobarometer_mod_complete.csv")

afrobarometer_mod <- afrobarometer_mod %>% 
  select(country_year, "Election Integrity (10=free/fair, 0=unfree/unfair)") %>% 
  rename("AFROBAR.Election Integrity (10=free/fair, 0=unfree/unfair)" =  "Election Integrity (10=free/fair, 0=unfree/unfair)") 

afrobarometer_mod_imputed <- afrobarometer_mod %>% 
  mutate(country = str_sub(country_year, 1, -6),
         year = as.numeric(as.character(str_sub(country_year, -4, -1)))) %>% 
  group_by(country_year) %>% 
  mutate(year_imp = list(seq(year))) %>% 
  ungroup() %>% 
  unnest(year_imp) %>% 
  filter(year_imp > 2000 & year_imp < 2021) %>% 
  filter()
  
afrobarometer_mod %>% 
  mutate(country = str_sub(country_year, 1, -6),
         year = as.numeric(as.character(str_sub(country_year, -4, -1)))) %>% 
  filter(country == "Benin") %>% 
  mutate(previous_year = dplyr::lag(year, n = 1))


pfi_mod <- PFI %>% 
  pivot_longer(`2001`:`2021`) %>% 
  mutate(country_year = paste(`Country Name`, name, sep = "-")) %>% 
  rename(`Press Freedom Index` = value) %>% 
  select(country_year, `Press Freedom Index`) 
         
# v2eltrnout
# took out: v2elembcap, v2elembcap_osp, v2elpeace_ord, v2elpeace
vDEM_mod <- vDEM %>% 
  select(country_name, year, v2elfrfair_osp, v2eltrnout, v2elvaptrn, v2elembcap_osp, v2elembcap_ord,
         e_peaveduc,v2xel_elecparl, v2xel_elecpres, v2elpeace_osp, v2elintmon, 
         v2peapspol, v2peapssoc, v2regsupgroups_6, v2regimpgroup, v2regpower) %>% 
  filter(country_name %in% all_africa_countries) %>% 
  mutate(country_name = case_when(country_name == "The Gambia" ~ "Gambia",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "Eswatini" ~ "Swaziland",
                                  TRUE ~ country_name)) %>% 
  filter(year > 2000) %>% 
  filter(v2xel_elecparl == 1 | v2xel_elecpres == 1) %>% 
  mutate(country_year = paste(country_name, year, sep = "-")) %>% 
  select(!c(country_name, year)) %>% 
  rename(VDEM.elction_free_fair = v2elfrfair_osp,
         VDEM.turnout = v2eltrnout,
         VDEM.VAP_turnout = v2elvaptrn,
         VDEM.EMB_capacity_osp = v2elembcap_osp,
         VDEM.EMB_capacity_ord = v2elembcap_ord,
         VDEM.yrs_education = e_peaveduc,
         VDEM.elec_viol = v2elpeace_osp, 
         VDEM.international_monitors_present = v2elintmon,
         VDEM.access_to_public_services_social_group = v2peapssoc,
         VDEM.access_to_public_services_pol_group = v2peapspol,
         VDEM.regime_support_by_an_ethnic_group = v2regsupgroups_6) %>% 
  mutate(VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group = case_when(v2regpower == 6 ~ 1,
                                                                     TRUE ~ 0),
         VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group = case_when(v2regimpgroup == 6 ~ 1,
                                                                                     TRUE ~ 0)) %>% 
  relocate(country_year, .before = VDEM.turnout) %>% 
  select(!v2regpower)
  
  

# to incorporate ECAV would require a lot more work and it's not clear what use it would be, it's just more granualar
# violence data
# ecav_mod <- ECAV %>% 
#   filter(country %in% iri_countries) %>% 
#   mutate(year = substr(Electiondate, 1, 4)) %>% 
#   mutate(country_year = paste(country, year, sep = "-"))

# similar to ecav, skipping mgep because it's a lot of work and only gets more geolocated violence data
# mgep_mod <- MGEP %>% 
#   filter(election == 1) %>% 
#   filter(targetstate %in% iri_countries) %>% 
#   mutate(country_year = paste(targetstate, year, sep = "-")) %>% 
#   select(country_year, part, viop, active) %>% 
#   rename_with(!country_year, .fn = ~ paste0("MGEP.", .x)) 

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

# New Africa country-election merge:

country_elec_list <- list(IAEP.QED, PEI_election_mod, nelda_mod, vDEM_mod)

merged_datasets2 <- reduce(country_elec_list, full_join) %>% 
  mutate(country = str_sub(country_year, end = -6)) %>% 
  filter(country %in% all_africa_countries) %>% 
  left_join(polity5_mod) %>% left_join(fh_mod) %>% left_join(africa_WDI) %>% 
  left_join(ciri_mod) %>% left_join(pfi_mod) %>% left_join(afrobarometer_mod) %>% 
  relocate(country_year) %>% 
  arrange(country_year) 

#write_csv(merged_datasets2, "merged_datasets2.csv")

merged_datasets3 <- merged_datasets2 %>% 
  select(country_year, electexec, electleg, electboth, v2xel_elecparl, v2xel_elecpres, `AFROBAR.Election Integrity (10=free/fair, 0=unfree/unfair)`,
         `PEI.Rating of electoral integrity (1-10)`, VDEM.elction_free_fair, `PEI.Electoral authorities index (0-100), imputed`, VDEM.EMB_capacity_osp,
         `PEI.Voting results/reactions index (protests/disputes) (0-100), imputed`,
          VDEM.turnout, VDEM.VAP_turnout, nelda17, nelda18, nelda11, nelda45, nelda46, nelda3:nelda5, 
         nelda29:nelda30, POLITY5.fragment:POLITY5.durable, POLITY5.parcomp, FH.fh_total, 
         `WB.GDP per capita (constant 2015 US$)`:`WB.%pop living in urban`, `WB.adult literacy rate`, VDEM.yrs_education,
         `Press Freedom Index`, VDEM.elec_viol, VDEM.international_monitors_present, VDEM.access_to_public_services_social_group, VDEM.access_to_public_services_pol_group,
         VDEM.regime_support_by_an_ethnic_group, VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group, 
         VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group) %>% 
  mutate(country = str_sub(country_year, end = -6),
         year = as.numeric(as.character(str_sub(country_year, start = -4, end = -1)))) %>% 
  filter(year >= 2000) %>% 
  distinct(country_year, .keep_all = TRUE) %>% 
  mutate(country = as.factor(country)) %>% 
  rename(literacy_imputed = `WB.adult literacy rate`) %>% 
  mutate(across(.cols = nelda17:nelda30, ~ ifelse(.x == "N/A" | .x == "unclear", NA, .x))) %>% 
  mutate(across(.cols = nelda17:nelda30, ~ ifelse(.x == "yes", 1, 0))) %>% 
  mutate(across(.cols = POLITY5.fragment:POLITY5.parcomp, ~ ifelse(.x == -77 | .x == -88, NA, .x))) %>% 
  relocate(c(country, year), .after = country_year) %>% 
  relocate(v2xel_elecpres, .after = electboth) %>% 
  rename(AF.el_rating = `AFROBAR.Election Integrity (10=free/fair, 0=unfree/unfair)`,
         PEI.el_rating = `PEI.Rating of electoral integrity (1-10)`,
         VDEM.el_rating = VDEM.elction_free_fair, 
         PEI.EMB_cap = `PEI.Electoral authorities index (0-100), imputed`,
         VDEM.EMB_cap = VDEM.EMB_capacity_osp, 
         PEI.protest = `PEI.Voting results/reactions index (protests/disputes) (0-100), imputed`,
         WB.GDP_ppc = `WB.GDP per capita (constant 2015 US$)`,
         WB.FDI = `WB.Foreign direct investment, net inflows (BoP, current US$`, 
         WB.inflation = `WB.Inflation, consumer prices (annual %)`,
         WB.gini = `WB.Gini index`,
         WB.urban_pop = `WB.%pop living in urban`,
         press_freedom = `Press Freedom Index`) %>% 
  group_by(country) %>% 
  mutate(VDEM.el_rating.lagged = Lag(VDEM.el_rating))

write_csv(merged_datasets3, "merged_datasets3.csv")

<<<<<<< Updated upstream

# ACLED data
acled <- read_excel("Raw data/Africa_1997-2022_May06.xlsx")

acled_mod <- acled %>% 
  filter(COUNTRY %in% all_africa_countries) %>% 
  filter(YEAR > 2000)
=======
>>>>>>> Stashed changes
