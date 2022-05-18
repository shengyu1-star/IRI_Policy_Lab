library(plm)

#Fix effect using year and country as indicator
min_fixeffect_id <- plm(VDEM.el_rating ~ VDEM.EMB_cap + VDEM.turnout + nelda17 + nelda3 + nelda4 + nelda5 + nelda29 + 
                          FH.fh_total + WB.FDI + WB.urban_pop +  literacy_imputed + VDEM.elec_viol + VDEM.international_monitors_present + 
                          VDEM.access_to_public_services_pol_group +  VDEM.regime_support_by_an_ethnic_group + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group,
                        data = merged_datasets3,
                        index = c("country", "year"), 
                        model = "within")

se_fixeffect_id <- plm(VDEM.EMB_cap + nelda3 + nelda29 + FH.fh_total + WB.urban_pop + 
                          `VDEM.elec_viol` + VDEM.international_monitors_present + VDEM.access_to_public_services_pol_groupp + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group,
                        data = merged_datasets3,
                        index = c("country", "year"), 
                        model = "within")

#Fix effect for year and country 
min_fixeffect_id <- plm(VDEM.el_rating ~ VDEM.EMB_cap + VDEM.turnout + nelda17 + nelda3 + nelda4 + nelda5 + nelda29 + 
                          FH.fh_total + WB.FDI + WB.urban_pop +  literacy_imputed + VDEM.elec_viol + VDEM.international_monitors_present + 
                          VDEM.access_to_public_services_pol_group +  VDEM.regime_support_by_an_ethnic_group + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group + factor(country),
                        data = merged_datasets3,
                        index =  "year", 
                        model = "within")

se_fixeffect_id <- plm(VDEM.EMB_cap + nelda3 + nelda29 + FH.fh_total + WB.urban_pop + 
                          `VDEM.elec_viol` + VDEM.international_monitors_present + VDEM.access_to_public_services_pol_groupp + 
                          VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group + factor(country),
                        data = merged_datasets3,
                        index = "year", 
                        model = "within")
