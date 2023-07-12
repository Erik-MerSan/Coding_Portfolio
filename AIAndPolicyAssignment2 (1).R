# Name: Erik Mercado
# ut eid: emm4376

library(tidyverse)
library(dplyr)

# filtering out DAT and Disposed at arraign
new_pre_adj <- pretrial_adjust |>
  filter(!Arrest_Type %in% "DAT") |>
  filter(!Release.Decision.at.Arraign %in% "Disposed at arraign") 

# create indicators for ror and rearrest
new_pre_adj <- new_pre_adj |>  
  mutate(ror_idctr = ifelse(Release.Decision.at.Arraign == "ROR", 1, 0),
         rearrest_indctr = ifelse(rearrest != "No Arrest", 1,  0))

#create numeric for ages
new_pre_adj <- new_pre_adj |>
  mutate(Age_at_Arrest = as.numeric(Age_at_Arrest),
         Age_at_Crime = as.numeric(Age_at_Crime))

# create the month, day, and year variables for offense, arrest, and first_arraign
# If it is not outputting the results you want, try running without "suppressWarnings()" first, then rerun with "suppressWarnings()"
new_pre_adj <- new_pre_adj |>
  separate(Arrest_Date, into = c("Arrest_Month", "Arrest_Day", "Arrest_Year"), sep = "/")
new_pre_adj <- new_pre_adj |>  
  suppressWarnings(separate(Arrest_Date, into = c("Arrest_Month", "Arrest_Day", "Arrest_Year"), sep = "/"))
new_pre_adj <- new_pre_adj |>
  separate(Offense_Date, into = c("Offense_Month", "Offense_Day", "Offense_Year"), sep = "/")
new_pre_adj <- new_pre_adj |>  
  suppressWarnings(separate(Offense_Date, into = c("Offense_Month", "Offense_Day", "Offense_Year"), sep = "/"))
new_pre_adj <- new_pre_adj |>  
  separate(First_Arraign_Date, into = c("First_Arraign_Month", "First_Arraign_Day", "First_Arraign_Year"), sep = "/")
new_pre_adj <- new_pre_adj |>  
  suppressWarnings(separate(First_Arraign_Date, into = c("First_Arraign_Month", "First_Arraign_Day", "First_Arraign_Year"), sep = "/"))

# create arraign type indicators         
# These indicators are meant to show the type of crime that was committed
new_pre_adj <- new_pre_adj |>          
  mutate(obstruction_indctr = ifelse(Arraign.Charge.Category == "Obstruction", 1, 0),
         rape_indctr = ifelse(Arraign.Charge.Category == "Rape", 1, 0),
         endangering_welfare_indctr = ifelse(Arraign.Charge.Category == "Endangering Welfare", 1,  0),
         unlicensed_operation_indctr = ifelse(Arraign.Charge.Category == "Unlicensed Operation",  1, 0),
         homicide_related_indctr = ifelse(Arraign.Charge.Category == "Homicide Related", 1, 0),
         other_vtl_indctr = ifelse(Arraign.Charge.Category == "Other VTL", 1 ,0),
         other_sex_offense_indctr = ifelse(Arraign.Charge.Category == "Other Sex Offense", 1, 0),
         larceny_indctr = ifelse(Arraign.Charge.Category == "Larceny", 1, 0),
         property_indctr = ifelse(Arraign.Charge.Category == "Property",  1, 0),
         assault_indctr = ifelse(Arraign.Charge.Category == "Assault", 1, 0),
         criminal_possesion_of_weapon_indctr = ifelse(Arraign.Charge.Category == "Criminal Possession of a Weapon", 1, 0),
         burglary_indctr = ifelse(Arraign.Charge.Category == "Burglary", 1, 0),
         criminal_trespass_indctr = ifelse(Arraign.Charge.Category == "Criminal Trespass", 1, 0),
         criminal_contempt_indctr = ifelse(Arraign.Charge.Category =="Criminal Contempt", 1, 0),
         drug_indctr = ifelse(Arraign.Charge.Category == "Drug", 1, 0),
         aggravated_harassment_indctr = ifelse(Arraign.Charge.Category == "Aggravated Harassment", 1, 0),
         strangulation_indctr = ifelse(Arraign.Charge.Category == "Strangulation", 1, 0),
         robbery_indctr = ifelse(Arraign.Charge.Category == "Robbery", 1, 0),
         other_arraign_charge_category_indctr = ifelse(Arraign.Charge.Category == "Other", 1, 0),
         dwi_indctr = ifelse(Arraign.Charge.Category == "DWI", 1, 0),
         conspiracy_indctr = ifelse(Arraign.Charge.Category == "Conspiracy", 1, 0))

# create race indicators  
# these indicators are meant to show the race of the individual
new_pre_adj <- new_pre_adj |>          
  mutate(wht_indctr = ifelse(Race == "White", 1, 0), 
         blk_indctr = ifelse(Race == "Black", 1, 0),
         amind_alsknat_indctr = ifelse(Race == "American Indian/Alaskan Native", 1, 0),
         aspi_indctr = ifelse(Race == "Asian/Pacific Islander", 1, 0), 
         other_race_indctr = ifelse(Race == "Other", 1, 0), 
         unkwn_race_idctr = ifelse(Race == "Unkown", 1, 0))

# create gender indicators     
# these indicators are meant to show the gender of the individual
new_pre_adj <- new_pre_adj |>          
  mutate(female_indctr = ifelse(Gender == "Female", 1, 0), 
         male_indctr = ifelse(Gender == "Male", 1,  0),
         unkwn_gndr_indctr = ifelse(Gender == "Unkown", 1, 0))

# create the unique judge ids
judge_factor <- factor(new_pre_adj$Judge_Name)
judege_levels <- levels(judge_factor)
judge_id <- as.integer(judge_factor)

# add judge ids to dataset
new_pre_adj <- new_pre_adj |>
  mutate(judge_id = judge_id)

# create and add judge release rates to a dataset
release_rates <- pretrial_adjust |>
  filter(!Arrest_Type %in% "DAT") |>
  filter(!Release.Decision.at.Arraign %in% "Disposed at arraign") |>
  group_by(Judge_Name) |>
  summarize(release_rate_j = sum(Release.Decision.at.Arraign == "ROR")/length(Release.Decision.at.Arraign)) |>
  mutate(release_rate_j = release_rate_j)

# join release rates to main dataset
new_pre_adj <- new_pre_adj |>
  left_join(release_rates, by = "Judge_Name")

# create missing data indicator
new_pre_adj[is.na(new_pre_adj) | new_pre_adj == "NULL" | new_pre_adj == "" | new_pre_adj == " "] <- " "
for(col_name in colnames(new_pre_adj)) {
  if(any(new_pre_adj[, col_name] == " ")) {
    new_pre_adj[[paste0(col_name, "_missing_data_indctr")]] <- as.integer(new_pre_adj[, col_name] == " ")
  }
}

# summarize variables
summary(new_pre_adj)

# create a table that shows release rates and rearrest rates by race and gender
 new_pre_adj |>
  select(Race, Gender, rearrest, Release.Decision.at.Arraign) |>
  group_by(Race, Gender) |>
  summarize(release_rate = sum(Release.Decision.at.Arraign == "ROR")/length(Release.Decision.at.Arraign),
    rearrest_rate = sum(rearrest != "NULL" & rearrest != " " )/ length(rearrest))

# share of people missing data for rearrest
new_pre_adj |>
  summarize(missing_rearrest = (sum(rearrest == "NULL" | rearrest == " ")  / length(rearrest)))

# density plot for release_rate_j
new_pre_adj |>
  ggplot(aes(x=release_rate_j))+
  geom_density()
