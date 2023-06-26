library(dplyr)
library(tidyverse)

housing_prices <- read.csv("/Users/erik/Downloads/prop50_train_anonym.csv")

housing_dat <- housing_prices |>
  mutate(pool_indctr = ifelse(pool == "Y", 1, 0),
         ceiling_fans_indctr = ifelse(ceiling_fans == "Y", 1, 0),
         fenced_yard_indctr = ifelse(fenced_yard == "Y", 1, 0),
         sec_sys_indctr = ifelse(security_deposit == "Y", 1, 0),
         gated_community_indctr = ifelse(gated_community == "Y", 1, 0),
         pets_allowed_indctr = ifelse(pets_allowed == TRUE, 1, 0),
         garage_indctr = ifelse(garage == "Y", 1, 0),
         app_dryer_indctr = ifelse(app_dryer == "Y", 1, 0),
         app_washer_indctr = ifelse(app_washer == "Y", 1, 0),
         app_hookups_indctr = ifelse(app_hookups == "Y", 1, 0),
         app_dishwasher_indctr = ifelse(app_dishwasher == "Y", 1, 0),
         app_stove_indctr = ifelse(app_stove == "Y", 1, 0),
         app_microwave_indctr = ifelse(app_microwave == "Y", 1, 0), 
         app_ref_indctr = ifelse(app_ref == "Y", 1, 0),
         app_gd_indctr = ifelse(app_gd == "Y", 1, 0),
         fireplace_indctr = ifelse(fireplace == TRUE, 1, 0),
         onsite_laundry_indctr = ifelse(onsite_laundry == TRUE, 1, 0), 
         sewer_paid_indctr = ifelse(sewer_paid == "Y", 1, 0),
         water_paid_indctr = ifelse(water_paid == "Y", 1, 0),
         electric_paid_indctr = ifelse(electric_paid == "Y", 1, 0),
         gas_paid_indctr = ifelse(gas_paid == "Y", 1, 0),
         oil_paid_indctr = ifelse(oil_paid == "Y", 1, 0),
         cable_paid_indctr = ifelse(cable_paid == "Y", 1, 0), 
         pest_control_paid_indctr = ifelse(pest_control_paid == "Y", 1, 0),
         trash_paid_indctr = ifelse(trash_paid == "Y", 1, 0),
         electric_indctr = ifelse(electric == "Y", 1, 0),
         gas_indctr = ifelse(gas == "Y", 1, 0),
         sewer_indctr = ifelse(sewer == "Y", 1, 0),
         septic_indctr = ifelse(septic == "Y", 1, 0),
         city_water_indctr = ifelse(city_water == "Y", 1, 0),
         well_water_indctr = ifelse(well_water == "Y", 1, 0),
         lawn_care_indctr  = ifelse(lawn_care == "Y", 1, 0),
         ac_heat_type_indctr = ifelse(ac_heat_type == "Y", 1, 0),
         gas_heat_indctr = ifelse(heat_type == "Natural Gas", 1, 0),
         electric_heat_indctr = ifelse(heat_type == "Electric", 1, 0),
         gas_hw_indctr = ifelse(hw_type == "Natural Gas", 1, 0),
         electric_hw_indctr = ifelse(hw_type == "Electric", 1, 0),
         gas_cook_indctr = ifelse(cook_type == "Natural Gas", 1, 0),
         electric_cook_indctr = ifelse(cook_type == "Electric", 1, 0),
         handicap_indctr = ifelse(handicap == "Y", 1, 0),
         ufas_indctr = ifelse(ufas_compliant == TRUE, 1, 0),
         fha_indctr = ifelse(fha_compliant == TRUE, 1, 0))

housing_dat_num <- housing_dat |>
  select(-c(6:13, 15:16, 18:19, 21:68, 70:73, 75, 78, 81:129, 132:155, 157:256))
  
housing_dat_num <- housing_dat_num |>
  select(-c(12))

housing_dat_num <- housing_dat_num |>
  select(-c(14))

housing_clean <- na.omit(housing_dat_num)

set.seed(123)
train_index <- sample(nrow(housing_clean), 0.7 * nrow(housing_clean))
train_data <- housing_clean[train_index, ]
test_data <- housing_clean[-train_index, ]

model_train_data <- train_data |>
  select(-c(1:5))

model <- lm(rent_amount ~ ., data = model_train_data)
summary(model)
pred_rent <- predict(model, newdata = test_data)

oosmse_rent <- mean((test_data$rent_amount - pred_rent)^2)
oosmse_rent

