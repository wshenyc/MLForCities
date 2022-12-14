library(tidyverse)

class_c_2022 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations (1).csv")

class_c_2022_slim <- class_c_2022 %>% 
  mutate(bbl = paste0(BoroID, sprintf("%05d",Block), sprintf("%04d", Lot)),
         bbl = as.numeric(bbl), 
         class_c_flag = 1) %>% 
  select(bbl, class_c_flag) %>% 
  distinct(bbl, .keep_all = T) 

#comparing against predictions
#changing those pred's with 0.5 above as yes

prob <- read_csv("/Users/winnieshen/Documents/GitHub/MLForCities/Code/pred_prob_all.csv")

prob = subset(prob, select=-c(...1))

prob_edit <- prob %>% 
  mutate(dt_flag = if_else(prob_vio_dt > 0.5, 1, 0),
         rf_flag = if_else(prob_vio_rf > 0.5, 1, 0),
         svm_flag = if_else(prob_vio_svm > 0.5, 1, 0),
         nb_flag = if_else(prob_vio_nb > 0.5, 1, 0),
         ) %>% 
  select(bbl, dt_flag, rf_flag, svm_flag, nb_flag) %>% 
  left_join(class_c_2022_slim, by = "bbl")

#filling in 0's
prob_edit$class_c_flag[is.na(prob_edit$class_c_flag)] <- 0

#flag sums
prob_edit$row_sum <- rowSums(prob_edit[ , c("dt_flag","rf_flag","svm_flag","nb_flag")], na.rm=TRUE)

#grouping
prob_sums <- prob_edit %>% 
  mutate(one_flag = if_else(row_sum == 1 & class_c_flag == 1, 1,0),
         two_flag = if_else(row_sum == 2 & class_c_flag == 1, 1,0),
         three_flag = if_else(row_sum == 3 & class_c_flag == 1, 1,0),
         four_flag=if_else(row_sum==4 & class_c_flag == 1, 1, 0)) %>% 
  select( one_flag, two_flag, three_flag, four_flag) %>% 
  summarize(total_one = sum(one_flag),
            total_two = sum(two_flag),
            total_three = sum(three_flag),
            total_four = sum(four_flag))

#how accurate was DT
dt_accuracy <- prob_edit %>% 
  select(bbl, dt_flag, class_c_flag) %>% 
  mutate(true_positive = if_else(dt_flag == 1 & class_c_flag == 1, 1,0),
         false_positive = if_else(dt_flag == 1 & class_c_flag == 0, 1,0),
         true_negative = if_else(dt_flag == 0 & class_c_flag == 0, 1,0),
         false_negative = if_else(dt_flag == 0 & class_c_flag == 1, 1,0)) %>% 
  summarize(model = "dt",
            total_true_positive = sum(true_positive),
            total_false_positive = sum(false_positive),
            total_true_negative = sum(true_negative),
            total_false_negative = sum(false_negative)) 

#how accurate was RF
rf_accuracy <- prob_edit %>% 
  select(bbl, rf_flag, class_c_flag) %>% 
  mutate(true_positive = if_else(rf_flag == 1 & class_c_flag == 1, 1,0),
         false_positive = if_else(rf_flag == 1 & class_c_flag == 0, 1,0),
         true_negative = if_else(rf_flag == 0 & class_c_flag == 0, 1,0),
         false_negative = if_else(rf_flag == 0 & class_c_flag == 1, 1,0)) %>% 
  summarize(model = "rf",
            total_true_positive = sum(true_positive),
            total_false_positive = sum(false_positive),
            total_true_negative = sum(true_negative),
            total_false_negative = sum(false_negative)) 


#how accurate was SVM 
svm_accuracy <- prob_edit %>% 
  select(bbl, svm_flag, class_c_flag) %>% 
  mutate(true_positive = if_else(svm_flag == 1 & class_c_flag == 1, 1,0),
         false_positive = if_else(svm_flag == 1 & class_c_flag == 0, 1,0),
         true_negative = if_else(svm_flag == 0 & class_c_flag == 0, 1,0),
         false_negative = if_else(svm_flag == 0 & class_c_flag == 1, 1,0)) %>% 
  summarize(model = "svm",
            total_true_positive = sum(true_positive),
            total_false_positive = sum(false_positive),
            total_true_negative = sum(true_negative),
            total_false_negative = sum(false_negative)) 

#how accurate was NB
nb_accuracy <- prob_edit %>% 
  select(bbl, nb_flag, class_c_flag) %>% 
  mutate(true_positive = if_else(nb_flag == 1 & class_c_flag == 1, 1,0),
         false_positive = if_else(nb_flag == 1 & class_c_flag == 0, 1,0),
         true_negative = if_else(nb_flag == 0 & class_c_flag == 0, 1,0),
         false_negative = if_else(nb_flag == 0 & class_c_flag == 1, 1,0)) %>% 
  summarize(model = "nb",
            total_true_positive = sum(true_positive),
            total_false_positive = sum(false_positive),
            total_true_negative = sum(true_negative),
            total_false_negative = sum(false_negative)) 

accuracy_rates <- rbind(dt_accuracy, rf_accuracy, svm_accuracy, nb_accuracy)

accuracy_rates %>% write_csv("accuracy_rates.csv")

#checking to see what rate a building has a Class C vio out of the four flags
testing <- prob_edit %>% 
  select(bbl, row_sum, class_c_flag) 

correctly_flagged = c(sum(testing$row_sum == 1 & testing$class_c_flag == 1),
                      sum(testing$row_sum == 2 & testing$class_c_flag == 1),
                      sum(testing$row_sum == 3 & testing$class_c_flag == 1),
                      sum(testing$row_sum == 4 & testing$class_c_flag == 1))

incorrect_flagged = c(sum(testing$row_sum == 1 & testing$class_c_flag == 0),
                      sum(testing$row_sum == 2 & testing$class_c_flag == 0),
                      sum(testing$row_sum == 3 & testing$class_c_flag == 0),
                      sum(testing$row_sum == 4 & testing$class_c_flag == 0))

testing_df <- data.frame(model_flag = c(1,2,3,4),
                         correct = correctly_flagged,
                         incorrect = incorrect_flagged
                         )

#filtering to just those bbls that had a row sum greater than 0 and a class c flag
prob_correct <- prob_edit %>% 
  filter(row_sum >= 1 & class_c_flag == 1)
#okay, so it looks like 13,763 BBLs were flagged correctly by at least 1 model
#this does mean the model's missing about 13k BBLs though, so that's not great 

missing_bbls <- setdiff(class_c_2022_slim$bbl, prob_correct$bbl)
# there are 11,316 BBLs that had a Class C violation in 2022 that were not flagged by any model

#okay were those BBLs in the original data set at all lol
missing_bbls_entirely <- setdiff(class_c_2022_slim$bbl, prob_edit$bbl)
#okay, so 4,528 BBLs were not included in the training data set that had a class C violation 

missing_bbls_check <- setdiff(missing_bbls, missing_bbls_entirely)

#checking model accuracy
summary <- prob_edit %>% 
  mutate(dt_pred = if_else(dt_flag == class_c_flag, 1, 0),
         rf_pred = if_else(rf_flag == class_c_flag, 1, 0),
         svm_pred = if_else(svm_flag == class_c_flag, 1, 0),
         nb_pred = if_else(nb_flag == class_c_flag, 1, 0)) %>% 
  select(bbl, dt_pred, rf_pred, svm_pred, nb_pred)

summary_add <- summary %>% 
  summarize(dt_add = sum(dt_pred),
            rf_add = sum(rf_pred),
            svm_add = sum(svm_pred),
            nb_add = sum(nb_pred))

summary_means <- summary %>% 
  summarize(dt_mean = mean(dt_pred),
            rf_mean = mean(rf_pred),
            svm_mean = mean(svm_pred),
            nb_mean = mean(nb_pred))

###gonna make like 4 maps

#by grabbing all the lat & long 

hmc_2017 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")
hmc_2018 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")
hmc_2019 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")
hmc_2020 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")
hmc_2021 <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")

hmc_2017_slim <- hmc_2017 %>% 
  select(BBL, Latitude, Longitude) %>% 
  distinct(BBL, .keep_all = T) %>% 
  write_csv("hmc_2017_slim.csv")

hmc_2018_slim <- hmc_2018 %>% 
  select(BBL, Latitude, Longitude) %>% 
  distinct(BBL, .keep_all = T) %>% 
  write_csv("hmc_2018_slim.csv")

hmc_2019_slim <- hmc_2019 %>% 
  select(BBL, Latitude, Longitude) %>% 
  distinct(BBL, .keep_all = T) %>% 
  write_csv("hmc_2019_slim.csv")

hmc_2020_slim <- hmc_2020 %>% 
  select(BBL, Latitude, Longitude) %>% 
  distinct(BBL, .keep_all = T) %>% 
  write_csv("hmc_2020_slim.csv")

hmc_2021_slim <- hmc_2021 %>% 
  select(BBL, Latitude, Longitude) %>% 
  distinct(BBL, .keep_all = T) %>% 
  write_csv("hmc_2021_slim.csv")

hmc_2022_slim <- class_c_2022 %>% 
  mutate(bbl = paste0(BoroID, sprintf("%05d",Block), sprintf("%04d", Lot)),
         bbl = as.numeric(bbl)) %>% 
  select(bbl, Latitude, Longitude) %>% 
  distinct(bbl, .keep_all = T) %>% 
  write_csv("hmc_2022_slim.csv")

hmc_2017 <- read_csv("hmc_2017_slim.csv")
hmc_2018 <- read_csv("hmc_2018_slim.csv")
hmc_2019 <- read_csv("hmc_2019_slim.csv")
hmc_2020 <- read_csv("hmc_2020_slim.csv")
hmc_2021 <- read_csv("hmc_2021_slim.csv")
hmc_2022 <- read_csv("hmc_2022_slim.csv")

hmc_geo <- rbind(hmc_2017, hmc_2018, hmc_2019, hmc_2020, hmc_2021)

hmc_geo <- hmc_geo %>% 
  rename(bbl = BBL)

hmc_geo <- rbind(hmc_geo, hmc_2022)


prob_edit_geo <- prob_edit %>% 
  left_join(hmc_geo, by = "bbl") %>% 
  distinct(bbl, .keep_all = T) %>% 
  filter(row_sum >=1) 

missing_geo <- prob_edit_geo %>% 
  filter(is.na(Latitude))
#wow 95 are missing lat & long

library(geoclient)

missing_geocoded <- geoclient::geo_bbl_data(missing_geo, bbl)

missing_geocoded <- missing_geocoded %>% 
  select(input_bbl, latitudeInternalLabel, longitudeInternalLabel) %>% 
  rename(bbl = input_bbl, 
         Latitude = latitudeInternalLabel,
         Longitude = longitudeInternalLabel)

missing_geocoded <- missing_geocoded %>% 
  mutate(bbl = as.numeric(bbl))

hmc_geo <- rbind(hmc_geo, missing_geocoded)

hmc_geo <- hmc_geo %>% 
  distinct() %>% 
  filter(!is.na(Latitude))

prob_edit_geo <- prob_edit %>% 
  left_join(hmc_geo, by = "bbl") %>% 
  distinct(bbl, .keep_all = T) %>% 
  filter(row_sum > 0 ) %>% 
  write_csv("prob_edit_geo.csv")

prob_edit_geo[prob_edit_geo$dt_flag==1,]

prob_edit_geo <- prob_edit_geo %>% 
  filter(!is.na(Latitude))

#okay maps

library(ggplot2)
library(sf)

cd <- read_sf("/Users/winnieshen/Downloads/nycd_22c/nycd.shp")

coord_model <- st_as_sf(prob_edit_geo, coords = c("Longitude", "Latitude"), crs = 4326)


dt_map <- ggplot() +
  geom_sf(data = cd, 
          fill = "white") +
  geom_sf(data = coord_model[coord_model$dt_flag==1,],
          color = "blue",
          fill= NA) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 
  
rf_map <- ggplot() +
  geom_sf(data = cd, 
          fill = "white") +
  geom_sf(data = coord_model[coord_model$rf_flag==1,],
          color = "green",
          fill= NA) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 


svm_map <- ggplot() +
  geom_sf(data = cd, 
          fill = "white") +
  geom_sf(data = coord_model[coord_model$svm_flag==1,],
          color = "yellow",
          fill= NA) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 

nb_map <- ggplot() +
  geom_sf(data = cd, 
          fill = "white") +
  geom_sf(data = coord_model[coord_model$nb_flag==1,],
          color = "red",
          fill= NA) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 

hmc_2022 <- hmc_2022 %>% 
  filter(!is.na(Latitude))

hmc_2022_coord <- st_as_sf(hmc_2022, coords = c("Longitude", "Latitude"), crs = 4326)

map_2022 <- ggplot() +
  geom_sf(data = coord_model[coord_model$nb_flag==1,],
          color = "orange",
          fill= NA) +
  geom_sf(data = cd, 
          fill = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 