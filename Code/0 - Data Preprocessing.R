#Referred heavily to this script: https://github.com/austensen/violations

#####Set up#####
library(tidyverse)



####Cleaning PLUTO data####
#Source: https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page

#pluto <- read_csv("/Users/winnieshen/Downloads/pluto_22v2.csv")

nycha_names <- c("HOUSING AUTHORITY", "NYCHA", "NEW YORK CITY HOUSING")


#PLUTO contains land use data on all buildings in NYC
#City Planning's data dictionary for all PLUTO fields is available here:
#https://www1.nyc.gov/assets/planning/download/pdf/data-maps/open-data/PLUTODD.pdf?v=22v2

clean_pluto <- pluto %>% 
  janitor::clean_names() %>% 
  filter(easements == 0, !ownername %in% nycha_names) %>%  #removing NYCHA
  transmute(bbl = str_sub(bbl, 1, 10),
            cd = as.character(cd),
            county = recode(borocode, `1` = "061", `2` = "005", `3` = "047", `4` = "081", `5` = "085"),
            tract10 = if_else(str_detect(ct2010, "\\."), str_replace(ct2010, "\\.", ""), str_c(ct2010, "00")),
            tract10 = str_c(county, str_pad(tract10, 6, "left", "0")),
            res_units = unitsres,
            other_units = unitstotal - unitsres,
            year_built = yearbuilt,
            year_reno = pmax(year_built, yearalter1, yearalter2, na.rm = TRUE),
            buildings = numbldgs,
            floors = numfloors, 
            building_class = bldgclass, 
            basement_code = as.character(bsmtcode),
            owner_type = ownertype,
            lot_area = lotarea,
            res_sqft = resarea) %>% 
  mutate_at(vars(year_built, year_reno), funs(if_else(. == 0, NA_real_, .))) %>% 
  filter( # remove vacant land, city-owned, single-family, coops, condos (imperfect)
    res_units >= 3, #focusing only on bldgs with 3 or more res units 
    !owner_type %in% c("C", "M", "O"),
    !building_class %in% c("C6","C8","D0","D4","R0","R1","R2","R3","R4","R5","R6","R7","R8","R9")
  )


#Writing the cleaned PLUTO 
#clean_pluto %>% write_csv("clean_pluto.csv")


####HPD COMPLAINTS DATA####
#Sources: https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints/uwyv-629c
#https://data.cityofnewyork.us/Housing-Development/Complaint-Problems/a2nx-4u46 

hpd_comps <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Complaints.csv")
comp_details <- read_csv("/Users/winnieshen/Downloads/Complaint_Problems.csv")

comps_join <- comp_details %>% 
  left_join(hpd_comps, by = "ComplaintID")

bbl_comp <-   comps_join %>%
  mutate(bbl = paste0(BoroughID, sprintf("%05d", Block), sprintf("%04d", Lot)),
         ReceivedDate = lubridate::mdy(ReceivedDate),
         year = lubridate::year(ReceivedDate),
         space = if_else(UnitType == "APARTMENT", "apt", "bldg")) %>%
  filter(str_detect(Type, "EMERGENCY")) %>%
  group_by(bbl, year, space) %>%
  summarise(complaints = n()) %>%
  ungroup

#slimming PLUTO to just BBL and residential units 
slim_pluto <- clean_pluto %>% 
  select(bbl, res_units)

#normalizing complaint totals by residential units if the complaint is apt-specific
adj_bbl_comp <- bbl_comp %>%
  filter(year %in% c("2017","2018","2019","2020","2021")) %>% 
  left_join(slim_pluto, by = "bbl") %>% 
  filter(res_units !=0, !is.na(res_units)) %>% 
  mutate(adj_bbl_comp = if_else(space == "apt", complaints / res_units, as.double(complaints))) %>%
  group_by(bbl, year) %>%
  summarise(adj_comp = sum(adj_bbl_comp)) %>%
  mutate(geo = "bbl") %>%
  ungroup

adj_bbl_comp_wide <- adj_bbl_comp %>% 
  mutate(geo_year = str_c("comp", geo, year, sep = "_")) %>% 
  select(bbl, geo_year, adj_comp) %>% 
  spread(geo_year, adj_comp, fill = 0)

#Writing CSV 
adj_bbl_comp_wide %>% write_csv("adj_bbl_comp_wide.csv")



####HPD VIOLATIONS####


viol_raw <- read_csv("/Users/winnieshen/Downloads/Housing_Maintenance_Code_Violations.csv")

#designating class c violations as severe 
bbl_viol <- viol_raw %>% 
  mutate(InspectionDate = lubridate::mdy(InspectionDate)) %>% 
  filter(lubridate::year(InspectionDate) %in% 2017:2021) %>% 
  mutate(bbl = paste0(BoroID, sprintf("%05d",Block), sprintf("%04d", Lot)),
         year = lubridate::year(InspectionDate),
         space = if_else(is.na(Apartment), "bldg", "apt"),
         class = if_else(Class == "C", "ser", "oth")) %>% 
  group_by(bbl, year, class, space) %>% 
  summarise(violations = n())

#normalizing violations by residential units if the violation's apt-specific 
adj_bbl_viol <- bbl_viol %>% 
  inner_join(slim_pluto, by = "bbl") %>% 
  mutate(adj_bbl_viol = if_else(space == "apt", violations / res_units, as.double(violations))) %>% 
  group_by(bbl, year, class) %>% 
  summarise(adj_viol = sum(adj_bbl_viol)) %>% 
  mutate(geo = "bbl") %>% 
  ungroup

adj_bbl_viol_wide <- adj_bbl_viol %>% 
  mutate(geo_class_year = str_c("viol", geo, class, year, sep = "_")) %>% 
  select(bbl, geo_class_year, adj_viol) %>% 
  spread(geo_class_year, adj_viol, fill = 0)

adj_bbl_viol_wide %>% write_csv("adj_bbl_viol_wide.csv")


####JOINING####

#Joining the complaints, violations with the PLUTO data  
training_data <- clean_pluto %>% 
  left_join(adj_bbl_comp_wide, by = "bbl") %>% 
  left_join(adj_bbl_viol_wide, by = "bbl") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(matches("^(viol|comp)")), funs(if_else(is.na(.), 0, .))) %>% 
  mutate(outcome = if_else(viol_bbl_ser_2021 > 0, "TRUE", "FALSE")) %>% 
  ungroup


training_data %>% write_csv("housing_data.csv")
