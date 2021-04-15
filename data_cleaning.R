### CLEANING OF RAW DATA
#library(tidyverse)
#fw <- read.csv("FW_GROUP_points.csv")
fw_tib <- as_tibble(fw)

fw_tib$event_year  <- as.numeric(fw_tib$event_year)
fw_tib$latitude    <- as.numeric(fw_tib$latitude)
fw_tib$longitude   <- as.numeric(fw_tib$longitude)

fw_tib <- subset(fw_tib,select = -c(presence,origin,
                                    seasonal,compiler,
                                    yrcompiled,citation,
                                    subpop,source,
                                    basisofrec,catalog_no,
                                    dist_comm,island,tax_comm,
                                    subspecies))

fw_tib <- fw_tib %>%
  mutate(area = case_when(latitude > -57 & latitude < 70 & 
                            longitude > -160 & longitude < -33 ~ 'America',
                          latitude > -38 & latitude < 38 & 
                            longitude > -26 & longitude < 56 ~ 'Africa',
                          latitude > 38 & latitude < 68 & 
                            longitude > -26 & longitude < 30 ~ 'Europe',
                          latitude > 22 & latitude < 74 & 
                            longitude > 35 & longitude < 170 |
                            latitude > -8 & latitude < 22 & 
                            longitude > 55 & longitude < 130  ~ 'Asia',
                          latitude > -8 & latitude < 25 & 
                            longitude > 130 & longitude < 179 |
                            latitude > -60 & latitude < -8 & 
                            longitude > 110 & longitude < 179.9 |
                            latitude > -20 & latitude < -13 & 
                            longitude > -180 & longitude < -170 ~ 'Oceania',
                          TRUE ~ "other"))


#### selecting LAST OBSERVATION OF SPECIES in EACH AREA

last_date_tib <- fw_tib %>% 
  filter(event_year >= "2000") %>%
  group_by(id_no,area) %>% 
  arrange(desc(event_year)) %>% 
  slice(1) %>% 
  ungroup()

last_date_tib <- rename(last_date_tib , order = order_)

last_date_tib$category <- case_when(
  last_date_tib$category == "CR" ~ "Critically Endangered",
  last_date_tib$category == "DD" ~ "Data Deficient",
  last_date_tib$category == "EN" ~ "Endangered",
  last_date_tib$category == "LC" ~ "Least Concern",
  last_date_tib$category == "NT" ~ "Near Threatened",
  last_date_tib$category == "VU" ~ "Vulnerable",
  last_date_tib$category == "EX" ~ "Extinct"
)


last_date_tib$class <- case_when(
  last_date_tib$class == "ACTINOPTERYGII" ~ "Actinopterygii (Ray-Finned Fish)",
  last_date_tib$class == "BIVALVIA" ~ "Bivalvia (Molluscs)",
  last_date_tib$class == "GASTROPODA" ~ "Gastropoda (Snails and Slugs)",
  last_date_tib$class == "INSECTA" ~ "Insecta (Insects)",
  last_date_tib$class == "LILIOPSIDA" ~ "Liliopsida (Plants)",
  last_date_tib$class == "MAGNOLIOPSIDA" ~ "Magnoliopsida (Plants)",
  last_date_tib$class == "MALACOSTRACA" ~ "Malacostraca (Crustaceans)",
  last_date_tib$class == "POLYPODIOPSIDA" ~ "Polypodiopsida (Plants)"
)

last_date_tib$kingdom <- case_when(
  last_date_tib$kingdom == "ANIMALIA" ~ "animals",
  last_date_tib$kingdom == "PLANTAE" ~ "plants")

last_date_tib$phylum <- case_when(
  last_date_tib$phylum == "CHORDATA" ~ "Chordata (animals)",
  last_date_tib$phylum == "ARTHROPODA" ~ "Arthropoda (animals)",
  last_date_tib$phylum == "MOLLUSCA" ~ "Mollusca (animals)",
  last_date_tib$phylum == "TRACHEOPHYTA" ~ "Tracheophyta (plants)"
)

last_date_tib <- last_date_tib %>% 
  filter(complete.cases(category),area != "other")


threatened = c("Endangered","Critically Endangered","Vulnerable")
near_threatened = c("Near Threatened")

last_date_tib <- last_date_tib %>%
  mutate(threat_level = case_when(category %in% threatened ~ "Threatened",
                                  category %in% near_threatened ~ "Near Threatened",
                                  category == "Least Concern" ~ "Least Concern",
                                  category == "Data Deficient" ~ "Data Deficient",
                                  category == "Extinct" ~ "Extinct"
  ))


#write.csv(last_date_tib,"last_date.csv")