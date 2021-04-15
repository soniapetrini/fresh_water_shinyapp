### PARALLEL COMPUTATION - MULTIDPLYR
# This package allows to split the dataset, assigning a group to each used core,
# so that dataframe manipulations can be carried out in parallel on each cluster.
# However, as proven here, the small dimensions of the dataset are such that the 
# performance is made worse off when the computation is done on multiple cores.
# (The test is run on a sample manipulation from the server function).

install.packages("multidplyr")
install.packages("devtools")
devtools::install_github("tidyverse/multidplyr")
library(multidplyr)
library(parallel)

cl <- detectCores()
cl

clus <- new_cluster(cl-1)
cluster_library(clus, "dplyr")


#### TIMING COMPARISON

# serial computation
start <- proc.time() # Start clock

test0 <- last_date_tib %>%
  filter(kingdom == "animals",class == "Insecta (Insects)",
         threat_level %in% c("Threatened","Near Threatened")) %>%
  mutate(threat_level = factor(threat_level)) %>%
  group_by(threat_level,area) %>%
  count() 

time_elapsed_parallel0 <- proc.time() - start # End clock

# group by area
start <- proc.time() # Start clock

test1 <- last_date_tib %>% group_by(area) %>% partition(clus) %>%
  filter(kingdom == "animals",class == "Insecta (Insects)",
         threat_level %in% c("Threatened","Near Threatened")) %>%
  mutate(threat_level = factor(threat_level)) %>%
  group_by(threat_level,area) %>%
  summarise(n = n()) %>%
  collect()

time_elapsed_parallel1 <- proc.time() - start # End clock

# group by threat_level
start <- proc.time() # Start clock

test2 <- last_date_tib %>% group_by(area) %>% partition(clus) %>%
  filter(kingdom == "animals",class == "Insecta (Insects)",
         threat_level %in% c("Threatened","Near Threatened")) %>%
  mutate(threat_level = factor(threat_level)) %>%
  group_by(threat_level,area) %>%
  summarise(n = n()) %>%
  collect()

time_elapsed_parallel2 <- proc.time() - start # End clock


# results
time_elapsed_parallel0  # serial
time_elapsed_parallel1  # par by area
time_elapsed_parallel2  # par by threat_level



