# 4 December 2025
# we have the file from Madagascar, and downloaded collection events by collector from tropicos
# this code combines the various tropicos output files, cleans up the collector name issues, and adds a new column IsInTropicos to the database sheet

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

#combine all of the files into one

files <- list.files(path = "C:/Users/taubuchon/OneDrive - Missouri Botanical Garden/Africa & Madagascar/Rfiles",
                    pattern = "\\.xlsx$",
                    full.names = TRUE)
tropicos_combined_data <- files %>%
  map_dfr(read_excel)

MASTER_BD_DI_17_11_2025_for_STL <- read_excel("MASTER_BD_DI_17_11_2025 for STL.xlsx")

# names don't perfectly match between the master DB and tropicos names

MASTER_BD_DI_17_11_2025_for_STL <- MASTER_BD_DI_17_11_2025_for_STL %>%
  select(-any_of(c("IsInTropicos", "Collection Event Id", "collector_standardized")))

standardize_name <- function(name) {
  
  if (is.na(name)) {
    return(NA_character_)
  }
  name <- str_trim(name)
  # if name contains comma, assume 'Lastname, Firstname" format
  if (str_detect(name, ",")) {
    parts <- str_split(name, ",\\s*")[[1]]
    name <- paste(parts[2], parts[1])
  }
  str_to_lower(str_squish(name))
}

MASTER_BD_DI_17_11_2025_for_STL <- MASTER_BD_DI_17_11_2025_for_STL %>%
  mutate(
    CollectorNumber = as.character(CollectorNumber),
    collector_standardized = map_chr(Collector, standardize_name)
  )

tropicos_standardized <- tropicos_combined_data %>%
  select(Collector, 'Coll No', 'Collection Event Id') %>%
  mutate(
    collector_standardized = map_chr(Collector, standardize_name)
  ) %>%
  rename(CollectorNumber = 'Coll No',
         IsInTropicos = 'Collection Event Id') %>%
  select(collector_standardized, CollectorNumber, IsInTropicos)

MASTER_BD_DI_17_11_2025_for_STL <- MASTER_BD_DI_17_11_2025_for_STL %>%
  left_join(
    tropicos_standardized,
          by = c('collector_standardized', 'CollectorNumber')
  ) %>%
  select(-collector_standardized)

# cases, formatting, and spelling are slightly different between the two data sets...


# Unique collectors in MASTER_BD_DI_17_11_2025_for_STL
master_collectors <- MASTER_BD_DI_17_11_2025_for_STL %>%
  distinct(Collector) %>%
  arrange(Collector) %>%
  pull(Collector)

print(master_collectors)

# Unique collectors in tropicos_combined_data
tropicos_collectors <- tropicos_combined_data %>%
  distinct(Collector) %>%
  arrange(Collector) %>%
  pull(Collector)

print(tropicos_collectors)

# Create a comparison tibble
comparison <- tibble(
  Master = c(sort(unique(MASTER_BD_DI_17_11_2025_for_STL$Collector)), 
             rep(NA, max(0, length(unique(tropicos_combined_data$Collector)) - 
                           length(unique(MASTER_BD_DI_17_11_2025_for_STL$Collector))))),
  Tropicos = c(sort(unique(tropicos_combined_data$Collector)), 
               rep(NA, max(0, length(unique(MASTER_BD_DI_17_11_2025_for_STL$Collector)) - 
                             length(unique(tropicos_combined_data$Collector)))))
)

print(comparison, n = Inf)

# create a name mapping table
name_mapping <- tibble(
  master_name = c("Benjamina Ralaijaona", 
                  "Casmir Z. Rakotonirina"),
  tropicos_name = c("Ralaijaona, Benjamina", 
                    "Rakotonirina, Casmir Zoly")
)
# Standardize the Tropicos collector names (handle case differences)
tropicos_for_join <- tropicos_combined_data %>%
  select(Collector, `Coll No`, `Collection Event Id`) %>%
  mutate(
    Collector_standardized = str_to_title(Collector)  # Fixes RAKOTONINDRIANA -> Rakotonindriana
  ) %>%
  # Replace tropicos names with master names where mapping exists
  left_join(name_mapping, by = c("Collector_standardized" = "tropicos_name")) %>%
  mutate(
    Collector_final = coalesce(master_name, Collector_standardized)
  ) %>%
  select(Collector_final, `Coll No`, `Collection Event Id`) %>%
  rename(IsInTropicos = `Collection Event Id`,
         CollectorNumber = `Coll No`)

# Clean up master data and join
MASTER_BD_DI_17_11_2025_for_STL <- MASTER_BD_DI_17_11_2025_for_STL %>%
  select(-any_of(c("IsInTropicos", "collector_standardized"))) %>%
  mutate(
    CollectorNumber = as.character(CollectorNumber),
    Collector_standardized = str_to_title(Collector)
  ) %>%
  left_join(tropicos_for_join,
            by = c("Collector_standardized" = "Collector_final", "CollectorNumber")) %>%
  select(-Collector_standardized)
# Count matches
sum(!is.na(MASTER_BD_DI_17_11_2025_for_STL$IsInTropicos))

not_in_tropicos <- MASTER_BD_DI_17_11_2025_for_STL %>%
  filter(is.na(IsInTropicos))

tropicos_collector_matching <- MASTER_BD_DI_17_11_2025_for_STL

write_xlsx(tropicos_collector_matching, "TropicosCollectorMatchingDB.xlsx")

write_xlsx(not_in_tropicos, "NotInTropicos.xlsx")



















