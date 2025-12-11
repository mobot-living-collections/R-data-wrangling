# Dec 9, 2025
# With data from Tropicos, LCMS, and Linan et al., 2025, update Pyrus taxonomy in Tropicos and LCMS

library(tidyverse)
library(readxl)

LCMSaccessions <- read_csv("LcmsPyrusAccessions.csv", col_select = c("accession", "collector", "collector number", "species"))
TropicosSpecimens <- read_csv("TropicosPyrusSpecimens.csv", col_select = c("collector", "collector number", "species", "tropicos collection id"))
GereauDets <- read_csv("TropicosPyrusGereauDets.csv", col_select = c("collector", "collector number", "species"))
PublishedTableS1 <-read_csv("PublishedTableS1.csv", col_select = c("collector", "collector number", "species", "original det"))

comparison <- PublishedTableS1 %>%
  left_join(LCMSaccessions %>% select(collector, `collector number`, species, accession),
            by = c("collector", "collector number"),
            suffix = c("", "_LCMS")) %>%
  left_join(TropicosSpecimens %>% select(collector, `collector number`, species, `tropicos collection id`),
            by = c("collector", "collector number"),
            suffix = c("", "_Tropicos")) %>%
  left_join(GereauDets %>% select(collector, `collector number`, species),
            by = c("collector", "collector number"),
            suffix = c("", "_GereauDet"))
comparison <- comparison %>%
  rename(species_published = species)

# do all the species names match across data sets?
comparison <- comparison %>%
  mutate(all_match = (species_published == species_LCMS | is.na(species_LCMS)) &
           (species_published == species_Tropicos | is.na(species_Tropicos)) &
           (species_published == species_GereauDet | is.na(species_GereauDet)))
write.csv(comparison, "PyrusTaxonomyComparisons.csv")
