# script for reading plain text files pasted from Index Seminum catalogues
#Feb 24, 2025


library(readr)
library(stringr)
library(openxlsx)
library(stringi)

# Read the plain text file with UTF-8 encoding
text_data <- read_lines("seed_species.txt", locale = locale(encoding = "UTF-8"))
text_data <- stri_enc_toutf8(text_data, validate = TRUE)
text_data <- stri_replace_all_regex(text_data, "[^\\p{Print}]", "")  # Remove non-printable characters

# Initialize empty vectors to store extracted data
item_numbers <- c()
ipen_numbers <- c()
species_references <- c()
ipen_letters <- c()

# Temporary storage for multi-line references
current_item <- ""
current_ipen <- ""
current_species <- ""

for (line in text_data) {
  match <- str_match(line, "^(\\d+)\\s+([A-Z0-9-]+)\\s+(.+)$")
  if (!is.na(match[1,1])) {
    # If there's a previous entry, save it before processing a new one
    if (current_item != "") {
      item_numbers <- c(item_numbers, current_item)
      ipen_numbers <- c(ipen_numbers, current_ipen)
      species_references <- c(species_references, str_trim(current_species))
      ipen_letters <- c(ipen_letters, str_extract(current_ipen, "[GWF]"))
    }
    # Start a new entry
    current_item <- match[1,2]
    current_ipen <- match[1,3]
    current_species <- match[1,4]
  } else {
    # Continuation line, append to current species reference
    current_species <- paste(current_species, str_trim(line))
  }
}

# Save the last entry
if (current_item != "") {
  item_numbers <- c(item_numbers, current_item)
  ipen_numbers <- c(ipen_numbers, current_ipen)
  species_references <- c(species_references, str_trim(current_species))
  ipen_letters <- c(ipen_letters, str_extract(current_ipen, "[GWF]"))
}

# Create a data frame
seed_data <- data.frame(
  Item_Number = stri_enc_toutf8(item_numbers),
  IPEN_Number = stri_enc_toutf8(ipen_numbers),
  IPEN_Letter = stri_enc_toutf8(ipen_letters),
  Species_Reference = stri_enc_toutf8(species_references),
  stringsAsFactors = FALSE
)
# Write to Excel
write.csv(seed_data, "seed_species.csv")


################################################################################
# Separate the author references from the species names
################################################################################

seed_listC <- read.csv("seed_listC.csv")
head(seed_listC)
summary(seed_listC)
class(seed_listC)
library(dplyr)
library(stringr)
library(tidyr)
seed_listC <- seed_listC %>%
  mutate(Taxon = str_extract(Taxon, "^\\w+\\s+\\w+"))
write.csv(seed_listC, "seed_listC_taxaSeparated.csv")

# fix the species that don't have the genus listed

seed_listC <- read.csv("seed_species_plain.csv", stringsAsFactors = FALSE)

seed_listC <- seed_listC %>%
  mutate(Genus = ifelse(grepl("^-", Taxon), NA, word(Taxon, 1))) %>%
  fill(Genus, .direction = "down") %>%
  mutate(Taxon = ifelse(grepl("^-", Taxon), paste(Genus, sub("^-", "", Taxon)), Taxon)) %>%
  select(-Genus)


print(seed_listC)
write.csv(seed_listC, "seed_listC.csv")

