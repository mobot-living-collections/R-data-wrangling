# Expand the rows of the Iris sale tags to correlate with the number to dig column #

install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr
        )
IrisSaleTags_week2 <- read.csv("IrisSaleTags_week2_07222024.csv")

irisExtraLabels07292024 <- read.csv("irisSale_extralabelRequest07292024.csv")

# Handle NA values in "Number to dig" column by replacing them with 1
#IrisSaleTags <- IrisSaleTags %>%
 # mutate(Number.to.dig = ifelse(is.na(Number.to.dig), 1, Number.to.dig))

# for the digs that are labeled as dig "all" I replaced them with 15 for the printer

# Expand the rows based on "Number to dig"
expanded_data <- irisExtraLabels07292024 %>%
  filter(extra.labels.requested > 0) %>%
  uncount(extra.labels.requested)

write.csv(expanded_data, file = "irisExtraLabels07292024.csv")
