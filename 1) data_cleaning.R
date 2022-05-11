library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(readxl)
library(stringr)

### --- FUNCTIONS ---

# reads in the World Bank data. 
read_WB_data <- function(indicator_name) {
  data <- read_xls(paste0("WB//",
                          indicator_name, 
                          ".xls"),
                   range = "A4:BM271") %>%
    gather(key = "year", 
           value = indicator_name,
           5:65) %>%
    select(!c(2:4,))
  colnames(data) <- c("country",
                      "year",
                      indicator_name)
  return(data)
}

# as parts of those in the TI list. 
get_data_non_idenitcal <- function(data, object) {
  scores <- list()
  for(i in 1:length(object)) {
    scores[[i]] <- if(mean(str_detect(object[i], data))>0) TRUE else FALSE
  }
  return(object[unlist(scores)])
}

# checks if any the data's country names appear as parts 
# of those in the non identical object list. 
get_object_non_idenitcal <- function(data, object) {
  scores <- list()
  for(i in 1:length(data)) {
    scores[[i]] <- if(mean(str_detect(data[i], object))>0) TRUE else FALSE
  }
  return(data[unlist(scores)])
}

# creates new data for each country in the 
# non identical list given.
create_country_data <- function(year, countries) {
  country_data <- list()
  for(i in 1:length(unique(countries))) {
    country_data[[i]] <- tibble(country = countries[i],
                                year = year,
                                col = NA)
  }
  return(rbind_pages(country_data))
}

# creates new data for each year in the 
# non_identical list.
create_year_data <- function(year, countries, colname) {
  year_data <- list()
  for(i in 1:length(unique(year))) {
    year_data[[i]] <- create_country_data(year[i],
                                          countries)
  }
  return(tibble(rbind_pages(year_data)))
}

# checks the ratio of NAs for each measure.  
get_measure_na_ratio <- function(data) {
  na <- list()
  for(i in 1:ncol(data)) {
    na[[i]] <- round(mean(is.na(data[,i])), 1)
  }
  data_frame <- tibble(measure = colnames(data),
                       na_ratio = unlist(na))
  return(data_frame[-c(1),])
}


### --- 1. WORLD BANK DATA ---

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data")

# creates a data frame with multiple variables. 
data_frame <- read_WB_data("gdp_per_capita") %>%
  left_join(read_WB_data("secondary_enrollment")) %>%
  left_join(read_WB_data("tertiary_enrollment"))

# creates a list of indicators to not include.
unwanted_indicators <- c("World",
                         "Arab World",
                         "Caribbean small states",
                         "Central Europe and the Baltics",
                         "East Asia & Pacific",
                         "East Asia & Pacific (excluding high income)",
                         "Euro area",
                         "Europe & Central Asia",
                         "Europe & Central Asia (excluding high income)",
                         "European Union",
                         "Fragile and conflict affected situations",
                         "Heavily indebted poor countries (HIPC)",
                         "Latin America & Caribbean",
                         "Latin America & Caribbean (excluding high income)",
                         "Least developed countries: UN classification",
                         "Middle East & North Africa",
                         "Middle East & North Africa (excluding high income)",
                         "North America",
                         "OECD members",
                         "Other small states",
                         "Pacific island small states",
                         "Small states",
                         "South Asia",
                         "Sub-Saharan Africa",
                         "Sub-Saharan Africa (excluding high income)",
                         "High income",
                         "Low & middle income",
                         "Low income",
                         "Lower middle income",
                         "Middle income",
                         "Upper middle income",
                         "Not classified",
                         "Africa Eastern and Southern",
                         "Africa Western and Central",
                         "Early-demographic dividend",
                         "IBRD only",
                         "IDA & IBRD total",
                         "IDA total",
                         "IDA blend",
                         "IDA only",
                         "Late-demographic dividend",
                         "Pre-demographic dividend",
                         "Post-demographic dividend",
                         "East Asia & Pacific (IDA & IBRD countries)",
                         "Europe & Central Asia (IDA & IBRD countries)",
                         "Latin America & the Caribbean (IDA & IBRD countries)",
                         "Middle East & North Africa (IDA & IBRD countries)",
                         "South Asia (IDA & IBRD)",
                         "Sub-Saharan Africa (IDA & IBRD countries)")

# filters out unwanted indicators. 
WB_filtered_indicators <- data_frame %>%
  filter(!country %in% unwanted_indicators)

# drops rows with na values in the country column 
# and makes the year column numeric. 
WB_no_country_na <- WB_filtered_indicators[-which(is.na(WB_filtered_indicators$country)),]
WB_no_country_na$year <- as.numeric(WB_no_country_na$year)

# adds a decade column and shows how many 
# data points there are for each decade. 
WB_decade_added <- WB_no_country_na %>%
  mutate(decade = (floor(year / 10)) * 10)
table(WB_decade_added$decade)

# drops the unwanted decades before dropping
# the decades column. 
WB_dropped_decades <- WB_decade_added %>%
  filter(!decade %in% c(1960, 1970, 1980, 1990, 2000, 2020))
WB_dropped_decades <- WB_dropped_decades[,-ncol(WB_dropped_decades)]


### --- 2. TRANSPARENCY INTERNATIONAL DATA ---

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data//TI")

# reads in the transparency international data. 
TI_data <- read_xls("all_corruption.xls")

# creates a list of the unique countries and checks 
# which are not in the World Bank data frame. 
WB <- sort(unique(WB_dropped_decades$country))
TI <- sort(unique(TI_data$Country))
WB_TI_non_idenntical <- WB[which(!WB %in% TI)]

# creates a list of world bank countries not in the 
# transparency international data and vice versa.
get_data_non_idenitcal(data=TI, object=WB_TI_non_idenntical)
get_object_non_idenitcal(data=TI, object=WB_TI_non_idenntical)

# creates a data frame of countries in both data 
# frames with different names. 
TI_same_names <- tibble(WB = c("United States",
                               "Bahamas, The",
                               "Congo, Rep.",
                               "Congo, Dem. Rep.",
                               "Egypt, Arab Rep.",
                               "Gambia, The",
                               "Guinea-Bissau",
                               "Hong Kong SAR, China",
                               "Iran, Islamic Rep.",
                               "Russian Federation",
                               "Syrian Arab Republic",
                               "Venezuela, RB",
                               "Yemen, Rep."),
                        TI = c("United States of America",
                               "Bahamas",
                               "Congo",
                               "Democratic Republic of the Congo",
                               "Egypt",
                               "Gambia",
                               "Guinea Bissau",
                               "Hong Kong",
                               "Iran",
                               "Russia",
                               "Syria",
                               "Venezuela",
                               "Yemen"))

# changes all TI country names in same_names to their WB version. 
for(i in 1:nrow(TI_same_names)) {
  TI_data$Country <- str_replace_all(TI_data$Country, TI_same_names$TI[i], TI_same_names$WB[i])
}

# checks again which country names are in the WB 
# but not TI list. 
TI <- sort(unique(TI_data$Country))
WB[which(!WB %in% TI)]

# creates a data frame of countries in both data 
# frames with different names. 
TI_same_names <- tibble(WB = c("Korea, Dem. People's Rep.",
                               "Kyrgyz Republic",
                               "Czech Republic",
                               "Korea, Rep.",
                               "Lao PDR",
                               "Slovak Republic",
                               "St. Lucia",
                               "St. Vincent and the Grenadines",
                               "Congo, Dem. Rep."),
                        TI = c("Korea, North",
                               "Kyrgyzstan",
                               "Czechia",
                               "Korea, South",
                               "Laos",
                               "Slovakia",
                               "Saint Lucia",
                               "Saint Vincent and the Grenadines",
                               "Congo, Dem. Rep., Rep."))

# changes all TI country names in same_names to their WB version. 
for(i in 1:nrow(TI_same_names)) {
  TI_data$Country <- str_replace_all(TI_data$Country, TI_same_names$TI[i], TI_same_names$WB[i])
}

# checks again which country names are in the WB 
# but not TI list 
TI <- sort(unique(TI_data$Country))
WB_TI_non_idenntical <- WB[which(!WB %in% TI)]

# checks which country names (Taiwan) are in the TI 
# list but not WB list before dropping them. 
drop <- which(!TI %in% WB)
TI <- TI[-drop]
TI_data <- TI_data[-drop,]

# creates a tidy data frame for the TI data that renames
# the country column and discludes the year 2020. 
tidy_TI_data <- TI_data %>%
  gather(key = year, value = corruption, -1) %>%
  rename(country = Country) %>%
  filter(!year == 2020)

# creates a data frame of NA values 1). for the countries present 
# in the WB but not the TI data frame for the years we have data 
# for (2012-2019)  and 2). for every country for the years we do 
# not have (2010-2011) have data for in the target decade (2010s). 
non_identical_TI_NAS <- create_year_data(year = unique(tidy_TI_data$year), 
                                         countries = WB_TI_non_idenntical) %>% 
  rename(corruption = col)

identical_TI_NAS <- create_year_data(year = c(2010, 2011), 
                                     countries = WB) %>%
  rename(corruption = col)

# combines the new data with the tidy TI data frame
# and arranges it by country name. 
combined_data_frame <- tidy_TI_data %>%
  rbind(non_identical_TI_NAS) %>%
  rbind(identical_TI_NAS) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(country, year)

# saves the new data frame to a csv file. 
write_csv(combined_data_frame, "TI_adjusted_data.csv")

# reads in the adjusted transparency international 
# corruption perceptions index data frame. 
TI <- read_csv("TI_adjusted_data.csv")


### --- 3. WORLDWIDE GOVERNACE DATA ---

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data//WGI")

# reads in the transparency international data. 
gov_effectivness <- read_xls("gov_effectivness.xls")

# creates a list of the unique countries and checks 
# which are not in the World Bank data frame. 
WGI <- sort(unique(gov_effectivness$country))
WB_WGI_non_idenntical <- WB[which(!WB %in% WGI)]

# creates a list of world bank countries not in the 
# transparency international data and vice versa.
get_data_non_idenitcal(data=WGI, object=WB_WGI_non_idenntical)
get_object_non_idenitcal(data=WGI, object=WB_WGI_non_idenntical)

# creates a data frame of countries in both data 
# frames with different names. 
WGI_same_names <- tibble(WB = "Channel Islands",
                         WGI = "Jersey, Channel Islands")

# changes all WGI country names in same_names to their WB version. 
gov_effectivness$country <- str_replace_all(gov_effectivness$country, 
                                            WGI_same_names$WGI[1], 
                                            WGI_same_names$WB[1])

# checks again which country names are in the WB 
# but not WGI list. 
WGI <- sort(unique(gov_effectivness$country))
WB[which(!WB %in% WGI)]         

# creates a data frame of countries in both data 
# frames with different names. 
WGI_same_names <- tibble(WB = c("Cabo Verde",
                                "Cote d'Ivoire",
                                "Korea, Dem. People's Rep."),
                         WGI = c("Cape Verde",
                                 "Côte d'Ivoire",
                                 "Korea, Dem. Rep."))  

# changes all WGI country names in same_names to their WB version. 
for(i in 1:nrow(WGI_same_names)) {
  gov_effectivness$country <- str_replace_all(gov_effectivness$country, 
                                              WGI_same_names$WGI[i], 
                                              WGI_same_names$WB[i])
}                  

# checks again which country names are in the WB 
# but not TI list 
WGI <- sort(unique(gov_effectivness$country))
non_idenntical <- WB[which(!WB %in% WGI)]

# checks which country names are in the WGI but
# not WB list. 
drop <- which(!WGI %in% WB)

# removes country names from the TI list not
# in the WB list. 
WGI <- WGI[-drop]
gov_effectivness <- gov_effectivness[-drop,]

# creates a tidy data frame for the WGI data. 
gov_effectivness_tidy <- gov_effectivness %>%
  gather(key = year, value = "gov_effectivness", -1)

# creates a data frame of NA values for the countries present 
# in the WB but not the WGI data frame. 
non_identical_WGI_GE_NAS <- create_year_data(year = unique(gov_effectivness_tidy$year), 
                                             countries = WB_WGI_non_idenntical) %>%
  rename(gov_effectivness = col)


# combines the new data with the tidy WGI data frames
# and arranges it by country name. 
GE_combined <- gov_effectivness_tidy %>%
  rbind(non_identical_WGI_GE_NAS) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(country, year)

# saves the new data frame to a csv file. 
write_csv(GE_combined, "WGI_adjusted_data.csv")

# reads in the adjusted worldwide governance 
# indicators data frame. 
WGI <- read_csv("WGI_adjusted_data.csv")


### --- 4. COMBINE THE DATA ---

# combines the transparency international data. 
merged_data_frame <- WB_dropped_decades %>% 
  left_join(TI) %>%
  left_join(WGI)

# checks how many NA values there are for each measure. 
get_measure_na_ratio(merged_data_frame)

# checks for countries where most of the values are NA.  
check_na_values <- list()
for(i in 1:length(merged_data_frame$country)) {
  check_na_values[[i]] <- sum(is.na(merged_data_frame[i,4:ncol(merged_data_frame)]))>=ncol(merged_data_frame)-3
}
view(merged_data_frame[unlist(check_na_values),])

# lists the useless countries after checking NA values.  
high_NA_countries <- c("Curacao",
                       "Isle of Man",
                       "St. Martin (French part)",
                       "Faroe Islands",
                       "French Polynesia",
                       "Northern Mariana Islands",
                       "Gibraltar",
                       "New Caledonia",
                       "Sint Maarten (Dutch part)",
                       "Turks and Caicos Islands",
                       "British Virgin Islands")

# filter the useless countries out of the merged data frame. 
merged_data_frame <- merged_data_frame %>%
  filter(!country %in% high_NA_countries)

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data")
  
# writes the data to a csv file
write_csv(merged_data_frame, "merged_data_frame.csv")

