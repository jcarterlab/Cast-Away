library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data")


### --- 1. CHECK THE RELATIONSHIP ---

# opens the merged data frame. 
merged_data_frame <- read_csv("merged_data_frame.csv") %>%
  left_join(ocean_access)

# makes a jitter plot of gdp per capita verses ocean 
# access and checks for correlation. 
raw_data <- merged_data_frame %>%
  group_by(country) %>%
  summarise(gdp = get_avg(gdp_per_capita)/10^3) %>% 
  left_join(ocean_access) %>%
  mutate(ocean_access = as.factor(ocean_access))

levels(raw_data$ocean_access) <- c(" No", " Yes")

make_jitter_plot(data=gdp, x=raw_data$ocean_access, y=raw_data$gdp) +
  ggtitle("Ocean access") +
  ylab("GDP per capita (000s)") +
  theme(legend.position = "none")

# checks the relationship . 
fit <- summary(lm(gdp ~ ocean_access, raw_data))
fit$coefficients


### --- 2. PLOT THE RAW CONTROL VARIABLES ---

# creates a scatter plot to visualize the relationship 
# between all the variables and gdp per capita. 
make_scatter_plot(data=merged_data_frame, x=all_vars, y="gdp_per_capita") +
  ggtitle("Raw Control variables") +
  theme(legend.position = "none")


### --- 3. PLOT THE ADJUSTED CONTROL MEASURES ---

# opens the clean data frame. 
clean_variables_table <- read_csv("clean_variables_table.csv")

# creates a list of the indexed control measures. 
measures <- c("")

# creates a scatter plot to visualize the relationship 
# between the adjusted control measures and gdp per capita. 
make_scatter_plot(data=clean_variables_table, x=measures, y="gdp_per_capita") +
  ggtitle("Raw Control variables") +
  theme(legend.position = "none")


### --- 3. CHECK THE RESULTS ---

# creates the variable for the regression analysis. 
y <- clean_variables_table$gdp_per_capita
ocean_access <- clean_variables_table$ocean_access
education <- clean_variables_table$education
science <- clean_variables_table$science
institutions <- clean_variables_table$institutions

# fits the regression model. 
fit <- lm(y~ocean_access+education+science+institutions)
summary(fit)
