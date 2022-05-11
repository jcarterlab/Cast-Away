library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C://Users//HUAWEI//Desktop//Projects//Cast-Away//Data")

# gets the z scores for each data distribution given. 
get_z_scores <- function(data) {
  mean <- mean(data[!is.na(data)])
  sigma <- sd(data[!is.na(data)])
  z_scores <- list()
  for(i in 1:length(data)) {
    z_scores[i] <- if(is.na(data[i])) NA else (data[i] - mean) / sigma
  }
  return(unlist(z_scores))
}

# checks for outliers of over 3 standard deviations. 
check_outliers <- function(vars) {
  outliers <- list()
  for(i in 1:length(vars)) {
    outliers[[i]] <- sum(abs(!is.na(merged_data_frame[,vars][i]))>3)
    print(paste0(outliers[[i]], " --- ", vars[i], " outliers"))
  }
}

# gets the average NA adjusted average value for a column.   
get_avg <- function(var) {
  avg <- sum(var[!is.na(var)])/length(var[!is.na(var)])
  return(avg)
}

# creates a tidy data frame of the z scores for 3 variables. 
get_tidy_z_scores <- function(vars) {
  df <- merged_data_frame[,c(groupby, vars)] %>%
    group_by(country, decade_half) %>%
    summarise(var_1 = get_avg(eval(parse(text=vars[1]))),
              var_2 = get_avg(eval(parse(text=vars[2]))))
  colnames(df) <- c(groupby,vars)
  final_df <- df %>%
    gather(key = measure, value = value, -c(1,2))
  return(final_df)
}

# gets the average z score for 3 variables. 
get_avg_z_scores <- function(z_scores) {
  df <- eval(parse(text=z_scores)) %>%
    group_by(country, decade_half) %>%
    summarise(col=sum(value[!is.na(value)]/length(value[!is.na(value)])))
  colnames(df)[3] <- z_scores
  return(df)
}

# imputes missing values by taking the average for 
# countries with a similar gdp per capita. 
fill_NAs <- function(measure, vars) {
  impute_values <- final_df %>%
    mutate(gdp_per_capita = round(gdp_per_capita)) %>%
    group_by(gdp_per_capita) %>%
    summarise(avg = mean(eval(parse(text=measure))))
  NAs <- final_df[final_df[,measure]==0,]
  imputations <- list()
  for(i in 1:nrow(NAs)) {
    imputations[[i]] <- impute_values$avg[which(impute_values["gdp_per_capita"] == round(NAs$gdp_per_capita[i]))]
  }
  final_df[pull(final_df[measure])==0,][measure] <- unlist(imputations)
  return(final_df)
}

# creates a histogram. 
make_histogram <- function(variable) {
  histogram <- merged_data_frame %>%
    gather(key=measure, value = value, -c(1,2)) %>%
    filter(measure %in% variable) %>%
    mutate(value = as.numeric(value)) %>%
    ggplot(aes(x=value, fill=measure)) +
    geom_histogram(alpha=0.6,
                   position="identity",
                   bins=20) +
    facet_wrap(~measure,
               scales="free",
               ncol=2) +
    ggtitle("Histogram") +
    my_theme
  return(histogram)
}

# creates a qqplot.  
make_qqplot <- function(variable) {
  qqplot <- merged_data_frame %>%
    gather(key = measure, value = value, -c(1,2)) %>%
    filter(measure %in% variable) %>%
    mutate(value = as.numeric(value)) %>%
    ggplot(aes(sample=value, col=measure)) +
    geom_qq(position="identity") +
    geom_qq_line(position="identity") +
    facet_wrap(~measure,
               scales="free",
               ncol=2) +
    ggtitle("QQplot") +
    my_theme
  return(qqplot)
}

# creates a single variable scatter plot. 
make_jitter_plot <- function(data, x, y) {
  scatter <- data %>%
    ggplot(aes(x=x,
               y=y,
               color = x)) +
    geom_jitter() +
    ggtitle("") +
    xlab("") +
    ylab("GDP Per Capita") +
    my_theme
  return(scatter)
}

# creates a multiple variable scatter plot. 
make_scatter_plot <- function(data, x, y) {
  scatter <- data %>%
    gather(key = measure, value = value, -c("country",
                                            "year",
                                            y)) %>%
    filter(measure %in% x) %>%
    mutate(measure = str_to_title(str_replace_all(measure, "_", " "))) %>%
    ggplot(aes(x=value,
               y=eval(parse(text=y)),
               col=measure)) +
    geom_point() +
    geom_smooth(method='lm', 
                formula=y~x) +
    ggtitle("Control Variables") +
    facet_wrap(~measure,
               ncol=3,
               scales="free") +
    xlab("") +
    ylab("GDP Per Capita") +
    my_theme +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  return(scatter)
}

# my personal plot theme for data visualizations. 
my_theme <- theme_economist_white(gray_bg = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 10,
                                  size = 10,
                                  color = "#474747"),
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
        axis.text = element_text(size = 9,
                                 color = "gray30"),
        axis.text.x=element_text(vjust = -2.5),
        axis.title.x = element_text(size = 9,
                                    color = "gray30",
                                    vjust = -10),
        axis.title.y = element_text(size = 9,
                                    color = "gray30",
                                    vjust = 10),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11,
                                   color = "gray20"),
        legend.margin=margin(1, -15, 1, 0),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(1, "cm"), 
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(hjust = 0.5,
                                  vjust = 1,
                                  size = 10,
                                  color = "#474747"),
        panel.spacing = unit(2, "lines"))


### --- 1. THE DATA ---

# reads in the merged data data frame. 
merged_data_frame <- read_csv("merged_data_frame.csv") %>%
  mutate(gov_effectivness = as.numeric(gov_effectivness))

# lists the landlocked countries in our sample. 
landlocked <- c("Afghanistan",
                "Armenia",
                "Austria",
                "Azerbaijan",
                "Burundi",
                "Burkina Faso",
                "Belarus",
                "Bolivia",
                "Botswana",
                "Central African Republic",
                "Switzerland",
                "Czech Republic",
                "Hungary",
                "Kazakhstan",
                "Kyrgyz Republic",
                "Lao PDR",
                "Lesotho",
                "North Macedonia",
                "Mali",
                "Mongolia",
                "Malawi",
                "Niger",
                "Nepal",
                "Rwanda",
                "Serbia",
                "South Sudan",
                "Slovak Republic",
                "Eswatini",
                "Chad",
                "Tajikistan",
                "Turkmenistan",
                "Uganda",
                "Uzbekistan",
                "Kosovo",
                "Zambia",
                "Zimbabwe")

# creates an ocean access variable for non-landlocked countries. 
ocean_access <- list() 
for(i in 1:length(unique(merged_data_frame$country))) {
  ocean_access[[i]] <- if(!unique(merged_data_frame$country)[i] %in% landlocked) 1 else 0
}
ocean_access <- tibble(country = unique(merged_data_frame$country),
                       ocean_access = unlist(ocean_access))

# makes a jitter plot of gdp per capita verses ocean 
# access to check for correlation. 
gdp <- merged_data_frame %>%
  group_by(country) %>%
  summarise(gdp = get_avg(gdp_per_capita)) %>% 
  left_join(ocean_access) %>%
  mutate(ocean_access = as.factor(ocean_access))

levels(gdp$ocean_access) <- c(" No", " Yes")

make_jitter_plot(data=gdp, x=gdp$ocean_access, y=gdp$gdp) +
  ggtitle("Ocean access") +
  theme(legend.position = "none")

# checks the relationship with a linear model. 
summary(lm(gdp ~ ocean_access, gdp))

# creates groups of variables to be used in transformations. 
groupby <- c("country", "decade_half")
ed_vars <- c("tertiary_enrollment", "secondary_enrollment")
sci_vars <- c("rd_expenditure", "rd_researchers")
inst_vars <- c("corruption", "gov_effectivness")
all_vars <- c(ed_vars, sci_vars, inst_vars)

# creates a scatter plot to visualize the relationship 
# between all the variables and gdp per capita. 
make_scatter_plot(data=merged_data_frame, x=all_vars, y="gdp_per_capita") +
  theme(legend.position = "none")


### --- 2. GDP PER CAPITA ---

# checks the distribution with a histogram and qqplot. 
make_histogram("gdp_per_capita")
make_qqplot("gdp_per_capita")

# transforms gdp per capita into log values. 
merged_data_frame$log_gdp_per_capita <- log(merged_data_frame$gdp_per_capita)

# checks the distribution with a histogram and qqplot. 
make_histogram("log_gdp_per_capita")
make_qqplot("log_gdp_per_capita")

# transforms the gdp per capita variable into 
# its z score distribution. 
merged_data_frame <- merged_data_frame %>%
  mutate(log_gdp_per_capita = get_z_scores(log_gdp_per_capita))

# creates a data frame containing the z scores for the 
# gdp per capita variable. 
gdp_per_capita <- merged_data_frame[,c("country", "decade_half", "log_gdp_per_capita")] %>%
  group_by(country, decade_half) %>%
  summarise(gdp_per_capita = get_avg(log_gdp_per_capita))


### --- 3. EDUCATION ---

# checks the distribution with a histogram and qqplot. 
make_histogram(ed_vars)
make_qqplot(ed_vars)

# transforms the variables. 
merged_data_frame$secondary_enrollment <- merged_data_frame$secondary_enrollment**(1/0.355)
merged_data_frame$tertiary_enrollment <- merged_data_frame$tertiary_enrollment**(1/2)

# checks the distribution with a histogram and qqplot. 
make_histogram(ed_vars)
make_qqplot(ed_vars)

# transforms the education variables into 
# their z score distributions. 
merged_data_frame <- merged_data_frame %>%
  group_by(decade_half) %>%
  mutate(secondary_enrollment = get_z_scores(secondary_enrollment),
         tertiary_enrollment = get_z_scores(tertiary_enrollment))

# checks for outliers. 
check_outliers(ed_vars)

# creates a data frame containing the z scores for the 
# education variables. 
education <- get_tidy_z_scores(ed_vars)

# gets the average z score for the education variables.
education_avg_z_scores <- get_avg_z_scores("education")


### --- 4. SCIENCE ---

# checks the distribution with a histogram and qqplot. 
make_histogram(sci_vars)
make_qqplot(sci_vars)

# transforms the variables. 
merged_data_frame$rd_expenditure <- merged_data_frame$rd_expenditure**(1/2.5)
merged_data_frame$rd_researchers <- merged_data_frame$rd_researchers**(1/3.5)

# checks the distribution with a histogram and qqplot. 
make_histogram(sci_vars)
make_qqplot(sci_vars)

# transforms the science variables into 
# their z score distributions. 
merged_data_frame <- merged_data_frame %>%
  mutate(rd_expenditure = get_z_scores(rd_expenditure),
         rd_researchers = get_z_scores(rd_researchers))

# checks for outliers. 
check_outliers(sci_vars)

# creates a data frame containing the z scores for the 
# science variables. 
science <- get_tidy_z_scores(sci_vars)

# gets the average z score for the human capital variables.
science_avg_z_scores <- get_avg_z_scores("science")


### --- 5. INSTITUTIONS ---

# checks the distribution with a histogram and qqplot. 
make_histogram(inst_vars)
make_qqplot(inst_vars)

# transforms the variables. 
merged_data_frame$corruption <- merged_data_frame$corruption**(1/2.25)

# checks the distribution with a histogram and qqplot. 
make_histogram(inst_vars)
make_qqplot(inst_vars)

# transforms the institutions variables into 
# their z score distributions. 
merged_data_frame <- merged_data_frame %>%
  mutate(corruption = get_z_scores(corruption),
         gov_effectivness = get_z_scores(gov_effectivness))

# checks for outliers. 
check_outliers(inst_vars)

# creates a data frame containing the z scores for our 
# institutions variables. 
institutions <- get_tidy_z_scores(inst_vars)

# gets the average z score for the institutions variables. 
institutions_avg_z_scores <- get_avg_z_scores("institutions")


### --- 6. FINAL DATA FRAME ---

# creates a final table for the combined variables. 
final_df <- gdp_per_capita %>%
  left_join(ocean_access) %>%
  left_join(education_avg_z_scores) %>%
  left_join(science_avg_z_scores) %>%
  left_join(institutions_avg_z_scores) %>%
  filter(!is.na(gdp_per_capita))

# checks how many NAs there are for each index. 
indexes <- c("education", "science", "institutions")
for(i in 1:length(indexes)) {
  print(paste0(sum(final_df[,indexes[i]]==0),
               " -- ",
               indexes[i],
               " (NAs)"))
}

# fills NA values based on the average for countries
# with a similar gdp per capita. 
final_df <- fill_NAs("education", ed_vars)
final_df <- fill_NAs("science", sci_vars)
final_df <- fill_NAs("institutions", inst_vars)

# saves the data to a csv file
write_csv(final_df, "clean_variables_table.csv")
