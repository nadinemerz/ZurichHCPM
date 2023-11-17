library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(grid)
library(gridExtra)

setwd("02_Datasets/Raw datasets CHIQI")


# Load CH-IQI datasets for years 2010-2019
years <- 2010:2019
for (i in years) {
  filename <- paste0("CHIQI_", i, ".csv")
  assign(paste0("chiqi_", i), read_delim(filename, ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="latin1")))
}

# Load and split the 2008/9 dataset into separate years
chiqi_2008_9 <- read_delim("CHIQI_2008-9.csv", ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="latin1"))
chiqi_2008 <- chiqi_2008_9[,1:6]
chiqi_2009 <- cbind(chiqi_2008_9[,1:2], chiqi_2008_9[,7:10])
rm(chiqi_2008_9)

# Identify data frames for processing
objects <- ls()
df_names <- grep("^chiqi_", objects, value = TRUE)
df_names <- df_names[-c(1,2)] # Excluding 2008 and 2009

# Remove unwanted columns (3:6, V11, V12) from 2010-2019 datasets
for (df_name in df_names) {
  df <- get(df_name)
  remove_cols <- 3:6
  v11_v12 <- which(colnames(df) %in% c("V11", "V12"))
  remove_cols <- c(remove_cols, v11_v12)
  df <- df[, -remove_cols]
  if (ncol(df) > 6) {
    df <- df[, 1:6]
  }
  assign(df_name, df, envir = .GlobalEnv)
}

# Add a "Year" column to each dataset
years <- c("2008", "2009", years)
for (year in years) {
  df_name <- paste0("chiqi_", year)
  df <- cbind(Year = year, get(df_name))
  assign(df_name, df)
}

## Modify 2019 df for Zurich's hospitals to match structure and naming of previous years (BAG changed naming logic)

# Backup Original Data for 2019
chiqi_2019_original <- chiqi_2019

# Remove non-acute hospital
chiqi_2019 <- chiqi_2019 %>%
  filter(institution != "Universitäts-Kinderspital Zürich das Spital der Eleonorenstiftung - Rehabilitationszentrum Affoltern, Mühlebergstrasse 104, 8910 Affoltern am Albis")

# Convert 'nombre de cas 2019' to numeric for calculations
chiqi_2019$`nombre de cas 2019` <- as.numeric(chiqi_2019$`nombre de cas 2019`)

# Combine Horgen and Kilchberg, as in previous years
chiqi_2019_horgen <- subset(chiqi_2019, institution == "See­Spital - See-Spital Horgen, Asylstrasse 19, 8810 Horgen")
chiqi_2019_kilchberg <- subset(chiqi_2019, institution =="See­Spital - See-Spital Kilchberg, Grütstrasse 60, 8802 Kilchberg")

# Combining rows for two indicators G.5.3.P and G.5.2.F
for(indicator_name in c("G.5.3.P Anteil brusterhaltende Resektionen bei Brustkrebs", "G.5.2.F Resektionen der Mamma bei Brustkrebs")) {
  # Finding and duplicating row
  row_to_duplicate <- filter(chiqi_2019, institution == "See­Spital - See-Spital Kilchberg, Grütstrasse 60, 8802 Kilchberg", indicator == indicator_name)
  
  # Modifying and creating new row
  new_row <- row_to_duplicate %>% 
    mutate(institution = "See-Spital",
           `nombre de cas 2019` = sum(chiqi_2019_horgen %>% 
                                        filter(indicator == indicator_name) %>% 
                                        pull(`nombre de cas 2019`),
                                      chiqi_2019_kilchberg %>% 
                                        filter(indicator == indicator_name) %>% 
                                        pull(`nombre de cas 2019`)))
  
  # Special condition for calculating 'taux observé 2019' for G.5.3.P
  if (indicator_name == "G.5.3.P Anteil brusterhaltende Resektionen bei Brustkrebs") {
    new_row$`taux observé 2019` <- paste0(round((new_row$`nombre de cas 2019` / 
                                                   sum(chiqi_2019_horgen %>% filter(indicator == "G.5.2.F Resektionen der Mamma bei Brustkrebs") %>% pull(`nombre de cas 2019`),
                                                       chiqi_2019_kilchberg %>% filter(indicator == "G.5.2.F Resektionen der Mamma bei Brustkrebs") %>% pull(`nombre de cas 2019`))) * 100, 1), "%")
  }
  
  # Adding new row and removing obsolete ones
  chiqi_2019 <- bind_rows(chiqi_2019, new_row) %>% 
    filter(!(institution == "See­Spital - See-Spital Kilchberg, Grütstrasse 60, 8802 Kilchberg" & indicator == indicator_name) | 
             institution == "See­Spital - See-Spital Horgen, Asylstrasse 19, 8810 Horgen")
}

# Tidying Up Institution Names in 2019 Data
chiqi_2019 <- chiqi_2019 %>%
  separate(institution, into = c("institution_1", "institution_2"), sep = " -", remove = FALSE) %>%
  select(-institution_2, -institution)  %>%
  rename(institution = institution_1)



## Compile into one big Master File

# Standardize column names across yearly data frames (original is French)
for (year in years) {
  df_name <- paste0("chiqi_", year)
  df <- get(df_name)
  colnames(df) <- c("year", "hospital", "indicator", "observed rate (decim.)", "expected rate (decim.)", 
                    "standardized mortality ratio", "case number")
  assign(df_name, df, envir = .GlobalEnv)
}

# Store all rows into one compiled master file
chiqi_compiled <- data.frame()
# Loop through the data frames, appending data to the master file and removing the individual yearly data frames post-appending.
for (year in years) {
  df_name <- paste0("chiqi_", year)
  df <- get(df_name)
  chiqi_compiled <- rbind(chiqi_compiled, df[-1, ])
}

# Clean up global environment 
all_dfs <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
rm(list = setdiff(all_dfs, "chiqi_compiled"), envir = .GlobalEnv)
# Reset rownames & show summary
rownames(chiqi_compiled) <- NULL 
summary(chiqi_compiled)



## Convert data types & clean Master File
chiqi_compiled$year <- as.numeric(chiqi_compiled$year)
chiqi_compiled$`observed rate (decim.)` <- as.numeric(gsub("%", "", chiqi_compiled$`observed rate (decim.)`)) / 100
chiqi_compiled$`expected rate (decim.)` <- as.numeric(gsub("%", "", chiqi_compiled$`expected rate (decim.)`)) / 100
chiqi_compiled$`case number`  <- as.numeric(chiqi_compiled$`case number`)
chiqi_compiled$`standardized mortality ratio` <- as.numeric(chiqi_compiled$`standardized mortality ratio`)

# Standardize hospital naming across data set
hospital_names <- unique(chiqi_compiled$hospital)
hospitals <- data.frame(hospital_name_original = hospital_names,
                        hospital_name_corrected = hospital_names, 
                        stringsAsFactors = FALSE)
replacements <- setNames(
  c("Adus Medica", "Geburtshaus Zürcher Oberland", "Klinik Hirslanden", "Klinik im Park", 
    "Klinik Pyramide am See", "Klinik Tiefenbrunnen", "Limmatklinik", "Paracelsus-Spital Richterswil", 
    "Paracelsus-Spital Richterswil", "Paracelsus-Spital Richterswil", "Privatklinik Bethanien", 
    "Privatklinik Bethanien", "Klinik Lindberg", "Klinik Lindberg", "Schulthess-Klinik", 
    "Schulthess-Klinik", "See-Spital", "See-Spital", "Spital Bülach", "Spital Männedorf", 
    "Kinderspital Zürich", "Uroviva Klinik für Urologie", "Universitätsklinik Balgrist"),
  c("Adus Medica AG", "Geburtshaus Zürcher Oberland AG", "Klinik Hirslanden AG", "Klinik Im Park", 
    "Klinik Pyramide am See AG", "Klinik Tiefenbrunnen AG", "Limmatklinik AG", "Paracelsus-Spital Richterswil AG", 
    "Paracelsus­Spital Richterswil", "Paracelsus­Spital Richterswil AG", "Privatklinik Bethanien - GSMN Schweiz AG", 
    "Privatklinik Bethanien AG", "Privatklinik Lindberg", "Privatklinik Lindberg - GSMN Schweiz AG", "Schulthess Klinik", 
    "Schulthess­Klinik", "See­Spital", "See-Spital", "Spital Bülach AG", "Spital Männedorf AG", 
    "Universitäts-Kinderspital Zürich das Spital der Eleonorenstiftung", "Uroviva Klinik AG", "Uniklinik Balgrist")
)

chiqi_compiled$hospital <- as.character(chiqi_compiled$hospital) 
chiqi_compiled$hospital <- ifelse(chiqi_compiled$hospital %in% names(replacements), 
                                  replacements[chiqi_compiled$hospital], 
                                  chiqi_compiled$hospital)


## Extract Zurich hospitasl from Master File

hospital_names_zurich <- c("Adus Medica",
                              "Geburtshaus Delphys",
                              "Geburtshaus Zürcher Oberland",
                              "GZO Spital Wetzikon",
                              "Kantonsspital Winterthur",
                              "Kinderspital Zürich",
                              "Klinik Hirslanden",
                              "Klinik im Park",
                              "Klinik Lindberg",
                              "Klinik Pyramide am See",
                              "Klinik Susenberg",
                              "Klinik Tiefenbrunnen",
                              "Limmatklinik",
                              "Paracelsus-Spital Richterswil",
                              "Privatklinik Bethanien",
                              "Schulthess-Klinik",
                              "See-Spital",
                             "Spital Affoltern",
                           "Spital Bülach",
                           "Spital Limmattal",
                           "Spital Männedorf",
                           "Spital Uster",
                           "Spital Zollikerberg",
                           "Stadtspital Triemli",
                           "Stadtspital Waid",
                           "Universitätsklinik Balgrist",
                           "Universitätsspital Zürich",
                           "Uroviva Klinik für Urologie")

# Extract Zurich hospital indicators into new df
chiqi_zurich <- chiqi_compiled %>% 
  filter(hospital %in% hospital_names_zurich)

# Remove observations where indicator is not missing (relative or absolute)
chiqi_zurich <- chiqi_zurich %>%
  filter(!(is.na(`case number`) & is.na(`observed rate (decim.)`)))


## Extract shortlisted hospital service groups from Zurich Master File

# Separate IQI codes from descriptions
chiqi_zurich <- chiqi_zurich %>%
  separate(indicator, c("indicator code", "indicator description"), sep = " ", extra = "merge")

# Define a function to extract and check codes
extract_and_check <- function(data, codes, category) {
  extracted_data <- data %>% filter(`indicator code` %in% codes)
  unmatched_codes <- setdiff(codes, unique(extracted_data$`indicator code`))
  print(paste("Unmatched", category, "codes: ", paste(unmatched_codes, collapse=", ")))
  return(extracted_data)
}

# Define shortlisted indicator codes
heart_indicator_codes <- c("A.1.1.F", "A.1.1.M", "A.2.1.M", "A.2.1.F", "A.7.11.M", "A.7.11.F", "A.7.12.M", "A.7.12.F", "A.7.18.P")
stroke_indicator_codes <- c("B.1.1.M", "B.1.1.F", "B.1.7.M", "B.1.7.F", "B.1.8.M", "B.1.8.F", "B.1.10.M", "B.1.10.F")
gyn_indicator_codes <- c("G.1.2.P","G.1.2N.F", "G.1.3.P", "G.5.3.P", "G.5.2.F")

# Extract data and check for unmatched codes using defined function (indicating that the patient volumes are not included as separate indicators)
heart_chiqi_zurich <- extract_and_check(chiqi_zurich, heart_indicator_codes, "Heart")
stroke_chiqi_zurich <- extract_and_check(chiqi_zurich, stroke_indicator_codes, "Stroke")
gyn_chiqi_zurich <- extract_and_check(chiqi_zurich, gyn_indicator_codes, "Gyn")


# Extrapolate absolute case numbers ONLY from proportion indicators
# Note: For "M" indicators, the "case number" column denotes the "F" indicator; i.e. the total number of cases (e.g. heart attacks) NOT total number of mortality cases. 
# For "P" indicators, the "case number" instead denotes the quality indicator in absolute numbers (e.g. number of births with perineal tear), but the corresponding "F" indicator is missing
# Source: Compare numbers with "Qualitätsindikatoren Abfrage" (https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-fakten-zu-spitaelern/qualitaetsindikatoren-der-schweizer-akutspitaeler/qualitaetsindikatoren-abfrage.html)

# Define function that overwrites total case number by dividing the absolute number of affected cases by the proportional rate
overwrite_case_number <- function(data, old_code) {
  rows_to_update <- which(data$`indicator code` == old_code)
  
  data[rows_to_update, ] <- data[rows_to_update, ] %>%
    mutate(
      `case number` = ifelse(`observed rate (decim.)` == 0, NA_real_,
                             round(`case number` / `observed rate (decim.)`))
    )
  return(data)
}


# Apply function for gyn indicators
gyn_chiqi_zurich <- overwrite_case_number(gyn_chiqi_zurich, "G.1.2.P")
gyn_chiqi_zurich <- overwrite_case_number(gyn_chiqi_zurich, "G.1.3.P")
gyn_chiqi_zurich <- overwrite_case_number(gyn_chiqi_zurich, "G.5.3.P")



# -------------------------------------
# EXPLORATIVE PLOTS & TABLES
# -------------------------------------


setwd("03_Exploratory Research Zurich")
base_output_path <- "03_Exploratory Research Zurich"
subfolders <- c("tables", "line_graphs", "smooth_line_graphs", "box_plots")
lapply(paste(base_output_path, subfolders, sep = "/"), dir.create, showWarnings = FALSE)



# -------------------------------------
# FUNCTIONS
# -------------------------------------

#Tables
# Case Numbers Table Function
generate_table_case_numbers <- function(data, title, path){
  table_data <- data %>%
    group_by(hospital, year) %>%
    summarise(case_number = sum(`case number`, na.rm = TRUE), .groups='drop') %>%
    pivot_wider(names_from = year, values_from = case_number) %>% 
    arrange(hospital) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), " ", .), .names="replaced_{col}"))
  
  table_grob <- gridExtra::tableGrob(as.matrix(table_data), 
                                     rows = NULL, 
                                     theme = ttheme_default(base_size = 10))
  pdf(path, width = 11, height = 8.5)
  grid.draw(table_grob)
  dev.off()
  return(invisible())
}

# Mortality Table Function
generate_table_mortality <- function(data, title, path){
  table_data <- data %>%
    group_by(hospital, year) %>%
    summarise(mortality = sum(`standardized mortality ratio`, na.rm = TRUE), .groups='drop') %>%
    pivot_wider(names_from = year, values_from = mortality) %>%
    arrange(hospital) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), " ", .), .names="replaced_{col}"))
  
  table_grob <- gridExtra::tableGrob(as.matrix(table_data), 
                                     rows = NULL, 
                                     theme = ttheme_default(base_size = 10))
  pdf(path, width = 11, height = 8.5)
  grid.draw(table_grob)
  dev.off()
  return(invisible())
}

# Proportion Table Function
generate_table_proportion <- function(data, title, path){
  table_data <- data %>%
    group_by(hospital, year) %>%
    summarise(proportion = ifelse(all(is.na(`observed rate (decim.)`)), NA, 
                                  paste0(round(na.omit(sum(`observed rate (decim.)`, na.rm = TRUE)) * 100, 2), "%")), .groups='drop') %>%
    pivot_wider(names_from = year, values_from = proportion) %>%
    arrange(hospital) %>%
    mutate(across(where(is.character), ~ifelse(is.na(.), " ", .), .names="replaced_{col}"))
  
  table_grob <- gridExtra::tableGrob(as.matrix(table_data), 
                                     rows = NULL, 
                                     theme = ttheme_default(base_size = 10))
  pdf(path, width = 11, height = 8.5)
  grid.draw(table_grob)
  dev.off()
  return(invisible())
}


#Line Graphs
generate_line_plot <- function(data, title, path, y_var){
  data <- data %>% drop_na(year, !!sym(y_var))
  p <- ggplot(data, aes(x = year, y = !!sym(y_var), color = hospital, group = hospital)) +
    geom_line() +
    geom_point() +
    labs(title = title) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
    theme(legend.text = element_text(size = 10), 
          legend.title = element_blank(), 
          legend.position = "top", 
          plot.margin = unit(c(1, 1, 1, 2), "cm"),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5))
  if(y_var == "observed rate (decim.)") {
    p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
  }
    ggsave(path, p, width = 15, height = 10, dpi = 300)
}

# Smooth Line Graphs
generate_smooth_line_plot <- function(data, title, path, y_var){
  data <- data %>% drop_na(year, !!sym(y_var)) 
  p <- ggplot(data, aes(x = year, y = !!sym(y_var), color = hospital, group = hospital)) +
    geom_smooth(method= lm, se = FALSE)+
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
    labs(title = title) +
    theme(legend.text = element_text(size = 10), 
          legend.title = element_blank(), 
          legend.position = "top", 
          plot.margin = unit(c(1, 1, 1, 2), "cm"),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5))
  if(y_var == "observed rate (decim.)") {
    p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
  }
  ggsave(path, p, width = 15, height = 10, dpi = 300)
}


# Boxplots
generate_box_plot <- function(data, title, path, y_var){
  data <- data %>% drop_na(year, !!sym(y_var))
  p <- ggplot(data, aes(x = factor(year), y = !!sym(y_var))) +
    geom_boxplot() +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    labs(title = title) +
    scale_x_discrete(labels = c("2008", "2009", "2010", "2011", "2012", 
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
    theme_classic()
  if(y_var == "observed rate (decim.)") {
    p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
  }
    ggsave(path, p, width = 15, height = 10, dpi = 300)
}


# -------------------------------------
# APPLICATION TO DATASETS
# -------------------------------------


#1) Heart: Case Numbers & Mortality

# Define titles
titles_cases <- c("Heart Attack Cases", "Heart Failure Cases", "Coronary Surgery with Heart Attack Cases", 
            "Coronary Surgery without Heart Attack Cases")
titles_indicator <- c("Heart Attack Standardized Mortality Ratio (SMR)", "Heart Failure Standardized Mortality Ratio (SMR)", 
                      "Coronary Surgery with Heart Attack Standardized Mortality Ratio (SMR)", 
                      "Coronary Surgery without Heart Attack Standardized Mortality Ratio (SMR)")
# Update indicator list to include only unique codes ending in M
heart_indicator_codes <- heart_chiqi_zurich %>%
  distinct(`indicator code`) %>%
  filter(str_ends(`indicator code`, "M")) %>%
  pull(`indicator code`)


#Execute loop
for(indicator_code in heart_indicator_codes){
  indicator_data <- heart_chiqi_zurich %>% 
    filter(`indicator code` == indicator_code)

  title_cases <- titles_cases[which(heart_indicator_codes == indicator_code)]
  title_mortality <- titles_indicator[which(heart_indicator_codes == indicator_code)]
  
  # Tables
  path_table_cases <- paste0(base_output_path, "/tables/", indicator_code, "_table_cases.pdf")
  path_table_mortality <- paste0(base_output_path, "/tables/", indicator_code, "_table_mortality.pdf")
  
  generate_table_case_numbers(indicator_data, title_cases, path_table_cases)
  generate_table_mortality(indicator_data, title_mortality, path_table_mortality)
  
  # Line Plots
  path_line_cases <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_cases.pdf")
  path_line_mortality <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_mortality.pdf")
  
  generate_line_plot(indicator_data, title_cases, path_line_cases, "case number")
  generate_line_plot(indicator_data, title_mortality, path_line_mortality, "standardized mortality ratio")
  
  # Smooth Line Plots
  path_smooth_line_cases <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_cases.pdf")
  path_smooth_line_mortality <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_mortality.pdf")
  
  generate_smooth_line_plot(indicator_data, title_cases, path_smooth_line_cases, "case number")
  generate_smooth_line_plot(indicator_data, title_mortality, path_smooth_line_mortality, "standardized mortality ratio")
  
  # Box Plots
  path_box_cases <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_cases.pdf")
  path_box_mortality <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_mortality.pdf")
  
  generate_box_plot(indicator_data, title_cases, path_box_cases, "case number")
  generate_box_plot(indicator_data, title_mortality, path_box_mortality, "standardized mortality ratio")
  
  print(paste("Finished generating visuals for:", indicator_code))
}


#2) Stroke: Case Numbers & Mortality

# Define titles
titles_cases <- c( "Stroke Cases", "Cerebral Infarction Cases", 
                   "Cerebral Infarction Direct Admission Cases", "Intracerebral Infarction Cases")
titles_indicator <- c( "Stroke Standardized Mortality Ratio (SMR)", "Cerebral Infarction Standardized Mortality Ratio (SMR)",
                       "Cerebral Infarction Direct Admission Standardized Mortality Ratio (SMR)", 
                       "Intracerebral Infarction Standardized Mortality Ratio (SMR)")
# Update indicator list to include only unique codes ending in M
stroke_indicator_codes <- stroke_chiqi_zurich %>%
  distinct(`indicator code`) %>%
  filter(str_ends(`indicator code`, "M")) %>%
  pull(`indicator code`)


#Execute loop
for(indicator_code in stroke_indicator_codes){
  indicator_data <- stroke_chiqi_zurich %>% 
    filter(`indicator code` == indicator_code)
  
  title_cases <- titles_cases[which(stroke_indicator_codes == indicator_code)]
  title_mortality <- titles_indicator[which(stroke_indicator_codes == indicator_code)]
  
  # Tables
  path_table_cases <- paste0(base_output_path, "/tables/", indicator_code, "_table_cases.pdf")
  path_table_mortality <- paste0(base_output_path, "/tables/", indicator_code, "_table_mortality.pdf")
  
  generate_table_case_numbers(indicator_data, title_cases, path_table_cases)
  generate_table_mortality(indicator_data, title_mortality, path_table_mortality)
  
  # Line Plots
  path_line_cases <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_cases.pdf")
  path_line_mortality <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_mortality.pdf")
  
  generate_line_plot(indicator_data, title_cases, path_line_cases, "case number")
  generate_line_plot(indicator_data, title_mortality, path_line_mortality, "standardized mortality ratio")
  
  # Smooth Line Plots
  path_smooth_line_cases <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_cases.pdf")
  path_smooth_line_mortality <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_mortality.pdf")
  
  generate_smooth_line_plot(indicator_data, title_cases, path_smooth_line_cases, "case number")
  generate_smooth_line_plot(indicator_data, title_mortality, path_smooth_line_mortality, "standardized mortality ratio")
  
  # Box Plots
  path_box_cases <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_cases.pdf")
  path_box_mortality <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_mortality.pdf")
  
  generate_box_plot(indicator_data, title_cases, path_box_cases, "case number")
  generate_box_plot(indicator_data, title_mortality, path_box_mortality, "standardized mortality ratio")
  
  print(paste("Finished generating visuals for:", indicator_code))
}


#3) Heart: Proportion

title_proportion <- c("Proporition of Ventilated Cases (>95h) during Coronary Surgery without Heart Attack")

# Update indicator list to include only unique codes ending in P
heart_indicator_codes <- heart_chiqi_zurich %>%
  distinct(`indicator code`) %>%
  filter(str_ends(`indicator code`, "P")) %>%
  pull(`indicator code`)

# Execute loop
for(indicator_code in heart_indicator_codes){
  indicator_data <- heart_chiqi_zurich %>% 
    filter(`indicator code` == indicator_code)
  
  title_proportion <- title_proportion[which(heart_indicator_codes == indicator_code)]
  
  # Tables
  path_table_proportion <- paste0(base_output_path, "/tables/", indicator_code, "_table_proportion.pdf")
  generate_table_proportion(indicator_data, title_proportion, path_table_proportion)
  
  # Line Plots
  path_line_proportion <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_proportion.pdf")
  generate_line_plot(indicator_data, title_proportion, path_line_proportion, "observed rate (decim.)")
  
  # Smooth Line Plots
  path_smooth_line_proportion <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_proportion.pdf")
  generate_smooth_line_plot(indicator_data, title_proportion, path_smooth_line_proportion, "observed rate (decim.)")
  
  # Box Plots
  path_box_proportion <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_proportion.pdf")
  generate_box_plot(indicator_data, title_proportion, path_box_proportion, "observed rate (decim.)")
  
  print(paste("Finished generating visuals for:", indicator_code))
}


#4) Gyn: Case Numbers & Proportion I

titles_cases <- c("Number of Vaginal Births", "Number of Mamma-Resections in Breast Cancer")
titles_proportion <- c("Proporition of Perineal Tear during Vaginal Birth", 
                        "Proportion of Breast-Conserving Mamma-Resections in Breast Cancer")

# Update indicator list to include only unique codes ending in P
gyn_indicator_codes <- gyn_chiqi_zurich %>%
  distinct(`indicator code`) %>%
  filter(str_ends(`indicator code`, "P") & `indicator code` != "G.1.3.P") %>%
  pull(`indicator code`)


#Execute loop
for(indicator_code in gyn_indicator_codes){
  indicator_data <- gyn_chiqi_zurich %>% 
    filter(`indicator code` == indicator_code)
  
  title_cases <- titles_cases[which(gyn_indicator_codes == indicator_code)]
  title_proportion <- titles_proportion[which(gyn_indicator_codes == indicator_code)]
  
  # Tables
  path_table_cases <- paste0(base_output_path, "/tables/", indicator_code, "_table_cases.pdf")
  path_table_proportion <- paste0(base_output_path, "/tables/", indicator_code, "_table_proportion.pdf")
  
  generate_table_case_numbers(indicator_data, title_cases, path_table_cases)
  generate_table_proportion(indicator_data, title_proportion, path_table_proportion)
  
  # Line Plots

  path_line_cases <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_cases.pdf")
  path_line_proportion <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_proportion.pdf")
  
  generate_line_plot(indicator_data, title_cases, path_line_cases, "case number")
  generate_line_plot(indicator_data, title_proportion, path_line_proportion, "observed rate (decim.)")
  
  # Smooth Line Plots
  path_smooth_line_cases <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_cases.pdf")
  path_smooth_line_proportion <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_proportion.pdf")
  
  generate_smooth_line_plot(indicator_data, title_cases, path_smooth_line_cases, "case number")
  generate_smooth_line_plot(indicator_data, title_proportion, path_smooth_line_proportion, "observed rate (decim.)")
  
  # Box Plots
  path_box_cases <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_cases.pdf")
  path_box_proportion <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_proportion.pdf")
  
  generate_box_plot(indicator_data, title_cases, path_box_cases, "case number")
  generate_box_plot(indicator_data, title_proportion, path_box_proportion, "observed rate (decim.)")
  
  print(paste("Finished generating visuals for:", indicator_code))
}


#4) Gyn: Proportion II

title_proportion <- c("Proporition of Episiotomy during Vaginal Birth")

# Update indicator list to include only unique codes ending in P
gyn_indicator_codes <- gyn_chiqi_zurich %>%
  distinct(`indicator code`) %>%
  filter(`indicator code` == "G.1.3.P") %>%
  pull(`indicator code`)

# Execute loop
for(indicator_code in gyn_indicator_codes){
  indicator_data <- gyn_chiqi_zurich %>% 
    filter(`indicator code` == indicator_code)
  
  title_proportion <- title_proportion[which(gyn_indicator_codes == indicator_code)]
  
  # Tables
  path_table_proportion <- paste0(base_output_path, "/tables/", indicator_code, "_table_proportion.pdf")
  generate_table_proportion(indicator_data, title_proportion, path_table_proportion)
  
  # Line Plots
  path_line_proportion <- paste0(base_output_path, "/line_graphs/", indicator_code, "_line_proportion.pdf")
  generate_line_plot(indicator_data, title_proportion, path_line_proportion, "observed rate (decim.)")
  
  # Smooth Line Plots
  path_smooth_line_proportion <- paste0(base_output_path, "/smooth_line_graphs/", indicator_code, "_smooth_line_proportion.pdf")
  generate_smooth_line_plot(indicator_data, title_proportion, path_smooth_line_proportion, "observed rate (decim.)")
  
  # Box Plots
  path_box_proportion <- paste0(base_output_path, "/box_plots/", indicator_code, "_box_proportion.pdf")
  generate_box_plot(indicator_data, title_proportion, path_box_proportion, "observed rate (decim.)")
  
  print(paste("Finished generating visuals for:", indicator_code))
}

