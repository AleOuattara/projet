

#-----------------      STEP I ---------------------------------------------


# ------------------------------------------------------------------------
# LIBRARY INSTALLATION AND LOADING
# ------------------------------------------------------------------------
install.packages("kableExtra")
library(kableExtra)

install.packages("openxlsx") 
library(magrittr)  # For the pipe operator (%>%)
library(dplyr)     # For data manipulation functions such as rename()
library(readxl)    # For reading Excel files
library(tidyr)     # For reshaping data with pivot_longer()
library(openxlsx)  # For exporting data to Excel files
install.packages("plm") # Install and load 'plm' package for panel data analysis
library(plm)
library(readr)     # For reading and writing data
library(ggplot2)   # For data visualization
library(zoo)       # For handling time series and irregular time series


#-----------------      STEP II ---------------------------------------------

# ------------------------------------------------------------------------
# DATA COLLECTION AND MERGING
# ------------------------------------------------------------------------
# Merging several databases to create the final dataset for the project
# Specify the folder path where different databases are located

setwd("M:/AMSE-MAG2/PROJET_R/bases_finales/projet/projet/base_de_depart/bases_var_exp")

# ------------------------------------------------------------------------
# READING AVERAGE WAGE DATA
# ------------------------------------------------------------------------
# Reading and processing average wage data specifically for France and the USA
sal_moy_fr_usa <- read.csv("sal_moy_world.csv")
sal_moy_fr_usa <- subset(sal_moy_fr_usa, LOCATION %in% c("FRA", "USA"))
sal_moy_fr_usa <- sal_moy_fr_usa[, -c(2:5, 8)]
# Renaming columns for better clarity and consistency
sal_moy_fr_usa <- sal_moy_fr_usa %>%
  rename(country = LOCATION,
         years = TIME,
         wg_mean = Value)

# ------------------------------------------------------------------------
# READING UNEMPLOYMENT RATE DATA
# ------------------------------------------------------------------------
# Extracting unemployment rate data for France and the USA
tx_chom_fr_usa <- read.csv("tx_chom_world.csv")
tx_chom_fr_usa <- subset(tx_chom_fr_usa, LOCATION %in% c("FRA", "USA"))
tx_chom_fr_usa <- tx_chom_fr_usa[, -c(2:5, 8)]
# Renaming columns for clarity
tx_chom_fr_usa <- tx_chom_fr_usa %>%
  rename(country = LOCATION,
         years = TIME,
         unemp_rate = Value)

# ------------------------------------------------------------------------
# READING GDP PER CAPITA DATA
# ------------------------------------------------------------------------
# Gathering GDP per capita data for France and the USA
pib_p_hbt_fr_usa <- read_excel("pib_p_hbt.xls")
pib_p_hbt_fr_usa <- subset(pib_p_hbt_fr_usa, `Country Code` %in% c("FRA", "USA"))
pib_p_hbt_fr_usa <- pib_p_hbt_fr_usa[, -c(1,3,4)]
# Reshaping and renaming columns for usability
pib_p_hbt_fr_usa <- pib_p_hbt_fr_usa %>%
  rename(country = `Country Code`) %>%
  pivot_longer(cols = -country, names_to = "years", values_to = "gdp_pc")

# ------------------------------------------------------------------------
# READING GINI INDEX DATA
# ------------------------------------------------------------------------
# Processing Gini index data for France and the USA
gini_fr_usa <- read_excel("gini_world.xls", sheet = "Data")
gini_fr_usa <- subset(gini_fr_usa, LOCATION %in% c("FRA", "USA"))
gini_fr_usa <- gini_fr_usa[, -c(1,3,4)]
# Reshaping data for better analysis and visualization
gini_fr_usa <- gini_fr_usa %>%
  rename(country = LOCATION) %>%
  pivot_longer(cols = -country, names_to = "years", values_to = "gini")

# ------------------------------------------------------------------------
# READING URBANIZATION RATE DATA
# ------------------------------------------------------------------------
# Extracting urbanization rate data and ensuring consistency in column names
pop_urb <- read.csv("urban_pop.csv", sep = ";")
pop_urb <- pop_urb %>%
  rename(country = Pays, years = Annee, urban_rate = Taux_urbanisation)

# ------------------------------------------------------------------------
# READING EDUCATION LEVEL DATA
# ------------------------------------------------------------------------
# Gathering education level data for France and the USA
niv_educ <- read.csv("niv_educ.csv")
niv_educ <- subset(niv_educ, LOCATION %in% c("FRA", "USA"))
niv_educ <- niv_educ[, -c(2:5,8)]
# Renaming columns for a unified data structure
niv_educ <- niv_educ %>%
  rename(country = LOCATION, years = TIME, niv_educ = Value)

# ------------------------------------------------------------------------
# READING MALE POPULATION PERCENTAGE DATA
# ------------------------------------------------------------------------
# Processing male population percentage data for France and the USA
tx_masc <- read_excel("tx_masc.xls")
tx_masc <- subset(tx_masc, `Country Code` %in% c("FRA", "USA"))
tx_masc <- tx_masc[, -c(1,3,4)]
# Reshaping and renaming columns for consistency
tx_masc <- tx_masc %>%
  rename(country = `Country Code`) %>%
  pivot_longer(cols = -country, names_to = "years", values_to = "pop_male")

# ------------------------------------------------------------------------
# READING WORKING AGE POPULATION DATA
# ------------------------------------------------------------------------
# Extracting working age population data for comparative analysis
wg_ag_pop <- read.csv("wkg_ag_pop.csv")
wg_ag_pop <- subset(wg_ag_pop, LOCATION %in% c("FRA", "USA"))
wg_ag_pop <- wg_ag_pop[, -c(2:5,8)]
# Renaming columns for clarity
wg_ag_pop <- wg_ag_pop %>%
  rename(country = LOCATION, years = TIME, wg_ag_pop = Value)

# ------------------------------------------------------------------------
# PROCESSING CRIME RATE DATA
# ------------------------------------------------------------------------
# Aggregating crime rate data for the USA
tx_crime_usa <- read.csv("tx_crime_usa.csv", sep = ";")
tx_crime_usa$tx_crime <- tx_crime_usa$violent_crime + tx_crime_usa$poverty_crime
tx_crime_usa <- tx_crime_usa[, -c(3:5)]

# Reading crime rate data for France
tx_crime_fr <- read_excel("tx_crime_fr.xlsx")

# Merging crime rate data for France and the USA into a single dataset
tx_crime <- merge(tx_crime_usa, tx_crime_fr, by = c("country", "years", "tx_crime"), all = TRUE)

# ------------------------------------------------------------------------
# COMBINING ALL DATASETS INTO A FINAL RESULT
# ------------------------------------------------------------------------
# Merging all individual datasets into one comprehensive dataset
resultat_final <- merge(sal_moy_fr_usa, tx_chom_fr_usa, by = c('country', 'years'), all = TRUE)
# Additional merging steps...

# Filtering rows based on the year being greater than or equal to 1990
resultat_final <- resultat_final %>% filter(years >= 1990)

# Reading additional data
oder_data <- read_excel("autre_donnees.xlsx")

# Ensuring consistent formatting of 'country' and 'years' columns
resultat_final <- resultat_final %>% mutate(across(c(country, years), as.character))
oder_data <- oder_data %>% mutate(across(c(country, years), as.character))

# ------------------------------------------------------------------------
# MERGING RESULTAT_FINAL AND ODER_DATA
# ------------------------------------------------------------------------
# Joining resultat_final and oder_data for comprehensive analysis
oder_data_joined <- oder_data %>%
  left_join(resultat_final, by = c("country", "years"), suffix = c("", "_resultat_final"))

# Replacing missing values in 'unemp_rate' and 'gini' with values from resultat_final
oder_data_joined <- oder_data_joined %>%
  mutate(unemp_rate = ifelse(is.na(unemp_rate), unemp_rate_resultat_final, unemp_rate)) %>%
  mutate(gini = ifelse(is.na(gini), gini_resultat_final, gini))

# Retaining only the original columns from oder_data
oder_data_final <- oder_data_joined %>% select(names(oder_data))

# Updating oder_data with the newly filled dataframe
oder_data <- oder_data_final



# EXPORTING FINAL RESULTS TO EXCEL
# ------------------------------------------------------------------------
# Exporting the processed and combined data to an Excel file
write.xlsx(oder_data_final, "base.xlsx")


#-----------------  STEP III ---------------------------------------------

# ------------------------------------------------------------------------
# DATA PROCESSING
# ------------------------------------------------------------------------
# Loading the initial dataset for further analysis
base <- read_excel("base.xlsx")

# Converting 'tx_crime' to a numeric type and rounding to 2 decimal places for accuracy
base$tx_crime <- round(as.numeric(base$tx_crime), 2)

# ------------------------------------------------------------------------
# HANDLING MISSING VALUES BY IMPUTATION
# ------------------------------------------------------------------------

# Filling missing values in 'wg_mean' using the Next Observation Carried Forward (NOCF) method
# This method fills missing values with the next available non-missing value
# ------------------------------------------------------------------------
# Filling missing values in 'wg_mean' using the Last Observation Carried Forward method
base$wg_mean <- na.locf(base$wg_mean, fromLast = TRUE, na.rm = FALSE)

# Imputing missing values in 'gini' with the median value for each country
base$gini <- with(base, ave(gini, country, FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))

# Imputing missing values in 'niv_educ' (education level) with the median value for each country
base$niv_educ <- with(base, ave(niv_educ, country, FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))



#-----------------  STEP IV ---------------------------------------------


# ------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------
# Filtering data for the USA and conducting statistical analysis
data_USA <- base[base$country == "USA", ]
data_USA$years <- as.numeric(as.character(data_USA$years))
data_USA <- data_USA %>% arrange(years)
summary_df1 <- summary(data_USA)

# Creating and printing a kable object for nicely formatted tables
kable_summary1 <- kable(summary_df1)
print(kable_summary1)

# Saving the USA data summary as an Excel file
write.csv2(as.data.frame(summary_df1), "figures_USA.csv", row.names = TRUE)

# Filtering data for France and conducting statistical analysis
data_FRANCE <- base[base$country == "FRA", ]
data_FRANCE$years <- as.numeric(as.character(data_FRANCE$years))
data_FRANCE <- data_FRANCE %>% arrange(years)
summary_df2 <- summary(data_FRANCE)

# Creating and printing a kable object for France data
kable_summary2 <- kable(summary_df2)
print(kable_summary2)

# Saving the France data summary as a CSV file
write.csv2(as.data.frame(summary_df2), "figures_FRANCE.csv", row.names = TRUE)

# ------------------------------------------------------------------------
# VISUALIZING DATA TRENDS OVER TIME
# ------------------------------------------------------------------------
# Creating a line plot to visualize the evolution of 'wg_mean' over time


data_USA$years <- as.numeric(as.character(data_USA$years))
data_USA <- data_USA %>% arrange(years)

ggplot(data = base, aes(x = years, y = wg_mean, color = country)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Evolution of Average Wage by Country",
       x = "Year",
       y = "Average Wage (US dollars)",
       caption = "Source: Your Data Source") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# ------------------------------------------------------------------------
# ANALYZING CORRELATIONS
# ------------------------------------------------------------------------
# Selecting only numeric columns for correlation analysis
base_numerique <- base[, sapply(base, is.numeric)]

# Computing the correlation matrix
matrice_corr <- cor(base_numerique, use = "complete.obs")

# Creating a correlation matrix plot with black labels
corrplot(matrice_corr, method = "color", tl.col = "black", tl.srt = 45)

# ------------------------------------------------------------------------
# BAR GRAPH VISUALIZATION
# ------------------------------------------------------------------------
# Preparing data for bar graph visualization

base$years <- factor(base$years, levels = unique(base$years))
base$Other_rate <- 100 - base$urban_rate
base_urban <- base %>% 
  select(country, years, urban_rate, Other_rate) %>%
  pivot_longer(cols = c("urban_rate", "Other_rate"), names_to = "Urban", values_to = "population")

# Creating a bar plot to visualize urban vs. non-urban population distribution
base$years <- as.numeric(as.character(base$years))
base <- base %>% arrange(years)
ggplot(data = base_urban, aes(x = as.factor(years), y = population, fill = Urban)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ country, scales = "free_x") +
  scale_fill_manual(values = c("urban_rate" = "blue4", "Other_rate" = "lightcoral")) +
  coord_flip() +
  labs(title = "Urban vs. Non-Urban Population Distribution by Year",
       x = "Years",
       y = "Percentage of Population",
       fill = "Population Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))



#-----------------  STEP V ---------------------------------------------

# ------------------------------------------------------------------------
# ECONOMETRIC MODELING
# ------------------------------------------------------------------------
# Preparing and formatting data for econometric analysis

# Ensuring that 'country' and 'years' variables are correctly formatted for panel data analysis
base$country <- as.factor(base$country)
base$years <- as.numeric(as.character(base$years))

# Converting commas to dots in numeric variables to standardize data format
# This step is crucial for handling European-formatted numbers where commas are used as decimal points
base <- base %>% mutate(across(where(is.numeric), ~ as.numeric(gsub(",", ".", .))))

# ------------------------------------------------------------------------
# STATISTICAL INFERENCE FOR PANEL DATA ANALYSIS
# ------------------------------------------------------------------------
# Performing panel data analysis using 'plm' package

# Converting the 'resultat_final' dataset into a pdata.frame format
# pdata.frame is specifically designed for panel data analysis in 'plm' package
pdata <- pdata.frame(base, index = c("country", "years"))

# Estimating a Fixed Effects Model (Within Model)
# This model captures unobserved heterogeneity by allowing a unique intercept for each entity (e.g., country)
modele_fixe <- plm(tx_crime ~ 1 + gdp_pc + wg_mean + gini + unemp_rate + pop_male + urban_rate + niv_educ + wg_ag_pop, 
                   data = pdata, 
                   model = "within")

# Displaying a detailed summary of the Fixed Effects Model
# The summary includes coefficients, standard errors, t-values, and p-values
summary(modele_fixe)

# Estimating an Ordinary Least Squares (OLS) Regression Model for comparison
# OLS model treats each observation independently and does not consider panel data structure
modele_mco <- lm(tx_crime ~ 1 + gdp_pc + wg_mean + gini + unemp_rate + pop_male + urban_rate + niv_educ + wg_ag_pop, 
                 data = pdata)

# Displaying the summary of the OLS model for comparative analysis
summary(modele_mco)

# Performing an F-test to compare Fixed Effects and OLS models
# This test assesses the significance of individual effects in the panel data
# A significant F-test result (low p-value) indicates the Fixed Effects model is more appropriate
pFtest(modele_fixe, modele_mco)


##-------After the above statistical tests, we will choose the OLS model for interpretation."
