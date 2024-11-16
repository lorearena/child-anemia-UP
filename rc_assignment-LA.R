## ----------------------------------------------------------------------------------------------------------------------------------------
#set working directory where the file is located
setwd("") 

#check working directory
getwd()


## ----------------------------------------------------------------------------------------------------------------------------------------
### Packages ### ----------------------------------------------------------------

pacman::p_load(
  "conflicted", 
  "tidyverse",
  "Amelia",                        # To visualize missing values
  "ggplot2",
  "epiDisplay",                    # For epidemiological analysis
  "sf",
  "jsonlite"
  )

conflict_prefer("select", "dplyr")  # Will prefer dplyr::select over any other package
conflict_prefer("filter", "dplyr")  # Will prefer dplyr::filter over any other package
options(digits=4, scipen = 999)     # Will use until the 4 decimal and not the scientific notation (scipen = 999)


## ----------------------------------------------------------------------------------------------------------------------------------------
#Load Children's Recode data from the National Family Health Survey (NFHS) -5 for Uttar Pradesh
df <- read.csv("data-raw/data_UP.csv")


## ----------------------------------------------------------------------------------------------------------------------------------------
# Function to allocate the mean values per variable to the missing values
impute_value <- function(value, class) {
    # Calculate the mean age for each class, excluding NA values in age
    mean_value <- tapply(value, class, mean, na.rm = TRUE)
    
    # Create an output vector
    out <- value
    
    # Loop through each element
    for (i in 1:length(value)) {
        # Check if age is NA
        if (is.na(value[i])) {
            # Impute based on the class mean
            out[i] <- mean_value[class[i]]
        } else {
            # Keep the original age if not missing
            out[i] <- value[i]
        }
    }
    return(out)
}

# Function to print only the table from tab1
print_tab1_only <- function(var) {
  tab1(var, cum.percent = FALSE, graph = FALSE)
}

# Function to conduct Chi-squared or Fisher's exact test based on counts
test_categorical <- function(var) {
  tbl <- table(df_complete$childanemia, var)
  if (any(tbl < 5)) {
    test_result <- fisher.test(tbl, simulate.p.value = TRUE, B = 1e6)  # Fisher's exact test if any count is less than 5
  } else {
    test_result <- chisq.test(tbl)  # Chi-squared test 
  }
  return(test_result)
}

#Function to capitalize words
capitalize_words <- function(text) {
  text <- tolower(text)  # Ensure all other letters are lowercase
  text <- gsub("(^|[[:space:]])([a-z])", "\\1\\U\\2", text, perl = TRUE)
  return(text)
}

# Function to perform Chi-square OR analysis
analyze_exposure <- function(exposure) {
  # Print the exposure variable name
  cat("\nAnalysis for exposure:", exposure, "\n")
  
  # Chi-square test
  cat("Chi-square test:\n")
  chi_square_result <- with(df_complete, cc(df_complete$childanemia, df_complete[[exposure]], graph = F))
  print(chi_square_result)
   # # Return results in a list for storage
  list(chi_square = chi_square_result)
}



## ----------------------------------------------------------------------------------------------------------------------------------------
# Define the state variable at the beginning so that it can be reused across the code:
state <- "Uttar Pradesh"
# Define the state variable within the dataset
df <- df %>% mutate(state = state) 

#Select the variables of interest for the analysis
df_VOI <- df %>%
  filter (state==state) %>% # change State to the state of interest
  select(caseid, v021, v022, sdist, v005, sweight, hw1, b4, v025, v130, s116, v133, m14, v161, v456, hw56, state) 


## ----------------------------------------------------------------------------------------------------------------------------------------
df_VOI %>% str()


## ----------------------------------------------------------------------------------------------------------------------------------------
df_VOI %>% summary()


## ----------------------------------------------------------------------------------------------------------------------------------------
#hw56
table(df_VOI$hw56) #check values present
# Clean hw56
df_VOI <- df_VOI %>% mutate(hw56 = ifelse(hw56 %in% c(".a", "", " "), 
                                          NA,
                                          as.numeric(hw56)))
#m14
table(df_VOI$m14)  #check values present
# Clean m14
df_VOI <- df_VOI %>% mutate(m14 = ifelse(m14 %in% c("don't know", "", " "), 
                                         NA, 
                      ifelse(m14 == "no antenatal visits", 
                             0,
                             as.numeric(m14))))


## ----------------------------------------------------------------------------------------------------------------------------------------
#Check if NAs are present in each of the column
df_VOI %>% sapply(function(x) sum(is.na(x)))

#Check if "" or " " are present in each of the column
df_VOI %>% sapply(function(x) sum(x == "" | x == " "))

#Define "" and " " as NAs
df_VOI <- df_VOI %>%
  mutate(across(everything(), ~ ifelse(. %in% c("", " "), NA, .)))

#Visualize missing values across the datasets
df_VOI %>% missmap(y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# Checking Missing Values as a Percentage
print ("Missing values present:")
df_VOI %>% sapply(function(x) mean(is.na(x)) * 100) 
#s116= 0.36% NAs  •	Caste 
#hw1= 6.6% NA     •	Child's age
#m14= 28.88% NAs  •	Number of ANC visits of mothers for last birth
#hw56= 25.77% NAs •	Child’s Hemoglobin



## ----------------------------------------------------------------------------------------------------------------------------------------
# using drop_na() function
df_complete <- df_VOI %>% drop_na(hw56)


## ----------------------------------------------------------------------------------------------------------------------------------------
#Check the distribution to check if mean and median are close. If so, it suggests a symmetric distribution, and the mean is appropriate
df_VOI %>% 
  ggplot(aes(hw56)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(hw56, na.rm = TRUE)), 
             color = "red", 
             linetype = "dashed", 
             size = 1, 
             label="Mean") +
  geom_vline(aes(xintercept = median(hw56, na.rm = TRUE)), 
             color = "blue", 
             linetype = "dashed",
             size = 1, 
             label="Median") +
  labs(title = "Histogram of Variable with Mean and Median",
       x = "Variable",
       y = "Frequency")

#Check if also the mean hb based on sex changes
df_VOI %>% 
  ggplot(aes(b4,hw56)) +
  geom_boxplot(aes(group=b4,fill=b4)) + 
  scale_y_continuous(breaks = seq(min(0), max(300), by = 50))


## ----------------------------------------------------------------------------------------------------------------------------------------
# Use impute_value function defined above to calculate the mean hb values per sex
#Define new datasets for mean imputation
df_impute <- df_VOI         
fixed.value_hw56 <- impute_value(df_impute$hw56, df_impute$b4)
df_impute$hw56 <- fixed.value_hw56  #Create new column where NAs are treated as mean


## ----------------------------------------------------------------------------------------------------------------------------------------
#It does for m14 among children based on sex (variable v130)
df_complete %>%
  ggplot(aes(b4,m14)) +
  geom_boxplot(aes(group=b4,fill=b4))

# Use impute_value function defined above to calculate the mean anc values per sex
fixed.value_m14 <- impute_value(df_complete$m14, df_complete$b4)  #imputing by sex, even though there is not a clear difference
df_complete$m14<- fixed.value_m14  #Create new column where NAs are treated as mean


## ----------------------------------------------------------------------------------------------------------------------------------------
#Check missing values across the new df_MI dataset to check if it worked
df_complete %>% missmap(y.at=c(1),y.labels = c(''),col=c('yellow','black'))
# Checking Missing Values as a Percentage
print ("Missing values present:")
df_complete %>% sapply(function(x) mean(is.na(x)) * 100) 


## ----------------------------------------------------------------------------------------------------------------------------------------
df_complete <- df_complete %>%
  rename(primarysamplingunit=v021,
         strata=v022,
         samplingweightsnational=v005,
         childage=hw1, 
         childsex=b4,
         residence=v025,
         mothereduyears=v133,
         religion=v130,
         caste=s116,
         ancvisits=m14,
         cookingfuel=v161,
         childhb=hw56,
         maternalhb=v456)


## ----------------------------------------------------------------------------------------------------------------------------------------
## Maternal Anemia 
# Amemia values range from 20 to 997, suggesting that values have been registered in g/L
df_complete$maternalhb %>% summary
df_complete <- df_complete %>%
  mutate(maternalhbgdL = maternalhb / 10) 
# Since No information on pregnant status for women is provided, 11.5 g/dL will be used to define anemia, which is often used when pregnancy status is unknown.
df_complete <- df_complete %>%
  mutate(maternal_anemia = ifelse(maternalhbgdL < 11.5, "Yes", "No"))
#Print out
df_complete %>% select(maternalhb, maternalhbgdL, maternal_anemia) %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------------
# Child Anemia
df_complete$childhb %>% summary
# Amemia values range from 20 to 997, suggesting that values have been registered in g/L
df_complete <- df_complete %>%
  mutate(childhbgdL = childhb / 10,
         childanemia = ifelse(childhbgdL < 11, "Yes", "No")) 
#Print out
df_complete %>% select(childhb, childhbgdL, childanemia) %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------------
# Religion
print("Religion")
table(df_complete$religion, useNA = "ifany")

#Religion (v130) to Minority religion (other religion vs Hindu) 
df_complete <- df_complete %>%
  mutate(
    religion_cat = case_when(
      religion == "hindu" ~ "hindu",          # Keep "hindu" as is
      !is.na(religion) ~ "other religion",    # All other religions as "other religion"
      is.na(religion) ~ NA_character_        # Retain NA as NA if there are missing values
      )
  )

#Print values
print("Religion new categories")
table(df_complete$religion, df_complete$religion_cat,useNA = "ifany")



## ----------------------------------------------------------------------------------------------------------------------------------------
# Caste
print("Caste")
table(df_complete$caste, useNA = "ifany")

#Recode Caste (s116) to scheduled Caste or tribe (SC/ST vs other caste groups) 
df_complete <- df_complete %>%
  mutate(
    caste_cat = case_when(
      caste %in% c("schedule caste", "schedule tribe") ~ "SC/ST",  # Group "schedule caste" and "schedule tribe" as "SC/ST"
      caste == "don't know" ~ NA,           # Assume "don't know" as NA
      !is.na(caste) ~ "Other Caste Groups", # All other values 
      is.na(caste) ~ NA_character_          # Retain NA as NA
    )
  )

#Print values
print("Caste new categories")
table(df_complete$caste, df_complete$caste_cat, useNA = "ifany")



## ----------------------------------------------------------------------------------------------------------------------------------------
# Clean Cooking Fuel
print("HH Cooking fuel")
table(df_complete$cookingfuel, useNA = "ifany")

#Recode cooking fuel (v161) to HH using clean cooking fuel (clean cooking fuel- Electricity, LPG/natural gas, biogas vs other fuels) 
df_complete <- df_complete %>%
  mutate(HH_cooking_fuel_cat = case_when(
      cookingfuel %in% c("electricity", "lpg", "biogas") ~ "clean cooking fuel",  # Group clean fuels
      !is.na(cookingfuel) ~ "other fuels", # All other fuels as "other fuels"
      is.na(cookingfuel) ~ NA_character_   # Retain NA if any
    )
  )

#Print values
print("HH Cooking fuel new categories")
table(df_complete$cookingfuel, df_complete$HH_cooking_fuel_cat, useNA = "ifany")


## ----------------------------------------------------------------------------------------------------------------------------------------
# ANC Visits
print("ANC visits summary")
df_complete$ancvisits %>% summary()

#Recode mothers’ ANC visits (m14) to Mothers who had 4+ ANC visits for last birth (4 or more ANC visit vs <4ANC or no ANC) 
df_complete <- df_complete %>%
  mutate(
    anc_visits_cat = case_when(
      ancvisits >= 4 ~ "4 or more ANC visit",             # 4 or more visits
      ancvisits < 4 & !is.na(ancvisits) ~ "<4 ANC or no ANC",   # Less than 4 visits
      is.na(ancvisits) ~ NA                              # Retain NA if any
    )
  )

print("ANC visits categories")
table(df_complete$anc_visits_cat, useNA = "ifany")


## ----------------------------------------------------------------------------------------------------------------------------------------
# Calculate summary statistics for selected columns
summary_stats <- df_complete %>%
  summarise(
    across(
      c(samplingweightsnational, 
        sweight, 
        childage, 
        childhbgdL,
        maternalhbgdL, 
        mothereduyears, 
        ancvisits, 
        ),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE)
        ),
      .names = "{.col}_{.fn}"  # Create standardized names for each statistic
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )

print(summary_stats)
write.csv(summary_stats %>% as.data.frame, "results/tables/summary_stats_continous.csv", row.names = F)


## ----------------------------------------------------------------------------------------------------------------------------------------
# Calculate summary statistics for selected columns grouped by anemic status
summary_stats_grouped <- df_complete %>%
  group_by(childanemia) %>%
  summarise(
    across(
      c(samplingweightsnational, 
        sweight, 
        childage, 
        childhbgdL,
        maternalhbgdL, 
        mothereduyears, 
        ),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE)
        ),
      .names = "{.col}_{.fn}"  # Create standardized names for each statistic
    )
  ) %>%
  pivot_longer(
    -childanemia,
    names_to = c("Variable", "Statistic"),
    names_sep = "_",
    values_to = "Value"
  ) %>% 
  pivot_wider(
    values_from = Value,
    names_from = Statistic)%>% 
  relocate(Variable, childanemia) %>% 
  arrange(Variable)

print(summary_stats_grouped)
write.csv(summary_stats_grouped %>% as.data.frame, "results/tables/summary_stats_continousgrouped.csv", row.names = F)



## ----------------------------------------------------------------------------------------------------------------------------------------
# Conduct t-tests for each variable between the two groups
t_test_results <- df_complete %>%
  select(samplingweightsnational, 
        sweight, 
        childage, 
        childhbgdL,
        maternalhbgdL, 
        mothereduyears) %>%
  summarise(
    across(
      everything(),
      ~t.test(. ~ childanemia, data = df_complete)$p.value
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "p_value"
  )

print(t_test_results)



## ----------------------------------------------------------------------------------------------------------------------------------------
categorical_df_complete <- df_complete %>% 
  select(childsex,
        residence,
        childanemia,
        religion_cat,
        caste_cat,
        HH_cooking_fuel_cat, 
        anc_visits_cat)

# Apply the function print_tab1_only() to obtain the frquency of each categorical variable
lapply(categorical_df_complete, print_tab1_only)



## ----------------------------------------------------------------------------------------------------------------------------------------
# Frequency tables by anemia status for each categorical variable
selected_columns <- c("childsex", "residence", "religion_cat", "caste_cat", "HH_cooking_fuel_cat", "anc_visits_cat", "maternal_anemia")

print("Frequency Tables:")
frequency_tables <- lapply(df_complete[selected_columns], 
                           function(var) {
                                   tabpct(var,
                                          df_complete$childanemia,
                                          percent = "row",
                                          graph = FALSE)                               
                             }
                           )



## ----------------------------------------------------------------------------------------------------------------------------------------
# Apply the test function to each categorical variable
print("Statistical Test Results:")
test_results <- lapply(df_complete[selected_columns], test_categorical)

# Print each test result
print(test_results)



## ----------------------------------------------------------------------------------------------------------------------------------------
#Convert childanemia from "Yes" "No" to 1 and 0 for the analysis
df_complete <- df_complete %>%
  mutate(childanemia = ifelse(childanemia == "Yes", 1, 0))


## ----------------------------------------------------------------------------------------------------------------------------------------
#Define variables of interest to conduct the bivariate analysis
exposure_columns<-c("childsex", 
                    "maternal_anemia", 
                    "residence",
                    "religion_cat", 
                    "caste_cat", 
                    "HH_cooking_fuel_cat", 
                    "anc_visits_cat")

# Apply the function to each exposure variable and store the results
results <- lapply(exposure_columns, analyze_exposure)


## ----------------------------------------------------------------------------------------------------------------------------------------
# Multivariate model for maternal_anemia
model_adjusted <- glm(childanemia ~ maternal_anemia + religion_cat + childsex + residence + caste_cat  + HH_cooking_fuel_cat + anc_visits_cat, 
                      data = df_complete, 
                      family = "binomial")
summary(model_adjusted)
# Get the odds ratios and confidence intervals for the adjusted model
odds_ratio_adjusted <- exp(coef(model_adjusted))
print(odds_ratio_adjusted)
conf_int_adjusted <- exp(confint(model_adjusted))
print(conf_int_adjusted)

#No confounder


## ----------------------------------------------------------------------------------------------------------------------------------------
# Multivariate model for religion_cat
model_adjusted <- glm(childanemia ~ religion_cat + maternal_anemia + childsex + residence + caste_cat  + HH_cooking_fuel_cat + anc_visits_cat, 
                      data = df_complete, 
                      family = "binomial")
summary(model_adjusted)
# Get the odds ratios and confidence intervals for the adjusted model
odds_ratio_adjusted <- exp(coef(model_adjusted))
print(odds_ratio_adjusted)
conf_int_adjusted <- exp(confint(model_adjusted))
print(conf_int_adjusted)

#No confounder


## ----------------------------------------------------------------------------------------------------------------------------------------
# Multivariate model for caste_cat
model_adjusted <- glm(childanemia ~  caste_cat + residence + religion_cat, 
                      data = df_complete, 
                      family = "binomial")
summary(model_adjusted)
# Get the odds ratios and confidence intervals for the adjusted model
odds_ratio_adjusted <- exp(coef(model_adjusted))
print(odds_ratio_adjusted)
conf_int_adjusted <- exp(confint(model_adjusted))
print(conf_int_adjusted)

#Adj-OR: 1.1 (1.04, 1.16) per religion adn residence


## ----------------------------------------------------------------------------------------------------------------------------------------
# Multivariate model for anc_visits_cat -> Cr-OR = 1.14 (1.08, 1.2) 
model_adjusted <- glm(childanemia ~ anc_visits_cat + religion + residence, 
                      data = df_complete, 
                      family = "binomial")
summary(model_adjusted)
# Get the odds ratios and confidence intervals for the adjusted model
odds_ratio_adjusted <- exp(coef(model_adjusted))
print(odds_ratio_adjusted)
conf_int_adjusted <- exp(confint(model_adjusted))
print(conf_int_adjusted)

#Adj-OR = 1.10 (1.04, 1.17) per residence and religion and residence


## ----------------------------------------------------------------------------------------------------------------------------------------
# Multivariate model for HH_cooking_fuel_cat -> Cr-OR = 1.08 (1.03, 1.14) 

model_adjusted <- glm(childanemia ~ HH_cooking_fuel_cat, 
                      data = df_complete, 
                      family = "binomial")
summary(model_adjusted)
# Get the odds ratios and confidence intervals for the adjusted model
odds_ratio_adjusted <- exp(coef(model_adjusted))
print(odds_ratio_adjusted)
conf_int_adjusted <- exp(confint(model_adjusted))
print(conf_int_adjusted)
#No confounder


## ----------------------------------------------------------------------------------------------------------------------------------------
# Kernel density plot
kernelplot<-df_complete %>% 
ggplot(aes(childhbgdL , fill = childsex)) +
  geom_density(alpha = 0.3) +
  labs(title = paste0("Hemoglobin Distribution by Child's Sex in ", state),
       x = "Hemoglobin Levels",
       y = "Density") +
  theme_minimal()
kernelplot
ggsave("results/graphs/kernelplot.png")



## ----------------------------------------------------------------------------------------------------------------------------------------
#Boxplot
boxplot <- df_complete %>% 
ggplot(aes(childsex, childhbgdL , fill =childsex )) +
  geom_boxplot() +
  labs(title = paste0("Boxplot of Hemoglobin Levels by Child's Sex in ", state),
       x = "Child's Sex",
       y = "Hemoglobin Levels") +
  theme_minimal()
boxplot
ggsave("results/graphs/boxplot.png")



## ----------------------------------------------------------------------------------------------------------------------------------------
# District level information
district_df_complete <- df_complete %>% 
  group_by(sdist) %>% 
  summarise(
    child_anemia_prevalence = sum(childanemia == 1)/n() * 100, # Child’s anemia prevalence at district level
    mother_education_mean= mean(mothereduyears),        # Mothers' mean years of education at district level
    mother_anemia_prevalence= sum(maternal_anemia == "Yes")/n()*100,
    clean_cooking_prevalence= sum(HH_cooking_fuel_cat == "clean cooking fuel")/n()*100) #Mothers' anemia prevalence rate 
#Capitalize district words using capitalize_words function
district_df_complete <- district_df_complete %>% mutate(district=capitalize_words(sdist))

#Change name of the districts to ensure it is standardized (reference to https://github.com/HindustanTimesLabs/shapefiles/blob/master/state_ut/uttarpradesh/district/uttarpradesh_district.json)
district_df_complete <- district_df_complete %>%
  mutate(district = ifelse(district == "Bara Banki", "Barabanki", 
                           ifelse(district == "Kheri", "Lakhimpur Kheri", 
                                  ifelse(district == "Mahrajganj", "Maharajganj", 
                                         ifelse(district == "Sant Ravidas Nagar (bhadohi)", "Sant Ravi Das Nagar(bhadohi)", 
                                                ifelse(district == "Siddharthnagar", "Siddharth Nagar",
                                                       ifelse(district == "Jyotiba Phule Nagar", "Amroha",
                                                              ifelse(district == "Kanshiram Nagar", "Kasganj", district)
                                                              )
                                                       )
                                                )
                                         )
                                  )
  )
  )

#Bubble chart
bubblechart <- district_df_complete %>% 
ggplot(aes(mother_education_mean, child_anemia_prevalence, size = mother_anemia_prevalence, label = district)) +
  geom_point(alpha = 0.7, color = "blue") + # Plot the bubbles
  geom_text(aes(label = district), vjust = -1.5, size = 1.9) +  # Add district names above the bubbles
  scale_size_continuous(name = "Mother's Anemia Prevalence") +  # Bubble size legend
  labs(title = paste0("Child’s Anemia Prevalence vs. Mothers’ Education and Anemia Prevalence in ", state),
       x = "Mother's Mean Years of Education",
       y = "Child's Anemia Prevalence",
       size = "Mother's Anemia Prevalence") +
  theme_minimal() +
  theme(legend.position = "bottom")

bubblechart
ggsave("results/graphs/bubblechart.png", plot = bubblechart, width = 8, height = 6, dpi = 300)


## ----------------------------------------------------------------------------------------------------------------------------------------
### Wrangle data to obtain dataset

# Load shapefile for Uttar Pradesh districts: extracted from https://github.com/HindustanTimesLabs/shapefiles/blob/master/state_ut/uttarpradesh/district/uttarpradesh_district.json

# Disable s2 geometry to allow reading districts name
sf_use_s2(FALSE)

# Read the Shapefile
districts_sf <- st_read("data-raw/districts.shp", , quiet = TRUE)
districts_sf <- st_make_valid(districts_sf)

# Merge the geospatial data with the district dataset
map_df_complete<- merge(district_df_complete, districts_sf,by = "district", all.x = T) %>% st_as_sf()

# Calculate centroids for district names
centroids <- st_centroid(map_df_complete)



## ----------------------------------------------------------------------------------------------------------------------------------------
# 1. Child Anemia Prevalence
childanemia_map<- map_df_complete %>% 
  ggplot() +
  geom_sf(aes(fill = child_anemia_prevalence), color = "white", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Child Anemia %") +
  geom_text(
    data = centroids,
    aes(
      x = st_coordinates(geometry)[,1],  # Longitude for label placement
      y = st_coordinates(geometry)[,2],  # Latitude for label placement
      label = district               # District name column
    ),
    size = 1.5, color = "black"
  ) +
  labs(title = paste0("Child Anemia Prevalence by District in ", state)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),      # Remove axis titles
    axis.text = element_blank(),       # Remove axis text
    axis.ticks = element_blank(),      # Remove axis ticks
    panel.grid = element_blank()       # Remove grid lines
  )
childanemia_map
ggsave("results/graphs/childanemia_map.png", plot = childanemia_map, width = 8, height = 6, dpi = 300)



## ----------------------------------------------------------------------------------------------------------------------------------------
# 2. Mothers’ Average Years of Education
motedu_map <- map_df_complete %>% 
  ggplot() +
  geom_sf(aes(fill = mother_education_mean), color = "white", size = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Avg. Years of Education") +
  geom_text(
    data = centroids,
    aes(
      x = st_coordinates(geometry)[,1],  # Longitude for label placement
      y = st_coordinates(geometry)[,2],  # Latitude for label placement
      label = district               # District name column
    ),
    size = 1.5, color = "black"
  ) +
  labs(title = paste0("Mother's Education (Mean Years) by District in ", state)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),      # Remove axis titles
    axis.text = element_blank(),       # Remove axis text
    axis.ticks = element_blank(),      # Remove axis ticks
    panel.grid = element_blank()       # Remove grid lines
  )
motedu_map
ggsave("results/graphs/motedu_map.png", plot = motedu_map, width = 8, height = 6, dpi = 300)


## ----------------------------------------------------------------------------------------------------------------------------------------
# 3. Prevalence of clean cooking fuel usage

clean_cooking_map <- map_df_complete %>% 
  ggplot() +
  geom_sf(aes(fill = clean_cooking_prevalence), color = "white", size = 0.1) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Clean Cooking Fuel Usage %") +
  geom_text(
    data = centroids,
    aes(
      x = st_coordinates(geometry)[,1],  # Longitude for label placement
      y = st_coordinates(geometry)[,2],  # Latitude for label placement
      label = district               # District name column
    ),
    size = 1.5, color = "black"
  ) +
  labs(title = paste0("Prevalence of Clean Cooking Fuel Usage by District in ", state)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),      # Remove axis titles
    axis.text = element_blank(),       # Remove axis text
    axis.ticks = element_blank(),      # Remove axis ticks
    panel.grid = element_blank()       # Remove grid lines
  )
clean_cooking_map
ggsave("results/graphs/clean_cooking_map.png", plot = clean_cooking_map, width = 8, height = 6, dpi = 300)

