# CAnD3-Krishnan
# Replicability Project 

# 1) Create dataset 

data <- read.csv(file.choose())
# select file from location on computer from file finder (should populate)

# clean data, select necessary variables 
# Age (AGEGRP), marital status (MarStH), sex (Sex), highest diploma/degree (HDGREE), Class of worker (COW), and place of work status (POWST) 
# RQ: What most effects likelihood of working from home/workplace flexibility? 
# select necessary variables 
# load necessary pacakages 

install.packages("dplyr")
library(dplyr)
data_set <- data %>% 
  select(AGEGRP, MarStH, Sex, HDGREE, COW, POWST) %>% 
  rename(
    age = AGEGRP,
    marital_status = MarStH, 
    sex = Sex, 
    education_deg = HDGREE, 
    class_of_worker = COW, 
    work_status = POWST
    ) 
head(data_set)

# 2) Code variables 
# Age 
# age measured in 18 5-year age group categories, 88= not available 
# 0= ages 0-19 (1-7); 1= 20s (8,9), 2= 30s (10,11), 3= 40s (12,13), 4= 50s (14, 15), 5= 60s  (16, 17), 6= 70s plus (18, 19, 20, 21), 88= missing/NA

data_set <- data_set %>%
  mutate(
    age_category = case_when(
      age %in% 1:7 ~ 0,  # Ages 0-19 coded as 0
      age %in% c(8, 9) ~ 1,  # 20s
      age %in% c(10, 11) ~ 2,  # 30s
      age %in% c(12, 13) ~ 3,  # 40s
      age %in% c(14, 15) ~ 4,  # 50s
      age %in% c(16, 17) ~ 5,  # 60s
      age %in% c(18, 19, 20, 21) ~ 6,  # 70s plus
      TRUE ~ NA_real_  # Missing/NA
    )
  )
data_set$age_category <- factor(data_set$age_category, 
      levels = 0:6, 
      labels = c("0-19", 
           "20s", 
           "30s", 
           "40s", 
           "50s", 
           "60s", 
           "70s plus"))
head(data_set)
table(data_set$age_category)


# Marital Status 
# marital status in 6 categories, collapse/re-code to binary variable (has been/is married or common law and never been married or common law), no missings; just is married or is not married/common law

table(data_set$marital_status)
# 1- never legally married or common law 
# 2- legally married, not separated 
# 3- common law 
# 4- separated and not common law 
# 5- divorced 
# 6- widowed 

data_set <- data_set %>% 
  mutate(
    married_binary = ifelse(marital_status %in% c(2,3),
    "Currently married/common law", 
    "Not currently married/common law"))
    
head(data_set)
table(data_set$married_binary) 

# Sex 
# measured and coded as binary already, male/female, 1= female, 2= male, no missings  

data_set <- data_set %>%
mutate(
  sex_binary = ifelse(sex == 1,
  "Female",
  "Male"))

head(data_set) 
table(data_set$sex_binary)

# Highest Diploma Earned/Degree of Education 
# measured and coded in 13 categories capturing education attainment 
# 1- no certificate/degree/diploma 
# 2- high school 
# 3- trade certificate
# 4- apprenticeship 
# 5- college/CEGEP (<1 year)
# 6- college/CEGEP (<2 years)
# 7- college/CEGEP (>2 years)
# 8- university, no diploma 
# 9- bachelors degree 
# 10- above university diploma/certificate
# 11- medical degree 
# 12- masters 
# 13- doctorate 
# 88- not available 
# 99- not applicable 

# 0= no college (1, 2, 3), 1= some college (5,6,7), 2= some university (8, 9, 10), 3= beyond bachelors (11, 12, 13), drop 88 and 99 as missing  

data_set <- data_set %>%
  mutate(
    education_category = case_when(
      education_deg %in% c(1, 2, 3) ~ 0,  # No college
      education_deg %in% c(5, 6, 7) ~ 1,  # Some college
      education_deg %in% c(8, 9, 10) ~ 2,  # Some university
      education_deg %in% c(11, 12, 13) ~ 3,  # Beyond bachelors
      education_deg %in% c(88, 99) ~ NA_real_  # Drop as missing
    )
  )
data_set$education_category <- factor(data_set$education_category, 
                levels = 0:3, 
                labels = c("No college", 
                           "Some college", 
                           "Some university", 
                           "Beyond bachelors"))
head(data_set)
table(data_set$education_category) 

# Class of Worker (employment status)
# measured and coded in 6 categories of employment status, 8= not available, 9= not applicable 
# 0= externally employed, 1= self-employed, 8/9= drop as missing 

data_set <- data_set %>%
  mutate( 
    employ_status = case_when(
    class_of_worker == 1 ~ 0, # externally employed 
    class_of_worker == 2 ~ 1, # self-employed 
    class_of_worker %in% c(8,9) ~ NA_real_ # drop as missing 
    ))
data_set$employ_status <- factor(data_set$employ_status, 
                                     levels = 0:1, 
                                     labels = c("Externally employed", 
                                                "Self-employed"))
head(data_set)
table(data_set$employ_status)


# Outcome variable: Place of Work status (flexible/inflexible) 
# measured and coded as categorical variable with 7 categories, re-coded/collapsed into binary outcome variable of must travel for work or not required, 8= not available, 9= not applicable 
# 0=worked outside of home (3,4, 5, 6, 7), 1= worked at home/flexible workplace (1, 2), 8/9 coded as missing 

data_set <- data_set %>%
  mutate(
    work_location = case_when(
      work_status %in% c(3, 4, 5, 6, 7) ~ 0,  # Worked outside of home
      work_status %in% c(1, 2) ~ 1,  # Worked at home/flexible workplace
      work_status %in% c(8, 9) ~ NA_real_  # Drop as missing
    )
  )
data_set$work_location <- factor(data_set$work_location, 
                               levels = 0:1, 
                               labels = c("Worked outside of home", 
                                          "Worked at home/flexible workplace"))

table(data_set$work_location)
head(data_set)

# 3) Build and run the logistic regression/model 

library(ggplot2)

# Logistic regression with work_status as the outcome variable

logistic_model <- glm(work_location ~ age_category + married_binary + education_category + employ_status, 
                      data = data_set, 
                      family = binomial)

# View the summary of the logistic regression model
summary(logistic_model)

# Basic visualizations (these don't mean anything) 

# Distrubution of work_status/work_location 
# Bar plot for work_status
ggplot(data_set, aes(x = work_status)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Work Status",
       x = "Work Status",
       y = "Count") +
  theme_minimal()
  
# Bar plot of employment status (not work status)  

ggplot(data_set, aes(x = employ_status, fill = work_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Working from Home by Employment Status",
       x = "Employment Status",
       y = "Proportion",
       fill = "Work Status") +
  theme_minimal()
















