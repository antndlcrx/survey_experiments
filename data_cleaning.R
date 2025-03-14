#### Data Cleaning Script #####


#### auxilary categories and helper funcs ####
# categories for education
uni_education <- c("Высшее образование (магистратура)",
                   "Высшее образование (бакалавриат / специалитет)",
                   "Научная степень (кандидат, доктор наук)")

na_education <- c("Затрудняюсь ответить", "Отказ от ответа", "no_answer")

wider_age_categories <- list(
  '18-24' = c('18 – 19', '20 – 24'),
  '25-34' = c('25 – 29', '30 – 34'),
  '35-44' = c('35 – 39', '40 – 44'),
  '45-54' = c('45 – 49', '50 – 54'),
  '55-64' = c('55 – 59', '60 – 64'),
  '65+' = c('65 – 69', '70 и более')
)

# function for age cats mapping
map_age_category <- function(age) {
  for (category in names(wider_age_categories)) {
    if (age %in% wider_age_categories[[category]]) {
      return(category)
    }
  }
  return(NA)
}

# function to calculate shares
calculate_shares <- function(dataset, dataset_name) {
  dataset %>%
    pivot_longer(cols = c(age_group, gender, university_education), names_to = "Variable", values_to = "Values") %>%
    group_by(Variable, Values) %>%
    summarise(Share = n() / nrow(dataset), .groups = 'drop') %>%
    mutate(Survey = dataset_name,
           Share = round(Share, 2))
}


#### Clean Data ####

### population frame for weights ###
ru_population_frame$wider_age <- sapply(ru_population_frame$Age, map_age_category)


collapsed_df <- ru_population_frame %>%
  group_by(Gender, wider_age) %>%
  summarise(
    `BA+` = sum(`BA+`, na.rm = TRUE),
    `BA-` = sum(`BA-`, na.rm = TRUE) + sum(`NA`, na.rm = TRUE)
  )


# harmonise categories
ru_population_frame <- collapsed_df %>%
  mutate(Gender = fct_recode(Gender,
                             "Man"= "Men",
                             "Woman" = "Women")
  ) %>%
  pivot_longer(
    cols = c("BA+", "BA-"), 
    names_to = "Education",
    values_to = "Count"
  )

# harmonise features
colnames(ru_population_frame) <- c("gender", "age_group", "university_education",
                                   "Freq")

### levada ###
levada_omnibus <- levada_omnibus %>% 
  mutate(gender = fct_recode(as.factor(qS1),
                             "Man"= "1",
                             "Woman" = "2"),
         #adjust age
         age_group = case_when(
           as.numeric(qS2) %in% c(18:24) ~ '18-24',
           as.numeric(qS2) %in% c(25:34) ~ '25-34',
           as.numeric(qS2) %in% c(35:44) ~ '35-44',
           as.numeric(qS2) %in% c(45:54) ~ '45-54',
           as.numeric(qS2) %in% c(55:64) ~ '55-64',
           as.numeric(qS2) >= 65 ~ '65+',
           TRUE ~ NA)
  )

### march ###
survey_march <- survey_march %>% 
  mutate(gender = fct_recode(Q4,
                             "Man"= "Мужской",
                             "Woman" = "Женский"),
         #adjust age
         age_group = case_when(
           as.numeric(Q1) %in% c(18:24) ~ '18-24',
           as.numeric(Q1) %in% c(25:34) ~ '25-34',
           as.numeric(Q1) %in% c(35:44) ~ '35-44',
           as.numeric(Q1) %in% c(45:54) ~ '45-54',
           as.numeric(Q1) %in% c(55:64) ~ '55-64',
           as.numeric(Q1) >= 65 ~ '65+',
           TRUE ~ NA),
         university_education = case_when(Q5 %in% uni_education ~ "BA+",
                                          TRUE ~ "BA-")
  )

### February ###
column_descriptions <- as.character(unlist(survey_feb[1, ]))
names(column_descriptions) <- names(survey_feb)
# example, call: column_descriptions["Q1"]

# remove first row
survey_feb <- survey_feb[-1,]

survey_feb <- survey_feb %>%
  mutate(
    gender = as.factor(Q4),
    # adjust dates 
    StartDate = as.POSIXct((as.numeric(StartDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    EndDate = as.POSIXct((as.numeric(EndDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    RecordedDate = as.POSIXct((as.numeric(RecordedDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    # adjust education
    # Q5 %in% na_education ~ "NA" (incude to count NAs)
    university_education = case_when(Q5 %in% uni_education ~ "BA+",
                                     TRUE ~ "BA-"),
    
    gender = fct_recode(gender,
                        "Man"= "Мужской",
                        "Woman" = "Женский"),
    #adjust age
    age_group = case_when(
      as.numeric(Q1) %in% c(18:24) ~ '18-24',
      as.numeric(Q1) %in% c(25:34) ~ '25-34',
      as.numeric(Q1) %in% c(35:44) ~ '35-44',
      as.numeric(Q1) %in% c(45:54) ~ '45-54',
      as.numeric(Q1) %in% c(55:64) ~ '55-64',
      as.numeric(Q1) >= 65 ~ '65+',
      TRUE ~ NA)
  )


### July ###
survey_july <- survey_july[-1,]

survey_july <- survey_july %>% 
  # adjust education
  # education %in% na_education ~ "NA"
  mutate(university_education = case_when(
    Q5 %in% uni_education ~ "BA+",
    TRUE ~ "BA-"),
    gender = fct_recode(Q4,
                        "Man"= "Мужской",
                        "Woman" = "Женский"),
    age = as.numeric(Q1),
    age_group = case_when(
      as.numeric(Q1) %in% c(18:24) ~ '18-24',
      as.numeric(Q1) %in% c(25:34) ~ '25-34',
      as.numeric(Q1) %in% c(35:44) ~ '35-44',
      as.numeric(Q1) %in% c(45:54) ~ '45-54',
      as.numeric(Q1) %in% c(55:64) ~ '55-64',
      as.numeric(Q1) >= 65 ~ '65+',
      TRUE ~ NA),
  )

### August ###
survey_aug <- survey_aug%>% 
  # adjust education
  # education %in% na_education ~ "NA"
  filter(gender != "no_answer") %>% 
  mutate(university_education = case_when(
    education %in% uni_education ~ "BA+",
    TRUE ~ "BA-"),
    gender = fct_recode(gender,
                        "Man"= "Мужской",
                        "Woman" = "Женский"))

### September ###

survey_sept <- survey_sept[-1,]
survey_sept <- survey_sept %>%
  mutate(university_education = case_when(
    Q5 %in% uni_education ~ "BA+",
    TRUE ~ "BA-"),
    gender = fct_recode(Q4,
                        "Man"= "Мужской",
                        "Woman" = "Женский"),
    age = as.numeric(Q1),
    age_group = case_when(
      as.numeric(Q1) %in% c(18:24) ~ '18-24',
      as.numeric(Q1) %in% c(25:34) ~ '25-34',
      as.numeric(Q1) %in% c(35:44) ~ '35-44',
      as.numeric(Q1) %in% c(45:54) ~ '45-54',
      as.numeric(Q1) %in% c(55:64) ~ '55-64',
      as.numeric(Q1) >= 65 ~ '65+',
      TRUE ~ NA),
  )

