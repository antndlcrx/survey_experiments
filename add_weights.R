#### Weights ####

#### create weights with survey package ####

### March
unweighted_data <- svydesign(ids = ~1, data = survey_march)


weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

survey_march$weight_poststratify <- weights(weighted)


### February 
unweighted_data <- svydesign(ids = ~1, data = survey_feb)

weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

survey_feb$weight_poststratify <- weights(weighted)

### August
unweighted_data <- svydesign(ids = ~1, data = survey_aug)

weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

survey_aug$weight_poststratify <- weights(weighted)

### July
unweighted_data <- svydesign(ids = ~1, data = survey_july)

weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

survey_july$weight_poststratify <- weights(weighted)


### September 
unweighted_data <- svydesign(ids = ~1, data = survey_sept)


weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

survey_sept$weight_poststratify <- weights(weighted)

#### Weigts created manually ####

pop_strata <- ru_population_frame %>% 
  ungroup() %>% 
  mutate(proportion = Freq/sum(Freq),
         source = "Census 2020")

combined_strata <- combined_strata <- rbind(survey_march_strata,  pop_strata) 

### March
survey_march_strata <- survey_march %>% 
  group_by(gender, age_group, university_education) %>% 
  summarise(count = n()) %>%
  mutate(proportion = count / nrow(survey_march),
         source = "March Survey")


weights_march_strata_man <- left_join(survey_march_strata,
                                      pop_strata,
                                      c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)

survey_march <- survey_march %>%
  left_join(weights_march_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 

### February
weights_feb_strata_man <- left_join(survey_feb_strata,
                                    pop_strata,
                                    c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)

survey_feb <- survey_feb %>%
  left_join(weights_feb_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 

### August

weights_aug_strata_man <- left_join(survey_aug_strata,
                                    pop_strata,
                                    c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)

survey_aug <- survey_aug %>%
  left_join(weights_aug_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 

### July
weights_jul_strata_man <- left_join(survey_jul_strata,
                                    pop_strata,
                                    c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)


survey_july <- survey_july %>%
  left_join(weights_jul_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 

### September 

weights_sept_strata_man <- left_join(survey_sept_strata,
                                     pop_strata,
                                     c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)


survey_sept <- survey_sept %>%
  left_join(weights_sept_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 


#### save data ####
write_csv(survey_sept, "data/cleaned_data/survey_sept_2024_with_weights.csv")
write_csv(survey_july, "data/cleaned_data/survey_july_2024_with_weights.csv")
write_csv(survey_march, "data/cleaned_data/survey_march_2024_with_weights.csv")
write_csv(survey_feb, "data/cleaned_data/survey_feb_2024_with_weights.csv")



