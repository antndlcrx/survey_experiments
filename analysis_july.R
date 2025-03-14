#### analysis survey july ####

#### load and prepare data ####
survey_july <- read_csv("data/cleaned_data/survey_july_2024_with_weights.csv") %>% 
  pivot_longer(
    cols = contains("DV1"),  # Dynamically select columns containing "Group"
    names_to = "fact_id",      # New column for the group names
    values_to = "how_justified"  # New column for the values
  ) %>%
  filter(!is.na(how_justified)) %>% 
  mutate(fact_id = sub(".*([A-Za-z])$", "\\1", fact_id),  # Extract group numbers,
         not_repressive_num = case_when(
           DV2 == "Совсем не репрессивными (10)" ~ 1,
           DV2 == "9" ~ 2,
           DV2 == "8" ~ 3,
           DV2 == "7" ~ 4,
           DV2 == "6" ~ 5,
           DV2 == "5" ~ 6,
           DV2 == "4" ~ 7,
           DV2 == "3" ~ 8,
           DV2 == "2" ~ 9,
           DV2 == "Крайне репрессивными (1)" ~ 10,
           DV2 == "Отказ от ответа" ~ 5,
           DV2 == "Затрудняюсь ответить" ~ 5,
           TRUE ~ NA_real_
         ),
         not_repressive_bin = case_when(
           DV2 %in% c("Совсем не репрессивными (10)", "9", "8", "7", "6", "Отказ от ответа", "Затрудняюсь ответить") ~ 0,
           DV2 %in% c("Крайне репрессивными (1)","5", "4", "3", "2") ~ 1,
           TRUE ~ NA_real_),
         
         how_justified_num = case_when(
           how_justified == "Полностью оправданы (10)" ~ 10,
           how_justified == "9" ~ 9,
           how_justified == "8" ~ 8,
           how_justified == "7" ~ 7,
           how_justified == "6" ~ 6,
           how_justified == "5" ~ 5,
           how_justified == "4" ~ 4,
           how_justified == "3" ~ 3,
           how_justified == "2" ~ 2,
           how_justified == "Совершенно не оправданы (1)" ~ 1,
           how_justified == "Отказ от ответа" ~ 5,
           how_justified == "Затрудняюсь ответить" ~ 5,
           TRUE ~ NA_real_),
         how_justified_bin = case_when(
           how_justified %in% c("Полностью оправданы (10)", "9", "8", "7", "6") ~ 1,
           how_justified %in% c("5", "4", "3", "2", "Совершенно не оправданы (1)", "Отказ от ответа", "Затрудняюсь ответить") ~ 0,
           TRUE ~ NA_real_
         ),
         denied_twice = case_when(
           fact_id %in% c("A", "B", "C", "D") ~ "once",
           TRUE ~ "twice"
         ), 
         authority = case_when(
           fact_id %in% c("A", "B", "E", "F") ~ "admin",
           TRUE ~ "police"
         ),
         grievance = case_when(
           fact_id %in% c("A", "C", "E", "H") ~ "historical",
           TRUE ~ "corruption"
         )
  )


#### Randomisation Check ####
test_rand_twice <- glm(factor(denied_twice) ~ age + gender  + university_education,
                       family=binomial, data=survey_july)

test_rand_authority <- glm(factor(authority) ~ age + gender + university_education,
                           family=binomial, data=survey_july)

test_rand_grievance <- glm(factor(grievance) ~ age + gender + university_education,
                           family=binomial, data=survey_july)


stargazer(test_rand_twice, test_rand_authority, test_rand_grievance,
          header = FALSE,
          title = "Randomisation Check",
          digits = 2,
          out = "tables/july/randomisation_check_survey_july.tex") 


#### Experiment Analysis Binary ####
m_repr <- lm(not_repressive_bin ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july)
m_repr_w <- lm(not_repressive_bin ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july, weights = weight_poststratify)
m_just <- lm(how_justified_bin ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july)
m_just_w <- lm(how_justified_bin ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july, weights = weight_poststratify)

models2 = list(m_repr, m_repr_w, m_just, m_just_w)

stargazer(models2,
          header=FALSE,
          title="Linear Prob. Models for Main Outcomes (Models 2 and 4 are Weighted)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("How Repressive?", "How Justified?"),
          covariate.labels = c("authority (police)", "grievance (historical)", "denied times (twice)", "Gender (Woman)", "Age", "University Education"),
          out = "tables/july/experiment_binary_survey_july.tex"
)

#### plots ####

## authority
pdf("plots/july/plotmeans_authority_july.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

# First plot
plotmeans(formula = not_repressive_bin ~ authority,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

# Second plot
plotmeans(formula = how_justified_bin ~ authority,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)
dev.off()

## grievance 
pdf("plots/july/plotmeans_grievance_july.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

plotmeans(formula = not_repressive_bin ~ grievance,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

plotmeans(formula = how_justified_bin ~ grievance,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)

dev.off()

## denied twice

pdf("plots/july/plotmeans_denied_twice_july.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))

plotmeans(formula = not_repressive_bin ~ denied_twice,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

plotmeans(formula = how_justified_bin ~ denied_twice,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)

dev.off()


#### Experiment Analysis Continous ####
m_reprn <- lm(not_repressive_num ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july)
m_repr_wn <- lm(not_repressive_num ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july, weights = weight_poststratify)
m_justn <- lm(how_justified_num ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july)
m_just_wn <- lm(how_justified_num ~ factor(authority)+factor(grievance) + factor(denied_twice) + gender + age + university_education, data = survey_july, weights = weight_poststratify)

models3 = list(m_reprn, m_repr_wn, m_justn, m_just_wn)

stargazer(models3,
          header=FALSE,
          title="Linear Models for Main Outcomes (Models 2 and 4 are Weighted)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("How Repressive?", "How Justified?"),
          covariate.labels = c("authority (police)", "grievance (historical)", "denied times (twice)", "Gender (Woman)", "Age", "University Education"),
          out = "tables/july/experiment_continous_survey_july.tex"
)

#### plots continous ####

## authority
pdf("plots/july/plotmeans_authority_num_july.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

plotmeans(formula = not_repressive_num ~ authority,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

plotmeans(formula = how_justified_num ~ authority,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)

dev.off()

## grievance
pdf("plots/july/plotmeans_grievance_num_july.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

plotmeans(formula = not_repressive_num ~ grievance,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

plotmeans(formula = how_justified_num ~ grievance,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)

dev.off()

## times denied 
dir.create("plots", showWarnings = FALSE)

pdf("plots/july/plotmeans_denied_twice_num_july.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

plotmeans(formula = not_repressive_num ~ denied_twice,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Repressive are Authorities?",          
          n.label = TRUE              
)

plotmeans(formula = how_justified_num ~ denied_twice,  
          data = survey_july,           
          xlab = "",  
          ylab = "How Justified are Authorities' Actions",          
          n.label = TRUE              
)

dev.off()


#### analysis with multiple imputation ####

survey_july <- read_csv("data/cleaned_data/survey_july_2024_with_weights.csv") %>% 
  pivot_longer(
    cols = contains("DV1"),  # Dynamically select columns containing "Group"
    names_to = "fact_id",      # New column for the group names
    values_to = "how_justified"  # New column for the values
  ) %>%
  filter(!is.na(how_justified)) %>% 
  mutate(fact_id = sub(".*([A-Za-z])$", "\\1", fact_id),  # Extract group numbers,
         not_repressive_num = case_when(
           DV2 == "Совсем не репрессивными (10)" ~ 1,
           DV2 == "9" ~ 2,
           DV2 == "8" ~ 3,
           DV2 == "7" ~ 4,
           DV2 == "6" ~ 5,
           DV2 == "5" ~ 6,
           DV2 == "4" ~ 7,
           DV2 == "3" ~ 8,
           DV2 == "2" ~ 9,
           DV2 == "Крайне репрессивными (1)" ~ 10,
           # DV2 == "Отказ от ответа" ~ 5,
           # DV2 == "Затрудняюсь ответить" ~ 5,
           TRUE ~ NA_real_
         ),
         not_repressive_bin = case_when(
           DV2 %in% c("Совсем не репрессивными (10)", "9", "8", "7", "6") ~ 0,
           DV2 %in% c("Крайне репрессивными (1)","5", "4", "3", "2") ~ 1,
           TRUE ~ NA_real_),
         
         how_justified_num = case_when(
           how_justified == "Полностью оправданы (10)" ~ 10,
           how_justified == "9" ~ 9,
           how_justified == "8" ~ 8,
           how_justified == "7" ~ 7,
           how_justified == "6" ~ 6,
           how_justified == "5" ~ 5,
           how_justified == "4" ~ 4,
           how_justified == "3" ~ 3,
           how_justified == "2" ~ 2,
           how_justified == "Совершенно не оправданы (1)" ~ 1,
           # how_justified == "Отказ от ответа" ~ 5,
           # how_justified == "Затрудняюсь ответить" ~ 5,
           TRUE ~ NA_real_),
         how_justified_bin = case_when(
           how_justified %in% c("Полностью оправданы (10)", "9", "8", "7", "6") ~ 1,
           how_justified %in% c("5", "4", "3", "2", "Совершенно не оправданы (1)") ~ 0,
           TRUE ~ NA_real_
         ),
         denied_twice = case_when(
           fact_id %in% c("A", "B", "C", "D") ~ "once",
           TRUE ~ "twice"
         ), 
         authority = case_when(
           fact_id %in% c("A", "B", "E", "F") ~ "admin",
           TRUE ~ "police"
         ),
         grievance = case_when(
           fact_id %in% c("A", "C", "E", "H") ~ "historical",
           TRUE ~ "corruption"
         )
  ) %>% 
  select(age, gender, university_education, not_repressive_bin, how_justified_bin,
         denied_twice, authority, grievance, weight_poststratify)



imputed_data <- mice(survey_july, m=5, maxit=10, method = "logreg", printFlag = FALSE)

# Define the dependent variables and their respective formulas
pooled_models <- list(
  "Not Repressive (Unweighted)" = with(
    imputed_data,
    lm(not_repressive_bin ~ factor(authority) + factor(grievance) + 
         factor(denied_twice) + gender + age + university_education)
  ),
  "Not Repressive (Weighted)" = with(
    imputed_data,
    lm(not_repressive_bin ~ factor(authority) + factor(grievance) + 
         factor(denied_twice) + gender + age + university_education, weights = weight_poststratify)
  ),
  "How Justified (Unweighted)" = with(
    imputed_data,
    lm(how_justified_bin ~ factor(authority) + factor(grievance) + 
         factor(denied_twice) + gender + age + university_education)
  ),
  "How Justified (Weighted)" = with(
    imputed_data,
    lm(how_justified_bin ~ factor(authority) + factor(grievance) + 
         factor(denied_twice) + gender + age + university_education, weights = weight_poststratify)
  )
)

# Pool results and format the output
results_table <- purrr::imap_dfr(pooled_models, ~ {
  summary_data <- summary(pool(.x))
  tibble(
    Term = summary_data$term,
    Model = .y,
    Estimate = round(summary_data$estimate, 2),
    SE = round(summary_data$std.error, 2),
    t.value = round(summary_data$statistic, 2)
  ) %>%
    mutate(Result = paste0(Estimate, " [", SE, "; ", t.value, "]"))
}) %>%
  select(Term, Model, Result) %>%
  pivot_wider(names_from = Model, values_from = Result) %>%
  filter(Term != "(Intercept)") %>% # Exclude the intercept
  mutate(
    Term = case_when(
      Term == "factor(authority)1" ~ "Authority (Yes)",
      Term == "factor(grievance)1" ~ "Grievance (Yes)",
      Term == "factor(denied_twice)1" ~ "Denied Twice (Yes)",
      Term == "genderWoman" ~ "Gender (Woman)",
      Term == "university_educationBA+" ~ "University Education (BA+)",
      TRUE ~ Term
    )
  )

# Create the xtable with the formatted results
xtable_table <- xtable(results_table, align = c("l", "l", "c", "c", "c", "c"))

# Define the note to add below the table
note <- "Note: Estimates are pooled from multiple imputed datasets and displayed as Estimate [SE; t-value]."

# Add the note below the table using `add.to.row`
add_to_row <- list(pos = list(nrow(results_table)), command = paste0("\\hline\n\\multicolumn{5}{l}{", note, "} \\\\\n"))


# Print the table with xtable
print.xtable(xtable_table,
             file = "tables/july/multiple_imputation_results_table_survey_july.tex",
             include.rownames = FALSE,
             comment = FALSE,
             add.to.row = add_to_row)

