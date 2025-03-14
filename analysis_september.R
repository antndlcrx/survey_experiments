#### Analysis September ####


#### load and prepare data ####

survey_sept <- read_csv("data/cleaned_data/survey_sept_2024_with_weights.csv",
                        locale = locale(encoding = "UTF-8"))


long_data <- survey_sept %>%
  ### Support protesters actions ###
  rename(age_cat = age_group,
         aggressive = DV1C_1,
         lawabiding = DV1C_2,
         dangerous = DV1C_3,
         trustworthy = DV1C_4,
         think_about_me = DV1C_5,
         is_violent = DV1D_1,
         is_lawabiding = DV1D_2,
         is_lifethreatening = DV1D_3) %>% 
  pivot_longer(
    cols = contains("Group"),  # Dynamically select columns containing "Group"
    names_to = "fact_id",      # New column for the group names
    values_to = "support_protesters_actions"  # New column for the values
  ) %>%
  filter(!is.na(support_protesters_actions)) %>% 
  mutate(
    group_number = as.numeric(sub(".*?([0-9])$", "\\1", fact_id)),  # Extract group numbers,
    protest_type = case_when(
      group_number %in% c(1, 2, 3, 4) ~ "environmental",
      group_number %in% c(5, 6, 7, 8) ~ "political",
    ),
    authorisation = case_when(
      group_number %in% c(6, 5, 2, 1) ~ "authorised",
      group_number %in% c(8, 7, 4, 3) ~ "unauthorised"
    ),
    protest_actions = case_when(
      group_number %% 2 == 1 ~ "demonstration",    # Odd group_numbers represent 'demonstration'
      group_number %% 2 == 0 ~ "clash_with_police" # Even group_numbers represent 'clash_with_police'
    ),
    support_protesters_actions = str_trim(support_protesters_actions),
    support_protesters_numeric = case_when(
      support_protesters_actions == "Oпределенно, не одобряю" ~ 1,
      support_protesters_actions == "Cкорее, не одобряю" ~ 2,
      support_protesters_actions == "Cкорее, одобряю" ~ 3,
      support_protesters_actions == "Определенно, одобряю" ~ 4,
      
      #support_protesters_actions == "Затрудняюсь ответить" ~ 5,
      #support_protesters_actions == "Отказ от ответа" ~ 6,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    support_protesters_binary = case_when(
      support_protesters_actions == "Oпределенно, не одобряю" ~ 0,
      support_protesters_actions == "Cкорее, не одобряю" ~ 0,
      support_protesters_actions == "Cкорее, одобряю" ~ 1,
      support_protesters_actions == "Определенно, одобряю" ~ 1,
      
      support_protesters_actions == "Затрудняюсь ответить" ~ 0,
      support_protesters_actions == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    ### Support Protesters' Demands ###
    support_protesters_demands = str_trim(DV1A),
    support_protesters_demands_numeric = case_when(
      support_protesters_demands == "Oпределенно, не одобряю" ~ 1,
      support_protesters_demands == "Cкорее, не одобряю" ~ 2,
      support_protesters_demands == "Cкорее, одобряю" ~ 3,
      support_protesters_demands == "Определенно, одобряю" ~ 4,
      
      #support_protesters_demands == "Затрудняюсь ответить" ~ 5,
      #support_protesters_demands == "Отказ от ответа" ~ 6,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    support_protesters_demands_binary = case_when(
      support_protesters_demands == "Oпределенно, не одобряю" ~ 0,
      support_protesters_demands == "Cкорее, не одобряю" ~ 0,
      support_protesters_demands == "Cкорее, одобряю" ~ 1,
      support_protesters_demands == "Определенно, одобряю" ~ 1,
      
      support_protesters_demands == "Затрудняюсь ответить" ~ 0,
      support_protesters_demands == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    ### Support Arrests ### 
    support_arrests = str_trim(DV1B),
    support_arrests_num = case_when(
      support_arrests == "0 (совершенно не оправданно)" ~ 0,
      support_arrests == "1" ~ 1,
      support_arrests == "2" ~ 2,
      support_arrests == "3" ~ 3,
      support_arrests == "4" ~ 4,
      support_arrests == "5" ~ 5,
      support_arrests == "6" ~ 6,
      support_arrests == "7" ~ 7,
      support_arrests == "8" ~ 8,
      support_arrests == "9" ~ 9,
      support_arrests == "10 (полностью оправданно)" ~ 10,
      support_arrests == "Затрудняюсь ответить" ~ 0,
      support_arrests == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_
    ),
    support_arrests_binary = case_when(
      support_arrests_num <=5 ~ 0,
      support_arrests_num > 5 ~ 1,
    ),
    ### Protesters' Description ###
    across(
      c(aggressive, lawabiding, trustworthy, dangerous, think_about_me),  # List of the variables to transform
      ~ case_when(
        str_trim(.) == "Абсолютно не применимо" ~ 0,
        str_trim(.) == "Скорее не применимо" ~ 0,
        str_trim(.) == "Частично применимо" ~ 1,
        str_trim(.) == "Полностью применимо" ~ 1,
        TRUE ~ 0  # Treat any other response as 0
      ),
      .names = "protesters_are_{col}"  # New variable naming pattern
    ),
    
    ### Protest Description ###
    across(c(is_violent, is_lawabiding, is_lifethreatening),  # List of the variables to transform,
           
           ~ case_when(
             str_trim(.) == "Нет" ~ 0,
             str_trim(.) == "Скорее нет" ~ 0,
             str_trim(.) == "Скорее да" ~ 1,
             str_trim(.) == "Да" ~ 1,
             TRUE ~ 0  # Treat any other response as 0
           )
    )
  ) 

data <- long_data %>% select(c(support_arrests, support_arrests_num, support_protesters_numeric, support_protesters_demands,
                               support_protesters_demands_numeric, protest_type, protest_actions, authorisation, protesters_are_aggressive, protesters_are_lawabiding,
                               protesters_are_trustworthy, protesters_are_think_about_me,
                               protesters_are_dangerous, is_violent, is_lawabiding, is_lifethreatening,
                               weight_manually_calculated, support_arrests_binary,
                               support_protesters_demands_binary, support_protesters_binary))


#### Randomisation Check #### 

test_rand_type <- glm(factor(protest_type) ~ age + gender + university_education,
                      family=binomial, data=long_data)

test_rand_actions <- glm(factor(protest_actions) ~ age + gender + university_education,
                         family=binomial, data=long_data)

test_rand_authorisation <- glm(factor(authorisation) ~ age + gender + university_education,
                               family=binomial, data=long_data)

stargazer(test_rand_type,test_rand_actions, test_rand_authorisation,
          header=FALSE,
          title="Randomisation Check",
          digits=2,
          out = "tables/september/randomisation_check_survey_september.tex")


#### plots #### 
## Protest Type Effect on Support of Protesters' Actions
pdf("plots/september/protest_type_effect_september.pdf", width = 10, height = 5)

plotmeans(formula = support_protesters_binary ~ protest_type,  # 
          data = data,           # the data frame
          xlab = "Protest type",  # x-axis label
          ylab = "Support for protesters' activities",          # y-axis label
          n.label = T              # (don't) display sample size
)
dev.off()

## Authorisation Effect on Support of Protesters' Demands
pdf("plots/september/authorisation_effect_september.pdf", width = 10, height = 5)
plotmeans(formula = support_protesters_demands_binary ~ authorisation,  # 
          data = data,           # the data frame
          xlab = "Protest type",  # x-axis label
          ylab = "Support for protesters' demands",          # y-axis label
          n.label = T              # (don't) display sample size
)
dev.off()

#### Interaction Authorisation and Protest Type on Support Arrests ####


m_arrests <- lm(support_arrests_binary ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)

m_arrests_w <- lm(support_arrests_binary ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)

stargazer(m_arrests, m_arrests_w,
          header=FALSE,
          title="Arrests are justified (Linear Prob. Model, Binary Outcome)",
          digits=2,
          dep.var.labels = c("support arrests", "support arrests (w)"),
          out = "tables/september/interaction_authorisation_type_survey_september.tex")


#### Descriptions of Protesters ####
### Unweighted

m_aggressive <- lm(protesters_are_aggressive ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_lawabiding <- lm(protesters_are_lawabiding ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_trustworthy <- lm(protesters_are_trustworthy ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_think_about_me <- lm(protesters_are_think_about_me ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_dangerous <- lm(protesters_are_dangerous ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)

models = list(m_aggressive, m_lawabiding, m_trustworthy,m_think_about_me, m_dangerous)

stargazer(models,
          header=FALSE,
          title="Protesters Described As...(Linear Prob. Model for Binary Outcome)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("Aggressive", "Law Abiding", "Trustworthy", "Think about Me",
                             "Dangerous"),
          covariate.labels = c("authorisation (unauthorised)", "type (political)", "actions (demonstation)", "auth x type"),
          out="tables/september/descr_protesters_unweighted_survey_september.tex"
)


### Weighted
m_aggressive <- lm(protesters_are_aggressive ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_lawabiding <- lm(protesters_are_lawabiding ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_trustworthy <- lm(protesters_are_trustworthy ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_think_about_me <- lm(protesters_are_think_about_me ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_dangerous <- lm(protesters_are_dangerous ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)

models = list(m_aggressive, m_lawabiding, m_trustworthy,m_think_about_me, m_dangerous)

stargazer(models,
          header=FALSE,
          title="Protesters Described As...(Linear Prob. Model for Binary Outcome. Weighted)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("Aggressive", "Law Abiding", "Trustworthy", "Think about Me",
                             "Dangerous"),
          covariate.labels = c("authorisation (unauthorised)", "type (political)", "actions (demonstation)", "auth x type"),
          out="tables/september/descr_protesters_weighted_survey_september.tex"
)

#### Descriptions of Protests ####
### Unweighted

m_violent <- lm(is_violent ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_lawabiding_protest <- lm(is_lawabiding ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)
m_is_lifethreatening <- lm(is_lifethreatening ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data)

models2 = list(m_violent, m_lawabiding_protest, m_is_lifethreatening)

stargazer(models2,
          header=FALSE,
          title="Protest Described As...(Linear Prob. Model for Binary Outcome))",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("Violent", "Law-Abiding", "Lifethreatening"),
          covariate.labels = c("authorisation (unauthorised)", "type (political)", "actions (demonstation)", "auth x type"),
          out="tables/september/descr_protests_unweighted_survey_september.tex"
)

### Weighted 

m_violent <- lm(is_violent ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_lawabiding_protest <- lm(is_lawabiding ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)
m_is_lifethreatening <- lm(is_lifethreatening ~ factor(authorisation)*factor(protest_type) + factor(protest_actions), data = data, weights = weight_manually_calculated)

models2 = list(m_violent, m_lawabiding_protest, m_is_lifethreatening)

stargazer(models2,
          header=FALSE,
          title="Protest Described As...(Linear Prob. Model for Binary Outcome. Weighted)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("Violent", "Law-Abiding", "Lifethreatening"),
          covariate.labels = c("authorisation (unauthorised)", "type (political)", "actions (demonstation)", "auth x type"),
          out="tables/september/descr_protests_weighted_survey_september.tex"
)

#### multiple imputation ####

long_data <- survey_sept %>%
  ### Support protesters actions ###
  rename(age_cat = age_group,
         aggressive = DV1C_1,
         lawabiding = DV1C_2,
         dangerous = DV1C_3,
         trustworthy = DV1C_4,
         think_about_me = DV1C_5,
         is_violent = DV1D_1,
         is_lawabiding = DV1D_2,
         is_lifethreatening = DV1D_3) %>% 
  pivot_longer(
    cols = contains("Group"),  # Dynamically select columns containing "Group"
    names_to = "fact_id",      # New column for the group names
    values_to = "support_protesters_actions"  # New column for the values
  ) %>%
  filter(!is.na(support_protesters_actions)) %>% 
  mutate(
    group_number = as.numeric(sub(".*?([0-9])$", "\\1", fact_id)),  # Extract group numbers,
    protest_type = case_when(
      group_number %in% c(1, 2, 3, 4) ~ "environmental",
      group_number %in% c(5, 6, 7, 8) ~ "political",
    ),
    authorisation = case_when(
      group_number %in% c(6, 5, 2, 1) ~ "authorised",
      group_number %in% c(8, 7, 4, 3) ~ "unauthorised"
    ),
    protest_actions = case_when(
      group_number %% 2 == 1 ~ "demonstration",    # Odd group_numbers represent 'demonstration'
      group_number %% 2 == 0 ~ "clash_with_police" # Even group_numbers represent 'clash_with_police'
    ),
    support_protesters_actions = str_trim(support_protesters_actions),
    support_protesters_numeric = case_when(
      support_protesters_actions == "Oпределенно, не одобряю" ~ 1,
      support_protesters_actions == "Cкорее, не одобряю" ~ 2,
      support_protesters_actions == "Cкорее, одобряю" ~ 3,
      support_protesters_actions == "Определенно, одобряю" ~ 4,
      
      #support_protesters_actions == "Затрудняюсь ответить" ~ 5,
      #support_protesters_actions == "Отказ от ответа" ~ 6,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    support_protesters_binary = case_when(
      support_protesters_actions == "Oпределенно, не одобряю" ~ 0,
      support_protesters_actions == "Cкорее, не одобряю" ~ 0,
      support_protesters_actions == "Cкорее, одобряю" ~ 1,
      support_protesters_actions == "Определенно, одобряю" ~ 1,
      
      # support_protesters_actions == "Затрудняюсь ответить" ~ 0,
      # support_protesters_actions == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    ### Support Protesters' Demands ###
    support_protesters_demands = str_trim(DV1A),
    support_protesters_demands_numeric = factor(case_when(
      support_protesters_demands == "Oпределенно, не одобряю" ~ 1,
      support_protesters_demands == "Cкорее, не одобряю" ~ 2,
      support_protesters_demands == "Cкорее, одобряю" ~ 3,
      support_protesters_demands == "Определенно, одобряю" ~ 4,
      
      #support_protesters_demands == "Затрудняюсь ответить" ~ 5,
      #support_protesters_demands == "Отказ от ответа" ~ 6,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    )),
    support_protesters_demands_binary = case_when(
      support_protesters_demands == "Oпределенно, не одобряю" ~ 0,
      support_protesters_demands == "Cкорее, не одобряю" ~ 0,
      support_protesters_demands == "Cкорее, одобряю" ~ 1,
      support_protesters_demands == "Определенно, одобряю" ~ 1,
      
      # support_protesters_demands == "Затрудняюсь ответить" ~ 0,
      # support_protesters_demands == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_ # Turn refuse to NA (no NAs in the data before transformation)
    ),
    ### Support Arrests ### 
    support_arrests = str_trim(DV1B),
    support_arrests_num = case_when(
      support_arrests == "0 (совершенно не оправданно)" ~ 0,
      support_arrests == "1" ~ 1,
      support_arrests == "2" ~ 2,
      support_arrests == "3" ~ 3,
      support_arrests == "4" ~ 4,
      support_arrests == "5" ~ 5,
      support_arrests == "6" ~ 6,
      support_arrests == "7" ~ 7,
      support_arrests == "8" ~ 8,
      support_arrests == "9" ~ 9,
      support_arrests == "10 (полностью оправданно)" ~ 10,
      # support_arrests == "Затрудняюсь ответить" ~ 0,
      # support_arrests == "Отказ от ответа" ~ 0,
      TRUE ~ NA_real_
    ),
    support_arrests_binary = case_when(
      support_arrests_num <=5 ~ 0,
      support_arrests_num > 5 ~ 1,
    ),
    ### Protesters' Description ###
    across(
      c(aggressive, lawabiding, trustworthy, dangerous, think_about_me),  # List of the variables to transform
      ~ case_when(
        str_trim(.) == "Абсолютно не применимо" ~ 0,
        str_trim(.) == "Скорее не применимо" ~ 0,
        str_trim(.) == "Частично применимо" ~ 1,
        str_trim(.) == "Полностью применимо" ~ 1,
        TRUE ~ NA_real_  # Treat any other response as 0
      ),
      .names = "protesters_are_{col}"  # New variable naming pattern
    ),
    
    ### Protest Description ###
    across(c(is_violent, is_lawabiding, is_lifethreatening),  # List of the variables to transform,
           
           ~ case_when(
             str_trim(.) == "Нет" ~ 0,
             str_trim(.) == "Скорее нет" ~ 0,
             str_trim(.) == "Скорее да" ~ 1,
             str_trim(.) == "Да" ~ 1,
             TRUE ~ NA_real_  # Treat any other response as 0
           )
    )
  ) 

data <- long_data %>% select(c(protest_type, protest_actions, authorisation, protesters_are_aggressive, protesters_are_lawabiding,
                               protesters_are_trustworthy, protesters_are_think_about_me,
                               protesters_are_dangerous, is_violent, is_lawabiding, is_lifethreatening,
                               weight_manually_calculated, support_arrests_binary,
                               support_protesters_demands_binary, support_protesters_binary,age, gender, university_education))


imputed_data <- mice(data, m=5, maxit=10, method = "logreg", printFlag = FALSE)

### Interaction Authorisation and Protest Type on Support Arrests

# Fit the models with imputed data
fit_arrests <- with(imputed_data, lm(support_arrests_binary ~ factor(authorisation) * factor(protest_type) + factor(protest_actions)))
fit_arrests_w <- with(imputed_data, lm(support_arrests_binary ~ factor(authorisation) * factor(protest_type) + factor(protest_actions), weights = weight_manually_calculated))

# Pool the results
pooled_arrests <- pool(fit_arrests)
pooled_arrests_w <- pool(fit_arrests_w)

# Extract pooled coefficients and standard errors for unweighted model
pooled_summary_arrests <- summary(pooled_arrests) %>%
  mutate(Model = "Unweighted", 
         Result = paste0(round(estimate, 2), " [", round(std.error, 2), "; ", round(statistic, 2), "]"))

# Extract pooled coefficients and standard errors for weighted model
pooled_summary_arrests_w <- summary(pooled_arrests_w) %>%
  mutate(Model = "Weighted", 
         Result = paste0(round(estimate, 2), " [", round(std.error, 2), "; ", round(statistic, 2), "]"))

# Combine the results for both models
results_table <- bind_rows(
  pooled_summary_arrests %>% select(Term = term, Model, Result),
  pooled_summary_arrests_w %>% select(Term = term, Model, Result)
) %>%
  pivot_wider(names_from = Model, values_from = Result)%>% 
  filter(Term!="(Intercept)") %>% 
  mutate(Term = case_when(
    Term == "factor(authorisation)unauthorised" ~ "authorisation (unauthorised)",
    Term == "factor(protest_type)political" ~ "protest type (political)",
    Term == "factor(protest_actions)demonstration" ~ "protest actions (demonstration)",
    Term == "factor(authorisation)unauthorised:factor(protest_type)political" ~ "authorisation * protest type",
    TRUE ~ Term
  ))


# Create an xtable object with custom alignment
table = xtable(results_table, align = c("l","l", "c", "c"),
               caption = "Pooled Results for Support for Arrests")

# Define the note to add below the table
note <- "Note: Estimates are displayed as Estimate [SE; t-value]."

# Add the note below the table using the `add.to.row` argument
add_to_row <- list(pos = list(nrow(results_table)), command = paste0("\\hline\n\\multicolumn{3}{l}{", note, "} \\\\\n"))

print.xtable(table,
             file = "tables/september/interaction_author_type_imputed_survey_july.tex",
             comment = F,
             include.rownames = FALSE,
             add.to.row = add_to_row)

### Descriptions of Protesters (Unweighted)

# List of dependent variables
dependent_vars <- c("protesters_are_aggressive", "protesters_are_lawabiding", 
                    "protesters_are_trustworthy", "protesters_are_think_about_me", 
                    "protesters_are_dangerous")

# Formula template
formula_template <- "~ factor(authorisation) * factor(protest_type) + factor(protest_actions)"

# Run models on imputed datasets and pool results
pooled_models <- lapply(dependent_vars, function(dv) {
  model <- with(imputed_data, lm(as.formula(paste(dv, formula_template))))
  pool(model)
})

# Extract and reshape summaries for all pooled models
results_table <- do.call(rbind, lapply(seq_along(pooled_models), function(i) {
  summary_data <- summary(pooled_models[[i]])
  data.frame(
    Term = summary_data$term,
    Model = dependent_vars[i],
    Estimate = round(summary_data$estimate, 2),
    SE = round(summary_data$std.error, 2),
    t.value = round(summary_data$statistic, 2)
  )
})) %>%
  # Combine Estimate, SE, and t-value into a single string
  mutate(Result = paste0(Estimate, " [", SE, "; ", t.value, "]")) %>%
  select(Term, Model, Result) %>%
  # Reshape the table into the desired format
  pivot_wider(names_from = Model, values_from = Result)%>% 
  filter(Term!="(Intercept)") %>% 
  mutate(Term = case_when(
    Term == "factor(authorisation)unauthorised" ~ "authorisation (unauthorised)",
    Term == "factor(protest_type)political" ~ "protest type (political)",
    Term == "factor(protest_actions)demonstration" ~ "protest actions (demonstration)",
    Term == "factor(authorisation)unauthorised:factor(protest_type)political" ~ "authorisation * protest type",
    TRUE ~ Term
  ),
  aggressive = protesters_are_aggressive,
  law_abiding = protesters_are_lawabiding,
  trustworthy = protesters_are_trustworthy,
  think_about_me = protesters_are_think_about_me, 
  dangerous=protesters_are_dangerous
  ) %>% select(Term, aggressive, law_abiding, trustworthy, think_about_me, dangerous)

# Create an xtable object with custom alignment
table = xtable(results_table, align = c("l","l", "c", "c", "c", "c", "c"),
               caption = "Pooled Results for Descriptions of Protesters (Unweighted)")


# Define the note to add below the table
note <- "Note: Estimates are displayed as Estimate [SE; t-value]."

# Add the note below the table using the `add.to.row` argument
add_to_row <- list(pos = list(nrow(results_table)), command = paste0("\\hline\n\\multicolumn{6}{l}{", note, "} \\\\\n"))

print.xtable(table,
             file = "tables/september/descriptions_protesters_unweighted_imputed_survey_july.tex",
             comment = F,
             include.rownames = FALSE,
             add.to.row = add_to_row)

### Descriptions of Protesters (Weighted)
# List of dependent variables
dependent_vars <- c("protesters_are_aggressive", "protesters_are_lawabiding", 
                    "protesters_are_trustworthy", "protesters_are_think_about_me", 
                    "protesters_are_dangerous")

# Formula template
formula_template <- "~ factor(authorisation) * factor(protest_type) + factor(protest_actions)"

# Run weighted models on imputed datasets and pool results
pooled_weighted_models <- lapply(dependent_vars, function(dv) {
  model <- with(imputed_data, lm(as.formula(paste(dv, formula_template)), weights = weight_manually_calculated))
  pool(model)
})

# Extract and reshape summaries for all pooled weighted models
weighted_results_table <- do.call(rbind, lapply(seq_along(pooled_weighted_models), function(i) {
  summary_data <- summary(pooled_weighted_models[[i]])
  data.frame(
    Term = summary_data$term,
    Model = dependent_vars[i],
    Estimate = round(summary_data$estimate, 2),
    SE = round(summary_data$std.error, 2),
    t.value = round(summary_data$statistic, 2)
  )
})) %>%
  mutate(Result = paste0(Estimate, " [", SE, "; ", t.value, "]")) %>%
  select(Term, Model, Result) %>%
  pivot_wider(names_from = Model, values_from = Result)%>% 
  filter(Term!="(Intercept)") %>% 
  mutate(Term = case_when(
    Term == "factor(authorisation)unauthorised" ~ "authorisation (unauthorised)",
    Term == "factor(protest_type)political" ~ "protest type (political)",
    Term == "factor(protest_actions)demonstration" ~ "protest actions (demonstration)",
    Term == "factor(authorisation)unauthorised:factor(protest_type)political" ~ "authorisation * protest type",
    TRUE ~ Term
  ),
  aggressive = protesters_are_aggressive,
  law_abiding = protesters_are_lawabiding,
  trustworthy = protesters_are_trustworthy,
  think_about_me = protesters_are_think_about_me, 
  dangerous=protesters_are_dangerous
  ) %>% select(Term, aggressive, law_abiding, trustworthy, think_about_me, dangerous)

# Create an xtable object with custom alignment
table = xtable(weighted_results_table, align = c("l","l", "c", "c", "c", "c", "c"),
               caption = "Pooled Results for Descriptions of Protesters (Weighted)")

# Define the note to add below the table
note <- "Note: Estimates are displayed as Estimate [SE; t-value]."

# Add the note below the table using the `add.to.row` argument
add_to_row <- list(pos = list(nrow(weighted_results_table)), command = paste0("\\hline\n\\multicolumn{6}{l}{", note, "} \\\\\n"))

print.xtable(table,
             file = "tables/september/descriptions_protesters_weighted_imputed_survey_july.tex",
             comment = F,
             include.rownames = FALSE,
             add.to.row = add_to_row)

### Descriptions of Protests (Unweighted)
# List of dependent variables
dependent_vars_protests <- c("is_violent", "is_lawabiding", "is_lifethreatening")

# Formula template
formula_template <- "~ factor(authorisation) * factor(protest_type) + factor(protest_actions)"

# Run models on imputed datasets and pool results
pooled_models_protests <- lapply(dependent_vars_protests, function(dv) {
  model <- with(imputed_data, lm(as.formula(paste(dv, formula_template))))
  pool(model)
})

# Extract and reshape summaries for all pooled models
results_table_protests <- do.call(rbind, lapply(seq_along(pooled_models_protests), function(i) {
  summary_data <- summary(pooled_models_protests[[i]])
  data.frame(
    Term = summary_data$term,
    Model = dependent_vars_protests[i],
    Estimate = round(summary_data$estimate, 2),
    SE = round(summary_data$std.error, 2),
    t.value = round(summary_data$statistic, 2)
  )
})) %>%
  mutate(Result = paste0(Estimate, " [", SE, "; ", t.value, "]")) %>%
  select(Term, Model, Result) %>%
  pivot_wider(names_from = Model, values_from = Result) %>% 
  filter(Term!="(Intercept)") %>% 
  mutate(Term = case_when(
    Term == "factor(authorisation)unauthorised" ~ "authorisation (unauthorised)",
    Term == "factor(protest_type)political" ~ "protest type (political)",
    Term == "factor(protest_actions)demonstration" ~ "protest actions (demonstration)",
    Term == "factor(authorisation)unauthorised:factor(protest_type)political" ~ "authorisation * protest type",
    TRUE ~ Term
  ))


# Create an xtable object with custom alignment
table = xtable(results_table_protests, align = c("l","l", "c", "c", "c"),
               caption = "Pooled Results for Descriptions of Protests (Weighted)")

# Define the note to add below the table
note <- "Note: Estimates are displayed as Estimate [SE; t-value]."

# Add the note below the table using the `add.to.row` argument
add_to_row <- list(pos = list(nrow(results_table_protests)), command = paste0("\\hline\n\\multicolumn{4}{l}{", note, "} \\\\\n"))

print.xtable(table,
             file = "tables/september/descriptions_protests_unweighted_imputed_survey_july.tex",
             comment = F,
             include.rownames = FALSE,
             add.to.row = add_to_row)


### Descriptions of Protests (Weighted)
# List of dependent variables
dependent_vars_protests <- c("is_violent", "is_lawabiding", "is_lifethreatening")

# Formula template
formula_template <- "~ factor(authorisation) * factor(protest_type) + factor(protest_actions)"

# Run weighted models on imputed datasets and pool results
pooled_weighted_models_protests <- lapply(dependent_vars_protests, function(dv) {
  model <- with(imputed_data, lm(as.formula(paste(dv, formula_template)), weights = weight_manually_calculated))
  pool(model)
})

# Extract and reshape summaries for all pooled weighted models
weighted_results_table_protests <- do.call(rbind, lapply(seq_along(pooled_weighted_models_protests), function(i) {
  summary_data <- summary(pooled_weighted_models_protests[[i]])
  data.frame(
    Term = summary_data$term,
    Model = dependent_vars_protests[i],
    Estimate = round(summary_data$estimate, 2),
    SE = round(summary_data$std.error, 2),
    t.value = round(summary_data$statistic, 2)
  )
})) %>%
  mutate(Result = paste0(Estimate, " [", SE, "; ", t.value, "]")) %>%
  select(Term, Model, Result) %>%
  pivot_wider(names_from = Model, values_from = Result) %>% 
  filter(Term!="(Intercept)") %>% 
  mutate(Term = case_when(
    Term == "factor(authorisation)unauthorised" ~ "authorisation (unauthorised)",
    Term == "factor(protest_type)political" ~ "protest type (political)",
    Term == "factor(protest_actions)demonstration" ~ "protest actions (demonstration)",
    Term == "factor(authorisation)unauthorised:factor(protest_type)political" ~ "authorisation * protest type",
    TRUE ~ Term
  ))

# Create an xtable object with custom alignment
table = xtable(weighted_results_table_protests, align = c("l","l", "c", "c", "c"),
               caption = "Pooled Results for Descriptions of Protests (Weighted)")

# Define the note to add below the table
note <- "Note: Estimates are displayed as Estimate [SE; t-value]."

# Add the note below the table using the `add.to.row` argument
add_to_row <- list(pos = list(nrow(weighted_results_table_protests)), command = paste0("\\hline\n\\multicolumn{4}{l}{", note, "} \\\\\n"))

print.xtable(table,
             file = "tables/september/descriptions_protests_weighted_imputed_survey_july.tex",
             comment = F,
             include.rownames = FALSE,
             add.to.row = add_to_row)
