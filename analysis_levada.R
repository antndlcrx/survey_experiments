#### Analysis Levada 2021####

#### load and prepare data###
data <- read_sav("data/21cur.sav")

data = data %>% 
  mutate(
    authorisation = case_when(
      qVAR1 %in% c(1,2,3,4) ~ "authorised",
      qVAR1 %in% c(5,6,7,8) ~ "not_authorised"
    ),
    legal_prompt = case_when(
      qVAR1 %in% c(1,3,5,7) ~ "prompted",
      qVAR1 %in% c(2,4,6,8) ~ "not_prompted"
    ),
    is_peaceful = case_when(
      qVAR1 %in% c(1,2,5,6) ~ "peaceful",
      qVAR1 %in% c(3,4,7,8) ~ "not_peaceful"
    ),
    outcome_bin = case_when(
      qQ1 %in% c(1,2) ~ 1,
      qQ1 %in% c(3,4) ~ 0,
      T ~ NA_real_),
    outcome_bin_na_disaprove= case_when(
      qQ1 %in% c(1,2) ~ 1,
      T ~ 0),
    blame_admin = case_when(
      qQ2 == 1 ~ 1,
      T ~ 0
    ),
    blame_police = case_when(
      qQ2 == 2 ~ 1,
      T ~ 0
    ),
    blame_orgs = case_when(
      qQ2 == 3 ~ 1,
      T ~ 0
    ),
    blame_participants = case_when(
      qQ2 == 4 ~ 1,
      T ~ 0
    ),
    blame_all = case_when(
      qQ2 == 5 ~ 1,
      T ~ 0
    ),
    how_justified_bin = case_when(
      qQ4 %in% c(10,9,8,7,6) ~ 1,
      qQ4 %in% c(5,4,3,2,1) ~ 0,
      TRUE ~ NA_real_
    ),
    how_repressive_bin = case_when(
      qQ5 %in% c(10,9,8,7,6) ~ 0,
      qQ5 %in% c(1,2,3,4,5) ~ 1,
      TRUE ~ NA_real_),
    # treatments
    denied_twice = case_when(
      qVAR2 %in% c(1,2,3,4) ~ "once",
      TRUE ~ "twice"
    ), 
    authority = case_when(
      qVAR2 %in% c(1,2,5,6) ~ "admin",
      TRUE ~ "police"
    ),
    grievance = case_when(
      qVAR2 %in% c(1, 3, 5, 7) ~ "historical",
      TRUE ~ "corruption"
    ),
    
    how_justified_bin_na = case_when(
      qQ4 %in% c(10,9,8,7,6) ~ 1,
      qQ4 %in% c(5,4,3,2,1) ~ 0,
      TRUE ~ 0
    ),
    how_repressive_bin_na = case_when(
      qQ5 %in% c(10,9,8,7,6) ~ 0,
      qQ5 %in% c(1,2,3,4,5) ~ 1,
      TRUE ~ 0),
    how_justified = case_when(
      qQ4 > 10 ~ NA_real_,
      T ~ qQ4
    ),
    how_repressive = case_when(
      qQ5 > 10 ~ NA_real_,
      T ~ qQ5),
    how_justified_na = case_when(
      qQ4 > 10 ~ 5.5,
      T ~ qQ4
    ),
    how_repressive_na = case_when(
      qQ5 > 10 ~ 5.5,
      T ~ qQ5)
  )


#### Binary Analysis####
m1 <- lm(how_justified_bin ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m2 <- lm(how_repressive_bin ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m3 <- lm(how_justified_bin_na ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m4 <- lm(how_repressive_bin_na ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)

models = list(m1,m2,m3,m4)

stargazer(models,
          header=FALSE,
          title="Authority, Grievance, Denied Twice",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("How Justified?", "How Repressive?", "How Justified (NAs)?", "How Repressive?(NAs)"),
          out="tables/levada/main_analysis_binary_levada.tex"
)

#### Continous ####
m1 <- lm(how_justified ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m2 <- lm(how_repressive ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m3 <- lm(how_justified_na ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)
m4 <- lm(how_repressive_na ~ factor(denied_twice)+factor(authority) + factor(grievance), data = data)

models = list(m1,m2,m3,m4)

stargazer(models,
          header=FALSE,
          title="Authority, Grievance, Denied Twice (Continous)",
          digits=2,
          no.space = TRUE,
          dep.var.labels = c("How Justified?", "How Repressive?", "How Justified (NAs)?", "How Repressive?(NAs)"),
          out="tables/levada/main_analysis_continous_levada.tex"
)


#### Support Protesters Actions ####
m1 <- lm(outcome_bin ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)
m2 <- lm(outcome_bin_na_disaprove ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)

stargazer(m1, m2,
          header=FALSE,
          title="Linear Prob. Models for Approval of Protesters' Actions",
          digits=2,
          no.space = TRUE,
          out="tables/levada/protesters_actions_levada.tex"
)

#### Who to blame####
m_admin = lm(blame_admin ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)
m_police = lm(blame_police ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)
m_orgs = lm(blame_orgs ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)
m_parts = lm(blame_participants ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)
m_all = lm(blame_all ~ factor(authorisation)+factor(legal_prompt) + factor(is_peaceful), data = data)

models = list(m_admin, m_police, m_orgs, m_parts, m_all)

stargazer(models,
          header=FALSE,
          title="Linear Prob. Models for Who is to Blame",
          digits=2,
          dep.var.labels = c("Admin",
                             "Police",
                             "Orgs",
                             "Participants",
                             "All"),
          no.space = TRUE,
          out="tables/levada/blame_levada.tex"
)
