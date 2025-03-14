#### Analysis March ####

#### load and prepare data ####
qualtrics <- read_csv("data/cleaned_data/survey_march_2024_with_weights.csv", locale = locale(encoding = "UTF-8"))

#Recode outcomes, higher values greater support, drop NA/Refuse to Answer
table(qualtrics$DV2)
qualtrics$outcome2 <- rep(NA, nrow(qualtrics))
qualtrics$outcome2[qualtrics$DV2=="Подавляющее большинство"] <- 5
qualtrics$outcome2[qualtrics$DV2=="Большинство"] <- 4
qualtrics$outcome2[qualtrics$DV2=="Многие, но не большинство"] <- 3
qualtrics$outcome2[qualtrics$DV2=="Незначительное меньшинство"] <- 2
qualtrics$outcome2[qualtrics$DV2=="Практически никто"] <- 1
qualtrics$outcome2[qualtrics$DV2=="Затрудняюсь ответить"] <- NA
qualtrics$outcome2[qualtrics$DV2=="Отказ от ответа"] <- NA
table(qualtrics$outcome2)

table(qualtrics$DV3)
qualtrics$outcome3 <- rep(NA, nrow(qualtrics))
qualtrics$outcome3[qualtrics$DV3=="Полностью контролирует элиты"] <- 4
qualtrics$outcome3[qualtrics$DV3=="Скорее контролирует элиты"] <- 3
qualtrics$outcome3[qualtrics$DV3=="Скорее не контролирует элиты"] <- 2
qualtrics$outcome3[qualtrics$DV3=="Вовсе не контролирует элиты"] <- 1
qualtrics$outcome3[qualtrics$DV3=="Затрудняюсь ответить"] <- NA
qualtrics$outcome3[qualtrics$DV3=="Отказ от ответа"] <- NA
table(qualtrics$outcome3)

qualtrics$out1<-qualtrics$DV1A
qualtrics$out1[is.na(qualtrics$out1)]<-qualtrics$DV1B[is.na(qualtrics$out1)]
qualtrics$out1[is.na(qualtrics$out1)]<-qualtrics$DV1C[is.na(qualtrics$out1)]
qualtrics$out1[is.na(qualtrics$out1)]<-qualtrics$DV1D[is.na(qualtrics$out1)]
table (qualtrics$out1)

qualtrics$outcome1 <- rep(NA, nrow(qualtrics))
qualtrics$outcome1[qualtrics$out1=="Полностью контролирует политическую ситуацию"] <- 4
qualtrics$outcome1[qualtrics$out1=="Скорее контролирует политическую ситуацию"] <- 3
qualtrics$outcome1[qualtrics$out1=="Скорее не контролирует политическую ситуацию"] <- 2
qualtrics$outcome1[qualtrics$out1=="Вовсе не контролирует политическую ситуацию"] <- 1
table(qualtrics$outcome1)

#Create exp group indicators
qualtrics$t1<-0
qualtrics$t2<-0

qualtrics$t1[!is.na(qualtrics$DV1B) | !is.na(qualtrics$DV1D) ]<-1
qualtrics$t2[!is.na(qualtrics$DV1C) | !is.na(qualtrics$DV1D) ]<-1


qualtrics$elec.know<- qualtrics$Q6_1 > 82 & qualtrics$Q6_1 < 92 
mean(qualtrics$elec.know,na.rm=T)


qualtrics$elec.know2<- qualtrics$Q6_1 > 77 & qualtrics$Q6_1 < 97 
mean(qualtrics$elec.know2,na.rm=T)


qualtrics$comp <- NA
qualtrics$comp[qualtrics$Q7=="Да"] <- 1
qualtrics$comp[qualtrics$Q7=="Нет"] <- 0
mean(qualtrics$comp,na.rm=T)

#### analysis ####
q1<-lm(outcome1 ~ t1*t2,data=qualtrics)
q2<-lm(outcome2 ~ t1*t2,data=qualtrics)
q3<-lm(outcome3 ~ t1*t2,data=qualtrics)

q1wp<-lm(outcome1 ~ t1*t2,data=qualtrics, weights=weight_poststratify)
q2wp<-lm(outcome2 ~ t1*t2,data=qualtrics, weights=weight_poststratify)
q3wp<-lm(outcome3 ~ t1*t2,data=qualtrics, weights=weight_poststratify)

q1wm<-lm(outcome1 ~ t1*t2,data=qualtrics, weights=weight_manually_calculated)
q2wm<-lm(outcome2 ~ t1*t2,data=qualtrics, weights=weight_manually_calculated)
q3wm<-lm(outcome3 ~ t1*t2,data=qualtrics, weights=weight_manually_calculated)

stargazer(q1, q1wp, q1wm,
          digits=2, title="Political Control",
          column.labels=c("No Weight","Weight Post-Stratify","Weight Manually Calculated"),
          dep.var.labels=c("Political Control"),
          covariate.labels = c("Treatment 1", "Treatment 2", "Treatment 1 x Treatment 2"),
          header=FALSE,
          out="tables/march/polit_control_march.tex")

stargazer(q2, q2wp, q2wm, digits=2,
          title="Public Support",
          column.labels=c("No Weight","Weight Post-Stratify","Weight Manually Calculated"),
          dep.var.labels=c("Public Support"),
          covariate.labels = c("Treatment 1", "Treatment 2", "Treatment 1 x Treatment 2"),
          header=FALSE,
          out="tables/march/public_support_march.tex")

stargazer(q3, q3wp, q3wm, digits=2,
          title="Elite Control",
          column.labels=c("No Weight","Weight Post-Stratify","Weight Manually Calculated"),
          dep.var.labels=c("Elite Control"),
          covariate.labels = c("Treatment 1", "Treatment 2", "Treatment 1 x Treatment 2"),
          header=FALSE,
          out="tables/march/elite_control_march.tex")


#### Analysis with Competitiveness and Election Knowledge ####
# no weights
e1<-lm(outcome1 ~ elec.know2*t1 + t2,data=qualtrics)
e2<-lm(outcome2 ~ elec.know2*t1 + t2,data=qualtrics)
e3<-lm(outcome3 ~ elec.know2*t1 + t2,data=qualtrics) ###This is cool

c1<-lm(outcome1 ~ t1 + comp*t2,data=qualtrics)###This is cool
c2<-lm(outcome2 ~ t1 + comp*t2,data=qualtrics)
c3<-lm(outcome3 ~ t1 + comp*t2,data=qualtrics)


stargazer(e1,c1,e2,c2,e3,c3,digits=2,
          header=FALSE,
          no.space = TRUE, # to remove the spaces after each 
          column.sep.width = "3pt", # to reduce column width
          font.size = "small", # to make font size smaller
          dep.var.labels=c("Political Control", "Public Support", "Elite Control"),
          covariate.labels = c("Election Knowledge", "Treatment 1", "Competitiveness",
                               "Treatment 2", "Knowledge x Treatment 1", "Competitiveness x Treatment 2"),
          title="No Weights",
          out="tables/march/treatments_no_weights_march.tex"
)

# weight poststratify
e1<-lm(outcome1 ~ elec.know2*t1 + t2,data=qualtrics, weights=weight_poststratify)
e2<-lm(outcome2 ~ elec.know2*t1 + t2,data=qualtrics, weights=weight_poststratify)
e3<-lm(outcome3 ~ elec.know2*t1 + t2,data=qualtrics, weights=weight_poststratify) ###This is cool

c1<-lm(outcome1 ~ t1 + comp*t2,data=qualtrics, weights=weight_poststratify)###This is cool
c2<-lm(outcome2 ~ t1 + comp*t2,data=qualtrics, weights=weight_poststratify)
c3<-lm(outcome3 ~ t1 + comp*t2,data=qualtrics, weights=weight_poststratify)

stargazer(e1,c1,e2,c2,e3,c3,digits=2,
          header=FALSE,
          no.space = TRUE, # to remove the spaces after each 
          column.sep.width = "3pt", # to reduce column width
          font.size = "small", # to make font size smaller
          dep.var.labels=c("Political Control", "Public Support", "Elite Control"),
          covariate.labels = c("Election Knowledge", "Treatment 1", "Competitiveness",
                               "Treatment 2", "Knowledge x Treatment 1", "Competitiveness x Treatment 2"),
          title="Weight Post-Stratify",
          out="tables/march/treatments_poststrat_weight_march.tex"
)

# weight manual

e1<-lm(outcome1 ~ elec.know2*t1 + t2,data=qualtrics,
       weights=weight_manually_calculated)
e2<-lm(outcome2 ~ elec.know2*t1 + t2,data=qualtrics, weights=weight_manually_calculated)
e3<-lm(outcome3 ~ elec.know2*t1 + t2,data=qualtrics, weights=weight_manually_calculated) ###This is cool

c1<-lm(outcome1 ~ t1 + comp*t2,data=qualtrics, weights=weight_manually_calculated)###This is cool
c2<-lm(outcome2 ~ t1 + comp*t2,data=qualtrics, weights=weight_manually_calculated)
c3<-lm(outcome3 ~ t1 + comp*t2,data=qualtrics, weights=weight_manually_calculated)


stargazer(e1,c1,e2,c2,e3,c3,digits=2,
          header=FALSE,
          no.space = TRUE, # to remove the spaces after each 
          column.sep.width = "3pt", # to reduce column width
          font.size = "small", # to make font size smaller
          dep.var.labels=c("Political Control", "Public Support", "Elite Control"),
          covariate.labels = c("Election Knowledge", "Treatment 1", "Competitiveness",
                               "Treatment 2", "Knowledge x Treatment 1", "Competitiveness x Treatment 2"),
          title="Weight Manually Calculated",
          out="tables/march/treatments_manual_weight_march.tex"
)


