# Packages
library(dplyr) # to manipulate variables
library(nlme) # To estimate MLMs with R structures
library(emmeans) # To get model-implied means
library(optimx)
library(dfoptim)
library(lmerTest) # To get Satterthwaite DDF in lmer
library(performance) # To get ICC in lmer
library(margins) # To get predicted values like Stata does
library(ordinal) #MGLM for Ordinal Outcome Variable (Holmes, p.151)
library(ggplot2)


# Database ####

## Primary database
dat <- readr::read_delim("8. Multivariate analysis of admission progression/2. Database/ADM2022.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

dat1 <- dat[,c('P012', # RBD
               'P009', # SEX
               'P034_1', # FAMILY INCOME
               'P037', # FATHER EDUCATION
               'P038', # MOTHER EDUCATION
               'P076', # SCHOOL TYPE
               'P077', # SCHOOL SECTOR
               'P084', # RANKING SCORE
               'P086')]  # ADMISSION TEST, MATHEMATICS

colnames(dat1) <- c('RBD', 'SEX', 'FAMILY_INCOME', 'FATHER_ED', 'MOTHER_ED','SCHOOL_TYPE', 'SCHOOL_SECTOR','RANKING', 'MATHEMATICS')

dat2 <- dat1 %>%
  mutate(
    SEX_FEMALE = ifelse(SEX == 1, 0, 1),
    Q_FAM_INC = recode(FAMILY_INCOME,
                       '1' = 1,'2' = 1,
                       '3' = 2,'4' = 2,
                       '5' = 3,'6' = 3,
                       '7' = 4,'8' = 4,
                       '9' = 5,'10' = 5),
    Q_FAM_INC = ifelse(Q_FAM_INC == 99, NA, Q_FAM_INC),
    Q2 = ifelse(Q_FAM_INC == 2,1,
                ifelse(is.na(Q_FAM_INC) == T, NA,0)),
    Q3 = ifelse(Q_FAM_INC == 3,1,
                ifelse(is.na(Q_FAM_INC) == T, NA,0)),
    Q4 = ifelse(Q_FAM_INC == 4,1,
                ifelse(is.na(Q_FAM_INC) == T, NA,0)),
    Q5 = ifelse(Q_FAM_INC == 5,1,
                ifelse(is.na(Q_FAM_INC) == T, NA,0)),
    EdFATHER_HS = ifelse(FATHER_ED >= 6 & FATHER_ED <= 9, 1,
                     ifelse(is.na(FATHER_ED) == T | FATHER_ED == 13 | FATHER_ED == 99, NA, 0)),
    EdFATHER_HE = ifelse(FATHER_ED >= 10 & FATHER_ED <= 12, 1,
                     ifelse(is.na(FATHER_ED) == T | FATHER_ED == 13 | FATHER_ED == 99, NA,0)),
    EdMOTHER_HS = ifelse(MOTHER_ED >= 6 & MOTHER_ED <= 9, 1,
                     ifelse(is.na(MOTHER_ED) == T | MOTHER_ED == 13 | MOTHER_ED == 99, NA,0)),
    EdMOTHER_HE = ifelse(MOTHER_ED >= 10 & MOTHER_ED <= 12, 1,
                     ifelse(is.na(MOTHER_ED) == T | MOTHER_ED == 13 | MOTHER_ED == 99, NA,0)),
    SCHOOL_TYPE_HUM = recode(SCHOOL_TYPE,
                          'H1' = 1,
                          'H2' = 1,
                          'T1' = 0,
                          'T2' = 0,
                          'T3' = 0,
                          'T4' = 0,
                          'T5' = 0),
    P037_1 = ifelse(FATHER_ED == 13 | FATHER_ED == 99, -1, FATHER_ED), # father
    P038_1 = ifelse(MOTHER_ED == 13 | MOTHER_ED == 99, -1, MOTHER_ED), # mother
    FIRST_G = ifelse(P037_1 > 9 | P038_1 > 9, 0, 1),
    FIRST_G = ifelse(P037_1 == -1 & P038_1 == -1, NA, FIRST_G),
    SCHOOL_SECTOR_SUB = ifelse(SCHOOL_SECTOR == 2,1,
                              ifelse(is.na(SCHOOL_SECTOR) == T, NA, 0)),
    SCHOOL_SECTOR_PRIV = ifelse(SCHOOL_SECTOR == 1,1,
                               ifelse(is.na(SCHOOL_SECTOR) == T, NA, 0)),

    RKG = ifelse(RANKING == 0, NA, RANKING),
    MATH = ifelse(MATHEMATICS == 0, NA, MATHEMATICS))

dat3 <- dat2[,c("RBD",
     "SEX_FEMALE",
     "Q_FAM_INC",
     "Q2",
     "Q3",
     "Q4",
     "Q5",
     "EdFATHER_HS",
     "EdFATHER_HE",
     "EdMOTHER_HS",
     "EdMOTHER_HE",
     "SCHOOL_TYPE_HUM",
     "FIRST_G",
     "SCHOOL_SECTOR_SUB",
     "SCHOOL_SECTOR_PRIV",
     "RKG",
     "MATH")]

dat3 <- dat3[complete.cases(dat3),] # 150317

## Additional information of schools  
dir <- readr::read_delim("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/1. Datos Abiertos MINEDUC/Directorio Establecimientos/Directorio-oficial-EE-2021/Directorio_Oficial_EE_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

dir1 <- dir[,c("RBD",
    "COD_DEPE2",
    "RURAL_RBD",
    "PAGO_MATRICULA",
    "PAGO_MENSUAL")]

summary(dir1)

dir2 <- dir1 %>% mutate(
  SCHOOL_SECTOR2 = recode(COD_DEPE2,
                          '4' = 2,
                          '5' = 1),
  FEES = recode(PAGO_MATRICULA,
                '$1.000 A $10.000' = 1,
                '$10.001 A $25.000' = 1,
                '$25.001 A $50.000' = 1,
                '$50.001 A $100.000' = 1,
                'MAS DE $100.000' = 1,
                'SIN INFORMACION' = 99,
                'GRATUITO' = 0),
  TUITION = recode(PAGO_MENSUAL,
                   '$1.000 A $10.000' = 1,
                   '$10.001 A $25.000' = 1,
                   '$25.001 A $50.000' = 1,
                   '$50.001 A $100.000' = 1,
                   'MAS DE $100.000' = 1,
                   'SIN INFORMACION' = 99,
                   'GRATUITO' = 0),
  TUITION_FEES = ifelse(FEES == 1 | TUITION == 1, 1, 0),
  TUITION_FEES = ifelse(FEES == 99 & TUITION == 99, NA, TUITION_FEES))

dir3 <- dir2[,c("RBD", "SCHOOL_SECTOR2", 'RURAL_RBD', "TUITION_FEES")]

sep <- readr::read_delim("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/1. Datos Abiertos MINEDUC/Resumen-Prioritarios-y-Beneficiarios-SEP-2022/20230228_Preferentes_Prioritarios_y_Beneficiarios_SEP_2022_RBD_20221130.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

sep1 <- sep[,c('RBD','CONVENIO_SEP', 'EE_GRATUITO')]

dir_sep <- left_join(x = dir3, y = sep1, by = 'RBD')

dir_sep2 <- dir_sep %>% mutate(
  TUITION_FEES = ifelse(is.na(TUITION_FEES) == T & EE_GRATUITO == 1, 0,TUITION_FEES),
  TUITION_FEES = ifelse(is.na(TUITION_FEES) == T & EE_GRATUITO == 0, 1,TUITION_FEES))

dir_sep2$EE_GRATUITO <- NULL

# write.csv(school_pace,'6. Clustered admission tests results/pace_data.csv', row.names = F)

school_pace <- readr::read_csv("6. Clustered admission tests results/pace_data.csv")

dir_sep3 <- left_join(dir_sep2, school_pace, by = 'RBD')
dir_sep3 <- dir_sep3 %>% mutate(
  SCHOOL_PACE = ifelse(is.na(SCHOOL_PACE) == T, 0,SCHOOL_PACE))

dat4 <- left_join(x = dat3, y = dir_sep3, by = 'RBD')
dat4$SCHOOL_SECTOR2 <- NULL

dat4 <- dat4[complete.cases(dat4),] # 149684

## Splitting schools

schools <- dat4 %>%
  group_by(RBD) %>% 
  summarise(n_analysis = n())

schools <- schools[schools$n_analysis > 9,]
schools$RBD_ID <- 1:nrow(schools)

set.seed(6223)
train <- sample(nrow(schools), nrow(schools)/2) # (# of total schools, # of schools in each half)
development <- schools[train,]
rm(train)
development <- development %>% select(RBD_ID) %>% mutate(train = 1)

schools <- left_join(schools, development, by = "RBD_ID")
rm(development)

schools <- schools %>% mutate(train = ifelse(is.na(train) == T, 0, train))
schools <- select(schools, RBD, train)

dat5 <- left_join(dat4, schools, by = "RBD")
dat5 <- dat5[is.na(dat5$train) == F,] # 147562

## Creating level 2 variables of level 1 predictors

RBD_means <- dat5 %>% #school means
  group_by(RBD) %>%
  summarise(
    SEX_FEMALE_RBD = mean(SEX_FEMALE, na.rm = T),
    Q2_RBD = mean(Q2, na.rm = T),
    Q3_RBD = mean(Q3, na.rm = T),
    Q4_RBD = mean(Q4, na.rm = T),
    Q5_RBD = mean(Q5, na.rm = T),
    EdFATHER_HS_RBD = mean(EdFATHER_HS, na.rm = T),
    EdFATHER_HE_RBD = mean(EdFATHER_HE, na.rm = T),
    EdMOTHER_HS_RBD = mean(EdMOTHER_HS, na.rm = T),
    EdMOTHER_HE_RBD = mean(EdMOTHER_HE, na.rm = T),
    FIRST_G_RBD = mean(FIRST_G, na.rm = T),
    RKG_RBD = mean(RKG, na.rm = T),
    MATH_RBD = mean(MATH, na.rm = T))

RBD_means <- RBD_means %>%
  mutate(
    SEX_FEMALE_L2 = SEX_FEMALE_RBD - .50,
    Q2_L2 = Q2_RBD - .30,
    Q3_L2 = Q3_RBD - .30,
    Q4_L2 = Q4_RBD - .30,
    Q5_L2 = Q5_RBD - .30,
    EdFATHER_HS_L2 = EdFATHER_HS_RBD - .30,
    EdFATHER_HE_L2 = EdFATHER_HE_RBD - .30,
    EdMOTHER_HS_L2 = EdMOTHER_HS_RBD - .30,
    EdMOTHER_HE_L2 = EdMOTHER_HE_RBD - .30,
    FIRST_G_L2 = FIRST_G_RBD - .30,
    RKG_L2 = RKG_RBD - 600)

dat6 <- left_join(dat5, RBD_means, by = 'RBD')

dat6 <- dat6 %>% mutate(
  SEX_FEMALE_CMC = SEX_FEMALE - SEX_FEMALE_RBD,
  Q2_CMC = Q2 - Q2_RBD,
  Q3_CMC = Q3 - Q3_RBD,
  Q4_CMC = Q4 - Q4_RBD,
  Q5_CMC = Q5 - Q5_RBD,
  EdFATHER_HS_CMC = EdFATHER_HS - EdFATHER_HS_RBD,
  EdFATHER_HE_CMC = EdFATHER_HE - EdFATHER_HE_RBD,
  EdMOTHER_HS_CMC = EdMOTHER_HS - EdMOTHER_HS_RBD,
  EdMOTHER_HE_CMC = EdMOTHER_HE - EdMOTHER_HE_RBD,
  FIRST_G_CMC = FIRST_G - FIRST_G_RBD,
  RKG_CMC = RKG - RKG_RBD,
  MATH_CMC = MATH - MATH_RBD)

dat6 <- dat6 %>% mutate(
  ED_FATHER = ifelse(EdFATHER_HS == 0 & EdFATHER_HE == 0,0,
                     ifelse(EdFATHER_HS == 1, 1,2)),
  ED_MOTHER = ifelse(EdMOTHER_HS == 0 & EdMOTHER_HE == 0,0,
                     ifelse(EdMOTHER_HS == 1, 1,2)),
  SCHOOL_SECTOR = ifelse(SCHOOL_SECTOR_SUB == 0 & SCHOOL_SECTOR_PRIV == 0,0,
                         ifelse(SCHOOL_SECTOR_SUB == 1, 1,2))) # 147562

tr_dat <- filter(dat6, train == 0) # validation data, 72.968
val_dat <- filter(dat6, train == 1) # training data, 74.594

# write.csv(tr_dat,'6. Clustered admission tests results/training_data.csv', row.names = F)
# write.csv(val_dat,'6. Clustered admission tests results/validation_data.csv', row.names = F)

# rm(list = ls())

# Descriptive analysis ####

names(dat6)
str(dat6)

dat7 <- dat6[,c("SEX_FEMALE",
        "Q_FAM_INC",
        "ED_FATHER",
        "ED_MOTHER",
        "SCHOOL_TYPE_HUM",
        "RURAL_RBD",
        "TUITION_FEES",
        "CONVENIO_SEP",
        "SCHOOL_PACE",
        "SCHOOL_SECTOR",
        "RKG",
        "MATH")]

dat7 <- dat7 %>% mutate(across(c(SEX_FEMALE,
        Q_FAM_INC,
        ED_FATHER,
        ED_MOTHER,
        SCHOOL_TYPE_HUM,
        RURAL_RBD,
        TUITION_FEES,
        CONVENIO_SEP,
        SCHOOL_PACE,
        SCHOOL_SECTOR), as.factor))

str(dat7)

dat8 <- dat7 %>%
  select(!RKG) %>% 
  tidyr::pivot_longer(!MATH, names_to = "var", values_to = "label")

dat8 <- dat8 %>% mutate(
  label2 = ifelse(var == 'SEX_FEMALE' & label == 0, 1,
           ifelse(var == 'SEX_FEMALE' & label == 1, 2,
           ifelse(var == 'Q_FAM_INC' & label == 1, 3,
           ifelse(var == 'Q_FAM_INC' & label == 2, 4,
           ifelse(var == 'Q_FAM_INC' & label == 3, 5,
           ifelse(var == 'Q_FAM_INC' & label == 4, 6,
           ifelse(var == 'Q_FAM_INC' & label == 5, 7,
           ifelse(var == 'ED_FATHER' & label == 0, 8,
           ifelse(var == 'ED_FATHER' & label == 1, 9,
           ifelse(var == 'ED_FATHER' & label == 2, 10,
           ifelse(var == 'ED_MOTHER' & label == 0, 11,
           ifelse(var == 'ED_MOTHER' & label == 1, 12,
           ifelse(var == 'ED_MOTHER' & label == 2, 13,
           ifelse(var == 'SCHOOL_TYPE_HUM' & label == 0, 14,
           ifelse(var == 'SCHOOL_TYPE_HUM' & label == 1, 15,
           ifelse(var == 'RURAL_RBD' & label == 0, 16,
           ifelse(var == 'RURAL_RBD' & label == 1, 17,
           ifelse(var == 'TUITION_FEES' & label == 0, 18,
           ifelse(var == 'TUITION_FEES' & label == 1, 19,
           ifelse(var == 'CONVENIO_SEP' & label == 0, 20,
           ifelse(var == 'CONVENIO_SEP' & label == 1, 21,
           ifelse(var == 'SCHOOL_PACE' & label == 0, 22,
           ifelse(var == 'SCHOOL_PACE' & label == 1, 23,
           ifelse(var == 'SCHOOL_SECTOR' & label == 0, 24,
           ifelse(var == 'SCHOOL_SECTOR' & label == 1, 25,
           ifelse(var == 'SCHOOL_SECTOR' & label == 2, 26, NA)))))))))))))))))))))))))))

str(dat8)
dat8$label2 <- as.factor(dat8$label2)

dat8$label2 <- factor(dat8$label2,
                              levels=c("1","2","3","4","5","6","7","8","9","10",
                                       "11","12","13","14","15","16","17","18","19",
                                       "20","21","22","23","24","25","26"),
                              labels = c("Mujeres",
                                         "Hombres",
                                         "Quintil 1",
                                         "Quintil 2",
                                         "Quintil 3",
                                         "Quintil 4",
                                         "Quintil 5",
                                         "Padre educacion básica",
                                         "Padre educacion media",
                                         "Padre educacion superior",
                                         "Madre educacion básica",
                                         "Madre educacion media",
                                         "Madre educacion superior",
                                         "Colegio Técnico",
                                         "Colegio Humanista",
                                         "Colegio Urbano",
                                         "Colegio Rural",
                                         "Colegio Copago",
                                         "Colegio Gratuito",
                                         "Colegio sin SEP",
                                         "Colegio SEP",
                                         "Colegio sin PACE",
                                         "Colegio PACE",
                                         "Colegio Publico",
                                         "Colegio Part Subv",
                                         "Colegio Part Priv"))

library(forcats)

descriptives <- ggplot(dat8 %>% 
         tidyr::drop_na(label2), aes(y = fct_reorder(label2, desc(label2)), x = MATH))+
  geom_violin(aes(fill = label2),alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA) +
  labs(y = "Variables de caracterización", x = 'Puntaje PDT Matemática (media = 505.156)')+
  scale_x_continuous(limits = c(150, 850), breaks = seq(400, 800, 200)) +
  theme_bw(base_size = 12)+
  theme(legend.position="")+
  geom_vline(xintercept = mean(dat8$MATH), colour="red", linetype = "longdash")

ggsave(
  filename = 'means.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = descriptives,
  device = png,
  width = 7,
  height = 9,
  dpi = 500)

# Correlation matrix

library(polycor)
library(psych)

dat9 <- dat6[,c(# Nivel 2
                "SEX_FEMALE_CMC",
                "Q_FAM_INC",
                "ED_FATHER",
                "ED_MOTHER",
                "RKG_CMC",
                # Nivel 2
                "SCHOOL_TYPE_HUM",
                "RURAL_RBD",
                "TUITION_FEES",
                "CONVENIO_SEP",
                "SCHOOL_PACE",
                "SCHOOL_SECTOR",
                "MATH_RBD",
                "MATH_CMC")]

# Level 1 variables

# Sex

cor.test(x=as.numeric(dat9$SEX_FEMALE_CMC), y=as.numeric(dat9$Q_FAM_INC), method = 'spearman')
cor.test(x=as.numeric(dat9$SEX_FEMALE_CMC), y=as.numeric(dat9$ED_FATHER), method = 'spearman')
cor.test(x=as.numeric(dat9$SEX_FEMALE_CMC), y=as.numeric(dat9$ED_MOTHER), method = 'spearman')

cor.test(y = dat9$SEX_FEMALE_CMC, x = dat9$RKG_CMC)
cor.test(y = dat9$SEX_FEMALE_CMC, x = dat9$MATH_CMC)

# Quintil family income

DescTools::CorPolychor(dat9$Q_FAM_INC, dat9$ED_FATHER)
DescTools::CorPolychor(dat9$Q_FAM_INC, dat9$ED_MOTHER)

polycor::polyserial(x = dat9$RKG_CMC, y = dat9$Q_FAM_INC)
cor.test(x = dat9$RKG_CMC, y = dat9$MATH_CMC)

# Education Father

DescTools::CorPolychor(dat9$ED_FATHER, dat9$ED_MOTHER)

polycor::polyserial(x = dat9$RKG_CMC, y = dat9$ED_FATHER)
polycor::polyserial(x = dat9$MATH_CMC, y = dat9$ED_FATHER)

# Education Mother

polycor::polyserial(x = dat9$RKG_CMC, y = dat9$ED_MOTHER)
polycor::polyserial(x = dat9$MATH_CMC, y = dat9$ED_MOTHER)

# Ranking

cor.test(dat9$RKG_CMC, dat9$MATH_CMC)

# Level 2 variables

# Type of school

stype_sterri <- table(x = as.numeric(dat9$SCHOOL_TYPE_HUM), y = as.numeric(dat9$RURAL_RBD))
stype_sterri <-  matrix(stype_sterri, 2, 2)
print(tetrachoric(x = stype_sterri), digits = 3)

stype_TF <- table(x = as.numeric(dat9$SCHOOL_TYPE_HUM), y = as.numeric(dat9$TUITION_FEES))
stype_TF <-  matrix(stype_TF, 2, 2)
print(tetrachoric(x = stype_TF), digits = 3)

stype_spe <- table(x = as.numeric(dat9$SCHOOL_TYPE_HUM), y = as.numeric(dat9$CONVENIO_SEP))
stype_spe <-  matrix(stype_spe, 2, 2)
print(tetrachoric(x = stype_spe), digits = 3)

stype_pace <- table(x = as.numeric(dat9$SCHOOL_TYPE_HUM), y = as.numeric(dat9$SCHOOL_PACE))
stype_pace <-  matrix(stype_pace, 2, 2)
print(tetrachoric(x = stype_pace), digits = 3)

cor.test(x=as.numeric(dat9$SCHOOL_TYPE_HUM), y=as.numeric(dat9$SCHOOL_SECTOR), method = 'spearman')
biserial(y = dat9$SCHOOL_TYPE_HUM, x = dat9$MATH_RBD)

# School territory

sterri_TF <- table(x = as.numeric(dat9$RURAL_RBD), y = as.numeric(dat9$TUITION_FEES))
sterri_TF <-  matrix(sterri_TF, 2, 2)
print(tetrachoric(x = sterri_TF), digits = 3)

sterri_sep <- table(x = as.numeric(dat9$RURAL_RBD), y = as.numeric(dat9$CONVENIO_SEP))
sterri_sep <-  matrix(sterri_sep, 2, 2)
print(tetrachoric(x = sterri_sep), digits = 3)

sterri_pace <- table(x = as.numeric(dat9$RURAL_RBD), y = as.numeric(dat9$SCHOOL_PACE))
sterri_pace <-  matrix(sterri_pace, 2, 2)
print(tetrachoric(x = sterri_pace), digits = 3)

cor.test(x=as.numeric(dat9$RURAL_RBD), y=as.numeric(dat9$SCHOOL_SECTOR), method = 'spearman')
biserial(y = dat9$RURAL_RBD, x = dat9$MATH_RBD)

# Tuition and Fees

TF_sep <- table(x = as.numeric(dat9$TUITION_FEES), y = as.numeric(dat9$CONVENIO_SEP))
TF_sep <-  matrix(TF_sep, 2, 2)
print(tetrachoric(x = TF_sep), digits = 3)

TF_pace <- table(x = as.numeric(dat9$TUITION_FEES), y = as.numeric(dat9$SCHOOL_PACE))
TF_pace <-  matrix(TF_pace, 2, 2)
print(tetrachoric(x = TF_pace), digits = 3)

cor.test(x=as.numeric(dat9$TUITION_FEES), y=as.numeric(dat9$SCHOOL_SECTOR), method = 'spearman')
biserial(y = dat9$TUITION_FEES, x = dat9$MATH_RBD)

# School SEP

sep_pace <- table(x = as.numeric(dat9$CONVENIO_SEP), y = as.numeric(dat9$SCHOOL_PACE))
sep_pace <-  matrix(sep_pace, 2, 2)
print(tetrachoric(x = sep_pace), digits = 4)

cor.test(x=as.numeric(dat9$CONVENIO_SEP), y=as.numeric(dat9$SCHOOL_SECTOR), method = 'spearman')
biserial(y = dat9$CONVENIO_SEP, x = dat9$MATH_RBD)

# School PACE

cor.test(x=as.numeric(dat9$SCHOOL_PACE), y=as.numeric(dat9$SCHOOL_SECTOR), method = 'spearman')
biserial(y = dat9$SCHOOL_PACE, x = dat9$MATH_RBD)

# School sector

polycor::polyserial(x = dat9$MATH_RBD, y = dat9$SCHOOL_SECTOR)

# Models ####

training_data <- data.frame(readr::read_csv("6. Clustered admission tests results/training_data.csv"))
training_data <- training_data %>% mutate(
  Q_FAM_INC = factor(Q_FAM_INC),
  ED_FATHER = factor(ED_FATHER),
  ED_MOTHER = factor(ED_MOTHER),
  SCHOOL_SECTOR = factor(SCHOOL_SECTOR))

# Empty model

summary(lm(MATH ~ 1, data = training_data))

empty_ri <- lmer(
  formula = MATH ~ 1 + (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = TRUE,
  data = training_data)
summary(empty_ri, ddf="Satterthwaite"); llikAIC(empty_ri, chkREML=FALSE)

icc(empty_ri); ranova(empty_ri, reduce.term=TRUE)

icc_empty_ri <- data.frame(VarCorr(empty_ri),comp=c("Variance"))
round(icc_empty_ri[1,4]/(sum(icc_empty_ri[,4])),3) # 0.313

AF1 <- allFit(empty_ri, verbose=F)
AF1_lliks <- sort(sapply(AF1,logLik))
AF1_lliks

# Testing whether level 1 predictors have level 2 variance 
m1 <- clm(ED_MOTHER ~ 1, data = training_data)
m2 <- clmm(ED_MOTHER ~ 1 + (1|RBD), data = training_data, nAGQ = 5L, link = "logit")

icc(m2); anova(m1,m2, test = 'LRT') # 0.325

m3 <- clm(ED_FATHER ~ 1, data = training_data)
m4 <- clmm(ED_FATHER ~ 1 + (1|RBD), data = training_data, nAGQ = 5L, link = "logit")

icc(m4); anova(m3,m4, test = 'LRT') # 0.381

m5 <- clm(Q_FAM_INC ~ 1, data = training_data)
m6 <- clmm(Q_FAM_INC ~ 1 + (1|RBD), data = training_data, nAGQ = 1L, link = "logit")

icc(m6); anova(m5,m6, test = 'LRT')  # 

m7 <- clm(factor(SEX_FEMALE) ~ 1, data = training_data)
m8 <- clmm(factor(SEX_FEMALE) ~ 1 + (1|RBD), data = training_data, nAGQ = 5L, link = "logit")

icc(m8); anova(m7,m8, test = 'LRT') # 

a1 = glmer(data=training_data, family=binomial(link="logit"), formula = SEX_FEMALE~1+(1|RBD))
a2 = glm(data=training_data, family=binomial(link="logit"), formula=SEX_FEMALE~1)

a1@theta^2/(a1@theta^2+(pi^2/3)) 

# Variable and ICC, p-values can be included as a note.
# 3,29 logit metric

# Likelihood Ratio Test for Addition of Random Intercept Variance
DevTest=-2*(logLik(a2)-logLik(a1))
Pvalue=pchisq((DevTest), df=1, lower.tail=FALSE)
# Test Statistic and P-values for DF=1 
DevTest; Pvalue

m9 <- lmer(
  formula = RKG ~ 1 + (1|RBD),
  REML = TRUE,
  data = training_data)

icc(m9); ranova(m9, reduce.term = T) #0.1540714
summary(m9, ddf="Satterthwaite")

# Model with level 1 effects

m10 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
  Q2_CMC+
  Q3_CMC+
  Q4_CMC+
  Q5_CMC+
  EdFATHER_HS_CMC+
  EdFATHER_HE_CMC+
  EdMOTHER_HS_CMC+
  EdMOTHER_HE_CMC+
  RKG_CMC +
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m10, ddf="Satterthwaite"); llikAIC(m10, chkREML=FALSE)


m11 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m11, ddf="Satterthwaite"); llikAIC(m11, chkREML=FALSE)

m12 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_SECTOR_SUB+
    SCHOOL_SECTOR_PRIV+
    RURAL_RBD+
    TUITION_FEES+
    CONVENIO_SEP+
    SCHOOL_PACE+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m12, ddf="Satterthwaite"); llikAIC(m12, chkREML=FALSE)

# contestMD() for more than 1 slope (contribution of the variable to the model)

m13 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_SECTOR_SUB+
    SCHOOL_SECTOR_PRIV+
    RURAL_RBD+
    TUITION_FEES+
    SCHOOL_PACE+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m13, ddf="Satterthwaite"); llikAIC(m13, chkREML=FALSE)

m14 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_SECTOR_SUB+
    SCHOOL_SECTOR_PRIV+
    RURAL_RBD+
    SCHOOL_PACE+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m14, ddf="Satterthwaite"); llikAIC(m14, chkREML=FALSE)

m15 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_SECTOR_SUB+
    SCHOOL_SECTOR_PRIV+
    SCHOOL_PACE+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m15, ddf="Satterthwaite"); llikAIC(m15, chkREML=FALSE)

contestMD(m15, ddf="Satterthwaite",
          L = rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
                    c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)))

m16 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = F,
  data = training_data)
summary(m16, ddf="Satterthwaite"); llikAIC(m16, chkREML=FALSE)

m17 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m17, ddf="Satterthwaite"); llikAIC(m17, chkREML=FALSE)

m18 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC
       |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m18, ddf="Satterthwaite"); llikAIC(m18, chkREML=FALSE)

DevTest <- -2*(logLik(m16) - logLik(m17))
pchisq((DevTest), df=2, lower.tail=FALSE)

m19 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC+
       Q2_CMC+
       Q3_CMC+
       Q4_CMC+
       Q5_CMC
     |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m19, ddf="Satterthwaite"); llikAIC(m19, chkREML=FALSE)

DevTest <- -2*(logLik(m18) - logLik(m19))
pchisq((DevTest), df=18, lower.tail=FALSE)

m20 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC+
       EdFATHER_HS_CMC+
       EdFATHER_HE_CMC
     |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m20, ddf="Satterthwaite"); llikAIC(m20, chkREML=FALSE)

DevTest <- -2*(logLik(m18) - logLik(m20))
pchisq((DevTest), df=7, lower.tail=FALSE)

m21 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC+
       EdMOTHER_HS_CMC+
       EdMOTHER_HE_CMC
     |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m21, ddf="Satterthwaite"); llikAIC(m21, chkREML=FALSE)

DevTest <- -2*(logLik(m18) - logLik(m21))
pchisq((DevTest), df=7, lower.tail=FALSE)

m22 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC+
       RKG_CMC
     |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = training_data)
summary(m22, ddf="Satterthwaite"); llikAIC(m22, chkREML=FALSE)

DevTest <- -2*(logLik(m18) - logLik(m22))
pchisq((DevTest), df=3, lower.tail=FALSE)

m18_coeff <- data.frame(summary(m18, ddf="Satterthwaite")$coeff)
m18_coeff <- print(m18_coeff, digits = 3)
m18_coeff <- m18_coeff[,c(1,2)]

# Pseudo standardized slopes

res_var <- data.frame(VarCorr(m18))[4,5]
ran_int_var <- data.frame(VarCorr(m18))[1,5]

m18_coeff[2,1] * (sd(training_data$SEX_FEMALE_CMC) / res_var)
m18_coeff[3,1] * (sd(training_data$Q2_CMC) / res_var)
m18_coeff[4,1] * (sd(training_data$Q3_CMC) / res_var)
m18_coeff[5,1] * (sd(training_data$Q4_CMC) / res_var)
m18_coeff[6,1] * (sd(training_data$Q5_CMC) / res_var)
m18_coeff[7,1] * (sd(training_data$EdFATHER_HS_CMC) / res_var)
m18_coeff[8,1] * (sd(training_data$EdFATHER_HE_CMC) / res_var)
m18_coeff[9,1] * (sd(training_data$EdMOTHER_HS_CMC) / res_var)
m18_coeff[10,1] * (sd(training_data$EdMOTHER_HE_CMC) / res_var)
m18_coeff[11,1] * (sd(training_data$RKG_CMC) / res_var)

m18_coeff[12,1] * (sd(training_data$SEX_FEMALE_L2) / ran_int_var)
m18_coeff[13,1] * (sd(training_data$Q2_L2) / ran_int_var)
m18_coeff[14,1] * (sd(training_data$Q3_L2) / ran_int_var)
m18_coeff[15,1] * (sd(training_data$Q4_L2) / ran_int_var)
m18_coeff[16,1] * (sd(training_data$Q5_L2) / ran_int_var)
m18_coeff[17,1] * (sd(training_data$EdFATHER_HS_L2) / ran_int_var)
m18_coeff[18,1] * (sd(training_data$EdFATHER_HE_L2) / ran_int_var)
m18_coeff[19,1] * (sd(training_data$EdMOTHER_HS_L2) / ran_int_var)
m18_coeff[20,1] * (sd(training_data$EdMOTHER_HS_L2) / ran_int_var)
m18_coeff[21,1] * (sd(training_data$RKG_L2) / ran_int_var)
m18_coeff[22,1] * (sd(training_data$SCHOOL_TYPE_HUM) / ran_int_var)
m18_coeff[23,1] * (sd(training_data$SCHOOL_PACE) / ran_int_var)

#Conditional ICC
ICC_cond <- data.frame(VarCorr(m18),comp=c("Variance"))
#round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(icc_empty_ri[1,4] - ICC_cond[1,4]) / icc_empty_ri[1,4] #PseudoR2 at level 2 relative to EM
# 0.784198
(icc_empty_ri[2,4] - ICC_cond[4,4]) / icc_empty_ri[2,4] #PseudoR2 at level 1 relative to EM
# 0.2143744

training_data$m18 <- predict(m18, re.form=NA)
(cor.test(training_data$m18, training_data$MATH, method="pearson")$estimate)^2 #Total R2 relative to EM
# 0.3867511

# Plotting probabilities

training_data <- training_data %>% mutate(
  SCHOOL_SECTOR = factor(SCHOOL_SECTOR,
                                        levels=c("0","1","2"),
                                        labels = c("Público",
                                                   "Particular Subvencionado",
                                                   "Privado")),
  SCHOOL_TYPE_HUM = factor(SCHOOL_TYPE_HUM,
                                        levels=c("0","1"),
                                        labels = c("Técnico Profesional",
                                                   "Humanista")),
  SCHOOL_PACE = factor(SCHOOL_PACE,
                                        levels=c("0","1"),
                                        labels = c("Sin PACE",
                                                   "Con PACE")))

## By sex and school sector
probs_sex_school <- ggplot(training_data, aes(y = m18, x = as.factor(SEX_FEMALE)))+
  geom_violin(aes(fill = as.factor(SEX_FEMALE)), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Sexo", labels = c("Hombres", "Mujeres"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_SECTOR)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_sex_school.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_sex_school,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)

## By sex and school type
probs_sex_schooltype <- ggplot(training_data, aes(y = m18, x = as.factor(SEX_FEMALE)))+
  geom_violin(aes(fill = as.factor(SEX_FEMALE)), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Sexo", labels = c("Hombres", "Mujeres"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_TYPE_HUM)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_sex_schooltype.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_sex_schooltype,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)

## By Sex and PACE
probs_sex_schoolPACE <- ggplot(training_data, aes(y = m18, x = as.factor(SEX_FEMALE)))+
  geom_violin(aes(fill = as.factor(SEX_FEMALE)), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Sexo", labels = c("Hombres", "Mujeres"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_PACE)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_sex_schoolPACE.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_sex_schoolPACE,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)

## By sex and school sector
probs_qfam_school <- ggplot(training_data, aes(y = m18, x = Q_FAM_INC))+
  geom_violin(aes(fill = Q_FAM_INC), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Quintil de ingresos familiares percápita",
                      labels = c("Q1", "Q2","Q3", "Q4","Q5"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_SECTOR)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_qfam_school.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_qfam_school,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)

# By qfam and school type
probs_qfam_schooltype <- ggplot(training_data, aes(y = m18, x = Q_FAM_INC))+
  geom_violin(aes(fill = Q_FAM_INC), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Quintil de ingresos familiares percápita",
                      labels = c("Q1", "Q2","Q3", "Q4","Q5"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_TYPE_HUM)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_qfam_schooltype.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_qfam_schooltype,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)

# By qfam and PACE
probs_qfam_schoolPACE <- ggplot(training_data, aes(y = m18, x = Q_FAM_INC))+
  geom_violin(aes(fill = Q_FAM_INC), alpha = .75)+
  geom_boxplot(width=0.1, color="black", alpha=0.6,outlier.shape = NA)+
  labs(y = "Puntaje predicho en PDT Matemática", x = '')+
  scale_fill_discrete("Quintil de ingresos familiares percápita",
                      labels = c("Q1", "Q2","Q3", "Q4","Q5"))+
  scale_y_continuous(limits = c(300, 850)) +
  facet_grid(. ~ SCHOOL_PACE)+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

ggsave(
  filename = 'probs_qfam_schoolPACE.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/6. Clustered admission tests results/IESED2023-MultilevelAnalysis/4. Tablas y figuras',
  plot = probs_qfam_schoolPACE,
  device = png,
  width = 7,
  height = 9,
  dpi = 400)








# Validation database

validation_data <- data.frame(readr::read_csv("6. Clustered admission tests results/validation_data.csv"))

empty_ri2 <- lmer(
  formula = MATH ~ 1 + (1|RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = TRUE,
  data = validation_data)
summary(empty_ri2, ddf="Satterthwaite"); llikAIC(empty_ri2, chkREML=FALSE)

icc(empty_ri2); ranova(empty_ri2, reduce.term=TRUE) # 0.281

icc_empty_ri2 <- data.frame(VarCorr(empty_ri2),comp=c("Variance"))
round(icc_empty_ri2[1,4]/(sum(icc_empty_ri2[,4])),3) # 0.281

AF2 <- allFit(empty_ri2, verbose=F)
AF2_lliks <- sort(sapply(AF2,logLik))
AF2_lliks

m23 <- lmer(
  formula = MATH ~ 1 +
    SEX_FEMALE_CMC+
    Q2_CMC+
    Q3_CMC+
    Q4_CMC+
    Q5_CMC+
    EdFATHER_HS_CMC+
    EdFATHER_HE_CMC+
    EdMOTHER_HS_CMC+
    EdMOTHER_HE_CMC+
    RKG_CMC +
    SEX_FEMALE_L2+
    Q2_L2+
    Q3_L2+
    Q4_L2+
    Q5_L2+
    EdFATHER_HS_L2+
    EdFATHER_HE_L2+
    EdMOTHER_HS_L2+
    EdMOTHER_HE_L2+
    RKG_L2+
    SCHOOL_TYPE_HUM+
    SCHOOL_PACE+
    (1 + SEX_FEMALE_CMC
     |RBD),
  control = lmerControl(optimizer = "Nelder_Mead"),
  REML = T,
  data = validation_data)
summary(m23, ddf="Satterthwaite"); llikAIC(m23, chkREML=FALSE)

#Conditional ICC
ICC_cond2 <- data.frame(VarCorr(m23),comp=c("Variance"))
#round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(icc_empty_ri2[1,4] - ICC_cond2[1,4]) / icc_empty_ri2[1,4] #PseudoR2 at level 2 relative to EM
# 0.7869012
(icc_empty_ri2[2,4] - ICC_cond2[4,4]) / icc_empty_ri2[2,4] #PseudoR2 at level 1 relative to EM
# 0.20307

validation_data$m23 <- predict(m23, re.form=NA)
(cor.test(validation_data$m23, validation_data$MATH, method="pearson")$estimate)^2 #Total R2 relative to EM
# 0.3555676 

