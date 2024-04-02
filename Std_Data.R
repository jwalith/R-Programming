library(tidyverse)
library(DataExplorer)
library(skimr)

sdf = read.csv('C:/Users/skristam/Downloads/Student_Data.csv')
head(sdf)
# STUDENT.ID X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19 X20 X21 X22 X23
# 1   STUDENT1  2  2  3  3  1  2  2  1  1   1   1   2   3   1   2   5   3   2   2   1   1   1   1
# 2   STUDENT2  2  2  3  3  1  2  2  1  1   1   2   3   2   1   2   1   2   2   2   1   1   1   1
# 3   STUDENT3  2  2  2  3  2  2  2  2  4   2   2   2   2   1   2   1   2   1   2   1   1   1   1
# 4   STUDENT4  1  1  1  3  1  2  1  2  1   2   1   2   5   1   2   1   3   1   2   1   1   1   1
# 5   STUDENT5  2  2  1  3  2  2  1  3  1   4   3   3   2   1   2   4   2   1   1   1   1   1   2
# 6   STUDENT6  2  2  2  3  2  2  2  2  1   1   3   3   2   1   2   3   1   1   2   1   1   1   1
# X24 X25 X26 X27 X28  X29 X30 COURSE.ID GRADE
# 1   1   3   2   1   2 1.22   1         1     1
# 2   1   3   2   3   2 2.25   3         1     1
# 3   1   2   2   1   1 2.18   2         1     1
# 4   2   3   2   2   1 2.82   2         1     1
# 5   1   2   2   2   1 2.19   2         1     1
# 6   1   1   2   1   2 3.18   4         1     2


colnames(sdf)[colnames(sdf) == 'X1'] <- 'X1_age'
colnames(sdf)[colnames(sdf) == 'X2'] <- 'X2_sex'
colnames(sdf)[colnames(sdf) == 'X3'] <- 'X3_high_school_type'
colnames(sdf)[colnames(sdf) == 'X4'] <- 'X4_scholarship_type'
colnames(sdf)[colnames(sdf) == 'X5'] <- 'X5_additional_work'
colnames(sdf)[colnames(sdf) == 'X6'] <- 'X6_activities'
colnames(sdf)[colnames(sdf) == 'X7'] <- 'X7_partner'
colnames(sdf)[colnames(sdf) == 'X8'] <- 'X8_total_salary'
colnames(sdf)[colnames(sdf) == 'X9'] <- 'X9_transportation'
colnames(sdf)[colnames(sdf) == 'X10'] <- 'X10_accomodation_type'
colnames(sdf)[colnames(sdf) == 'X11'] <- 'X11_mother_education'
colnames(sdf)[colnames(sdf) == 'X12'] <- 'X12_father_education'
colnames(sdf)[colnames(sdf) == 'X13'] <- 'X13_number_of_siblings'
colnames(sdf)[colnames(sdf) == 'X14'] <- 'X14_parental_status'
colnames(sdf)[colnames(sdf) == 'X15'] <- 'X15_mother_occupation'
colnames(sdf)[colnames(sdf) == 'X16'] <- 'X16_father_occupation'
colnames(sdf)[colnames(sdf) == 'X17'] <- 'X17_study_hours'
colnames(sdf)[colnames(sdf) == 'X18'] <- 'X18_reading_freq_nonsci'
colnames(sdf)[colnames(sdf) == 'X19'] <- 'X19_reading_freq_sci'
colnames(sdf)[colnames(sdf) == 'X20'] <- 'X20_attende_seminars'
colnames(sdf)[colnames(sdf) == 'X21'] <- 'X21_impact_projects'
colnames(sdf)[colnames(sdf) == 'X22'] <- 'X22_attendance'
colnames(sdf)[colnames(sdf) == 'X23'] <- 'X23_perparation_examns_company'
colnames(sdf)[colnames(sdf) == 'X24'] <- 'X24_preparation_examns_time'
colnames(sdf)[colnames(sdf) == 'X25'] <- 'X25_taking_notes'
colnames(sdf)[colnames(sdf) == 'X26'] <- 'X26_listening'
colnames(sdf)[colnames(sdf) == 'X27'] <- 'X27_discussion_interest'
colnames(sdf)[colnames(sdf) == 'X28'] <- 'X28_flip_classroom'
colnames(sdf)[colnames(sdf) == 'X29'] <- 'X29_cumulative_grade_point'
colnames(sdf)[colnames(sdf) == 'X30'] <- 'X30_expected_cumulative_grade_point'

View(sdf)

colSums(is.na(sdf))

# STUDENT.ID                              X1_age 
# 0                                   0 
# X2_sex                 X3_high_school_type 
# 0                                   0 
# X4_scholarship_type                  X5_additional_work 
# 0                                   0 
# X6_activities                          X7_partner 
# 0                                   0 
# X8_total_salary                   X9_transportation 
# 0                                   0 
# X10_accomodation_type                X11_mother_education 
# 0                                   0 
# X12_father_education              X13_number_of_siblings 
# 0                                   0 
# X14_parental_status               X15_mother_occupation 
# 0                                   0 
# X16_father_occupation                     X17_study_hours 
# 0                                   0 
# X18_reading_freq_nonsci                X19_reading_freq_sci 
# 0                                   0 
# X20_attende_seminars                 X21_impact_projects 
# 0                                   0 
# X22_attendance      X23_perparation_examns_company 
# 0                                   0 
# X24_preparation_examns_time                    X25_taking_notes 
# 0                                   0 
# X26_listening             X27_discussion_interest 
# 0                                   0 
# X28_flip_classroom          X29_cumulative_grade_point 
# 0                                   0 
# X30_expected_cumulative_grade_point                           COURSE.ID 
# 0                                   0 
# GRADE 
# 0 

plot_intro(sdf,title='Student Dataset') #Plot the introduction to the dataset  #Need to put Intro image

##plot_bar(sdf,ncol=2)

boxplot(GRADE ~ X2_sex, data = sdf, main = "GRADE by GENDER", ylab = "GRADE", col = "skyblue", border = "black",names=c("female", "male"))#GRADE_GENDER
boxplot(GRADE ~ X1_age, data = sdf, main = "GRADE by AGE", ylab = "GRADE", col = "skyblue", border = "black",names=c("18-21","22-25","Above 26"))
##########################################################################################
#Null Hypothesis (H0):
#There is no association between scholarship level and grades. In other words, the distribution of grades is independent of the scholarship level.

#Alternative Hypothesis (H1):
#There is an association between scholarship level and grades. The distribution of grades is not independent of the scholarship level.
##########################################################################################


table(sdf$X4_scholarship_type)
# 1  2  3  4  5 
# 1  3 76 42 23 

sdf$Scholar_type <- ifelse(df$X4_scholarship_type %in% c(1, 2, 3), "L", "H")
table(sdf$Scholar_type)
# H  L 
# 65 80

table(sdf$GRADE)
# 0  1  2  3  4  5  6  7 
# 8 35 24 21 10 17 13 17

#histogram of GRADE
hist(sdf$GRADE, breaks = 30, col = "grey") #put the Grade image

sdf <- sdf %>%
  mutate(GRADE_2 = case_when(
    GRADE %in% c(0,1,2,3) ~ "Low",
    GRADE %in% c(4,5,6,7) ~ "High"
  ))

plot_bar(sdf$GRADE_2,ncol=2)# need to put GRADE_2 image
table(sdf$GRADE_2)
# High  Low 
# 57   88 

counts = table(sdf$GRADE_2,sdf$Scholar_type)
counts
# H  L
# High 31 26
# Low  34 54

barplot(counts, beside = TRUE, col = c("lightblue", "lightgreen"), main = "Counts of Scholarship and Grade", xlab = "Scholarship", ylab = "Count",names.arg = c("High", "Low"))
legend_values <- c("High Grades", "Low Grades")  # Customize legend values
legend("bottomright", legend = legend_values, fill = c("lightblue", "lightgreen"))
#need to put counts_S_G Image

contingency_table  = table(sdf$Scholar_type, sdf$GRADE_2)
contingency_table
# High Low
# H   31  34
# L   26  54


#To test the hypothesis regarding the association or difference between two categorical variables, you can use a chi-squared test of independence. This test assesses whether there is a significant association between two categorical variables in a contingency table.

result =chisq.test(contingency_table)

result$observed

result$expected

# High      Low
# H 25.55172 39.44828
# L 31.44828 48.55172

result$statistic #mention Formula
# X-squared 
# 2.861877 
result$parameter
# df 
# 1

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 2.8619, df = 1, p-value = 0.0907

library(corrplot)
# corrplot 0.92 loaded
corrplot(r$residuals, is.cor = FALSE) #corr image
#Positive residuals are in blue. Positive values in cells specify an attraction (positive association) between the corresponding row and column variables
#Negative residuals are in red. This implies a repulsion (negative association) between the corresponding row and column variables. 


# Based on the p-value of 0.0907, there is not enough evidence to reject the null hypothesis at a 0.05 significance level. This suggests that, according to the chi-squared test, there is no significant association between grades and scholarship in your data
#The chi-squared value (2.8619 in your case) is a measure of the discrepancy between the observed and expected frequencies in the contingency table. Larger values indicate a greater discrepancy.

# Adding Missing values in Scholar_type and GRADE_2
library(missMethods)
#MCAR - Missing Completely At Random
set.seed(123)
ds_mcar <- delete_MCAR(sdf, 0.1, "Scholar_type") #adding 10% of missing values in Scholar_type column
ds_mcar <- delete_MCAR(ds_mcar, 0.1, "GRADE_2") #adding 10% of missing values in GRADE_2 column

contingency_table  = table(ds_mcar$Scholar_type, ds_mcar$GRADE_2)
contingency_table

# High Low
# H   25  28
# L   22  43

chisq.test(contingency_table)
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 2.5916, df = 1, p-value = 0.1074
#Based on the p-value of 0.1074, there is not enough evidence to reject the null hypothesis at a 0.05 significance level. This suggests that, according to the chi-squared test, even after deleting 10% of the values, there is no significant association between grades and scholarship 

#adding 30% of missing values
set.seed(134)
ds_mcar <- delete_MCAR(sdf, 0.3, "Scholar_type")
ds_mcar <- delete_MCAR(ds_mcar, 0.3, "GRADE_2") 
contingency_table  = table(ds_mcar$Scholar_type, ds_mcar$GRADE_2)
contingency_table

# High Low
# H   18  15
# L   14  28
chisq.test(contingency_table)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 2.5873, df = 1, p-value = 0.1077

#Based on the p-value of 0.1077, there is not enough evidence to reject the null hypothesis at a 0.05 significance level.

#Handling Missing Values
#Deleting missing Values

set.seed(123)
ds_mcar <- delete_MCAR(sdf, 0.1, "Scholar_type")
ds_mcar <- delete_MCAR(ds_mcar, 0.1, "GRADE_2")

sum(is.na(ds_mcar$Scholar_type))
sum(is.na(ds_mcar$GRADE_2))
#[1] 14
ds_mcar = na.omit(ds_mcar[, c("Scholar_type", "GRADE_2")]) #omitting NULL values

contingency_table  = table(ds_mcar$Scholar_type, ds_mcar$GRADE_2)
contingency_table
#High Low
#H   24  27
#L   21  47
chisq.test(contingency_table)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 2.5916, df = 1, p-value = 0.1074
#Based on the p-value of 0.1074, there is not enough evidence to reject the null hypothesis at a 0.05 significance level.

#Replacing missing Values with Mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

set.seed(134)
ds_mcar <- delete_MCAR(sdf, 0.3, "Scholar_type")
ds_mcar <- delete_MCAR(ds_mcar, 0.3, "GRADE_2")

sum(is.na(ds_mcar$Scholar_type))
sum(is.na(ds_mcar$GRADE_2))
# [1] 44
Mode(ds_mcar$GRADE_2)
#[1] "Low"
Mode(ds_mcar$Scholar_type)
#"L"

ds_mcar <- ds_mcar %>%
  mutate(Scholar_type = ifelse(is.na(Scholar_type), Mode(Scholar_type), Scholar_type))
ds_mcar <- ds_mcar %>%
  mutate(GRADE_2 = ifelse(is.na(GRADE_2), Mode(GRADE_2), GRADE_2))

contingency_table  = table(ds_mcar$Scholar_type, ds_mcar$GRADE_2)
contingency_table
# High Low
# H   18  26
# L   24  77
chisq.test(contingency_table)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 3.5857, df = 1, p-value = 0.05828

#The p-value of 0.05828 is close to the typical 0.05 significance level. While it does not reach the conventional threshold for statistical significance
#Therefore, there is evidence to suggest that there is a significant association between Scholar_type and GRADE (after deleting 30% of missing values in MCAR). The association is significant at a conventional significance level (e.g., 0.05 or lower).

#MNAR
#Adding missing values in GRADE where Course =9

ds_mnar <- sdf
ds_mnar$GRADE_2 <- ifelse(ds_mnar$COURSE.ID == 9, NA, ds_mnar$GRADE_2)
contingency_table  = table(ds_mnar$Scholar_type, ds_mnar$GRADE_2)
contingency_table

# High Low
# H   28  21
# L   25  50

chisq.test(contingency_table)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 5.9267, df = 1, p-value = 0.01491
#Based on the p-value of 0.01491, there is evidence to reject the null hypothesis at a 0.05 significance level

#Logistic Regression

######################################################################################
# Null Hypothesis (H0):
#   H0: The coefficients of all the predictor variables in the model are equal to zero. In other words, none of the predictor variables have a significant effect on the response variable.
# Alternative Hypothesis (Ha):
#   Ha: At least one of the coefficients of the predictor variables in the model is not equal to zero. In other words, there is evidence that at least one predictor variable has a significant effect on the response variable.
########################################################################################

library(glm2)
sdf$GRADE_2 <- as.factor(as.character(sdf$GRADE_2))
sdf$GRADE_2 <- as.factor(sdf$GRADE_2)

logit_model <- glm(GRADE_2 ~ X5_additional_work + X17_study_hours + X20_attende_seminars +
                             X22_attendance + X23_perparation_examns_company +
                             X24_preparation_examns_time + X25_taking_notes + X26_listening,
                             family = binomial, data = sdf)
summary(logit_model)

# Call:
#   glm(formula = GRADE_2 ~ X5_additional_work + X17_study_hours + 
#         X20_attende_seminars + X22_attendance + X23_perparation_examns_company + 
#         X24_preparation_examns_time + X25_taking_notes + X26_listening, 
#       family = binomial, data = sdf)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                     1.93278    1.85802   1.040   0.2982  
# X5_additional_work             -0.59880    0.40472  -1.480   0.1390  
# X17_study_hours                 0.08597    0.21917   0.392   0.6949  
# X20_attende_seminars            1.12174    0.49896   2.248   0.0246 *
#   X22_attendance                  0.06661    0.44074   0.151   0.8799  
# X23_perparation_examns_company  0.01999    0.31779   0.063   0.9498  
# X24_preparation_examns_time    -0.47029    0.50215  -0.937   0.3490  
# X25_taking_notes               -0.37842    0.33065  -1.144   0.2524  
# X26_listening                  -0.29826    0.27832  -1.072   0.2839  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 184.74  on 136  degrees of freedom
# AIC: 202.74
# 
# Number of Fisher Scoring iterations: 4
library(lmtest)
lrt_result <- lrtest(null_model, logit_model)
print(lrt_result)
# Likelihood ratio test
# 
# Model 1: GRADE_2 ~ 1
# Model 2: GRADE_2 ~ X5_additional_work + X17_study_hours + X20_attende_seminars + 
#   X22_attendance + X23_perparation_examns_company + X24_preparation_examns_time + 
#   X25_taking_notes + X26_listening
# #Df  LogLik Df Chisq Pr(>Chisq)
# 1   1 -97.167                    
# 2   9 -92.371  8 9.592     0.2948
# 
# The likelihood ratio test compares the fit of Model 1 (null model) and Model 2 (model with predictors).
# The test statistic (9.592) with 8 degrees of freedom has a p-value of 0.2948.
# The p-value (0.2948) is greater than the typical significance level of 0.05, indicating that there is not enough evidence to reject the null hypothesis.
# Therefore, based on the likelihood ratio test, there is no significant improvement in model fit by adding the predictors in Model 2 compared to the null model.
1-pchisq(logit_model$null.deviance-logit_model$deviance,logit_model$df.null-logit_model$df.residual)
#[1] 0.1972973
# logit_model$null.deviance: The null deviance, which represents the deviance of the null model (model with only the intercept).
# 
# logit_model$deviance: The deviance of the fitted logistic regression model.
# 
# logit_model$df.null: Degrees of freedom of the null model.
# 
# logit_model$df.residual: Degrees of freedom of the fitted model.

# The difference logit_model$null.deviance - logit_model$deviance represents the change in deviance between the null model and the fitted model. In logistic regression, this change is often used in a likelihood ratio test to assess the overall significance of the model.
# 
# The pchisq function is the cumulative distribution function (CDF) of the chi-square distribution. The expression 1 - pchisq(...) calculates the probability that a chi-square random variable with degrees of freedom equal to the difference in degrees of freedom between the null and fitted models exceeds the observed difference in deviance.
# The expression you provided (1 - pchisq(logit_model$null.deviance - logit_model$deviance, logit_model$df.null - logit_model$df.residual)) is related to a likelihood ratio test (LRT), 


# The p-value you obtained from the likelihood ratio test is approximately 0.1973. This p-value is associated with the hypothesis test that compares the fit of the full logistic regression model (with predictors) against the fit of the null model (without predictors). Here's how to interpret this result:
# 
# Interpretation: The p-value of 0.1973 is greater than the commonly chosen significance level of 0.05.
# 
# Decision: Based on the p-value, you would fail to reject the null hypothesis.

# The individual predictor X20_attende_seminars is deemed significant based on its p-value.
# The likelihood ratio test, which considers the joint significance of all predictors, does not provide strong evidence to reject the null hypothesis that all coefficients are zero.

# The model suggests that attending seminars (X20_attende_seminars) has a statistically significant positive effect on the probability of having a high grade
#X20_attende_seminars is statistically significant (Pr(>|z|) = 0.0353 *). The positive coefficient suggests that an increase in the variable is associated with an increase in the log-odds of the response variable.
# Coefficient Interpretation:
#   Intercept:
#   The estimated log-odds when all predictor variables are zero is 1.93278.
# Coefficients for Predictor Variables:
#X20_attende_seminars:
#For a one-unit increase in X20_attende_seminars, the log-odds of the outcome increase by approximately 1.12174.The p-value for X20_attende_seminars is 0.0246, indicating that it is statistically significant at the 0.05 significance level.
# X5_additional_work:
#For a one-unit increase in X5_additional_work, the log-odds of the outcome decrease by approximately 0.59880.
# The p-value for X5_additional_work is 0.1390, indicating that it is not statistically significant at the 0.05 significance level.
# Odds Ratios:
#   X5_additional_work:
#   The odds of the outcome decrease by approximately exp(-0.59880) times for a one-unit increase in X5_additional_work.
# X20_attende_seminars:
#   The odds of the outcome increase by approximately exp(1.12174) times for a one-unit increase in X20_attende_seminars.

logit_model <- glm(GRADE_2 ~ X2_sex+X1_age+ X11_mother_education + X7_partner + X12_father_education +
                     X13_number_of_siblings + X14_parental_status +
                     X15_mother_occupation + X16_father_occupation +X10_accomodation_type,
                   family = binomial, data = sdf)
summary(logit_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)             2.30575    1.80362   1.278   0.2011   
# X2_sex                 -1.08626    0.40211  -2.701   0.0069 **
#   X1_age                  0.55088    0.31545   1.746   0.0808 . 
# X11_mother_education   -0.12790    0.17632  -0.725   0.4682   
# X7_partner              0.08522    0.39168   0.218   0.8278   
# X12_father_education   -0.03635    0.17839  -0.204   0.8386   
# X13_number_of_siblings -0.22349    0.15328  -1.458   0.1448   
# X14_parental_status    -0.28503    0.39180  -0.728   0.4669   
# X15_mother_occupation   0.12501    0.26640   0.469   0.6389   
# X16_father_occupation   0.10933    0.14041   0.779   0.4362   
# X10_accomodation_type  -0.20942    0.26021  -0.805   0.4209   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 179.77  on 134  degrees of freedom
# AIC: 201.77
# 
# Number of Fisher Scoring iterations: 4
# 
# X2_sex:
#   
#   Interpretation: The coefficient is -1.08626.
# Significance: The p-value is 0.0069, which is less than the significance level of 0.05.
# Conclusion: We reject the null hypothesis for X2_sex and infer that gender (X2_sex) has a significant effect on the response variable.
# 
# Based on the individual predictor variables, gender (X2_sex) appears to have a significant effect on the response variable.
# The model, as a whole, shows improvement over a null model based on the decrease in deviance.


logit_model <- glm(GRADE_2 ~ X11_mother_education + X7_partner + X12_father_education +
                             X13_number_of_siblings + X14_parental_status +
                             X15_mother_occupation + X16_father_occupation +X10_accomodation_type,
                             family = binomial, data = sdf)
summary(logit_model)

# Call:
#   glm(formula = GRADE_2 ~ X11_mother_education + X7_partner + X12_father_education + 
#         X13_number_of_siblings + X14_parental_status + X15_mother_occupation + 
#         X16_father_occupation + X10_accomodation_type, family = binomial, 
#       data = sdf)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)             1.11192    1.39287   0.798    0.425
# X11_mother_education   -0.14570    0.16922  -0.861    0.389
# X7_partner              0.28901    0.36959   0.782    0.434
# X12_father_education   -0.10892    0.17167  -0.634    0.526
# X13_number_of_siblings -0.23478    0.14678  -1.600    0.110
# X14_parental_status    -0.09003    0.36569  -0.246    0.806
# X15_mother_occupation   0.10391    0.25713   0.404    0.686
# X16_father_occupation   0.08448    0.13555   0.623    0.533
# X10_accomodation_type  -0.12580    0.24717  -0.509    0.611
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 189.55  on 136  degrees of freedom
# AIC: 207.55
# 
# Number of Fisher Scoring iterations: 4

#None of the predictor variables in your model have p-values less than 0.05. Therefore, based on the given significance level, there isn't enough evidence to reject the null hypothesis for any of the predictors.





####MISSING
d = delete_MCAR(sdf, 0.2, c("X17_study_hours","X20_attende_seminars","X25_taking_notes","X26_listening"))

d$GRADE_2 <- as.factor(as.character(d$GRADE_2))
d$GRADE_2 <- as.factor(d$GRADE_2)
logit_model <- glm(GRADE_2 ~ X5_additional_work + X17_study_hours + X20_attende_seminars +
                     X22_attendance + X23_perparation_examns_company +
                     X24_preparation_examns_time + X25_taking_notes + X26_listening,
                   family = binomial, data = d)
summary(logit_model)

# Call:
#   glm(formula = GRADE_2 ~ X5_additional_work + X17_study_hours + 
#         X20_attende_seminars + X22_attendance + X23_perparation_examns_company + 
#         X24_preparation_examns_time + X25_taking_notes + X26_listening, 
#       family = binomial, data = d)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                     -0.9595     3.5223  -0.272   0.7853  
# X5_additional_work              -0.1850     0.7265  -0.255   0.7990  
# X17_study_hours                  0.4252     0.4063   1.047   0.2953  
# X20_attende_seminars             2.8475     1.2156   2.343   0.0192 *
#   X22_attendance                   0.5107     0.8288   0.616   0.5378  
# X23_perparation_examns_company   0.9774     0.6026   1.622   0.1048  
# X24_preparation_examns_time     -0.9397     0.9499  -0.989   0.3225  
# X25_taking_notes                -1.2084     0.6207  -1.947   0.0516 .
# X26_listening                   -0.1504     0.4903  -0.307   0.7591  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 76.028  on 56  degrees of freedom
# Residual deviance: 61.863  on 48  degrees of freedom
# (88 observations deleted due to missingness)
# AIC: 79.863
# 
# Number of Fisher Scoring iterations: 5


#HANDLING MISSING DATA BY REPLACING MODE

d <- d %>%
    mutate(X17_study_hours = ifelse(is.na(X17_study_hours), Mode(X17_study_hours), X17_study_hours))
d <- d %>%
        mutate(X20_attende_seminars = ifelse(is.na(X20_attende_seminars), Mode(X20_attende_seminars), X20_attende_seminars))
d <- d %>%
       mutate(X25_taking_notes = ifelse(is.na(X25_taking_notes), Mode(X25_taking_notes), X25_taking_notes))
d <- d %>%
       mutate(X26_listening = ifelse(is.na(X26_listening), Mode(X26_listening), X26_listening))
logit_model <- glm(GRADE_2 ~ X5_additional_work + X17_study_hours + X20_attende_seminars +
                                               X22_attendance + X23_perparation_examns_company +
                                               X24_preparation_examns_time + X25_taking_notes + X26_listening,
                                         family = binomial, data = d)
summary(logit_model)

# Call:
#   glm(formula = GRADE_2 ~ X5_additional_work + X17_study_hours + 
#         X20_attende_seminars + X22_attendance + X23_perparation_examns_company + 
#         X24_preparation_examns_time + X25_taking_notes + X26_listening, 
#       family = binomial, data = d)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                     1.41892    1.76698   0.803   0.4220  
# X5_additional_work             -0.60674    0.39893  -1.521   0.1283  
# X17_study_hours                 0.11807    0.24137   0.489   0.6247  
# X20_attende_seminars            1.26219    0.59955   2.105   0.0353 *
#   X22_attendance                  0.19086    0.43171   0.442   0.6584  
# X23_perparation_examns_company  0.06627    0.32085   0.207   0.8364  
# X24_preparation_examns_time    -0.34987    0.49959  -0.700   0.4837  
# X25_taking_notes               -0.27635    0.34494  -0.801   0.4230  
# X26_listening                  -0.40567    0.30549  -1.328   0.1842  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 184.09  on 136  degrees of freedom
# AIC: 202.09
# 
# Number of Fisher Scoring iterations: 4


#MNAR

imp=sdf
missing_vars <- c("X20_attende_seminars", "X5_additional_work", "X25_taking_notes","X17_study_hours")
missing_prob <- c(0.2, 0.3, 0.1, 0.1)  # Adjust probabilities as needed
n <- nrow(sdf)
for (i in seq_along(missing_vars)) {
imp[sample(n, floor(missing_prob[i] * n)), missing_vars[i]] <- NA
}

#HANDILING MISSING VALUES WITH MICE
#predictive mean matching (PMM) to select which values are imputed. PMM involves selecting a datapoint from the original, nonmissing data which has a predicted value close to the predicted value of the missing sample
m = mice(imp,m=5,method="pmm",maxit=10)
m$imp$X17_study_hours
# 1 2 3 4 5
# 15  1 3 1 1 1
# 22  1 2 3 2 2
# 27  5 3 2 2 3
# 30  2 1 2 2 2
# 46  3 2 5 2 2
# 58  3 2 1 2 4
# 67  2 1 2 3 2
# 78  2 1 1 2 1
# 86  2 1 2 1 3
# 108 2 2 3 2 2
# 134 2 2 2 2 2
# 135 3 3 1 2 1
# 138 2 2 4 2 2
# 140 1 2 2 1 1
cle = complete(m,5)

logit_model <- glm(GRADE_2 ~ X5_additional_work + X17_study_hours + X20_attende_seminars +
                     X22_attendance + X23_perparation_examns_company +
                     X24_preparation_examns_time + X25_taking_notes + X26_listening,
                   family = binomial, data = cle)
summary(logit_model)

# Call:
#   glm(formula = GRADE_2 ~ X5_additional_work + X17_study_hours + 
#         X20_attende_seminars + X22_attendance + X23_perparation_examns_company + 
#         X24_preparation_examns_time + X25_taking_notes + X26_listening, 
#       family = binomial, data = cle)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                     2.81962    1.87041   1.507   0.1317  
# X5_additional_work             -0.83668    0.43021  -1.945   0.0518 .
# X17_study_hours                 0.01882    0.21601   0.087   0.9306  
# X20_attende_seminars            1.19319    0.50452   2.365   0.0180 *
#   X22_attendance                  0.03554    0.43934   0.081   0.9355  
# X23_perparation_examns_company -0.05623    0.32027  -0.176   0.8606  
# X24_preparation_examns_time    -0.37699    0.50845  -0.741   0.4584  
# X25_taking_notes               -0.45761    0.34116  -1.341   0.1798  
# X26_listening                  -0.37736    0.28410  -1.328   0.1841  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 183.26  on 136  degrees of freedom
# AIC: 201.26
# 
# Number of Fisher Scoring iterations: 4

###################################################################

# > logit_model <- glm(GRADE_2 ~ + X20_attende_seminars+X21_impact_projects   +X24_preparation_examns_time+X19_reading_freq_sci+
#                        + X23_perparation_examns_company + X18_reading_freq_nonsci,
#                      + family = binomial, data = sdf)
> summary(logit_model)
# 
# Call:
#   glm(formula = GRADE_2 ~ +X20_attende_seminars + X21_impact_projects + 
#         X24_preparation_examns_time + X19_reading_freq_sci + X23_perparation_examns_company + 
#         X18_reading_freq_nonsci, family = binomial, data = sdf)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                    -0.04943    1.12992  -0.044   0.9651  
# X20_attende_seminars            0.78321    0.49517   1.582   0.1137  
# X21_impact_projects             0.85432    0.43100   1.982   0.0475 *
#   X24_preparation_examns_time    -0.10853    0.46023  -0.236   0.8136  
# X19_reading_freq_sci            0.25911    0.37869   0.684   0.4938  
# X23_perparation_examns_company -0.15534    0.30793  -0.504   0.6139  
# X18_reading_freq_nonsci        -0.82487    0.37089  -2.224   0.0261 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 194.33  on 144  degrees of freedom
# Residual deviance: 179.19  on 138  degrees of freedom
# AIC: 193.19
# 
# Number of Fisher Scoring iterations: 4

> 1-pchisq(194.33-179.19,144-138)
# [1] 0.01919534


#### Handling Missing Values using MICE

```{r}
d$X18_reading_freq_nonsci <- as.factor(d$X18_reading_freq_nonsci)
d$X21_impact_projects <- as.factor(d$X21_impact_projects)
d$X26_listening <- as.factor(d$X26_listening)
```
```{r include=FALSE}
#using mice to impute missing values
m = mice(d,m=5,method="polyreg",maxit=10)
```
* In the polyreg method of the mice package, missing categorical values are imputed by estimating the probability distribution of the target variable based on a polynomial regression model using observed values of specified predictor variables. 
* The imputation involves random draws from this estimated probability distribution, resulting in imputed values that reflect the uncertainty associated with the missing data. This process is repeated for multiple imputations(repeated m times to generate multiple imputed datasets), and the pooled results provide a comprehensive understanding of the imputed categorical values.
* The algorithm of mice.impute.polyreg uses the function multinom() from the nnet package.

```{r}
#selecting 5th set
clean = complete(m,5)

logit_model =glm( GRADE_2 ~  X1_age+X25_taking_notes + X17_study_hours  +  X18_reading_freq_nonsci+X29_last_sem_CGPA+X26_listening + X21_impact_projects, 
                  family = binomial, data = clean)
summary(logit_model)
```

* After addressing missing data, notable variables such as X29_cumulative_grade_point, X18_reading_freq_nonsci, X1_age, and X21_impact_projects, which demonstrated significance in the original model, are still influential. Additionally, X26_listening has now shown significance in influencing the log-odds of achieving GRADE_2

* AIC values is closely aligning with the original model's AIC of 179.28.








