# Load necessary libraries
library("psych")
library("corrplot")
library("lavaan")
library("dplyr")
library("rsample")
library("semPlot")


# Load dataset01 from "faculty_set_score_raw_combined.csv"
dataset01 <- read.csv("../datasets/faculty_set_score_raw_combined.csv")


# Subset SET items into dataset02 and department_id, and then omit missing obs.
dataset02 <- dataset01[, c("department_id", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10")]
dataset02 <- na.omit(dataset02)


# Inspect departments and counts in dataset02
table(dataset02$department_id)


# Perform stratified split
set_split <- initial_split(dataset02, prop = 0.4, strata = department_id)


# Extract EFA and CFA datasets from dataset02
set_efa_dataset <- training(set_split)    # 40% for EFA
set_cfa_dataset <- testing(set_split)     # 60% for CFA


# Validate proportions
set_efa_dataset_props <- prop.table(table(set_efa_dataset$department_id))
set_cfa_dataset_props <- prop.table(table(set_cfa_dataset$department_id))
dataset02_full_props <- prop.table(table(dataset02$department_id))


# Compare proportions
comparison <- data.frame(
  Department = names(dataset02_full_props),
  Full = as.vector(dataset02_full_props),
  EFA = as.vector(set_efa_dataset_props[names(dataset02_full_props)]),
  CFA = as.vector(set_cfa_dataset_props[names(dataset02_full_props)])
)


# Round numeric columns only
comparison[, 2:4] <- round(comparison[, 2:4], 3)


# Print the comparison result
print(comparison)


# Exclude department_ID from EFA dataset
set_efa_dataset <- set_efa_dataset  %>% select(-department_id)


# Testing hypothesis H1: Correlation analysis between faculty's peer evaluation score and SET score
# Create dataset03
dataset03 <- read.csv("../datasets/faculty-peer-score-raw-and-SET-score-agg-41people.csv")


# Visual check for normality before choosing either spearman or pearson based correlation analysis for testing hypothesis H1
# Histogram for SET total score
hist(dataset03$set_score_percentage_reported, main = "Histogram of aggregated SET scores from ETLBS scale", xlab = "Aggregated SET scores from ETLBS scale")


# Histogram for peer evaluation score
hist(dataset03$peer_score_percentage_reported, main = "Histogram of overall peer evaluation scores from MTLBL scale", xlab = "Overall peer evaluation scores from MTLBL scale")


# Q-Q plots
qqnorm(dataset03$set_score_percentage_reported, main = "Normal Q-Q plot for distribution of SET scores from ETLBS scale"); qqline(dataset03$set_score_percentage_reported, col = "red")
qqnorm(dataset03$peer_score_percentage_reported, main = "Normal Q-Q plot for distribution of peer evaluation scores from MTLBL scale"); qqline(dataset03$peer_score_percentage_reported, col = "red")


# Statistical check for normality before choosing either spearman or pearson based correlation analysis
# H0: data is normally distributed
# H1: data is not normally distributed
# significance: if significant, reject null hypothesis
# W values: ~ 1.0 = nearly perfectly normal; .95 - 1.0 = approximately normal; .90 - .95 = mild deviation
shapiro.test(dataset03$set_score_percentage_reported)
shapiro.test(dataset03$peer_score_percentage_reported)


# Since visual and statistical checks have confirmed non-normality in dataset03, proceed with Spearman's for testing hypothesis H1
# H0: no monotonic correlation (zero correlation)
# H1: relationship exists
# significance: if significant, reject null hypothesis
# rho value: lower value indicate little to no relationship
cor.test(dataset03$set_score_percentage_reported, dataset03$peer_score_percentage_reported, method = "spearman")


# Testing hypothesis H2: EFA of ETLBS scale
# Assumption check - statistical: correlation matrix, p-values, CIs
print(corr.test(set_efa_dataset), short = FALSE)


# Assumption check - visual: correlation matrix heat map
corrplot(cor(set_efa_dataset), method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8, title = "Correlation matrix of SET scores from EFA-based dataset")


# Conducting reliability analysis to return McDonald's omega of SET instrument
omega(set_efa_dataset, title = "Reliability Analysis of ETLBS Items From EFA-based Dataset")


# Bartlett's test of sphericity
# assess whether a dataset is suitable for factor analysis by testing the overall significance of the correlation matrix.
# null-hypothesis: correlation matrix is an identity matrix; variables are uncorrelated, and not suitable for factor analysis.
# alternative-hypothesis: correlation matrix is not an identity matrix; some correlations exist, suggesting the data is factorable.
# χ² value tells us how much does correlation matrix differ from identity matrix
# df value tells us how many pairwise correlations are being tested
cortest.bartlett(set_efa_dataset)


# KMO test
# High KMO (close to 1): Sampling is adequate, correlations are compact — EFA is likely to succeed.
# Low KMO (< 0.50): Strong partial correlations exist — variables may not form clear factors.
KMO(set_efa_dataset)


# Determine number of factors using eigenvalues from correlation test and scree plot of eigenvalues
# cor()calculates the correlation matrix for a dataset
# eigen(cor()) performs an eigen-decomposition of a correlation matrix
# Eigen-decomposition breaks down the correlation matrix into eigenvalues and eigenvectors
# Eigenvalues measure the amount of variance captured by each dimension (factor/component)
# Eigenvectors represent the direction of these dimensions.
set_efa_dataset_ev <- eigen(cor(set_efa_dataset))$values
#plot with type "b" plots both lines and points to show trend clearly
plot(set_efa_dataset_ev, type = "b", main = "Eigenvalues (Scree Test) for EFA dataset", xlab = "Factor Number", ylab = "Eigenvalue")


# Run parallel analysis to determine number of factors from dataset directly
# fa with "fa" indicates we are performing factor analysis, not principal components analysis
# n.iter determines the number of simulated datasets for parallel analysis
fa.parallel(set_efa_dataset, fa = "fa", n.iter = 1000, show.legend = TRUE, main = "Scree Plot & Parallel Analysis for EFA dataset")


# based on initial scree plot of eigenvalues, we will apply no rotation and 1 factor test like chi-square 
set_efa_dataset_model01 <- fa(set_efa_dataset, nfactors = 1, rotate = "none", fm = "ml")
# print(set_efa_dataset_model01$loadings, cutoff = 0.1) # set cutoff value at 0.1 to inspect all small and large relationships
print(set_efa_dataset_model01$loadings, cutoff = 0.5) # set cutoff value at 0.5 to simplify interpretation and reduce noise


# Testing hypothesis H3
# Defining a CV-derived, 5-factor model for CFA, using groupings from content-validation
set_5_factor_model <- '
  f1 =~ s1
  f2 =~ s2 + s3 + s4 + s5 + s6
  f3 =~ s8 + s9
  f4 =~ s7
  f5 =~ s10
'


# Fit the CV-derived, 5-factor CFA model using ML estimator
set_5_factor_model_fit <- cfa(set_5_factor_model, data = set_cfa_dataset, std.lv = TRUE)


# Summarise the model
summary(set_5_factor_model_fit, fit.measures = TRUE, standardized = TRUE)


# Visualize the standardized CV-derived, 4-factor CFA model
semPaths(set_5_factor_model_fit, "std", whatLabels = "std", edge.label.cex = 0.8,
         layout = "tree", residuals = FALSE, nCharNodes = 0)


# Define 1-factor model, which was already confirmed via EFA
set_1_factor_model <- 'f1 =~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10'


# Fit and summarise the 1-factor model
set_1_factor_model_fit <- cfa(set_1_factor_model, data = set_cfa_dataset, std.lv = TRUE)
summary(set_1_factor_model_fit, fit.measures = TRUE, standardized = TRUE)


# Compare 5-factor model vs 1-factor model using these
# fit indices: RMSEA, CFI, TLI, AIC/BIC
# RMSEA means Root Mean Square Error of Approximation, or how well the model approximates the population covariance matrix, per degree of freedom
# RMSEA values: < 0.05 = very good, 0.05–0.08 = acceptable, > 0.10 = poor
# CFI means Comparative Fit Index, which compares a model’s fit to a null model (a very bad baseline model where nothing is related).
# CFI values: 	> 0.95 = very good, 0.90–0.95 = acceptable, <0.90 = poor
# TLI means: Tucker-Lewis Index, which is similar to CFI, but more heavily penalizes model complexity (too many parameters).
# TLI values: > 0.95 = excellent, 0.90–0.95 = acceptable
# AIC means Akaike Information Criterion, and BIC means Bayesian Information Criterion
# AIC/BIC are model comparison indices, they tell us which model is better by focusing on predictive accuracy
# Lower AIC/BIC = better model when comparing two or more models


# Get a full list of fit measures
fit_1 <- fitMeasures(set_1_factor_model_fit)
fit_5 <- fitMeasures(set_5_factor_model_fit)


# Select key indices for reporting
indices <- c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic", "npar", "chisq", "df", "pvalue")


# Combine into a data frame for easy comparison
fit_comparison <- data.frame(
  Index = indices,
  One_factor_model = unname(fit_1[indices]),
  Five_factors_model = unname(fit_5[indices])
)


# Print beautifully
knitr::kable(fit_comparison, digits = 3, caption = "Model Fit Comparison: 1-Factor model vs 5-Factors model")
