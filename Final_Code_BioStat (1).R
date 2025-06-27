library(car)
library(dunn.test)
library(report)
library(multcomp)
library(dplyr)
library(skimr)
library(ggplot2)

# rm(list = ls())

load("C:/Users/Mohamed/Downloads/PWD.Rdata")
  data <- PWD

# a) Summary statistics for numeric variables (W0, P0, ADWG0021, ADWG2150, ADWG0050)
cat("\nDescriptive Statistics for Numeric Variables:\n")
numeric_vars <- c("W0", "ADWG0021", "ADWG2150", "ADWG0050")
summary_stats <- sapply(data[numeric_vars], function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE)
  )
})
print(summary_stats)

# b) Frequency table for categorical variables (Treatment and Sex)
cat("\nFrequency Table for Treatment:\n")
table(data$Treatment)

cat("\nFrequency Table for Sex:\n")
table(data$Sex)

# c) Correlation coefficients
cat("\nCorrelation between ADWG0021 and ADWG2150:\n")
cor_ADWG0021_2150 <- cor(data$ADWG0021, data$ADWG2150, use = "complete.obs")
print(cor_ADWG0021_2150) # 0.2270356

cat("\nCorrelation between ADWG0021 and ADWG0050:\n")
cor_ADWG0021_0050 <- cor(data$ADWG0021, data$ADWG0050, use = "complete.obs")
print(cor_ADWG0021_0050) # 0.4427165
#--------------------------------------------------------------------
# 2. Graphics

# a) bar chart for the gender (Sex parameter).
data$Sex <- factor(data$Sex, levels = c(1, 2), labels = c("male", "female"))

cat("Levels of Sex:", levels(data$Sex), "\n")
cat("Table of Sex:\n")
print(table(data$Sex))

# Generating Bar Chart for Sex
cat("\nGenerating Bar Chart for Sex\n")
ggplot(data, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink"), 
                    name = "Gender") +
  labs(title = "Bar Chart of Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(legend.position = "right")

# b) Generating Bar Chart for Mean ADWG0021 by Sex
# Calculate mean ADWG0021 by Sex
mean_adwg <- aggregate(ADWG0021 ~ Sex, data = data, FUN = mean, na.rm = TRUE)
print(mean_adwg)

cat("\nGenerating Bar Chart for Mean ADWG0021 by Sex\n")
ggplot(mean_adwg, aes(x = Sex, y = ADWG0021, fill = Sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink"), 
                    name = "Gender") +
  labs(title = "Mean ADWG0021 by Gender", x = "Gender", y = "Mean ADWG0021") +
  theme_minimal() +
  theme(legend.position = "right")

# c) histograms of “ADWG2150” and “ADWG0021”.
# Verify data structure for ADWG2150 and ADWG0021
cat("Summary of ADWG2150:\n")
print(summary(data$ADWG2150))
cat("Summary of ADWG0021:\n")
print(summary(data$ADWG0021))

# Generating Histogram for ADWG2150
cat("\nGenerating Histogram for ADWG2150\n")
ggplot(data, aes(x = ADWG2150)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black") +
  labs(title = "Histogram of ADWG2150", x = "ADWG2150", y = "Frequency") +
  theme_minimal()

# Generating Histogram for ADWG0021
cat("\nGenerating Histogram for ADWG0021\n")
ggplot(data, aes(x = ADWG0021)) +
  geom_histogram(binwidth = 0.4, fill = "purple", color = "black") +
  labs(title = "Histogram of ADWG0021", x = "ADWG0021", y = "Frequency") +
  theme_minimal()

# d) scatterplot of 2 continuous variables ADWG0050 and ADWG0021, and add the regression lines for each gender
# Verify data structure
cat("Summary of ADWG0050:\n")
print(summary(data$ADWG0050))
cat("Summary of ADWG0021:\n")
print(summary(data$ADWG0021))
cat("Levels of Sex:", levels(data$Sex), "\n")
cat("Table of Sex:\n")
print(table(data$Sex))

# Generating Scatterplot of ADWG0050 vs ADWG0021 by Sex
cat("\nGenerating Scatterplot of ADWG0050 vs ADWG0021 by Sex\n")
ggplot(data, aes(x = ADWG0050, y = ADWG0021, color = Sex)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("male" = "lightblue", "female" = "lightpink"), 
                     name = "Gender") +
  labs(title = "Scatterplot of ADWG0050 vs ADWG0021 by Gender",
       x = "ADWG0050", y = "ADWG0021") +
  theme_minimal() +
  theme(legend.position = "right")

# e) boxplot of ADWG0021 in and a separate boxplots per Treatment (as.factors). 
# Ensure Treatment is a factor
data$Treatment <- as.factor(data$Treatment)

# Verify data structure
cat("Summary of ADWG0021:\n")
print(summary(data$ADWG0021))
cat("Levels of Treatment:", levels(data$Treatment), "\n")
cat("Table of Treatment:\n")
print(table(data$Treatment))

# Generating Boxplot of ADWG0021 (overall)
cat("\nGenerating Boxplot of ADWG0021\n")
ggplot(data, aes(y = ADWG0021)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of ADWG0021", y = "ADWG0021", x = "") +
  theme_minimal()

# Generating Boxplot of ADWG0021 by Treatment
cat("\nGenerating Boxplot of ADWG0021 by Treatment\n")
ggplot(data, aes(x = Treatment, y = ADWG0021, fill = Treatment)) +
  geom_boxplot(color = "black") +
  scale_fill_brewer(palette = "Set2", name = "Treatment") +
  labs(title = "Boxplot of ADWG0021 by Treatment", x = "Treatment", y = "ADWG0021") +
  theme_minimal() +
  theme(legend.position = "right")


# 3. Outliers detection 

# Function to detect outliers using IQR rule for("ADWG0021", "ADWG2150", "ADWG0050", "W0")
detect_outliers <- function(x) {
  if (all(is.na(x))) return(NA)
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

# Numeric variables to check 
num_vars <- c("ADWG0021", "ADWG2150", "ADWG0050", "W0")

# Detect and print outliers 
cat("\nOutlier Detection using IQR Rule:\n")
for (var in num_vars) {
  outliers <- detect_outliers(data[[var]])
  cat("\nVariable:", var, "\n")
  if (length(outliers) > 0) {
    cat("Outlier indices:", outliers, "\n")
    cat("Outlier values:", data[[var]][outliers], "\n")
    cat("Summary of", var, ":\n")
    print(summary(data[[var]]))
  } else {
    cat("No outliers detected.\n")
    cat("Summary of", var, ":\n")
    print(summary(data[[var]]))
  }
}

# Visualize outliers with boxplots
cat("\nGenerating Boxplots for Outlier Visualization:\n")
for (var in num_vars) {
  cat("Boxplot for", var, "\n")
  print(
    ggplot(data, aes(y = .data[[var]])) +
      geom_boxplot(fill = "lightblue", color = "black") +
      labs(title = paste("Boxplot of", var), y = var, x = "") +
      theme_minimal()
  )
}



# Verify data structure
cat("Summary of ADWG0021:\n")
print(summary(data$ADWG0021))
cat("Levels of Treatment:", levels(data$Treatment), "\n")
cat("Table of Treatment:\n")
print(table(data$Treatment))

# Function to detect outliers using IQR rule for a group for in ADWG0021 by Treatment
detect_outliers_by_group <- function(data, var, group) {
  outliers_list <- list()
  groups <- levels(data[[group]])
  for (g in groups) {
    subset_data <- data[data[[group]] == g, var]
    if (all(is.na(subset_data))) next
    q <- quantile(subset_data, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower_bound <- q[1] - 1.5 * iqr
    upper_bound <- q[2] + 1.5 * iqr
    outlier_indices <- which(data[[group]] == g & (data[[var]] < lower_bound | data[[var]] > upper_bound))
    if (length(outlier_indices) > 0) {
      outliers_list[[g]] <- list(
        indices = outlier_indices,
        values = data[[var]][outlier_indices]
      )
    }
  }
  return(outliers_list)
}

# Detect outliers in ADWG0021 by Treatment
cat("\nOutlier Detection in ADWG0021 by Treatment:\n")
outliers <- detect_outliers_by_group(data, "ADWG0021", "Treatment")
if (length(outliers) > 0) {
  for (group in names(outliers)) {
    cat("Treatment:", group, "\n")
    cat("Outlier indices:", outliers[[group]]$indices, "\n")
    cat("Outlier values:", outliers[[group]]$values, "\n")
    cat("Summary of ADWG0021 for Treatment", group, ":\n")
    print(summary(data$ADWG0021[data$Treatment == group]))
  }
} else {
  cat("No outliers detected in ADWG0021 by Treatment.\n")
}



########################################################################
########################################################################
#4.1.Check the normality using two methods
# Shapiro-Wilk Test 1st way
features <- c("ADWG0021", "ADWG2150", "ADWG0050")
cat("Normality Testing:\n")
for (feature in features) {
  cat("\nFeature:", feature, "\n")
  
  shapiro_result <- shapiro.test(PWD[[feature]])
  cat("Shapiro-Wilk p-value:", shapiro_result$p.value, "\n")
  if (shapiro_result$p.value > 0.05) {
    cat("=> Likely normal distribution\n")
  } else {
    cat("=> Likely not normal distribution\n")
  }
  # Q-Q Plot 2nd way
  qqnorm(PWD[[feature]], main = paste("Q-Q Plot of", feature))
  qqline(PWD[[feature]], col = "red")
}


#4.2.homoscedasticity using two methods
####Method 1: Levene’s Test
# Levene's test (center = median, robust)
levene_0021 <- leveneTest(ADWG0021 ~ Treatment, data = PWD, center = median)
levene_2150 <- leveneTest(ADWG2150 ~ Treatment, data = PWD, center = median)
levene_0050 <- leveneTest(ADWG0050 ~ Treatment, data = PWD, center = median)
cat("Levene's Test p-values:\n")
cat("ADWG0021:", levene_0021$`Pr(>F)`[1], "\n")
cat("ADWG2150:", levene_2150$`Pr(>F)`[1], "\n")
cat("ADWG0050:", levene_0050$`Pr(>F)`[1], "\n")
####Method 2: Bartlett’s Test
# Bartlett's test
bartlett_0021 <- bartlett.test(ADWG0021 ~ Treatment, data = PWD)
bartlett_2150 <- bartlett.test(ADWG2150 ~ Treatment, data = PWD)
bartlett_0050 <- bartlett.test(ADWG0050 ~ Treatment, data = PWD)
cat("Bartlett's Test p-values:\n")
cat("ADWG0021:", bartlett_0021$p.value, "\n")
cat("ADWG2150:", bartlett_2150$p.value, "\n")
cat("ADWG0050:", bartlett_0050$p.value, "\n")

#5.
# Split data by Gender
grouped_data <- PWD %>% group_by(Sex)
# Function to calculate mean and confidence intervals
calculate_ci <- function(x, conf_level) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  error_margin <- qt((1 + conf_level) / 2, df = n - 1) * sd_x / sqrt(n)
  lower_bound <- mean_x - error_margin
  upper_bound <- mean_x + error_margin
  return(c(mean = mean_x, lower = lower_bound, upper = upper_bound))
}
# Now calculate for each gender
genders <- unique(PWD$Sex)

for (gender in genders) {
  cat("\nGender:", gender, "\n")
  gender_data <- PWD %>% filter(Sex == gender) %>% pull(ADWG0021)
  
  ci_90 <- calculate_ci(gender_data, 0.90)
  ci_95 <- calculate_ci(gender_data, 0.95)
  ci_99 <- calculate_ci(gender_data, 0.99)
  cat("We are 90%, 95%, or 99% confident that the true mean ADWG0021 for each gender is between [Lower Bound] and [Upper Bound]:") 
  cat("90% CI: (", ci_90["lower"], ",", ci_90["upper"], ")\n")
  cat("95% CI: (", ci_95["lower"], ",", ci_95["upper"], ")\n")
  cat("99% CI: (", ci_99["lower"], ",", ci_99["upper"], ")\n")
}

# Updated function to also return width
library(dplyr)

# Function to calculate mean, SD, SEM, and confidence intervals + widths
calculate_ci <- function(x, conf_level) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sem <- sd_x / sqrt(n)
  t_val <- qt((1 + conf_level) / 2, df = n - 1)
  error_margin <- t_val * sem
  lower_bound <- mean_x - error_margin
  upper_bound <- mean_x + error_margin
  interval_width <- upper_bound - lower_bound
  
  return(list(
    n = n,
    mean = mean_x,
    sd = sd_x,
    sem = sem,
    lower = lower_bound,
    upper = upper_bound,
    width = interval_width
  ))
}

# Initialize a data frame to collect results
results <- data.frame()

# Calculate CIs by gender
genders <- unique(PWD$Sex)

for (gender in genders) {
  gender_data <- PWD %>% filter(Sex == gender) %>% pull(ADWG0021)
  
  ci90 <- calculate_ci(gender_data, 0.90)
  ci95 <- calculate_ci(gender_data, 0.95)
  ci99 <- calculate_ci(gender_data, 0.99)
  
  results <- rbind(results, data.frame(
    Sex = gender,
    n = ci90$n,
    mean = ci90$mean,
    sd = ci90$sd,
    sem = ci90$sem,
    ci90_lower = ci90$lower,
    ci90_upper = ci90$upper,
    ci90_width = ci90$width,
    ci95_lower = ci95$lower,
    ci95_upper = ci95$upper,
    ci95_width = ci95$width,
    ci99_lower = ci99$lower,
    ci99_upper = ci99$upper,
    ci99_width = ci99$width
  ))
}
print(results)


########################################################################
########################################################################
# Hypothesis 1

# Step 1: test normality 

# For female group 
# by Q-Q Plot
qqnorm(data[data$Sex == "female",]$ADWG0021, main="females weight from 0-21")
qqline(data[data$Sex == "female",]$ADWG0021)
# by Histogram
hist(data[data$Sex == "female",]$ADWG0021, main="females weight from 0-21")
# by Shapiro Statistical test 
shapiro.test(data[data$Sex == "female",]$ADWG0021) # p-value = 0.9513, We do not reject the null , alpha = 0.05, p>a

######  So the female data is normally dist.
#---------------------------------------------
# For male group 

# by Shapiro Statistical test 
shapiro.test(data[data$Sex == "male",]$ADWG0021) # p-value = 0.9771

# So the male data is normally dist.
#--------------------------------------------------------------------
# Step 2: Test Variance 

var.test(ADWG0021~Sex , data=data) 
# var test = 0.4193 ==> the variance is equal (dont reject null)
#--------------------------------------------------------------------
# Step 3: T-Test 
# since variance is equal we will use normal t-test

t.test(ADWG0021~Sex , data=data , var.equal = TRUE)
# p-value = 0.7557

##############################################################################
# Hypothesis 2

# Step 1: Extract A & B Treatment only 

Subgroub <- data %>%
  select(ADWG0021, Treatment) %>%           
  filter(Treatment %in% c("A", "B")) 

# Step 2: Check normality for treatment A & B 

# by Shapiro Statistical test 
shapiro.test(Subgroub[Subgroub$Treatment == "A",]$ADWG0021) # p-value = 0.0395
# So the treatment A data is noooooooooooooot normally dist.

#-------------------------------

# by Shapiro Statistical test 
shapiro.test(Subgroub[Subgroub$Treatment == "B",]$ADWG0021) # p-value = 0.8132
# so the treatment B data is normally distributed

#--------------------------------------------------------------
# Step 3: Check Variance 

var.test(ADWG0021~Treatment , data=Subgroub) 
#  F test = 0.955  ==> the variance is equal ,
#--------------------------------------------------------------

# Step 4: Mann-Whitney rank-based test

#?wilcox.test
wilcox.test(ADWG0021~Treatment, data = Subgroub, exact = FALSE)
# p-value = 0.0312 ==> the 2 groups are different 

#############################################################
# Hypothesis 3 
# step 1: Test normality

shapiro.test(data[data$Treatment == "A",]$ADWG0021) # p-value = 0.0395
shapiro.test(data[data$Treatment == "B",]$ADWG0021) # p-value = 0.8132
shapiro.test(data[data$Treatment == "C",]$ADWG0021) # p-value = 0.6954
shapiro.test(data[data$Treatment == "D",]$ADWG0021) # p-value = 0.7126
shapiro.test(data[data$Treatment == "E",]$ADWG0021) # p-value = 0.86
# A is not normal , the rest are normal 
#-------------------------------------------------
# step 2: Test Variance

leveneTest(ADWG0021~Treatment , data=data) # P = 0.8968 ==> equal variance 

#-------------------------------------------------
# step 3: Anova Test 


Amodel <- aov(ADWG0021~Treatment , data=data ) # p-value = 0.101
summary(Amodel)
report(Amodel)

#-------------------------------------------------

# step 4: Post-hoc Test 
pairwise_comp <- TukeyHSD(Amodel)
pairwise_comp

#$Treatment
#diff        lwr       upr     p adj
#B-A -24.590774 -50.670281  1.488734 0.0724945  << significant 
#C-A -14.794147 -40.873654 11.285361 0.4884256
#D-A -10.701885 -36.781392 15.377622 0.7626339
#E-A  -5.865575 -31.945083 20.213932 0.9661386
#C-B   9.796627 -16.282880 35.876134 0.8154751
#D-B  13.888889 -12.190619 39.968396 0.5499153
#E-B  18.725198  -7.354309 44.804706 0.2580810
#D-C   4.092262 -21.987246 30.171769 0.9910644
#E-C   8.928571 -17.150936 35.008079 0.8605295
#E-D   4.836310 -21.243198 30.915817 0.9832666

# Group B-A is the only significant group ==> Dont reject null 


#After testing the assumptions we found that the data is not normal so we will do # Kruskal-Wallis test
Kruskal <- kruskal.test( ADWG0021~Treatment , data = data )

# Print the result of Kruskal
print(Kruskal)

#Dunn for post hoc in non parametric tests
dunn_result <- dunnTest(ADWG0021 ~ Treatment, data = data, method = "holm")

# View the results
print(dunn_result)

#Linear Model

# Fit linear regression of ADWG0021 on Sex
model_sex <- lm(ADWG0021 ~ factor(Sex), data = data)
# Show summary of the model
summary_model <- summary(model_sex)
print(summary_model)
# Compute 95% confidence interval for the slope (difference between Sex levels)
ci <- confint(model_sex, level = 0.95)
print(ci)
 
#The estimated difference in ADWG0021 when changing Sex from 1 (male) to 2 (female) is –1.92.
#The p-value for this coefficient is 0.756, so we fail to reject the null hypothesis of no difference (at any conventional level).
#The 95% confidence interval for the slope is [–14.32, 10.48], which includes zero, confirming that the effect is not statistically significant.
#Thus, under normality and homoscedasticity assumptions, there is no evidence that ADWG0021 differs between males and females, and the estimated mean change when moving from Sex = 1 to Sex = 2 is –1.92 units.