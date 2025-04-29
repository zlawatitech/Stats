# Load required packages
library(dplyr)
library(ggplot2)
set.seed(42)

# Simulate data
n <- 100
data <- data.frame(
  PatientID = 1:n,
  Treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
  Comorbidities = factor(sample(c("0", "1", "2+"), n, replace = TRUE)),
  RecoveryTime = rnorm(n, mean = 7, sd = 2)
)

# Add effects: Treatment A recovers faster, more comorbidities = longer recovery
data$RecoveryTime <- data$RecoveryTime -
  ifelse(data$Treatment == "A", 2, 0) +
  ifelse(data$Comorbidities == "1", 1, ifelse(data$Comorbidities == "2+", 3, 0))

head(data)


## Indepdent t-test samples##

# Compare RecoveryTime between Treatment A and B
t_test_result <- t.test(RecoveryTime ~ Treatment, data = data)
print(t_test_result)


## One-Way ANOVA ##
# Compare RecoveryTime across Comorbidity groups
anova_result <- aov(RecoveryTime ~ Comorbidities, data = data)
summary(anova_result)

# Post-hoc test (Tukey's HSD)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)


## Visualization ##
# Boxplot: Recovery Time by Treatment
ggplot(data, aes(x = Treatment, y = RecoveryTime, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Recovery Time by Treatment Group", y = "Recovery Time (days)")

# Boxplot: Recovery Time by Comorbidities
ggplot(data, aes(x = Comorbidities, y = RecoveryTime, fill = Comorbidities)) +
  geom_boxplot() +
  labs(title = "Recovery Time by Number of Comorbidities", y = "Recovery Time (days)")

## Assumption Checks ##

# Normality check
shapiro.test(data$RecoveryTime[data$Treatment == "A"])
shapiro.test(data$RecoveryTime[data$Treatment == "B"])

# Homogeneity of variances
library(car)
leveneTest(RecoveryTime ~ Comorbidities, data = data)
