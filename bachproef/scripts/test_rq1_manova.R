# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$birthdate <- as.Date(dataSet$birthdate, "%Y-%m-%d")

subSetWithoutOnboarding <- subset(dataSet, onboarding_elements == FALSE, select = -c(onboarding_elements))
subSetWithOnboarding <- subset(dataSet, onboarding_elements == TRUE, select = -c(onboarding_elements))

# MANOVA

## Assumpties MANOVA
### Univariate outliers
library(rstatix)
subset(dataSet, select = c(onboarding_elements, settings_task)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(settings_task)

subset(dataSet, select = c(onboarding_elements, new_task)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(new_task)

subset(dataSet, select = c(onboarding_elements, add_task)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(add_task)

subset(dataSet, select = c(onboarding_elements, delete_task)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(delete_task)

subset(dataSet, select = c(onboarding_elements, calculator_task)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(calculator_task)

subset(dataSet, select = c(onboarding_elements, add_task_repeat)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(add_task_repeat)

subset(dataSet, select = c(onboarding_elements, sus)) %>%
  group_by(onboarding_elements) %>%
  identify_outliers(sus)

### Multivariate outliers (output moet p-waarde kleiner dan 0.05 returnen)
subset(dataSet, select = c(onboarding_elements, settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus)) %>%
  group_by(onboarding_elements) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

### Univariaat normaal verdeeld (output moet p-waarde groter dan 0.05 returnen)
dataSet %>%
  group_by(onboarding_elements) %>%
  shapiro_test(settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus) %>%
  arrange(variable)

### Multivariaat normaal verdeeld (output moet p-waarde kleiner dan 0.05 returnen)
dataSet %>%
  select(settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus) %>%
  mshapiro_test()

### Homogeniteit covariantie (output moet p-waarde groter dan 0.05 returnen)
box_m(dataSet[, c("settings_task", "new_task", "add_task", "delete_task", "calculator_task", "add_task_repeat", "sus")], dataSet$onboarding_elements)

### Homogeniteit variantie (output moet p-waarde groter dan 0.05 returnen)
dataSet %>%
  gather(key = "variable", value = "value", settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus) %>%
  group_by(variable) %>%
  levene_test(value ~ onboarding_elements)

## MANOVA uitvoeren
summary(
  manova(cbind(settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus) ~ onboarding_elements, data = dataSet),
  test="Pillai" # "Wilks" "Hotelling-Lawley" "Roy"
)
summary.aov(
  manova(cbind(settings_task, new_task, add_task, delete_task, calculator_task, add_task_repeat, sus) ~ onboarding_elements, data = dataSet),
  test="Pillai" # "Wilks" "Hotelling-Lawley" "Roy"
)
