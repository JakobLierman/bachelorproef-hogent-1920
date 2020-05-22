# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")

# Changed name
chisq.test(x = dataSet$onboarding_elements, y = dataSet$uses_personal_initial, correct = FALSE)
