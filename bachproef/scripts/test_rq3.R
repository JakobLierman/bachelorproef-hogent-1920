# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")

# Would keep
chisq.test(x = dataSet$onboarding_elements, y = dataSet$would_keep, correct = FALSE)
