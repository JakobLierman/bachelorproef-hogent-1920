# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$sus_4 <- factor(dataSet$sus_4,
                        levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                        ordered = TRUE)
dataSet$sus_10 <- factor(dataSet$sus_10,
                         levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                         ordered = TRUE)

subSetWithoutOnboarding <- subset(dataSet, onboarding_elements == FALSE, select = -c(onboarding_elements))
subSetWithOnboarding <- subset(dataSet, onboarding_elements == TRUE, select = -c(onboarding_elements))

# SUS 4
meanSUS4WithoutOnboarding <- mean(as.numeric(subSetWithoutOnboarding$sus_4))
round(meanSUS4WithoutOnboarding, 2)
sdSUS4WithoutOnboarding <- sd(as.numeric(subSetWithoutOnboarding$sus_4))
round(sdSUS4WithoutOnboarding, 2)

meanSUS4WithOnboarding <- mean(as.numeric(subSetWithOnboarding$sus_4))
round(meanSUS4WithOnboarding, 2)
sdSUS4WithOnboarding <- sd(as.numeric(subSetWithOnboarding$sus_4))
round(sdSUS4WithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = as.numeric(dataSet$sus_4))

# SUS 10
meanSUS10WithoutOnboarding <- mean(as.numeric(subSetWithoutOnboarding$sus_10))
round(meanSUS10WithoutOnboarding, 2)
sdSUS10WithoutOnboarding <- sd(as.numeric(subSetWithoutOnboarding$sus_10))
round(sdSUS10WithoutOnboarding, 2)

meanSUS10WithOnboarding <- mean(as.numeric(subSetWithOnboarding$sus_10))
round(meanSUS10WithOnboarding, 2)
sdSUS10WithOnboarding <- sd(as.numeric(subSetWithOnboarding$sus_10))
round(sdSUS10WithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = as.numeric(dataSet$sus_10))
