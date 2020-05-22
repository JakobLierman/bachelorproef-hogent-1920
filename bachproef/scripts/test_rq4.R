# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$birthdate <- as.Date(dataSet$birthdate, "%Y-%m-%d")

subSetWithoutOnboarding <- subset(dataSet, onboarding_elements == FALSE, select = -c(onboarding_elements))
subSetWithOnboarding <- subset(dataSet, onboarding_elements == TRUE, select = -c(onboarding_elements))

# Changed name
chisq.test(x = dataSet$onboarding_elements, y = dataSet$uses_personal_initial, correct = FALSE)

countsChangedName <- table(dataSet$uses_personal_initial, dataSet$onboarding_elements)
pdf("../img/beschrijving-changed-name.pdf", height=5, width=6)
barplot(prop.table(countsChangedName, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Naam niet gewijzigd", "Naam gewijzigd"))
dev.off()
