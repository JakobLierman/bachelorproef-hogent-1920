# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$birthdate <- as.Date(dataSet$birthdate, "%Y-%m-%d")
dataSet$sus_4 <- factor(dataSet$sus_4,
                        levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                        ordered = TRUE)
dataSet$sus_10 <- factor(dataSet$sus_10,
                         levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                         ordered = TRUE)

dataSet$times_mean <- (dataSet$settings_task + dataSet$new_task + dataSet$add_task + dataSet$delete_task + dataSet$calculator_task + dataSet$add_task_repeat) / 6

subSetWithoutOnboarding <- subset(dataSet, onboarding_elements == FALSE, select = -c(onboarding_elements))
subSetWithOnboarding <- subset(dataSet, onboarding_elements == TRUE, select = -c(onboarding_elements))

# Descriptives sample

## Gender
countsGender <- table(dataSet$gender, dataSet$onboarding_elements)
pdf("../img/beschrijving-steekproef-geslacht.pdf", height=5, width=6)
barplot(countsGender,
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Vrouw", "Man"))
dev.off()

## Age
library(eeptools)
dataSet$age <- age_calc(dataSet$birthdate, enddate = as.Date("2020-05-22"), units = "years", precise = FALSE)
age_floor <- floor(dataSet$age/10)*10
dataSet$age_group <- paste(age_floor, (age_floor+9), sep = "-")

countsAge <- table(dataSet$age_group, dataSet$onboarding_elements)
pdf("../img/beschrijving-steekproef-leeftijd.pdf", height=5, width=6)
barplot(countsAge,
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col=c("#00587d","#0076a6", "#0093d0", "#33a9d9", "#66bee3"),
        legend = row.names(countsAge))
dev.off()

# Descriptives test

## Times

### Settings Task
summary(subSetWithoutOnboarding$settings_task)
sdSettingsTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$settings_task)
round(sdSettingsTaskWithoutOnboarding, 2)

summary(subSetWithOnboarding$settings_task)
sdSettingsTaskWithOnboarding <- sd(subSetWithOnboarding$settings_task)
round(sdSettingsTaskWithOnboarding, 2)

### New Task
summary(subSetWithoutOnboarding$new_task)
sdNewTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$new_task)
round(sdNewTaskWithoutOnboarding, 2)

summary(subSetWithOnboarding$new_task)
sdNewTaskWithOnboarding <- sd(subSetWithOnboarding$new_task)
round(sdNewTaskWithOnboarding, 2)

### Add Task
summary(subSetWithoutOnboarding$add_task)
sdAddTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task)
round(sdAddTaskWithoutOnboarding, 2)

summary(subSetWithOnboarding$add_task)
sdAddTaskWithOnboarding <- sd(subSetWithOnboarding$add_task)
round(sdAddTaskWithOnboarding, 2)

### Delete Task
summary(subSetWithoutOnboarding$delete_task)
sdDeleteTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$delete_task)
round(sdDeleteTaskWithoutOnboarding, 2)

summary(subSetWithOnboarding$delete_task)
sdDeleteTaskWithOnboarding <- sd(subSetWithOnboarding$delete_task)
round(sdDeleteTaskWithOnboarding, 2)

### Calculator Task
summary(subSetWithoutOnboarding$calculator_task)
sdCalculatorTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$calculator_task)
round(sdCalculatorTaskWithoutOnboarding, 2)

summary(subSetWithOnboarding$calculator_task)
sdCalculatorTaskWithOnboarding <- sd(subSetWithOnboarding$calculator_task)
round(sdCalculatorTaskWithOnboarding, 2)

### Add Task Repeat
summary(subSetWithoutOnboarding$add_task_repeat)
sdAddTaskRepeatWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithoutOnboarding, 2)

summary(subSetWithOnboarding$add_task_repeat)
sdAddTaskRepeatWithOnboarding <- sd(subSetWithOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithOnboarding, 2)

### Combined tasks
summary(subSetWithoutOnboarding$times_mean)
sdCombinedTasksWithoutOnboarding <- sd(subSetWithoutOnboarding$times_mean)
round(sdCombinedTasksWithoutOnboarding, 2)

summary(subSetWithOnboarding$times_mean)
sdCombinedTasksWithOnboarding <- sd(subSetWithOnboarding$times_mean)
round(sdCombinedTasksWithOnboarding, 2)

## SUS
summary(subSetWithoutOnboarding$sus)
sdSUSWithoutOnboarding <- sd(subSetWithoutOnboarding$sus)
round(sdSUSWithoutOnboarding, 2)

summary(subSetWithOnboarding$sus)
sdSUSWithOnboarding <- sd(subSetWithOnboarding$sus)
round(sdSUSWithOnboarding, 2)

### Learnability vragen

# TODO

## Help

### Settings Task
countsSettingsHelp <- table(dataSet$settings_task_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-instellingen.pdf", height=5, width=6)
barplot(prop.table(countsSettingsHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

### New Task
countsNewHelp <- table(dataSet$new_task_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-spaardoel-toevoegen.pdf", height=5, width=6)
barplot(prop.table(countsNewHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

### Add Task
countsAddHelp <- table(dataSet$add_task_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-bedrag-toevoegen.pdf", height=5, width=6)
barplot(prop.table(countsAddHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

countsAddPlus <- table(dataSet$add_task_plus, dataSet$onboarding_elements)
countsAddPlus <- countsAddPlus[c(2,1),]
pdf("../img/beschrijving-plus-bedrag-toevoegen.pdf", height=5, width=6)
barplot(prop.table(countsAddPlus, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Plus gebruikt", "Geen plus gebruikt"))
dev.off()

### Delete Task
countsDeleteHelp <- table(dataSet$delete_task_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-spaardoel-verwijderen.pdf", height=5, width=6)
barplot(prop.table(countsDeleteHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

### Calculator Task
countsCalculatorHelp <- table(dataSet$calculator_task_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-berekening.pdf", height=5, width=6)
barplot(prop.table(countsCalculatorHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

### Add Task Repeat
countsAddRepeatHelp <- table(dataSet$add_task_repeat_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-hulp-groot-bedrag-toevoegen.pdf", height=5, width=6)
barplot(prop.table(countsAddRepeatHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Geen hulp nodig", "Hulp nodig"))
dev.off()

countsAddRepeatPlus <- table(dataSet$add_task_repeat_plus, dataSet$onboarding_elements)
countsAddRepeatPlus <- countsAddRepeatPlus[c(2,1),]
pdf("../img/beschrijving-plus-groot-bedrag-toevoegen.pdf", height=5, width=6)
barplot(prop.table(countsAddRepeatPlus, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Plus gebruikt", "Geen plus gebruikt"))
dev.off()

## Finds learnability better
countsFindsBetter <- table(dataSet$finds_learnability_better, dataSet$onboarding_elements)
pdf("../img/beschrijving-finds-better.pdf", height=5, width=6)
barplot(prop.table(countsFindsBetter, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Prefereert zonder onboarding en help", "Prefereert met onboarding en help"))
dev.off()

## Would use help
countsWouldUseHelp <- table(dataSet$would_use_help, dataSet$onboarding_elements)
pdf("../img/beschrijving-would-use-help.pdf", height=5, width=6)
barplot(prop.table(countsWouldUseHelp, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Zou geen help-sectie gebruiken", "Zou help-sectie gebruiken"))
dev.off()

## Would keep
countsWouldKeep <- table(dataSet$would_keep, dataSet$onboarding_elements)
pdf("../img/beschrijving-would-keep.pdf", height=5, width=6)
barplot(prop.table(countsWouldKeep, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Applicatie verwijderen", "Applicatie houden"))
dev.off()

## Changed name
countsChangedName <- table(dataSet$uses_personal_initial, dataSet$onboarding_elements)
pdf("../img/beschrijving-changed-name.pdf", height=5, width=6)
barplot(prop.table(countsChangedName, 2),
        ylab = "Aantal",
        names.arg = c("Zonder onboarding en help", "Met onboarding en help"),
        col = c("#eb3655","#0093d0"),
        legend = c("Naam niet gewijzigd", "Naam gewijzigd"))
dev.off()
