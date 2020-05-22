# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$birthdate <- as.Date(dataSet$birthdate, "%Y-%m-%d")

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
meanSettingsTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$settings_task)
round(meanSettingsTaskWithoutOnboarding, 2)
sdSettingsTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$settings_task)
round(sdSettingsTaskWithoutOnboarding, 2)

meanSettingsTaskWithOnboarding <- mean(subSetWithOnboarding$settings_task)
round(meanSettingsTaskWithOnboarding, 2)
sdSettingsTaskWithOnboarding <- sd(subSetWithOnboarding$settings_task)
round(sdSettingsTaskWithOnboarding, 2)

### New Task
meanNewTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$new_task)
round(meanNewTaskWithoutOnboarding, 2)
sdNewTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$new_task)
round(sdNewTaskWithoutOnboarding, 2)

meanNewTaskWithOnboarding <- mean(subSetWithOnboarding$new_task)
round(meanNewTaskWithOnboarding, 2)
sdNewTaskWithOnboarding <- sd(subSetWithOnboarding$new_task)
round(sdNewTaskWithOnboarding, 2)

### Add Task
meanAddTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$add_task)
round(meanAddTaskWithoutOnboarding, 2)
sdAddTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task)
round(sdAddTaskWithoutOnboarding, 2)

meanAddTaskWithOnboarding <- mean(subSetWithOnboarding$add_task)
round(meanAddTaskWithOnboarding, 2)
sdAddTaskWithOnboarding <- sd(subSetWithOnboarding$add_task)
round(sdAddTaskWithOnboarding, 2)

### Delete Task
meanDeleteTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$delete_task)
round(meanDeleteTaskWithoutOnboarding, 2)
sdDeleteTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$delete_task)
round(sdDeleteTaskWithoutOnboarding, 2)

meanDeleteTaskWithOnboarding <- mean(subSetWithOnboarding$delete_task)
round(meanDeleteTaskWithOnboarding, 2)
sdDeleteTaskWithOnboarding <- sd(subSetWithOnboarding$delete_task)
round(sdDeleteTaskWithOnboarding, 2)

### Calculator Task
meanCalculatorTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$calculator_task)
round(meanCalculatorTaskWithoutOnboarding, 2)
sdCalculatorTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$calculator_task)
round(sdCalculatorTaskWithoutOnboarding, 2)

meanCalculatorTaskWithOnboarding <- mean(subSetWithOnboarding$calculator_task)
round(meanCalculatorTaskWithOnboarding, 2)
sdCalculatorTaskWithOnboarding <- sd(subSetWithOnboarding$calculator_task)
round(sdCalculatorTaskWithOnboarding, 2)

### Add Task Repeat
meanAddTaskRepeatWithoutOnboarding <- mean(subSetWithoutOnboarding$add_task_repeat)
round(meanAddTaskRepeatWithoutOnboarding, 2)
sdAddTaskRepeatWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithoutOnboarding, 2)

meanAddTaskRepeatWithOnboarding <- mean(subSetWithOnboarding$add_task_repeat)
round(meanAddTaskRepeatWithOnboarding, 2)
sdAddTaskRepeatWithOnboarding <- sd(subSetWithOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithOnboarding, 2)

### Combined tasks
meanCombinedTasksWithoutOnboarding <- mean(subSetWithoutOnboarding$times_mean)
round(meanCombinedTasksWithoutOnboarding, 2)
sdCombinedTasksWithoutOnboarding <- sd(subSetWithoutOnboarding$times_mean)
round(sdCombinedTasksWithoutOnboarding, 2)

meanCombinedTasksWithOnboarding <- mean(subSetWithOnboarding$times_mean)
round(meanCombinedTasksWithOnboarding, 2)
sdCombinedTasksWithOnboarding <- sd(subSetWithOnboarding$times_mean)
round(sdCombinedTasksWithOnboarding, 2)

## SUS
meanSUSWithoutOnboarding <- mean(subSetWithoutOnboarding$sus)
round(meanSUSWithoutOnboarding, 2)
sdSUSWithoutOnboarding <- sd(subSetWithoutOnboarding$sus)
round(sdSUSWithoutOnboarding, 2)

meanSUSWithOnboarding <- mean(subSetWithOnboarding$sus)
round(meanSUSWithOnboarding, 2)
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
