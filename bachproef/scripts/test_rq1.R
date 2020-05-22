# Data
dataSet <- read.csv(file = "dataset.csv", sep = ";")
dataSet$birthdate <- as.Date(dataSet$birthdate, "%Y-%m-%d")

dataSet$times_mean <- (dataSet$settings_task + dataSet$new_task + dataSet$add_task + dataSet$delete_task + dataSet$calculator_task + dataSet$add_task_repeat) / 6

subSetWithoutOnboarding <- subset(dataSet, onboarding_elements == FALSE, select = -c(onboarding_elements))
subSetWithOnboarding <- subset(dataSet, onboarding_elements == TRUE, select = -c(onboarding_elements))

# Hulp nodig

## Settings Task
#chisq.test(x = dataSet$onboarding_elements, y = dataSet$settings_task_help, correct = FALSE)

## New Task
#chisq.test(x = dataSet$onboarding_elements, y = dataSet$new_task_help, correct = FALSE)

## Add Task
#chisq.test(x = dataSet$onboarding_elements, y = dataSet$add_task_help, correct = FALSE)

## Delete Task
chisq.test(x = dataSet$onboarding_elements, y = dataSet$delete_task_help, correct = FALSE)

## Calculator Task
#chisq.test(x = dataSet$onboarding_elements, y = dataSet$calculator_task_help, correct = FALSE)

## Add Task Repeat
chisq.test(x = dataSet$onboarding_elements, y = dataSet$add_task_repeat_help, correct = FALSE)

# T-test van gemiddelde van tijden
meanCombinedTasksWithoutOnboarding <- mean(subSetWithoutOnboarding$times_mean)
round(meanCombinedTasksWithoutOnboarding, 2)
sdCombinedTasksWithoutOnboarding <- sd(subSetWithoutOnboarding$times_mean)
round(sdCombinedTasksWithoutOnboarding, 2)

meanCombinedTasksWithOnboarding <- mean(subSetWithOnboarding$times_mean)
round(meanCombinedTasksWithOnboarding, 2)
sdCombinedTasksWithOnboarding <- sd(subSetWithOnboarding$times_mean)
round(sdCombinedTasksWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$times_mean)

# T-tests per opdracht

## Settings Task
meanSettingsTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$settings_task)
round(meanSettingsTaskWithoutOnboarding, 2)
sdSettingsTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$settings_task)
round(sdSettingsTaskWithoutOnboarding, 2)

meanSettingsTaskWithOnboarding <- mean(subSetWithOnboarding$settings_task)
round(meanSettingsTaskWithOnboarding, 2)
sdSettingsTaskWithOnboarding <- sd(subSetWithOnboarding$settings_task)
round(sdSettingsTaskWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$settings_task)

## New Task
meanNewTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$new_task)
round(meanNewTaskWithoutOnboarding, 2)
sdNewTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$new_task)
round(sdNewTaskWithoutOnboarding, 2)

meanNewTaskWithOnboarding <- mean(subSetWithOnboarding$new_task)
round(meanNewTaskWithOnboarding, 2)
sdNewTaskWithOnboarding <- sd(subSetWithOnboarding$new_task)
round(sdNewTaskWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$new_task)

## Add Task
meanAddTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$add_task)
round(meanAddTaskWithoutOnboarding, 2)
sdAddTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task)
round(sdAddTaskWithoutOnboarding, 2)

meanAddTaskWithOnboarding <- mean(subSetWithOnboarding$add_task)
round(meanAddTaskWithOnboarding, 2)
sdAddTaskWithOnboarding <- sd(subSetWithOnboarding$add_task)
round(sdAddTaskWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$add_task)

## Delete Task
meanDeleteTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$delete_task)
round(meanDeleteTaskWithoutOnboarding, 2)
sdDeleteTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$delete_task)
round(sdDeleteTaskWithoutOnboarding, 2)

meanDeleteTaskWithOnboarding <- mean(subSetWithOnboarding$delete_task)
round(meanDeleteTaskWithOnboarding, 2)
sdDeleteTaskWithOnboarding <- sd(subSetWithOnboarding$delete_task)
round(sdDeleteTaskWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$delete_task)

## Calculator Task
meanCalculatorTaskWithoutOnboarding <- mean(subSetWithoutOnboarding$calculator_task)
round(meanCalculatorTaskWithoutOnboarding, 2)
sdCalculatorTaskWithoutOnboarding <- sd(subSetWithoutOnboarding$calculator_task)
round(sdCalculatorTaskWithoutOnboarding, 2)

meanCalculatorTaskWithOnboarding <- mean(subSetWithOnboarding$calculator_task)
round(meanCalculatorTaskWithOnboarding, 2)
sdCalculatorTaskWithOnboarding <- sd(subSetWithOnboarding$calculator_task)
round(sdCalculatorTaskWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$calculator_task)

## Add Task Repeat
meanAddTaskRepeatWithoutOnboarding <- mean(subSetWithoutOnboarding$add_task_repeat)
round(meanAddTaskRepeatWithoutOnboarding, 2)
sdAddTaskRepeatWithoutOnboarding <- sd(subSetWithoutOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithoutOnboarding, 2)

meanAddTaskRepeatWithOnboarding <- mean(subSetWithOnboarding$add_task_repeat)
round(meanAddTaskRepeatWithOnboarding, 2)
sdAddTaskRepeatWithOnboarding <- sd(subSetWithOnboarding$add_task_repeat)
round(sdAddTaskRepeatWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$add_task_repeat)

# T-test SUS-score
meanSUSWithoutOnboarding <- mean(subSetWithoutOnboarding$sus)
round(meanSUSWithoutOnboarding, 2)
sdSUSWithoutOnboarding <- sd(subSetWithoutOnboarding$sus)
round(sdSUSWithoutOnboarding, 2)

meanSUSWithOnboarding <- mean(subSetWithOnboarding$sus)
round(meanSUSWithOnboarding, 2)
sdSUSWithOnboarding <- sd(subSetWithOnboarding$sus)
round(sdSUSWithOnboarding, 2)

t.test(x = dataSet$onboarding_elements, y = dataSet$sus)


