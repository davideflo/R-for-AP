############################## scheduler ####################################

library(taskscheduleR)

## run script every day at 14:40. Change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
taskscheduler_create(taskname = "myschedulertest", rscript = "C:/Users/utente/Documents/mytest.R", 
                     schedule = "DAILY", starttime = "14:40", startdate = format(Sys.Date(), "%d/%m/%Y"))


## get a data.frame of all tasks
tasks <- taskscheduler_ls()
str(tasks)

## delete the tasks
taskscheduler_delete(taskname = "myschedulertest")