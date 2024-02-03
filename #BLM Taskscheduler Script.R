# Notwendige Pakete laden----
pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("tidyverse", "taskscheduleR")
Sys.setenv(language="en")


###     Zuerst muss das entsprechende Script in dem ext_data Ordner des Taskscheduler Pakets
###     hinterlegt sein (Script: Scraping). Anschlie?end kann dieses Script als 
###     Handlungsanweisung f?r den Taskscheduler festgelegt werden


myscript <- system.file("extdata",
                        "#BLMScript.R", 
                        package = "taskscheduleR")

###     anschlie?end wird ein Task erstellt, der festlegt wann das definierte
###     Script ausgefuehrt wird. Die Funktionsargumente definieren die Tage (Daily) und
###     den Zeitpunkt der Ausfuehrung (stattime).


taskscheduler_create(taskname = "TwitterScriptBLMDaily", 
                     rscript = myscript, 
                     schedule = "DAILY",
                     starttime = "11:00")

###     Parallel dazu wird eine Artbuch Logbuch erstellt.
###     Hier werden alle Ergebnisse und/ oder Fehlermeldungen dokumentiert. 


system.file("extdata", "#BLMScript.log", package = "taskscheduleR")


###     Bei Bedarf kann der erstellte Task jeder Zeit geloescht werden.

taskscheduler_delete(taskname = "TwitterScriptBLMDaily")
