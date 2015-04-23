library(RTutor)
library(foreign)
library(dplyr)
library(ggplot2)
library(dummies)
library(googleVis)
library(regtools)
library(reshape2)
library(gridExtra)
library(lattice)
library(RColorBrewer)
library(pROC)
library(survey)
library(aod)
library(car)

setwd("./work")
app =  show.ps(user.name = "Guest",
			   ps.name = "creditboomsgonebust",
	           make.web.app = TRUE, save.nothing=FALSE,
	           offline=FALSE,sample.solution = FALSE)

app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
