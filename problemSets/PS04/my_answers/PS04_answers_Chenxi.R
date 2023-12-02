#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("ggplot2", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############## Question 1 ##############

# First, run the following commands

install.packages(car); library(car)
data(Prestige); help(Prestige)

####### (a) #######

# Create variable professional
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 
  ifelse(Prestige$type %in% c("wc", "bc"), 0, NA))
summary(Prestige$professional); table(Prestige$professional)

####### (b) #######

# Run a linear regression model, in this model:

# - prestige: the outcome variable
# - income: dependent variable
# - professional: dependent variable
# - incprof: the interaction of income and professional

q1b_reg <- lm(prestige ~ income + professional + income * professional,
                data = Prestige)
summary(q1b_reg)

# Get Latex table
stargazer(q1b_reg, no.space = TRUE)

############## Question 2 ##############

####### (a) #######

q2a_pvalue <- 1 - pt(2.625, df = 128); print(q2a_pvalue)

####### (b) #######
q2b_pvalue <- 1 - pt(3.231, df = 128); print(q2b_pvalue)
