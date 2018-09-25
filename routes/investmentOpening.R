#!/usr/bin/Rscript


### Imports ###

# PROD
source('/home/saveup-pas/saveup-api.R', chdir = TRUE)

# DEV
# source('C:/Users/holden/Documents/devel/express-r/routes/saveup-api.R', chdir = TRUE)


### Get Params ###

args <- commandArgs(TRUE)
environment = args[2]
cat("[R] environment: ", environment)


### SaveUp ###

# TEST
Sys.sleep(120)

saveupConnect(environment)
pas = loginPASAdmin("testautomation@mailinator.com", "TestAutomation")
pas$trigger$investmentOpening()