#!/usr/bin/Rscript


### Imports ###

# source('/home/saveup-pas/saveup-api.R', chdir = TRUE)
source('C:/Users/holden/Documents/devel/express-r/routes/saveup-api.R', chdir = TRUE)


### Get Params ###

args <- commandArgs(TRUE)
environment = args[2]
cat("[R] environment: ", environment)


### SaveUp ###

Sys.sleep(120)
saveupConnect(environment)
pas = loginPASAdmin("testautomation@mailinator.com", "TestAutomation")
pas$trigger$investmentOpening()