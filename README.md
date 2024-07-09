# Project2
Project Making an app in RShiny for ST 558
Purpose: This app accesses an API from the US Treasury Department. It gives details about this API, allows the user to access certain data sets from this API, and then allows the user to subset the data sets and download a CSV file with the subsetted data they want. After this, the user is able to access contingency tables, numerical summaries, and graphs from certain data sets that have been selected.

Packages Required to run App: shiny, jsonlite, DT, caret, httr, tidyverse, ggbeeswarm

Line to install these packages: install.packages(c("shiny","jsonlite", "DT", "caret", "httr", "tidyverse", "ggbeeswarm"))

shiny::runGitHub(repo = "Project2", username = "lukefreud", subdir = "ST558Project2", ref = "main")
