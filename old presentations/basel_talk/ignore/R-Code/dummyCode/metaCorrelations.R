# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

# library(dmetar)
library(tidyverse)
library(meta)

load("healthwellbeing.rda")

glimpse(HealthWellbeing)

m.cor <- metacor(cor = cor,
                 n = n,
                 studlab = author,
                 data = HealthWellbeing,
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Health and Wellbeing")
summary(m.cor)



forest.meta(m.cor,
            sortvar = TE,
            prediction = TRUE,
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))



update.meta(m.cor,
            subgroup = population,
            tau.common = FALSE)
