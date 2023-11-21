# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

########################################
# load packages
########################################
# library(shiny)
# library(shinyWidgets)
# library(shinyjs)


# library(shinycssloaders) %>% withSpinner(color="#0dc5c1")

library(tidyverse)


# library(rjson) # write JSON files


# library(igraph)

library(xlsx)
# library(sortable)

########################################
# Daten
########################################
dir()

files_xlsx <- list.files(path = getwd(), pattern = "*.xlsx", full.names = FALSE)


list_NetworkParams <- list(); h=1
for(f in files_xlsx){
  tmp_name <- str_remove_all(string = f, pattern = "CAMindicator_|\\.xlsx")
  tmp_dat <-  xlsx::read.xlsx2(file = f, sheetIndex = 1)


  list_NetworkParams[[tmp_name]] <- tmp_dat

  tmp_dat$group <- tmp_name
  if(h==1){
    allstudies <- tmp_dat
  }else{
    allstudies <- rbind(allstudies, tmp_dat)
  }

  h=h+1
}

allstudies <- as.data.frame(allstudies)
allstudies$X. <- NULL
allstudies$CAM_ID <- NULL
allstudies$participantCAM <- NULL


allstudies[, 1:26] <- as.data.frame(sapply(allstudies[, 1:26], as.numeric))

table(allstudies$group)

colnames(allstudies)
psych::cor.plot(r = cor(allstudies[, 1:26], use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)


psych::cor.plot(r = cor(allstudies[allstudies$group == "Feedback2022", 1:26], use = "pairwise.complete.obs"), upper = FALSE, xlas = 2)
psych::cor.plot(r = cor(allstudies[allstudies$group == "FTI2021_t1", 1:26], use = "pairwise.complete.obs"), upper = FALSE, xlas = 2)
psych::cor.plot(r = cor(allstudies[allstudies$group == "FTI2021_t2", 1:26], use = "pairwise.complete.obs"), upper = FALSE, xlas = 2)


a <- allstudies$CAM_ID
b <- a[allstudies$group == "SAI2022"]
###
tmp <- allstudies[allstudies$group == "SAI2022",]
tmp <- data.frame(density = tmp$density_macro, ID = b)
tmp[order(tmp$density),]
###

########################################
# meta-analysis of means can be conducted using the metamean function
########################################

# allstudies$num_nodes_macro

out_metanalysis <- allstudies %>%
  group_by(group) %>%
  summarise(n = n(),
            mean = mean(x = num_nodes_macro, na.rm = TRUE),
            sd = sd(x = num_nodes_macro, na.rm = TRUE))
colnames(out_metanalysis)[1] <- "CAMstudy"
out_metanalysis$forcedDesign <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)



m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = CAMstudy,
                   data = out_metanalysis,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Mean Number of Concepts")
summary(m.mean)




forest.meta(m.mean,
            sortvar = TE,
            prediction = TRUE,
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))







update.meta(m.mean,
            subgroup = forcedDesign,
            tau.common = FALSE)





out_metanalysis_noForced <- out_metanalysis[!out_metanalysis$forcedDesign, ]
m.mean_noForced <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = CAMstudy,
                   data = out_metanalysis_noForced,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Mean Number of Concepts")
summary(m.mean)




forest.meta(m.mean_noForced,
            sortvar = TE,
            prediction = TRUE,
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))







########################################
# pooled using the metacor function, which uses the generic inverse variance pooling method
########################################
out_metanalysis <- allstudies %>%
  group_by(group) %>%
  summarise(n = n())
colnames(out_metanalysis)[1] <- "CAMstudy"
out_metanalysis$cor <- NA



whichParams <- c("density_macro", "num_nodes_macro")

h=1
for(c in out_metanalysis$CAMstudy){
  tmp <- allstudies[allstudies$group == c,whichParams]
  # print(c)
  # print(cor(tmp)[1,2])
  out_metanalysis$cor[h] <- cor(tmp)[1,2]
    h=h+1
}



m.cor <- metacor(cor = cor,
                 n = n,
                 studlab = CAMstudy,
                 data = out_metanalysis,
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




hist(allstudies$density_macro[allstudies$group == "LW2020"])
hist(allstudies$density_macro[allstudies$group == "FTI2021_t1"])
hist(allstudies$density_macro[allstudies$group == "SAI2022"])





##############################################################################
out_metanalysis <- allstudies %>%
  group_by(group) %>%
  summarise(n = n())
colnames(out_metanalysis)[1] <- "CAMstudy"
out_metanalysis$cor <- NA



whichParams <- c("meanDistance_undirected_macro", "num_nodes_macro")

h=1
for(c in out_metanalysis$CAMstudy){
  tmp <- allstudies[allstudies$group == c,whichParams]
  # print(c)
  # print(cor(tmp)[1,2])
  out_metanalysis$cor[h] <- cor(tmp)[1,2]
  h=h+1
}



m.cor <- metacor(cor = cor,
                 n = n,
                 studlab = CAMstudy,
                 data = out_metanalysis,
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





plot(allstudies[allstudies$group == "SAI2022", whichParams])
plot(allstudies[allstudies$group == "LW2020", whichParams])
