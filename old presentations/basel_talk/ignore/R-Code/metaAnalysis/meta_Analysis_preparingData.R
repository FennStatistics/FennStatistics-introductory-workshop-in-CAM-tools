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


library(rjson) # write JSON files


library(igraph)

library(xlsx)
# library(sortable)

########################################
# Daten
########################################
# create files
source("www/functions_CAM/create_CAMfiles.R", encoding = "utf-8")
source("www/functions_CAM/create_ValenceFiles.R", encoding = "utf-8")
# > fix Valence data
source("www/functions_CAM/fix_ValenceData.R", encoding = "utf-8")


# draw CAMs
source("www/functions_CAM/draw_CAM.R", encoding = "utf-8")

# compute network indicators
source("www/functions_CAM/compute_indicatorsCAM.R", encoding = "utf-8")


# helper functions for protocol
source("./www/functions_CAM/protocolFunctions.R", encoding = "utf-8")



################################################################################
########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Motivation of Car vs. Public Transport Use 2021")


files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../..")

# plot(CAMdrawn[["612"]])


########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}

# plot(CAMdrawn[["1561"]])


# table(CAMindicators$diameter_unweighted_undirected_macro, wrong$diameter_unweighted_undirected_macro)
# plot(CAMindicators$centr_degree_macro, wrong$centr_degree_macro)
# plot(CAMindicators$centr_betw_macro, wrong$centr_betw_macro)

### CAM indicators
# wrong <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)

CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)

write.xlsx2(x = CAMindicators, file = "CAMindicator_CarvPT2021.xlsx")




################################################################################

########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Intervetion study Leisure Walks 2020")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
    }else{
    print(files_links[i])
  }
}
setwd("../..")

# plot(CAMdrawn[["612"]])


########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                dat_connectors = CAMfiles[[2]],
                dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}



# table(CAMindicators$diameter_unweighted_undirected_macro, wrong$diameter_unweighted_undirected_macro)
# plot(CAMindicators$centr_degree_macro, wrong$centr_degree_macro)
# plot(CAMindicators$centr_betw_macro, wrong$centr_betw_macro)

### CAM indicators
# wrong <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)

CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)

write.xlsx2(x = CAMindicators, file = "CAMindicator_LW2020.xlsx")



################################################################################
########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Intervetion study Fictional Technological Implant 2021")
### tp1
setwd("tp1")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "CAMindicator_FTI2021_t1.xlsx")





########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Intervetion study Fictional Technological Implant 2021")
### tp1
setwd("tp2")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)


write.xlsx2(x = CAMindicators, file = "CAMindicator_FTI2021_t2.xlsx")




################################################################################
########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Network Approach CAMs 2021")
### tp1
setwd("canada")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "CAMindicator_NetApp2021_canada.xlsx")





########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Network Approach CAMs 2021")
### tp1
setwd("germany")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)


write.xlsx2(x = CAMindicators, file = "CAMindicator_NetApp2021_germany.xlsx")











































################################################################################
########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Feedback psychology program Freiburg 2022")


read_file("jatos_results_20220105151113.txt") %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat_CAM


raw_CAM <- list()
for(i in 1:length(dat_CAM)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat_CAM[[i]])
}
rm(i)


setwd("../..")



########################################
# pre-processing
########################################
## create CAM files
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)


## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

# for(i in 1:length(x = CAMdrawn)){
#   tmp_com <- components(graph = CAMdrawn[[i]])
#   if(tmp_com$no != 1){
#     print(names(CAMdrawn)[i])
#   }
# }



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "CAMindicator_Feedback2022.xlsx")





################################################################################
########################################
# Daten
########################################
setwd("rawData")
dir()
setwd("Stratospheric Aerosol Injection Multi Method 2022")


read_file("jatos_results_20230131145704.txt") %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat_CAM


raw_CAM <- list()
for(i in 1:length(dat_CAM)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat_CAM[[i]])
}
rm(i)


setwd("../..")



########################################
# pre-processing
########################################
## create CAM files
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)


## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)
plot(CAMdrawn[[4]])

## check number of components

# for(i in 1:length(x = CAMdrawn)){
#   tmp_com <- components(graph = CAMdrawn[[i]])
#   if(tmp_com$no != 1){
#     print(names(CAMdrawn)[i])
#   }
# }



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "CAMindicator_SAI2022.xlsx")
