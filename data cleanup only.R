library(dbscan)
library(ClusterR)
library(readxl)
library(tidyverse)
#library(clValid)
library(clusterCrit)
#library(scatterplot3d)
library(magrittr)
#library(factoextra)
#library(NbClust)
library(WriteXLS)

setwd("C:/Users/Rolandgrad/Documents/MSA Programs/MSA 8350 Analytics Practicum/Orioles Project/6.19.19 files")

data.dirty <- read_excel("pitches_3_org_2019.xlsx")
head(data.dirty)
dim(data.dirty)

# View list
dirty.list <- sort(unique(data.dirty$Pitcher))
dirty.list

# Remove duplicate long names
data.dirty$Pitcher <- gsub("Gruener, Nickolas", "Gruener, Nick", data.dirty$Pitcher)
data.dirty$Pitcher <- gsub("Lowther, Zacary", "Lowther, Zac", data.dirty$Pitcher)
data.dirty$Pitcher <- gsub("Naughton, Timothy", "Naughton, Tim", data.dirty$Pitcher)
data.dirty$Pitcher <- gsub("Vespi, Nicholas", "Vespi, Nick", data.dirty$Pitcher)
data.dirty$Pitcher <- gsub("Wright, Mike", "Wright Jr., Mike", data.dirty$Pitcher)

#Verify Pitcher List
sort(unique(data.dirty$Pitcher))

# Remove pit_throw "L" from Cashner, Andrew
unique(data.dirty$pit_throw[data.dirty$Pitcher == "Cashner, Andrew"])
length(data.dirty$pit_throw[data.dirty$Pitcher == "Cashner, Andrew"])
data.dirty$pit_throw[data.dirty$Pitcher == "Cashner, Andrew"] <- "R"
# Verify Changes and Count 
unique(data.dirty$pit_throw[data.dirty$Pitcher == "Cashner, Andrew"])
length(data.dirty$pit_throw[data.dirty$Pitcher == "Cashner, Andrew"]) 

# Remove pit_throw "R" from Gonzalez, Luis
unique(data.dirty$pit_throw[data.dirty$Pitcher == "Gonzalez, Luis"])
length(data.dirty$pit_throw[data.dirty$Pitcher == "Gonzalez, Luis"])
data.dirty$pit_throw[data.dirty$Pitcher == "Gonzalez, Luis"] <- "L"
# Verify Changes and Count 
unique(data.dirty$pit_throw[data.dirty$Pitcher == "Gonzalez, Luis"])
length(data.dirty$pit_throw[data.dirty$Pitcher == "Gonzalez, Luis"]) 

data.org <- data.frame(data.dirty)
class(data.org)
head <- head(data.org)

setwd("C:/Users/Rolandgrad/Documents/MSA Programs/MSA 8350 Analytics Practicum/Orioles Project/Phase 2 clean/NFB2/outputs")
WriteXLS(data.org, ExcelFileName = "Cleaned.Clustering.Data.full.xlsx")
