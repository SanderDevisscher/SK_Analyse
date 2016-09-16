rm(list = ls())

library(googlesheets) 
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

#### Set standard paths => Veranderen indien andere pc/laptop

##WERK##
#setwd("C://Users/sander_devisscher/Google Drive/EU_IAS/Stierkikker/Stierkikker data-analyse") #Werk
#imagepath <- "C://Users/sander_devisscher/Google Drive/EU_IAS/Stierkikker/Stierkikker data-analyse/Afbeeldingen" #Werk

##THUIS##
imagepath <- "C://Users/Sander/Google Drive Werk/EU_IAS/Stierkikker/Stierkikker data-analyse/Afbeeldingen"
setwd("C://Users/Sander/Google Drive Werk/EU_IAS/Stierkikker/Stierkikker data-analyse") 

####Import data####
title <- gs_title(x = "Stierkikker formulieren - Natuurwerk (Reacties)", verbose = TRUE)
Token <- gs_auth()
gs_auth(token = Token)
gdrive <- gs_read(title)
gdrive2 <- gs_read(title, range = cell_cols(12:13))
offline <- read.csv2("file://Ruwe Data/Stierkikker formulieren - Natuurwerk (Reacties) - Formulierreacties _2016-08-13.csv", sep=",")

temp <- get0("gdrive", ifnotfound=offline)

if(exists("gdrive")){
  ONLINE <- data.frame()
  remove(offline)
}

temp$Location <- ifelse(!is.na(temp$`Vijver - Arendonk`),temp$`Vijver - Arendonk`,ifelse(!is.na(temp$`Vijver - Kasterlee`), temp$`Vijver - Kasterlee`,ifelse(!is.na(temp$`Vijver - Hoogstraten`), temp$`Vijver - Hoogstraten`,NA)))
table(temp$Location)

temp$Sample_Type <- temp$`Wat wil je melden`
table(temp$Sample_Type)

VasteLOC <- subset(temp, !is.na(temp$Location))
NieuweLOC <- subset(temp, is.na(temp$Location))
if(nrow(NieuweLOC)==0){
  remove(NieuweLOC)
  remove(VasteLOC)
}

table(temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "kris", "kris meeus", temp$Invuller)
temp$Recorder <- temp$Invuller
for (i in nrow(temp)){ 
  n <- temp$`Aantal werknemers`
  temp$Recorder2 <- ifelse(n==2 ,temp$Recorder <- paste(temp$Invuller, "Arbeider 1", sep=";" ), 
                           ifelse(n==3 ,temp$Recorder <- paste(temp$Invuller, "Arbeider 1", "Arbeider 2", sep=";" ), 
                                  ifelse(n==4 ,temp$Recorder <- paste(temp$Invuller, "Arbeider 1", "Arbeider 2", "Arbeider 3", sep=";" ), 
                                         temp$Invuller)))
}

table(temp$Recorder2)
temp$Recorder <- temp$Recorder2

#Opruimen

temp$`Wat wil je melden` <- NULL
temp$`Vijver - Arendonk` <- NULL
temp$`Vijver - Kasterlee` <- NULL
temp$`Vijver - Hoogstraten` <- NULL
#temp$Locatie <- NULL
temp$`Gemeente/Deelgemeente` <- NULL
temp$Invuller <- NULL
temp$Recorder2 <- NULL
temp$`Aantal werknemers` <- NULL



#Enkel Afvangsten voor grafiekjes
Afvangsten <- subset(temp, Sample_Type == "Afvangst")


Afvangsten$L00[!is.na(Afvangsten$`Fuik 1 - L00`)
               |!is.na(Afvangsten$`Fuik 2 - L00`)
               |!is.na(Afvangsten$`Fuik 3 - L00`)
               |!is.na(Afvangsten$`Fuik 4 - L00`)
               |!is.na(Afvangsten$`Fuik 5 - L00`)
               |!is.na(Afvangsten$`Fuik 6 - L00`)
               |!is.na(Afvangsten$`Fuik 7 - L00`)
               |!is.na(Afvangsten$`Fuik 8 - L00`)
               |!is.na(Afvangsten$`Fuik 9 - L00`)
               |!is.na(Afvangsten$`Fuik 10 - L00`)
               |!is.na(Afvangsten$`Fuik 11 - L00`)
               |!is.na(Afvangsten$`Fuik 12 - L00`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - L00`)
                     |!is.na(Afvangsten$`Fuik 2 - L00`)
                     |!is.na(Afvangsten$`Fuik 3 - L00`)
                     |!is.na(Afvangsten$`Fuik 4 - L00`)
                     |!is.na(Afvangsten$`Fuik 5 - L00`)
                     |!is.na(Afvangsten$`Fuik 6 - L00`)
                     |!is.na(Afvangsten$`Fuik 7 - L00`)
                     |!is.na(Afvangsten$`Fuik 8 - L00`)
                     |!is.na(Afvangsten$`Fuik 9 - L00`)
                     |!is.na(Afvangsten$`Fuik 10 - L00`)
                     |!is.na(Afvangsten$`Fuik 11 - L00`)
                     |!is.na(Afvangsten$`Fuik 12 - L00`), 
                     c('Fuik 1 - L00', 
                       'Fuik 2 - L00',
                       'Fuik 3 - L00',
                       'Fuik 4 - L00',
                       'Fuik 5 - L00',
                       'Fuik 6 - L00',
                       'Fuik 7 - L00',
                       'Fuik 8 - L00',
                       'Fuik 9 - L00',
                       'Fuik 10 - L00',
                       'Fuik 11 - L00',
                       'Fuik 12 - L00')], 
          na.rm = T)

Afvangsten$L0[!is.na(Afvangsten$`Fuik 1 - L0`)
              |!is.na(Afvangsten$`Fuik 2 - L0`)
              |!is.na(Afvangsten$`Fuik 3 - L0`)
              |!is.na(Afvangsten$`Fuik 4 - L0`)
              |!is.na(Afvangsten$`Fuik 5 - L0`)
              |!is.na(Afvangsten$`Fuik 6 - L0`)
              |!is.na(Afvangsten$`Fuik 7 - L0`)
              |!is.na(Afvangsten$`Fuik 8 - L0`)
              |!is.na(Afvangsten$`Fuik 9 - L0`)
              |!is.na(Afvangsten$`Fuik 10 - L0`)
              |!is.na(Afvangsten$`Fuik 11 - L0`)
              |!is.na(Afvangsten$`Fuik 12 - L0`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - L0`)
                     |!is.na(Afvangsten$`Fuik 2 - L0`)
                     |!is.na(Afvangsten$`Fuik 3 - L0`)
                     |!is.na(Afvangsten$`Fuik 4 - L0`)
                     |!is.na(Afvangsten$`Fuik 5 - L0`)
                     |!is.na(Afvangsten$`Fuik 6 - L0`)
                     |!is.na(Afvangsten$`Fuik 7 - L0`)
                     |!is.na(Afvangsten$`Fuik 8 - L0`)
                     |!is.na(Afvangsten$`Fuik 9 - L0`)
                     |!is.na(Afvangsten$`Fuik 10 - L0`)
                     |!is.na(Afvangsten$`Fuik 11 - L0`)
                     |!is.na(Afvangsten$`Fuik 12 - L0`), 
                     c('Fuik 1 - L0', 
                       'Fuik 2 - L0',
                       'Fuik 3 - L0',
                       'Fuik 4 - L0',
                       'Fuik 5 - L0',
                       'Fuik 6 - L0',
                       'Fuik 7 - L0',
                       'Fuik 8 - L0',
                       'Fuik 9 - L0',
                       'Fuik 10 - L0',
                       'Fuik 11 - L0',
                       'Fuik 12 - L0')], 
          na.rm = T)

Afvangsten$L1[!is.na(Afvangsten$`Fuik 1 - L1`)
              |!is.na(Afvangsten$`Fuik 2 - L1`)
              |!is.na(Afvangsten$`Fuik 3 - L1`)
              |!is.na(Afvangsten$`Fuik 4 - L1`)
              |!is.na(Afvangsten$`Fuik 5 - L1`)
              |!is.na(Afvangsten$`Fuik 6 - L1`)
              |!is.na(Afvangsten$`Fuik 7 - L1`)
              |!is.na(Afvangsten$`Fuik 8 - L1`)
              |!is.na(Afvangsten$`Fuik 9 - L1`)
              |!is.na(Afvangsten$`Fuik 10 - L1`)
              |!is.na(Afvangsten$`Fuik 11 - L1`)
              |!is.na(Afvangsten$`Fuik 12 - L1`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - L1`)
                     |!is.na(Afvangsten$`Fuik 2 - L1`)
                     |!is.na(Afvangsten$`Fuik 3 - L1`)
                     |!is.na(Afvangsten$`Fuik 4 - L1`)
                     |!is.na(Afvangsten$`Fuik 5 - L1`)
                     |!is.na(Afvangsten$`Fuik 6 - L1`)
                     |!is.na(Afvangsten$`Fuik 7 - L1`)
                     |!is.na(Afvangsten$`Fuik 8 - L1`)
                     |!is.na(Afvangsten$`Fuik 9 - L1`)
                     |!is.na(Afvangsten$`Fuik 10 - L1`)
                     |!is.na(Afvangsten$`Fuik 11 - L1`)
                     |!is.na(Afvangsten$`Fuik 12 - L1`), 
                     c('Fuik 1 - L1', 
                       'Fuik 2 - L1',
                       'Fuik 3 - L1',
                       'Fuik 4 - L1',
                       'Fuik 5 - L1',
                       'Fuik 6 - L1',
                       'Fuik 7 - L1',
                       'Fuik 8 - L1',
                       'Fuik 9 - L1',
                       'Fuik 10 - L1',
                       'Fuik 11 - L1',
                       'Fuik 12 - L1')], 
          na.rm = T)

Afvangsten$L2[!is.na(Afvangsten$`Fuik 1 - L2`)
              |!is.na(Afvangsten$`Fuik 2 - L2`)
              |!is.na(Afvangsten$`Fuik 3 - L2`)
              |!is.na(Afvangsten$`Fuik 4 - L2`)
              |!is.na(Afvangsten$`Fuik 5 - L2`)
              |!is.na(Afvangsten$`Fuik 6 - L2`)
              |!is.na(Afvangsten$`Fuik 7 - L2`)
              |!is.na(Afvangsten$`Fuik 8 - L2`)
              |!is.na(Afvangsten$`Fuik 9 - L2`)
              |!is.na(Afvangsten$`Fuik 10 - L2`)
              |!is.na(Afvangsten$`Fuik 11 - L2`)
              |!is.na(Afvangsten$`Fuik 12 - L2`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - L2`)
                     |!is.na(Afvangsten$`Fuik 2 - L2`)
                     |!is.na(Afvangsten$`Fuik 3 - L2`)
                     |!is.na(Afvangsten$`Fuik 4 - L2`)
                     |!is.na(Afvangsten$`Fuik 5 - L2`)
                     |!is.na(Afvangsten$`Fuik 6 - L2`)
                     |!is.na(Afvangsten$`Fuik 7 - L2`)
                     |!is.na(Afvangsten$`Fuik 8 - L2`)
                     |!is.na(Afvangsten$`Fuik 9 - L2`)
                     |!is.na(Afvangsten$`Fuik 10 - L2`)
                     |!is.na(Afvangsten$`Fuik 11 - L2`)
                     |!is.na(Afvangsten$`Fuik 12 - L2`), 
                     c('Fuik 1 - L2', 
                       'Fuik 2 - L2',
                       'Fuik 3 - L2',
                       'Fuik 4 - L2',
                       'Fuik 5 - L2',
                       'Fuik 6 - L2',
                       'Fuik 7 - L2',
                       'Fuik 8 - L2',
                       'Fuik 9 - L2',
                       'Fuik 10 - L2',
                       'Fuik 11 - L2',
                       'Fuik 12 - L2')], 
          na.rm = T)

Afvangsten$M1[!is.na(Afvangsten$`Fuik 1 - M1`)
              |!is.na(Afvangsten$`Fuik 2 - M1`)
              |!is.na(Afvangsten$`Fuik 3 - M1`)
              |!is.na(Afvangsten$`Fuik 4 - M1`)
              |!is.na(Afvangsten$`Fuik 5 - M1`)
              |!is.na(Afvangsten$`Fuik 6 - M1`)
              |!is.na(Afvangsten$`Fuik 7 - M1`)
              |!is.na(Afvangsten$`Fuik 8 - M1`)
              |!is.na(Afvangsten$`Fuik 9 - M1`)
              |!is.na(Afvangsten$`Fuik 10 - M1`)
              |!is.na(Afvangsten$`Fuik 11 - M1`)
              |!is.na(Afvangsten$`Fuik 12 - M1`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - M1`)
                     |!is.na(Afvangsten$`Fuik 2 - M1`)
                     |!is.na(Afvangsten$`Fuik 3 - M1`)
                     |!is.na(Afvangsten$`Fuik 4 - M1`)
                     |!is.na(Afvangsten$`Fuik 5 - M1`)
                     |!is.na(Afvangsten$`Fuik 6 - M1`)
                     |!is.na(Afvangsten$`Fuik 7 - M1`)
                     |!is.na(Afvangsten$`Fuik 8 - M1`)
                     |!is.na(Afvangsten$`Fuik 9 - M1`)
                     |!is.na(Afvangsten$`Fuik 10 - M1`)
                     |!is.na(Afvangsten$`Fuik 11 - M1`)
                     |!is.na(Afvangsten$`Fuik 12 - M1`), 
                     c('Fuik 1 - M1', 
                       'Fuik 2 - M1',
                       'Fuik 3 - M1',
                       'Fuik 4 - M1',
                       'Fuik 5 - M1',
                       'Fuik 6 - M1',
                       'Fuik 7 - M1',
                       'Fuik 8 - M1',
                       'Fuik 9 - M1',
                       'Fuik 10 - M1',
                       'Fuik 11 - M1',
                       'Fuik 12 - M1')], 
          na.rm = T)

Afvangsten$M2[!is.na(Afvangsten$`Fuik 1 - M2`)
              |!is.na(Afvangsten$`Fuik 2 - M2`)
              |!is.na(Afvangsten$`Fuik 3 - M2`)
              |!is.na(Afvangsten$`Fuik 4 - M2`)
              |!is.na(Afvangsten$`Fuik 5 - M2`)
              |!is.na(Afvangsten$`Fuik 6 - M2`)
              |!is.na(Afvangsten$`Fuik 7 - M2`)
              |!is.na(Afvangsten$`Fuik 8 - M2`)
              |!is.na(Afvangsten$`Fuik 9 - M2`)
              |!is.na(Afvangsten$`Fuik 10 - M2`)
              |!is.na(Afvangsten$`Fuik 11 - M2`)
              |!is.na(Afvangsten$`Fuik 12 - M2`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - M2`)
                     |!is.na(Afvangsten$`Fuik 2 - M2`)
                     |!is.na(Afvangsten$`Fuik 3 - M2`)
                     |!is.na(Afvangsten$`Fuik 4 - M2`)
                     |!is.na(Afvangsten$`Fuik 5 - M2`)
                     |!is.na(Afvangsten$`Fuik 6 - M2`)
                     |!is.na(Afvangsten$`Fuik 7 - M2`)
                     |!is.na(Afvangsten$`Fuik 8 - M2`)
                     |!is.na(Afvangsten$`Fuik 9 - M2`)
                     |!is.na(Afvangsten$`Fuik 10 - M2`)
                     |!is.na(Afvangsten$`Fuik 11 - M2`)
                     |!is.na(Afvangsten$`Fuik 12 - M2`), 
                     c('Fuik 1 - M2', 
                       'Fuik 2 - M2',
                       'Fuik 3 - M2',
                       'Fuik 4 - M2',
                       'Fuik 5 - M2',
                       'Fuik 6 - M2',
                       'Fuik 7 - M2',
                       'Fuik 8 - M2',
                       'Fuik 9 - M2',
                       'Fuik 10 - M2',
                       'Fuik 11 - M2',
                       'Fuik 12 - M2')], 
          na.rm = T)

Afvangsten$AM[!is.na(Afvangsten$`Fuik 1 - AM`)
              |!is.na(Afvangsten$`Fuik 2 - AM`)
              |!is.na(Afvangsten$`Fuik 3 - AM`)
              |!is.na(Afvangsten$`Fuik 4 - AM`)
              |!is.na(Afvangsten$`Fuik 5 - AM`)
              |!is.na(Afvangsten$`Fuik 6 - AM`)
              |!is.na(Afvangsten$`Fuik 7 - AM`)
              |!is.na(Afvangsten$`Fuik 8 - AM`)
              |!is.na(Afvangsten$`Fuik 9 - AM`)
              |!is.na(Afvangsten$`Fuik 10 - AM`)
              |!is.na(Afvangsten$`Fuik 11 - AM`)
              |!is.na(Afvangsten$`Fuik 12 - AM`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - AM`)
                     |!is.na(Afvangsten$`Fuik 2 - AM`)
                     |!is.na(Afvangsten$`Fuik 3 - AM`)
                     |!is.na(Afvangsten$`Fuik 4 - AM`)
                     |!is.na(Afvangsten$`Fuik 5 - AM`)
                     |!is.na(Afvangsten$`Fuik 6 - AM`)
                     |!is.na(Afvangsten$`Fuik 7 - AM`)
                     |!is.na(Afvangsten$`Fuik 8 - AM`)
                     |!is.na(Afvangsten$`Fuik 9 - AM`)
                     |!is.na(Afvangsten$`Fuik 10 - AM`)
                     |!is.na(Afvangsten$`Fuik 11 - AM`)
                     |!is.na(Afvangsten$`Fuik 12 - AM`), 
                     c('Fuik 1 - AM', 
                       'Fuik 2 - AM',
                       'Fuik 3 - AM',
                       'Fuik 4 - AM',
                       'Fuik 5 - AM',
                       'Fuik 6 - AM',
                       'Fuik 7 - AM',
                       'Fuik 8 - AM',
                       'Fuik 9 - AM',
                       'Fuik 10 - AM',
                       'Fuik 11 - AM',
                       'Fuik 12 - AM')], 
          na.rm = T)

Afvangsten$AV[!is.na(Afvangsten$`Fuik 1 - AV`)
              |!is.na(Afvangsten$`Fuik 2 - AV`)
              |!is.na(Afvangsten$`Fuik 3 - AV`)
              |!is.na(Afvangsten$`Fuik 4 - AV`)
              |!is.na(Afvangsten$`Fuik 5 - AV`)
              |!is.na(Afvangsten$`Fuik 6 - AV`)
              |!is.na(Afvangsten$`Fuik 7 - AV`)
              |!is.na(Afvangsten$`Fuik 8 - AV`)
              |!is.na(Afvangsten$`Fuik 9 - AV`)
              |!is.na(Afvangsten$`Fuik 10 - AV`)
              |!is.na(Afvangsten$`Fuik 11 - AV`)
              |!is.na(Afvangsten$`Fuik 12 - AV`)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$`Fuik 1 - AV`)
                     |!is.na(Afvangsten$`Fuik 2 - AV`)
                     |!is.na(Afvangsten$`Fuik 3 - AV`)
                     |!is.na(Afvangsten$`Fuik 4 - AV`)
                     |!is.na(Afvangsten$`Fuik 5 - AV`)
                     |!is.na(Afvangsten$`Fuik 6 - AV`)
                     |!is.na(Afvangsten$`Fuik 7 - AV`)
                     |!is.na(Afvangsten$`Fuik 8 - AV`)
                     |!is.na(Afvangsten$`Fuik 9 - AV`)
                     |!is.na(Afvangsten$`Fuik 10 - AV`)
                     |!is.na(Afvangsten$`Fuik 11 - AV`)
                     |!is.na(Afvangsten$`Fuik 12 - AV`), 
                     c('Fuik 1 - AV', 
                       'Fuik 2 - AV',
                       'Fuik 3 - AV',
                       'Fuik 4 - AV',
                       'Fuik 5 - AV',
                       'Fuik 6 - AV',
                       'Fuik 7 - AV',
                       'Fuik 8 - AV',
                       'Fuik 9 - AV',
                       'Fuik 10 - AV',
                       'Fuik 11 - AV',
                       'Fuik 12 - AV')], 
          na.rm = T)

print(Afvangsten$L00)
print(Afvangsten$L0)
print(Afvangsten$L1)
print(Afvangsten$L2)
print(Afvangsten$M1)
print(Afvangsten$M2)
print(Afvangsten$AM)
print(Afvangsten$AV)

if(is.null(Afvangsten$L00)){
  Afvangsten$L00 <- NA
}
if(is.null(Afvangsten$L0)){
  Afvangsten$L0 <- NA
}
if(is.null(Afvangsten$L1)){
  Afvangsten$L1 <- NA
}
if(is.null(Afvangsten$L2)){
  Afvangsten$L2 <- NA
}
if(is.null(Afvangsten$M1)){
  Afvangsten$M1 <- NA
}
if(is.null(Afvangsten$M2)){
  Afvangsten$M2 <- NA
}
if(is.null(Afvangsten$AM)){
  Afvangsten$AM <- NA
}
if(is.null(Afvangsten$AV)){
  Afvangsten$AV <- NA
}

Afvangsten$Totaal[!is.na(Afvangsten$L00)
                  |!is.na(Afvangsten$L0)
                  |!is.na(Afvangsten$L1)
                  |!is.na(Afvangsten$L2)
                  |!is.na(Afvangsten$M1)
                  |!is.na(Afvangsten$M2)
                  |!is.na(Afvangsten$AM)
                  |!is.na(Afvangsten$AV)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$L00)
                     |!is.na(Afvangsten$L0)
                     |!is.na(Afvangsten$L1)
                     |!is.na(Afvangsten$L2)
                     |!is.na(Afvangsten$M1)
                     |!is.na(Afvangsten$M2)
                     |!is.na(Afvangsten$AM)
                     |!is.na(Afvangsten$AV),
                     c('L00',
                       'L0',
                       'L1',
                       'L2',
                       'M1',
                       'M2',
                       'AM',
                       'AV')]
          ,na.rm=T)

Afvangsten$Totaal_Larven.All[!is.na(Afvangsten$L00)
                             |!is.na(Afvangsten$L0)
                             |!is.na(Afvangsten$L1)
                             |!is.na(Afvangsten$L2)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$L00)
                     |!is.na(Afvangsten$L0)
                     |!is.na(Afvangsten$L1)
                     |!is.na(Afvangsten$L2),
                     c('L00',
                       'L0',
                       'L1',
                       'L2')]
          ,na.rm=T)

Afvangsten$Totaal_Larven.CPUE[!is.na(Afvangsten$L0)
                              |!is.na(Afvangsten$L1)
                              |!is.na(Afvangsten$L2)] <- 
  rowSums(Afvangsten[!is.na(Afvangsten$L0)
                     |!is.na(Afvangsten$L1)
                     |!is.na(Afvangsten$L2),
                     c('L0',
                       'L1',
                       'L2')]
          ,na.rm=T)

Afvangsten$Totaal_Larven.CPUE <- ifelse(is.na(Afvangsten$Totaal_Larven.CPUE),0, Afvangsten$Totaal_Larven.CPUE)

Afvangsten$CPUE <- Afvangsten$Totaal_Larven.CPUE/Afvangsten$`Aantal fuiken (Totaal)`

print(Afvangsten$Totaal)
print(Afvangsten$Totaal_Larven.All)
print(Afvangsten$Totaal_Larven.CPUE)
print(Afvangsten$CPUE)


#Selecteer brondata voor grafieken
GRA_Brondata <- Afvangsten[c("Datum", "Location", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV", "Totaal", "Totaal_Larven.All", "Totaal_Larven.CPUE","Aantal fuiken (Totaal)","CPUE")]

Locations <- unique(GRA_Brondata$Location)


GRA_Brondata$Datum <- as.Date(GRA_Brondata$Datum,'%d-%m-%Y')
GRA_Brondata$Dag <- format(GRA_Brondata$Datum, format='%d')
GRA_Brondata$Maand <- format(GRA_Brondata$Datum, format='%m')
GRA_Brondata$Jaar <- format(GRA_Brondata$Datum, format='%Y')
GRA_Brondata$Jaar <- as.numeric(GRA_Brondata$Jaar)
GRA_Brondata$Maand <- as.numeric(GRA_Brondata$Maand)
GRA_Brondata$Dag <- as.numeric(GRA_Brondata$Dag)
GRA_Brondata <- GRA_Brondata[with(GRA_Brondata, order(Jaar, Maand, Dag)),]
#GRA_Brondata$Datum <- format(GRA_Brondata$Datum, format= '%d-%m-%Y')
GRA_Brondata$Datum <- as.factor(GRA_Brondata$Datum)
#GRA_Brondata$Datum <- sort(GRA_Brondata$Datum, decreasing = F)
Jaren <- unique(GRA_Brondata$Jaar)

#CPUE per dag
for(i in Locations){
  
  temp2 <- subset(GRA_Brondata, Location == i )
  fNaam <- paste(i,"CPUE", sep="_")
  fNaam <- paste(fNaam, ".jpeg", sep="")
  plot <- ggplot(temp2, aes(x=Datum, y=CPUE)) + 
    geom_bar(stat="identity", aes(fill="red"))
  plot <- plot+ggtitle(i)
  plot <- plot + expand_limits(x = 0, y = 0)
  plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
  plot <- plot + theme(legend.position="none")
  plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
  print(plot)
  ggsave(filename = fNaam, path = imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
}

#absoluut per dag
for(i in Locations){
  temp2 <- subset(GRA_Brondata, Location == i )
  fNaam <- paste(i ,"TOT", sep="_")
  #fNaam <- paste("file://Afbeeldingen/", fNaam, sep="")
  fNaam <- paste(fNaam, ".jpeg", sep = "")
  plot <- ggplot(temp2, aes(x=Datum, y=Totaal)) + geom_bar(stat="identity", aes(colour="dark grey"))
  plot <- plot + ggtitle(i)
  plot <- plot + expand_limits(x = 0, y = 0)
  plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
  plot <- plot + theme(legend.position="none")
  plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
  print(plot)
  ggsave(fNaam, path= imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
}

# tabel duur




# Bereken GSL
GSL <- data.frame(X = Locations)
remove(temp6)
temp6 <- data.frame()

for (a in Jaren){
  temp8 <- subset(GRA_Brondata, Jaar == a)
  for(i in Locations){
    
    temp2 <- subset(temp8, Location == i )
    number <- count(temp2)
    temp7 <- head(temp2, n=1)
    temp4 <- tail(temp2, n=3)
    temp5 <- data.frame(x=1)
    temp5$Location <- i
    temp5$StartCPUE <- temp7$CPUE
    temp5$Uitgevoerd <- number$n
    temp5$MeanCPUE <- mean(temp4$CPUE)
    if(is.na(temp5$MeanCPUE)){
      print(i)
      print(temp4)
      break}
    temp6 <- rbind(temp6, temp5)
    
    
  }
  
  temp6$x <- NULL
  GSL <- temp6
  remove(temp2)
  remove(temp4)
  remove(temp5)
  remove(temp6)
  remove(temp7)
  remove(temp8)
  remove(number)
  
  GSL$StartGSL <- GSL$StartCPUE*26
  GSL$MinVangst_Start <- ifelse(GSL$StartGSL<500, 4, 
                                ifelse(GSL$StartGSL >=500 & GSL$StartGSL < 1000 , 6, 
                                       ifelse(GSL$StartGSL >= 1000 & GSL$StartGSL < 5000, 8, 
                                              ifelse(GSL$StartGSL >= 5000, (13/5000)*GSL$StartGSL, NA))))
  GSL$MaxVangst_Start <- ifelse(GSL$StartGSL<500, 8, 
                                ifelse(GSL$StartGSL >=500 & GSL$StartGSL < 1000 , 13, 
                                       ifelse(GSL$StartGSL >= 1000 & GSL$StartGSL < 5000, 15, 
                                              ifelse(GSL$StartGSL >= 5000, (20/5000)*GSL$StartGSL, NA))))
  GSL$Resterend_Min <- GSL$MinVangst_Start - GSL$Uitgevoerd
  GSL$Resterend_Max <- GSL$MaxVangst_Start - GSL$Uitgevoerd
  
  GSL$HuidigGSL <- GSL$MeanCPUE*26
  GSL$MinVangst_Huidig <- ifelse(GSL$HuidigGSL<500, 4, 
                                 ifelse(GSL$HuidigGSL >=500 & GSL$HuidigGSL < 1000 , 6, 
                                        ifelse(GSL$HuidigGSL >= 1000 & GSL$HuidigGSL < 5000, 8, 
                                               ifelse(GSL$HuidigGSL >= 5000, (13/5000)*GSL$HuidigGSL, NA))))
  GSL$MaxVangst_Huidig <- ifelse(GSL$HuidigGSL<500, 8, 
                                 ifelse(GSL$HuidigGSL >=500 & GSL$HuidigGSL < 1000 , 13, 
                                        ifelse(GSL$HuidigGSL >= 1000 & GSL$HuidigGSL < 5000, 15, 
                                               ifelse(GSL$HuidigGSL >= 5000, (20/5000)*GSL$HuidigGSL, NA))))
  
  GSL$MinDoelBereikt <- ifelse(GSL$HuidigGSL < 100, "Ja", "Nee")
  GSL$MaxDoelBereikt <- ifelse(GSL$HuidigGSL < 10, "Ja", "Nee")
  
  GSL$Location <- sort(GSL$Location)
  
  GSL <- GSL[,c(1,2,5,6,7,3,8,9,4,10,13,14,11,12)]
  
  
  #GSLfNaama <- paste("Geschat startaantal larven",a, sep="_")
  GSLfNaam <- paste("Geschat startaantal larven",a, sep="_")
  GSLfNaama <- paste("file://Afbeeldingen/", GSLfNaam, sep="" )
  GSLfNaama <- paste(GSLfNaama, ".csv", sep="")
  
  #test <- gs_new(title = "Geschat startaantal larven", ws_title = "GSL", input = GSL, trim = TRUE)
  write.csv(GSL, GSLfNaama)
}

####Klaarzetten voor recorder####

Recorder_Ruw <- temp

Locations_Recorder <- unique(Recorder_Ruw$Location)

temp2 <- data.frame()
o <- 0

#Afvangsten
temp3A <- subset(Recorder_Ruw, Sample_Type == "Afvangst" )
for(x in Locations_Recorder){
  temp3 <- subset(temp3A, Location == x)
  temp3$Sample_Type <- "Schietfuik"
  Datums_Recorder <- unique(temp3$Datum)
  print(temp3$Location)
  for(y in Datums_Recorder){
    temp4 <- subset(temp3, Datum == y)
    Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type) 
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    iter <- ifelse(iter == 0, 1, iter)
    print(iter)
    for(p in 1:iter){
      o <- o + 1
      FNR <- paste("Fuik", o, sep= " ")
      temp5$Locationname <- FNR
      FNRL00 <- paste(FNR, "L00", sep= " - ")
      FNRL0 <- paste(FNR, "L0", sep= " - ")
      FNRL1 <- paste(FNR, "L1", sep= " - ")
      FNRL2 <- paste(FNR, "L2", sep= " - ")
      FNRM1 <- paste(FNR, "M1", sep= " - ")
      FNRM2 <- paste(FNR, "M2", sep= " - ")
      #FNRAM <- paste(FNR, "AM", sep= " - ") => voorlopig geen AM gevangen geeft error :-(
      FNRAV <- paste(FNR, "AV", sep= " - ")
      temp5$L00 <- ifelse(is.na(temp5[FNRL00]), 0, temp5[FNRL00])
      temp5$L0 <- ifelse(is.na(temp5[FNRL0]), 0, temp5[FNRL0])
      temp5$L1 <- ifelse(is.na(temp5[FNRL1]), 0, temp5[FNRL1])
      temp5$L2 <- ifelse(is.na(temp5[FNRL2]), 0, temp5[FNRL2])
      temp5$M1 <- ifelse(is.na(temp5[FNRM1]), 0, temp5[FNRM1])
      temp5$M2 <- ifelse(is.na(temp5[FNRM2]), 0, temp5[FNRM2])
      #temp5$AM <- ifelse(is.na(temp5[FNRAM]), 0, temp5[FNRAM]) => voorlopig geen AM gevangen geeft error :-(
      temp5$AM <- 0
      temp5$AV <- ifelse(is.na(temp5[FNRAV]), 0, temp5[FNRAV])
      temp2 <- rbind(temp2,temp5)
    }
    o <- 0
  }
}

temp2 <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV")]

temp6 <- temp2
remove(temp2)
remove(temp5)
remove(temp3)
remove(temp4)

Recorder_Afvangst <- data.frame()
Recorder_Afvangst <- temp6

remove(temp6)



#Vergelijken met vorig bestand
Recorder_Afvangst_Vorig <- read.csv2("file://Ruwe Data/Recorder_Afvangst_2016-08-29.csv")
Recorder_Afvangst_Vorig$X <- NULL
RAVnrow <- nrow(Recorder_Afvangst_Vorig)
RAnrow <- nrow(Recorder_Afvangst)
if(RAVnrow != RAnrow){
  Recorder_Afvangst_Nieuw <- merge(Recorder_Afvangst_Vorig, Recorder_Afvangst, all.x=F)
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_Afvangst", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_Afvangst_Nieuw, file = RAFN)    
}

if(!file.exists("file://Ruwe Data/Recorder_Afvangst_2016-08-29.csv")){
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_Afvangst", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_Afvangst, file = RAFN) 
}
#Opruimen
remove(tempL00) 
remove(tempL0)
remove(tempL1)
remove(tempL2)
remove(tempM1)
remove(tempM2)
remove(tempAM)
remove(tempAV)

#Afvangsten SFB
unique(Recorder_Ruw$Sample_Type)
temp3A <- subset(Recorder_Ruw, Sample_Type == "Afvangst SFB")
Locations_Recorder2 <- unique(temp3A$Location)
temp2 <- data.frame()
for(x in Locations_Recorder2){
  temp3 <- subset(temp3A, Location == x)
  temp3$Sample_Type <- "Salamander fuik_bodem"
  Datums_Recorder <- unique(temp3$Datum)
  print(temp3$Location)
  for(y in Datums_Recorder){
    temp4 <- subset(temp3, Datum == y)
    Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type) 
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    iter <- ifelse(iter == 0, 1, iter)
    print(iter)
    for(p in 1:iter){
      o <- o + 1
      FNR <- paste("Fuik", o, sep= " ")
      temp5$Locationname <- FNR
      FNRL00 <- paste(FNR, "L00", sep= " - ")
      FNRL0 <- paste(FNR, "L0", sep= " - ")
      FNRL1 <- paste(FNR, "L1", sep= " - ")
      FNRL2 <- paste(FNR, "L2", sep= " - ")
      FNRM1 <- paste(FNR, "M1", sep= " - ")
      FNRM2 <- paste(FNR, "M2", sep= " - ")
      #FNRAM <- paste(FNR, "AM", sep= " - ") => voorlopig geen AM gevangen geeft error :-(
      FNRAV <- paste(FNR, "AV", sep= " - ")
      temp5$L00 <- ifelse(is.na(temp5[FNRL00]), 0, temp5[FNRL00])
      temp5$L0 <- ifelse(is.na(temp5[FNRL0]), 0, temp5[FNRL0])
      temp5$L1 <- ifelse(is.na(temp5[FNRL1]), 0, temp5[FNRL1])
      temp5$L2 <- ifelse(is.na(temp5[FNRL2]), 0, temp5[FNRL2])
      temp5$M1 <- ifelse(is.na(temp5[FNRM1]), 0, temp5[FNRM1])
      temp5$M2 <- ifelse(is.na(temp5[FNRM2]), 0, temp5[FNRM2])
      #temp5$AM <- ifelse(is.na(temp5[FNRAM]), 0, temp5[FNRAM]) => voorlopig geen AM gevangen geeft error :-(
      temp5$AM <- 0
      temp5$AV <- ifelse(is.na(temp5[FNRAV]), 0, temp5[FNRAV])
      temp2 <- rbind(temp2,temp5)
    }
    o <- 0
  }
}

temp2 <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV")]

temp6 <- temp2
remove(temp2)
remove(temp5)
remove(temp3)
remove(temp4)

Recorder_AfvangstSFB <- data.frame()

tempL00 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L00")]
tempL00$Number <- tempL00$L00
tempL00$L00 <- NULL
tempL00$Measurement <- "Abundance of L00"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempL00)

tempL0 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L0")]
tempL0$Number <- tempL0$L0
tempL0$L0 <- NULL
tempL0$Measurement <- "Abundance of L0"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempL0)

tempL1 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L1")]
tempL1$Number <- tempL1$L1
tempL1$L1 <- NULL
tempL1$Measurement <- "Abundance of L1"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempL1)

tempL2 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L2")]
tempL2$Number <- tempL2$L2
tempL2$L2 <- NULL
tempL2$Measurement <- "Abundance of L2"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempL2)

tempM1 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "M1")]
tempM1$Number <- tempM1$M1
tempM1$M1 <- NULL
tempM1$Measurement <- "Abundance of M1"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempM1)

tempM2 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "M2")]
tempM2$Number <- tempM2$M2
tempM2$M2 <- NULL
tempM2$Measurement <- "Abundance of M2"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempM2)

tempAM <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "AM")]
tempAM$Number <- tempAM$AM
tempAM$AM <- NULL
tempAM$Measurement <- "Abundance of AM"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempAM)

tempAV <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "AV")]
tempAV$Number <- tempAV$AV
tempAV$AV <- NULL
tempAV$Measurement <- "Abundance of AV"
Recorder_AfvangstSFB <- rbind(Recorder_AfvangstSFB, tempAV)
Recorder_AfvangstSFB$Number <- as.numeric(Recorder_AfvangstSFB$Number)

#Vergelijken met vorig bestand
Recorder_AfvangstSFB_Vorig <- read.csv2("file://Ruwe Data/Recorder_AfvangstSFB_2016-08-29.csv")
Recorder_AfvangstSFB_Vorig$X <- NULL
RAVnrow <- nrow(Recorder_AfvangstSFB_Vorig)
RAnrow <- nrow(Recorder_AfvangstSFB)
if(RAVnrow != RAnrow){
  Recorder_AfvangstSFB_Nieuw <- merge(Recorder_AfvangstSFB_Vorig, Recorder_AfvangstSFB, all.x=F)
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstSFB", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstSFB_Nieuw, file = RAFN)    
}

if(!file.exists("file://Ruwe Data/Recorder_AfvangstSFB_2016-08-29.csv")){
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstSFB", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstSFB, file = RAFN) 
}

#Leegmaken emmers
unique(Recorder_Ruw$Sample_Type)
temp3A <- subset(Recorder_Ruw, Sample_Type == "Leegmaken emmers")
Locations_Recorder2 <- unique(temp3A$Location)
temp2 <- data.frame()
for(x in Locations_Recorder2){
  temp3 <- subset(temp3A, Location == x)
  temp3$Sample_Type <- "Pitfall trap"
  Datums_Recorder <- unique(temp3$Datum)
  print(temp3$Location)
  for(y in Datums_Recorder){
    temp4 <- subset(temp3, Datum == y)
    Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type) 
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    iter <- ifelse(iter == 0, 1, iter)
    print(iter)
    for(p in 1:iter){
      o <- o + 1
      FNR <- paste("Fuik", o, sep= " ")
      FNR2 <- paste("Emmer", o, sep= " ")
      temp5$Locationname <- FNR2
      FNRL00 <- paste(FNR, "L00", sep= " - ")
      FNRL0 <- paste(FNR, "L0", sep= " - ")
      FNRL1 <- paste(FNR, "L1", sep= " - ")
      FNRL2 <- paste(FNR, "L2", sep= " - ")
      FNRM1 <- paste(FNR, "M1", sep= " - ")
      FNRM2 <- paste(FNR, "M2", sep= " - ")
      #FNRAM <- paste(FNR, "AM", sep= " - ") => voorlopig geen AM gevangen geeft error :-(
      FNRAV <- paste(FNR, "AV", sep= " - ")
      temp5$L00 <- ifelse(is.na(temp5[FNRL00]), 0, temp5[FNRL00])
      temp5$L0 <- ifelse(is.na(temp5[FNRL0]), 0, temp5[FNRL0])
      temp5$L1 <- ifelse(is.na(temp5[FNRL1]), 0, temp5[FNRL1])
      temp5$L2 <- ifelse(is.na(temp5[FNRL2]), 0, temp5[FNRL2])
      temp5$M1 <- ifelse(is.na(temp5[FNRM1]), 0, temp5[FNRM1])
      temp5$M2 <- ifelse(is.na(temp5[FNRM2]), 0, temp5[FNRM2])
      #temp5$AM <- ifelse(is.na(temp5[FNRAM]), 0, temp5[FNRAM]) => voorlopig geen AM gevangen geeft error :-(
      temp5$AM <- 0
      temp5$AV <- ifelse(is.na(temp5[FNRAV]), 0, temp5[FNRAV])
      temp2 <- rbind(temp2,temp5)
    }
    o <- 0
  }
}

temp2 <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV")]

temp6 <- temp2
remove(temp2)
remove(temp5)
remove(temp3)
remove(temp4)

Recorder_AfvangstPT <- data.frame()

tempL00 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L00")]
tempL00$Number <- tempL00$L00
tempL00$L00 <- NULL
tempL00$Measurement <- "Abundance of L00"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempL00)

tempL0 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L0")]
tempL0$Number <- tempL0$L0
tempL0$L0 <- NULL
tempL0$Measurement <- "Abundance of L0"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempL0)

tempL1 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L1")]
tempL1$Number <- tempL1$L1
tempL1$L1 <- NULL
tempL1$Measurement <- "Abundance of L1"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempL1)

tempL2 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "L2")]
tempL2$Number <- tempL2$L2
tempL2$L2 <- NULL
tempL2$Measurement <- "Abundance of L2"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempL2)

tempM1 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "M1")]
tempM1$Number <- tempM1$M1
tempM1$M1 <- NULL
tempM1$Measurement <- "Abundance of M1"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempM1)

tempM2 <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "M2")]
tempM2$Number <- tempM2$M2
tempM2$M2 <- NULL
tempM2$Measurement <- "Abundance of M2"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempM2)

tempAM <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "AM")]
tempAM$Number <- tempAM$AM
tempAM$AM <- NULL
tempAM$Measurement <- "Abundance of AM"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempAM)

tempAV <- temp6[, c("Location", "Datum", "Sample_Type", "Locationname", "AV")]
tempAV$Number <- tempAV$AV
tempAV$AV <- NULL
tempAV$Measurement <- "Abundance of AV"
Recorder_AfvangstPT <- rbind(Recorder_AfvangstPT, tempAV)
Recorder_AfvangstPT$Number <- as.numeric(Recorder_AfvangstPT$Number)

#Vergelijken met vorig bestand
Recorder_AfvangstPT_Vorig <- read.csv2("file://Ruwe Data/Recorder_AfvangstPT_2016-08-29.csv")
Recorder_AfvangstPT_Vorig$X <- NULL
RAVnrow <- nrow(Recorder_AfvangstPT_Vorig)
RAnrow <- nrow(Recorder_AfvangstPT)
if(RAVnrow != RAnrow){
  Recorder_AfvangstPT_Nieuw <- merge(Recorder_AfvangstPT_Vorig, Recorder_AfvangstPT, all.x=F)
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstPT", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstPT_Nieuw, file = RAFN)    
}

if(!file.exists("file://Ruwe Data/Recorder_AfvangstPT_2016-08-29.csv")){
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstPT", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstPT, file = RAFN) 
}

#Leegmaken emmers
unique(Recorder_Ruw$Sample_Type)
temp3A <- subset(Recorder_Ruw, Sample_Type == "Plaatsen van fuiken")
Locations_Recorder2 <- unique(temp3A$Location)
temp2 <- data.frame()
for(x in Locations_Recorder2){
  temp3 <- subset(temp3A, Location == x)
  temp3$Sample_Type <- "Plaatsen Fuiken"
  Datums_Recorder <- unique(temp3$Datum)
  print(temp3$Location)
  for(y in Datums_Recorder){
    temp4 <- subset(temp3, Datum == y)
    Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type) 
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    iter <- ifelse(iter == 0, 1, iter)
    print(iter)
    for(p in 1:iter){
      o <- o + 1
      FNR <- paste("Fuik", o, sep= " ")
      FNR2 <- paste("Emmer", o, sep= " ")
      temp5$Locationname <- FNR
      temp2 <- rbind(temp2,temp5)
    }
    o <- 0
  }
}

temp2 <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname")]

temp6 <- temp2
remove(temp2)
remove(temp5)
remove(temp3)
remove(temp4)

Recorder_AfvangstPF <- temp6

#Vergelijken met vorig bestand
Recorder_AfvangstPF_Vorig <- read.csv2("file://Ruwe Data/Recorder_AfvangstPF_2016-08-29.csv")
Recorder_AfvangstPF_Vorig$X <- NULL
RAVnrow <- nrow(Recorder_AfvangstPF_Vorig)
RAnrow <- nrow(Recorder_AfvangstPF)
if(RAVnrow != RAnrow){
  Recorder_AfvangstPF_Nieuw <- merge(Recorder_AfvangstPF_Vorig, Recorder_AfvangstPF, all.x=F)
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstPF", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstPF, file = RAFN)    
}

if(!file.exists("file://Ruwe Data/Recorder_AfvangstPF_2016-08-29.csv")){
  Today <- Sys.Date()
  RAFN <- paste("file://Ruwe Data/Recorder_AfvangstPF", Today, sep = "_")
  RAFN <- paste(RAFN, ".csv", sep="")
  write.csv2(x=Recorder_AfvangstPF, file = RAFN) 
}
