rm(list = ls())

library(googlesheets) 
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
#library(INBOtheme)


#### Set standard paths => Veranderen indien andere pc/laptop ####

##WERK##
#imagepath <- "C://Users/sander_devisscher/Google Drive/Faunabeheer/EU_IAS/Stierkikker/Stierkikker data-analyse/Afbeeldingen" #Werk
#wd <- "C://Users/sander_devisscher/Google Drive/EU_IAS/Stierkikker/Stierkikker data-analyse/SK Analyse"
imagepath <- "./Output/" #Werk
wd <- "./Input/"



##THUIS##
#imagepath <- "C://Users/Sander/Google Drive Werk/EU_IAS/Stierkikker/Stierkikker data-analyse/Afbeeldingen"
#wd <- "C://Users/Sander/Google Drive Werk/EU_IAS/Stierkikker/Stierkikker data-analyse/SK Analyse"

####Import data####
#Import online data####
title <- gs_title(x = "Stierkikker formulieren (Reacties)", verbose = TRUE)
Token <- gs_auth()
gs_auth(token = Token)
gdrive <- gs_read(title)
tail(gdrive$Tijdstempel)

#Import offline data####
#Check in de ruwe datamap of reeds nieuwere brondata backups zijn 
OFFLINE <- data.frame()
offlinepath <- paste(wd, "Stierkikkerformulieren(Reacties)-Formulierreacties_2017-08-11.csv", sep="" )
OFFLINE <- read.csv(offlinepath, sep=";")

#Verify connection status####
Brondata <- get0("gdrive", ifnotfound=OFFLINE)

if(exists("gdrive")){
  ONLINE <- data.frame("x")
  if(nrow(Brondata)!=nrow(OFFLINE)){
  today <- Sys.Date()
  today2 <- paste("_", today, sep="")
  offlinepath2 <- paste(wd,"Stierkikkerformulieren(Reacties)-Formulierreacties",today2, sep="")
  offlinepath2 <- paste(offlinepath2, ".csv", sep="")
  write.csv2(Brondata, offlinepath2)
  ONLINE$Status <- "OFFLINE has been updated"
  }
  if(nrow(Brondata)==nrow(OFFLINE)){ONLINE$Status <-"OFFLINE has equal rows as Brondata"}
  remove(OFFLINE)
}

####Some Standard checks and simplification####
temp <- Brondata

#Check for new locations####
temp$`Vijver - Kasterlee` <- ifelse(temp$`Vijver - Kasterlee`=="retiebaan 1", "Retiebaan 1", 
                                    ifelse(temp$`Vijver - Kasterlee` =="retiebaan 2", "Retiebaan 2", 
                                           ifelse(temp$`Vijver - Kasterlee` == "watermolen", "Watermolen", temp$`Vijver - Kasterlee`)))
temp$`Vijver - Arendonk` <- ifelse(temp$`Vijver - Arendonk`== "arendonk 2 b", "Arendonk 2b", temp$`Vijver - Arendonk`)

temp$Location <- ifelse(!is.na(temp$`Vijver - Arendonk`),temp$`Vijver - Arendonk`,
                        ifelse(!is.na(temp$`Vijver - Kasterlee`), temp$`Vijver - Kasterlee`,
                               ifelse(!is.na(temp$`Vijver - Hoogstraten`), temp$`Vijver - Hoogstraten`,
                                      ifelse(!is.na(temp$`Vijver - Nijlen`), temp$`Vijver - Nijlen`,
                                             ifelse(!is.na(temp$`Vijver - Scheps`),temp$`Vijver - Scheps`, NA)))))

table(temp$Location)


temp$Sample_Type <- temp$`Wat wil je melden`
table(temp$Sample_Type)

#Newlocs loop
VasteLOC <- subset(temp, !is.na(temp$Location))
NieuweLOC <- subset(temp, is.na(temp$Location))
if(nrow(NieuweLOC)==0){
  remove(NieuweLOC)
  remove(VasteLOC)
}

#Remove incorrect new locs
temp <- subset(temp, Tijdstempel != "15-5-2017 23:50:47")
temp <- subset(temp, Tijdstempel != "15-5-2017 23:58:58")
temp <- subset(temp, Tijdstempel != "15-5-2017 23:45:35")
temp <- subset(temp, Tijdstempel != "15-5-2017 23:47:36")
temp <- subset(temp, Tijdstempel != "17-5-2017 11:50:04")
temp <- subset(temp, Tijdstempel != "17-5-2017 11:56:51")
#Rerun newlocs loop
VasteLOC <- subset(temp, !is.na(temp$Location))
NieuweLOC <- subset(temp, is.na(temp$Location))
if(nrow(NieuweLOC)==0){
  remove(NieuweLOC)
  remove(VasteLOC)
}

####Check fouten invullers####
table(temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "kris", "Kris Meeus", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "pieter liekens", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "kris meeus", "Kris Meeus", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "kris meeus (prive)", "Kris Meeus", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "pieter liekebns", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "pieter Liekens", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "pIETER LIEKENS", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "Pieter liekens", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "PIETER LIEKENS", "Pieter Liekens", temp$Invuller)
temp$Invuller <- ifelse(temp$Invuller == "PIETER LIEKES", "Pieter Liekens", temp$Invuller)
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
temp$`Vijver - Nijlen` <- NULL
temp$`Vijver - Scheps` <- NULL
#temp$Locatie <- NULL
temp$`Gemeente/Deelgemeente` <- NULL
temp$Invuller <- NULL
temp$Recorder2 <- NULL
temp$`Aantal werknemers` <- NULL

####Manually correct NA's in Aantal fuiken in Brondata####
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "10-5-2016 20:50:24", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "11-5-2016 22:45:28", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "11-5-2016 22:50:30", 3, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:04:04", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:18:34", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:12:05", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:48:21", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:52:58", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:55:16", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:56:54", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 12:59:21", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 13:02:53", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "14-5-2016 13:06:01", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "19-5-2016 0:08:54", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 22:25:13", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 22:41:34", 3, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 22:55:46", 3, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 22:48:47", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 23:00:13", 4, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 23:15:32", 2, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 23:17:03", 1, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 23:19:12", 3, temp$`Aantal fuiken (Totaal)`)
temp$`Aantal fuiken (Totaal)` <- ifelse(temp$Tijdstempel == "13-6-2016 23:21:17", 3, temp$`Aantal fuiken (Totaal)`)

test <- subset(temp, Sample_Type == "Afvangst")
test <- subset(test, is.na(`Aantal fuiken (Totaal)`))
if(nrow(test)==0){
  remove(test)
}
####Dubbele inputs verwijderen####
#Dubbele datums verwijderen
temp <- subset(temp, Tijdstempel != "14-5-2016 13:06:01")
temp <- subset(temp, Tijdstempel != "9-8-2016 23:58:14")
temp <- subset(temp, Tijdstempel != "26-10-2016 20:16:52")
temp <- subset(temp, Tijdstempel != "26-10-2016 20:18:19")
temp <- subset(temp, Tijdstempel != "26-10-2016 20:19:35")
temp <- subset(temp, Tijdstempel != "17-5-2017 23:21:58")
temp <- subset(temp, Tijdstempel != "17-5-2017 23:28:57")
temp <- subset(temp, Tijdstempel != "6-6-2017 17:18:18")
temp <- subset(temp, Tijdstempel != "6-6-2017 17:13:07")


#foute sample type fixen
temp$Sample_Type <- ifelse(temp$Tijdstempel == "9-8-2016 23:49:54", "Afvangst SFB",temp$Sample_Type )
temp$Sample_Type <- ifelse(temp$Tijdstempel == "9-8-2016 23:51:44", "Afvangst SFD",temp$Sample_Type )
temp$Sample_Type <- ifelse(temp$Tijdstempel == "9-8-2016 23:34:23", "Afvangst SFB",temp$Sample_Type )
temp$Sample_Type <- ifelse(temp$Tijdstempel == "9-8-2016 23:35:49", "Afvangst SFD",temp$Sample_Type )

#NA datums verwijderen
temp <- subset(temp, !is.na(Datum))

Brondata_Backup <- temp
temp <- Brondata_Backup 
#Check loop
temp$Datum <- as.Date(temp$Datum, "%d-%m-%Y")
temp$Jaar <- format(temp$Datum, format='%Y')
table(temp$Jaar)
Jaren <- unique(temp$Jaar)
Jaren <- Jaren[!is.na(Jaren)]

DubbeleInputs <- data.frame()
DubbeleDatums <- data.frame()
temp7 <- data.frame()
temp8 <- temp
#for(j in Jaren){
 # temp2 <- subset(temp, Jaar == j)
  #Locations <- unique(temp2$Location)
  #for(i in Locations){
   # temp3 <- subset(temp2, Location == i )
  #  Aantalrecords <- nrow(temp3)
   # Aantaldatums <- n_distinct(temp3$Datum)
    #if(Aantalrecords != Aantaldatums){
     # ThisLocation <- data.frame(x=i,y=j)
      #DubbeleInputs <- rbind(DubbeleInputs, ThisLocation)
      #temp4 <- subset(temp, Location != i)
#      temp5 <- subset(temp, Location == i)
 #     temp6 <- subset(temp5, Jaar != j)
  #    temp7 <- rbind(temp4, temp6)
      #Filter Duplicates
   #   temp5 <- subset(temp5, Sample_Type == "Afvangst" )
    #  datums <- unique(temp5$Datum)
     # for(d in datums){
      #  DubbeleDatums <- subset(temp5, Datum == d)
       # if(nrow(DubbeleDatums)>1){
        #  stop("dubbele datums")
#        }
 #     }
  #  }
#  }
#}
#print("dubbele inputs verwijderd")
#if(nrow(temp8)==nrow(temp4)){
 # temp <- temp8
#}else{
 # temp <- temp7
#}

#Brondata <- temp
Brondata <- Brondata_Backup
tail(Brondata$Tijdstempel)
unique(Brondata$Location)

####Opkuis####
remove(DubbeleDatums)
remove(DubbeleInputs)
remove(temp2)
remove(temp3)
remove(temp4)
remove(temp5)
remove(temp6)
remove(temp7)
remove(temp8)
remove(ThisLocation)
remove(gdrive)
remove(temp)
remove(test)
remove(Aantaldatums)
remove(Aantalrecords)
remove(d)
remove(datums)
remove(i)
remove(j)
remove(Jaren)
remove(Locations)
remove(n)
remove(title)
remove(Token)

