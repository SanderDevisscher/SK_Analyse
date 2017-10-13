#RUN ANa.R first

library(googlesheets) 
library(dplyr)


####Klaarzetten voor recorder####
#Data Selectie
Recorder_Ruw <- Brondata
Recorder_Ruw$`Aantal fuiken (Totaal)` <- ifelse(is.na(Recorder_Ruw$`Aantal fuiken (Totaal)`), 0, Recorder_Ruw$`Aantal fuiken (Totaal)`)
Recorder_Ruw$`Aantal fuiken (Totaal)` <- as.numeric(Recorder_Ruw$`Aantal fuiken (Totaal)`)

####Afvangsten voor Recorder ####
temp2 <- data.frame()
o <- 0
temp3A <- subset(Recorder_Ruw, Sample_Type == "Afvangst" )
checksum <- sum(temp3A$`Aantal fuiken (Totaal)`)
Locations_Recorder <- unique(temp3A$Location)

#Resultaten Doelsoort toevoegen

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
    #iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    #iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    #iter <- ifelse(iter == 0, 1, iter)
    #print(iter)
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
      FNRAM <- paste(FNR, "AM", sep= " - ")
      FNRAV <- paste(FNR, "AV", sep= " - ")
      temp5$L00 <- ifelse(is.na(temp5[FNRL00]), 0, temp5[FNRL00])
      temp5$L0 <- ifelse(is.na(temp5[FNRL0]), 0, temp5[FNRL0])
      temp5$L1 <- ifelse(is.na(temp5[FNRL1]), 0, temp5[FNRL1])
      temp5$L2 <- ifelse(is.na(temp5[FNRL2]), 0, temp5[FNRL2])
      temp5$M1 <- ifelse(is.na(temp5[FNRM1]), 0, temp5[FNRM1])
      temp5$M2 <- ifelse(is.na(temp5[FNRM2]), 0, temp5[FNRM2])
      temp5$AM <- ifelse(is.na(temp5[FNRAM]), 0, temp5[FNRAM])
      temp5$AV <- ifelse(is.na(temp5[FNRAV]), 0, temp5[FNRAV])
      temp2 <- rbind(temp2,temp5)
    }
    o <- 0
  }
}

#Nuttige kolommen selecteren

Recorder_Afvangst <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV", "Recorder")]
if(nrow(Recorder_Afvangst) != checksum){
  print("checksum fail")
}else{
  temp2 <- NULL
}

#Tussentijdse opruim

remove(temp5)
remove(temp3)
remove(temp3A)
remove(temp4)

#Soort, Grid reference en accuracy toevoegen

Recorder_Afvangst$Species <- "Lithobates catesbeianus"

title <- gs_title(x="Locaties", verbose = TRUE)
Locaties <- gs_read(title)

Recorder_Afvangst <- merge(Recorder_Afvangst, Locaties, all.x=T)

Recorder_Afvangst$TaxonDataAccuracy <- "Exact"
temp <- subset(Recorder_Afvangst, is.na(GridReference))
missinglocations <- unique(temp$Location)
print(missinglocations)
remove(temp6)

#Bijvangsten toevoegen 

temp7 <- data_frame()
temp6b <- data.frame(x)
temp2 <- subset(Recorder_Ruw, Sample_Type == "Afvangst")
Locations_Recorder <- unique(Recorder_Ruw$Location)
for(a in Locations_Recorder){
  temp3 <- subset(temp2, Location == a)
  Datums_Recorder <- unique(temp3$Datum)
  for(t in Datums_Recorder){
    temp4 <- subset(temp3, Datum == t)
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    print(iter)
    for(p in 1:iter){
      o <- o + 1
      FNR <- paste("Fuik", o, "- Bijvangst ", sep= " ")
      FNR2 <- paste("Fuik", o, sep= " ")
      Recorder_Bijvangst <- c("[3 - doornige stekelbaars]", "[Amerikaanse gevlekte rivierkreeft]"
                              , "[Baars]", "[Blankvoorn]", "[Blauwbandgrondel]","[Brasem]"
                              , "[Bruine Amerikaanse dwergmeerval]", "[Giebel]", "[Karper]", "[Paling]" 
                              , "[Rietvoorn]", "[Riviergrondel]", "[Zonnebaars]", "[Zeelt]", "[Snoek]"
                              , "[Kolblei]", "[Chinese wolhandkrab]", "[Geelgerande watertor]", "[Groene kikker]"
                              , "[Bruine kikker]", "[Pad]", "[Goudvis]", "[Europese meerval]" )
      for(q in Recorder_Bijvangst){
        FNR3 <- paste(FNR, q, sep="")
        #temp6 <- subset(temp5, !is.na(FNR3))
        if(!is.na(temp5[FNR3])){
          temp6b$Location <- unique(temp5$Location)
          temp6b$Date <- unique(temp5$Datum)
          temp6b$Locationname <- FNR2
          temp6b$soort <- q
          temp6b$Recorder <- unique(temp5$Recorder)
          Formaat <- unique(FNR3)
          temp6b$None <- 1
          temp7 <- rbind(temp7, temp6b)
        }else{next}
      }
    }
    o <- 0
  }
}

temp7$x <- NULL

title <- gs_title(x="Bijvangst", verbose = TRUE)
Soorten <- gs_read(title)

temp8 <- merge(temp7, Soorten)
temp8$soort <- NULL
temp8$Sample_Type <- "Schietfuik"
temp8$L00 <- NA
temp8$L0 <- NA
temp8$L1 <- NA
temp8$L2 <- NA
temp8$M1 <- NA
temp8$M2 <- NA
temp8$AM <- NA
temp8$AV <- NA
temp8$TaxonDataAccuracy <- "Estimate"
temp8 <- merge(temp8, Locaties)
temp8 <- temp8[,c("Location", "Sample_Type","Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV", "Species", "GridReference", "TaxonDataAccuracy", "None", "Date","Recorder")]

Recorder_Afvangst$None <- NA
Recorder_Afvangst$Date <- Recorder_Afvangst$Datum
Recorder_Afvangst$Datum <- NULL

Recorder_Afvangst_2 <- rbind(Recorder_Afvangst, temp8)

Recorder_Afvangst_2$L00 <- as.numeric(Recorder_Afvangst_2$L00)
Recorder_Afvangst_2$L0 <- as.numeric(Recorder_Afvangst_2$L0)
Recorder_Afvangst_2$L1 <- as.numeric(Recorder_Afvangst_2$L1)
Recorder_Afvangst_2$L2 <- as.numeric(Recorder_Afvangst_2$L2)
Recorder_Afvangst_2$M1 <- as.numeric(Recorder_Afvangst_2$M1)
Recorder_Afvangst_2$M2 <- as.numeric(Recorder_Afvangst_2$M2)
Recorder_Afvangst_2$AM <- as.numeric(Recorder_Afvangst_2$AM)
Recorder_Afvangst_2$AV <- as.numeric(Recorder_Afvangst_2$AV)

Today <- Sys.Date()

rec_afvg_fname <- paste(wd, "Recorder/Recorder_Afvangst_",Today, sep="")
rec_afvg_fname <- paste(rec_afvg_fname, ".csv", sep="")
write.csv(Recorder_Afvangst_2, rec_afvg_fname)

Log <- read.csv("./Input/Recorder/Log.csv")
Log$X <- NULL
Log$Date <- as.Date.factor(Log$Date)
i <- tail(Log$i, n=1)+1
temp_Log <- data.frame(i)
temp_Log$Date <- as.Date.factor(Today)
temp_Log$FileName <- rec_afvg_fname
Log <- rbind(Log, temp_Log)
write.csv(Log, "./Input/Recorder/Log.csv")

####Vergelijken met vorige imports####
#Import 2016
Recorder_Afvangst_2016 <- read.csv2("G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Stierkikker data-analyse/SK Analyse/Ruwe Data/Recorder_Afvangst_2016-12-12 .csv")
#Import 2017
#Nog uit te voeren

#Merge
#Just 2016 for now, when more years are imported they will be merged with the previous years following this seperator. 
Recorder_Afvangst_vorig <- Recorder_Afvangst_2016

#Compare

Recorder_Afvangst_vorig$comparekey <- paste(Recorder_Afvangst_vorig$Location, Recorder_Afvangst_vorig$Date, 
                                            Recorder_Afvangst_vorig$Locationname, Recorder_Afvangst_vorig$Species)
Recorder_Afvangst_2$comparekey <- paste(Recorder_Afvangst_2$Location, Recorder_Afvangst_2$Date, 
                                        Recorder_Afvangst_2$Locationname, Recorder_Afvangst_2$Species)


Recorder_Afvangst_3 <- anti_join(Recorder_Afvangst_2, Recorder_Afvangst_vorig, by = "comparekey")

#Opruimen
remove(tempL00) 
remove(tempL0)
remove(tempL1)
remove(tempL2)
remove(tempM1)
remove(tempM2)
remove(tempAM)
remove(tempAV)