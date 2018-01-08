#RUN ANa.R first

library(googlesheets) 
library(dplyr)


####Klaarzetten voor recorder####
#Data Selectie
Recorder_Ruw <- Brondata
Recorder_Ruw$`Aantal fuiken (Totaal)` <- ifelse(is.na(Recorder_Ruw$`Aantal fuiken (Totaal)`), Recorder_Ruw$`Aantal fuiken geplaatst`, Recorder_Ruw$`Aantal fuiken (Totaal)`)
Recorder_Ruw$`Aantal fuiken (Totaal)` <- ifelse(is.na(Recorder_Ruw$`Aantal fuiken (Totaal)`), 0, Recorder_Ruw$`Aantal fuiken (Totaal)`)
Recorder_Ruw$`Aantal fuiken (Totaal)` <- as.numeric(Recorder_Ruw$`Aantal fuiken (Totaal)`)
summary(Recorder_Ruw$`Aantal fuiken (Totaal)`)
table(Recorder_Ruw$`Aantal fuiken (Totaal)`)
Recorder_Ruw <- subset(Recorder_Ruw, !is.na(Datum))
gnFuiken <- subset(Recorder_Ruw, is.na(`Aantal fuiken (Totaal)`)|`Aantal fuiken (Totaal)`==0)
Recorder_Ruw <- subset(Recorder_Ruw, !is.na(`Aantal fuiken (Totaal)`))

####Foutieve NA in aantal fuiken corrigeren####
Recorder_Ruw$`Aantal fuiken (Totaal)`[Recorder_Ruw$Tijdstempel == "8-8-2016 23:34:27"] <- 1
Recorder_Ruw$`Aantal fuiken (Totaal)`[Recorder_Ruw$Tijdstempel == "8-8-2016 23:36:05"] <- 1
Recorder_Ruw$`Aantal fuiken (Totaal)`[Recorder_Ruw$Tijdstempel == "8-8-2016 23:38:47"] <- 1
Recorder_Ruw$`Aantal fuiken (Totaal)`[Recorder_Ruw$Tijdstempel == "8-8-2016 23:40:20"] <- 1

summary(Recorder_Ruw$`Aantal fuiken (Totaal)`)
gnFuiken <- subset(Recorder_Ruw, is.na(`Aantal fuiken (Totaal)`))
if(nrow(gnFuiken) >= 1) {
  ERROR_Aantal_Fuiken <- data_frame()
}else{
  remove(gnFuiken)
}

Recorder_Ruw$comparekey <- paste(Recorder_Ruw$Location, Recorder_Ruw$Datum)

#Data geïmporteerde data
#Import 2016
Recorder_Afvangst_2016 <- read.csv2("G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Imported/Recorder_Afvangst_2016-12-12 .csv")
Recorder_Afvangst_2016$comparekey <- paste(Recorder_Ruw$Location, Recorder_Ruw$Date)

#Import 2017
#Nog uit te voeren

#=>Recorder_Ruw

####Data per Sample_Type####
Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type)
for(s in Sample_Types_Recorder){
  
}





temp2 <- data.frame()
o <- 0
temp3A <- subset(Recorder_Ruw, Sample_Type == s )
checksum <- sum(temp3A$`Aantal fuiken (Totaal)`)
Locations_Recorder <- unique(temp3A$Location)

#Resultaten Doelsoort toevoegen

for(x in Locations_Recorder){
  temp3 <- subset(temp3A, Location == x)
  if(s=="Afvangst"){
    temp3$Sample_Type <- s
  }
  Datums_Recorder <- unique(temp3$Datum)
  print(temp3$Location)
  print(c("doelsoort",s,x))
  for(y in Datums_Recorder){
    temp4 <- subset(temp3, Datum == y)
    Sample_Types_Recorder <- unique(Recorder_Ruw$Sample_Type) 
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    #iter2 <- sum(temp5$`Aantal fuiken geplaatst`)
    #iter <- ifelse(is.na(iter), ifelse(is.na(iter2), 1, iter2 ),iter)
    #iter <- ifelse(iter == 0, 1, iter)
    #print(iter)
    print(c("doelsoort",s,x,y))
    for(p in 1:iter){
      o <- o + 1
      print(c("doelsoort",s,x,y,iter))
      if(o<=12){
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
      }else{break}
    }
    o <- 0
  }
}

#Nuttige kolommen selecteren

Recorder_Prep <- temp2[, c("Location", "Datum", "Sample_Type", "Locationname", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV", "Recorder")]
if(nrow(Recorder_Prep) != checksum){
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

Recorder_Prep$Species <- "Lithobates catesbeianus"

title <- gs_title(x="Locaties", verbose = TRUE)
Locaties <- gs_read(title)

Recorder_Prep <- merge(Recorder_Prep, Locaties, all.x=T)

Recorder_Prep$TaxonDataAccuracy <- "Exact"
temp <- subset(Recorder_Prep, is.na(GridReference))
missinglocations <- unique(temp$Location)
print(missinglocations)
remove(temp6)

#Bijvangsten toevoegen 

temp7 <- data_frame()
temp6b <- data.frame(x)
temp2 <- subset(Recorder_Ruw, Sample_Type == s)
Locations_Recorder <- unique(temp2$Location)
if(temp2$Sample_Type != "Plaatsen van fuiken"){
for(a in Locations_Recorder){
  temp3 <- subset(temp2, Location == a)
  Datums_Recorder <- unique(temp3$Datum)
  print(c("bijvangst",s,a))
  for(t in Datums_Recorder){
    temp4 <- subset(temp3, Datum == t)
    temp5 <- temp4
    iter <- sum(temp5$`Aantal fuiken (Totaal)`)
    print(c("bijvangst", s,a,t,iter))
    for(p in 1:iter){
      o <- o + 1
      if(o <= 12){
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
        if(FNR3 %in% colnames(temp5)){
        if(!is.na(temp5[FNR3])){
          print(c("bijvangst", s,a,t,iter,q))
          temp6b$Location <- unique(temp5$Location)
          temp6b$Date <- unique(temp5$Datum)
          temp6b$Locationname <- FNR2
          temp6b$soort <- q
          temp6b$Recorder <- unique(temp5$Recorder)
          Formaat <- unique(FNR3)
          temp6b$None <- 1
          
          temp7 <- rbind(temp7, temp6b)
          
        }else{
          print(c("bijvangst", s,a,t,q,"is NA"))
          next
          }
        }else{next}
      }
      }else{break}
    }
    o <- 0
  }
}


temp7$x <- NULL

title <- gs_title(x="Bijvangst", verbose = TRUE)
Soorten <- gs_read(title)
if(nrow(temp7)>0){
temp8 <- merge(temp7, Soorten)
temp8$soort <- NULL
if(s=="Afvangst"){
  temp8$Sample_Type <- "Schietfuik" 
}else{
  temp8$Sample_Type <- s
}
}else{
  print(c(s,a,t,q,"geen bijvangst"))
  next}
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

Recorder_Prep$None <- NA
Recorder_Prep$Date <- Recorder_Prep$Datum
Recorder_Prep$Datum <- NULL

Recorder_Prep_2 <- rbind(Recorder_Prep, temp8)
}else{
  Recorder_Prep_2 <- Recorder_Prep
}
Recorder_Prep_2$L00 <- as.numeric(Recorder_Prep_2$L00)
Recorder_Prep_2$L0 <- as.numeric(Recorder_Prep_2$L0)
Recorder_Prep_2$L1 <- as.numeric(Recorder_Prep_2$L1)
Recorder_Prep_2$L2 <- as.numeric(Recorder_Prep_2$L2)
Recorder_Prep_2$M1 <- as.numeric(Recorder_Prep_2$M1)
Recorder_Prep_2$M2 <- as.numeric(Recorder_Prep_2$M2)
Recorder_Prep_2$AM <- as.numeric(Recorder_Prep_2$AM)
Recorder_Prep_2$AV <- as.numeric(Recorder_Prep_2$AV)

rec_fname <- paste(wd, "Recorder/Recorder_", s ,"_",Today, sep="")
rec_fname <- paste(rec_fname, ".csv", sep="")
write.csv(Recorder_Prep_2, rec_fname)

Log <- read.csv("G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Stierkikker data-analyse/Log.csv")
Log$X <- NULL
Log$Date <- as.Date.factor(Log$Date)
i <- tail(Log$i, n=1)+1
temp_Log <- data.frame(i)
temp_Log$Date <- as.Date.factor(Today)
temp_Log$FileName <- rec_fname
temp_Log$rows <- nrow(Recorder_Prep_2)
Log <- rbind(Log, temp_Log)
write.csv(Log, "G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Stierkikker data-analyse/Log.csv")

}

####Afvangsten Vergelijken met vorige imports####


#Merge
#Just 2016 for now, when more years are imported they will be merged with the previous years following this seperator. 
Recorder_Afvangst_vorig <- Recorder_Afvangst_2016

#Compare

Recorder_Afvangst_vorig$comparekey <- paste(Recorder_Afvangst_vorig$Location, Recorder_Afvangst_vorig$Date, 
                                            Recorder_Afvangst_vorig$Locationname, Recorder_Afvangst_vorig$Species)
Recorder_Afvangst_2$comparekey <- paste(Recorder_Afvangst_2$Location, Recorder_Afvangst_2$Date, 
                                        Recorder_Afvangst_2$Locationname, Recorder_Afvangst_2$Species)

Recorder_Afvangst_vorig$Location <- as.character(Recorder_Afvangst_vorig$Location)
Recorder_Afvangst_vorig$Sample_Type <- as.character(Recorder_Afvangst_vorig$Sample_Type)
Recorder_Afvangst_vorig$Locationname <- as.character(Recorder_Afvangst_vorig$Locationname)
Recorder_Afvangst_vorig$L00 <- as.numeric(Recorder_Afvangst_vorig$L00)
Recorder_Afvangst_vorig$L0 <- as.numeric(Recorder_Afvangst_vorig$L0)
Recorder_Afvangst_vorig$L1 <- as.numeric(Recorder_Afvangst_vorig$L1)
Recorder_Afvangst_vorig$L2 <- as.numeric(Recorder_Afvangst_vorig$L2)
Recorder_Afvangst_vorig$M1 <- as.numeric(Recorder_Afvangst_vorig$M1)
Recorder_Afvangst_vorig$M2 <- as.numeric(Recorder_Afvangst_vorig$M2)
Recorder_Afvangst_vorig$AV <- as.numeric(Recorder_Afvangst_vorig$AV)
Recorder_Afvangst_vorig$AM <- as.numeric(Recorder_Afvangst_vorig$AM)
Recorder_Afvangst_vorig$Recorder <- as.character(Recorder_Afvangst_vorig$Recorder)
Recorder_Afvangst_vorig$Species <- as.character(Recorder_Afvangst_vorig$Species)
Recorder_Afvangst_vorig$GridReference <- as.character(Recorder_Afvangst_vorig$GridReference)
Recorder_Afvangst_vorig$TaxonDataAccuracy <- as.character(Recorder_Afvangst_vorig$TaxonDataAccuracy)
Recorder_Afvangst_vorig$None <- as.numeric(Recorder_Afvangst_vorig$None)
Recorder_Afvangst_vorig$Date <- as.character(Recorder_Afvangst_vorig$Date)
Recorder_Afvangst_vorig$comparekey <- as.character(Recorder_Afvangst_vorig$comparekey)
Recorder_Afvangst_vorig$X <- NULL

Recorder_Afvangst_vorig$comparekey <- ordered(x =Recorder_Afvangst_vorig$comparekey)
Recorder_Afvangst_vorig$comparekey <- as.character(Recorder_Afvangst_vorig$comparekey)
Recorder_Afvangst_2$comparekey <- ordered(x=Recorder_Afvangst_2$comparekey)
Recorder_Afvangst_2$comparekey <- as.character(Recorder_Afvangst_2$comparekey)

Recorder_Afvangst_3 <- anti_join(x = Recorder_Afvangst_2, y= Recorder_Afvangst_vorig, by = "comparekey")

####Output####
Today <- Sys.Date()

rec_afvg_fname <- paste(wd, "Recorder/Recorder_Afvangst_",Today, sep="")
rec_afvg_fname <- paste(rec_afvg_fname, ".csv", sep="")
write.csv(Recorder_Afvangst_3, rec_afvg_fname)

Log <- read.csv("G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Stierkikker data-analyse/Log.csv")
Log$X <- NULL
Log$Date <- as.Date.factor(Log$Date)
i <- tail(Log$i, n=1)+1
temp_Log <- data.frame(i)
temp_Log$Date <- as.Date.factor(Today)
temp_Log$FileName <- rec_afvg_fname
temp_Log$rows <- nrow(Recorder_Afvangst_3)
Log <- rbind(Log, temp_Log)
write.csv(Log, "G://Mijn Drive/INBOPRJ-10217 - Monitoring exoten ikv EU- verordening IAS  Coördinatie, voorbereiding, implementatie en opvolging/Stierkikker/Opvolging beheer/Stierkikker data-analyse/Log.csv")






