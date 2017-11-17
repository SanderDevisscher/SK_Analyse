####GRAFIEKEN####
#Enkel Afvangsten voor grafiekjes
Afvangsten <- subset(Brondata, Sample_Type == "Afvangst")
Afvangsten$L00 <- NA
Afvangsten$L0 <- NA
Afvangsten$L1 <- NA
Afvangsten$L2 <- NA
Afvangsten$M1 <- NA
Afvangsten$M2 <- NA
Afvangsten$AM <- NA
Afvangsten$AV <- NA

#Sum of all stage specefic columns####

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

#Making sure NA's are NA####

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

#Calculating Total, Total of all larvea, Total larvea for CPUE Calculation####
#Larvea for CPUE calculation excludes those larvea that are too small (L00)
#Post - Metamorfs (M1, M2, AM, AV) are excluded by default
Afvangsten$Totaal <- NA
Afvangsten$Totaal_Larven.All <- NA
Afvangsten$Totaal_Larven.CPUE <- NA

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
print(Afvangsten$Totaal)
print(Afvangsten$Totaal_Larven.All)
print(Afvangsten$Totaal_Larven.CPUE)
Afvangsten$`Aantal fuiken (Totaal)` <- as.numeric(Afvangsten$`Aantal fuiken (Totaal)`)
print(Afvangsten$`Aantal fuiken (Totaal)`)




####Calculate CPUE####
#CPUE = Catch per unit of effort.
#Unit of effort = 1 fyke per 24 hours* 
#Catch = total of larvea for CPUE calculation
#*Data is as such that every 24h a number of fykes are used a record is made
Afvangsten$CPUE <- Afvangsten$Totaal_Larven.CPUE/Afvangsten$`Aantal fuiken (Totaal)`
print(Afvangsten$CPUE)

#### Grafieken ####
#Selecteer brondata voor grafieken
GRA_Brondata <- Afvangsten[c("Datum", "Location", "L00", "L0", "L1", "L2", "M1", "M2", "AM", "AV", "Totaal", "Totaal_Larven.All", "Totaal_Larven.CPUE","Aantal fuiken (Totaal)","CPUE")]



GRA_Brondata$Datum <- as.Date(GRA_Brondata$Datum,'%d-%m-%Y')
GRA_Brondata$Dag <- format(GRA_Brondata$Datum, format='%d')
GRA_Brondata$Maand <- format(GRA_Brondata$Datum, format='%m')
GRA_Brondata$Jaar <- format(GRA_Brondata$Datum, format='%Y')
GRA_Brondata$Jaar <- as.numeric(GRA_Brondata$Jaar)
GRA_Brondata$Maand <- as.numeric(GRA_Brondata$Maand)
GRA_Brondata$Dag <- as.numeric(GRA_Brondata$Dag)



GRA_Brondata$Maand2 <- ifelse(GRA_Brondata$Maand==1, "jan", 
                              ifelse(GRA_Brondata$Maand==2, "feb",
                                     ifelse(GRA_Brondata$Maand==3, "mar",
                                            ifelse(GRA_Brondata$Maand==4, "apr",
                                                   ifelse(GRA_Brondata$Maand==5, "mei",
                                                          ifelse(GRA_Brondata$Maand==6, "jun",
                                                                 ifelse(GRA_Brondata$Maand==7, "jul",
                                                                        ifelse(GRA_Brondata$Maand==8, "aug",
                                                                               ifelse(GRA_Brondata$Maand==9, "sep",
                                                                                      ifelse(GRA_Brondata$Maand==10, "okt",
                                                                                             ifelse(GRA_Brondata$Maand==11, "nov",
                                                                                                    ifelse(GRA_Brondata$Maand==12, "dec",NA))))))))))))
GRA_Brondata$Datum2 <- paste(GRA_Brondata$Dag, GRA_Brondata$Maand2, sep="/")
GRA_Brondata <- GRA_Brondata[with(GRA_Brondata, reorder(Jaar, Maand, Dag)),]
#GRA_Brondata$Datum <- format(GRA_Brondata$Datum, format= '%d-%m-%Y')

GRA_Brondata$Datum3 <- factor(GRA_Brondata$Datum2, levels = GRA_Brondata$Datum2[order(GRA_Brondata$Maand, GRA_Brondata$Dag)], ordered = TRUE)

#Foute locaties verwijderen
temp <- subset(GRA_Brondata, !is.na(Location))
Jaren <- unique(temp$Jaar)

#### Grafieken per jaar ####
#CPUE per dag
for(j in Jaren){
  temp2 <- subset(temp, Jaar == j)
  Locations <- unique(temp2$Location)
  for(i in Locations){
    temp3 <- subset(temp2, Location == i )
    temp3$Datum3 <- factor(temp3$Datum2, levels = temp3$Datum2[order(temp3$Jaar, temp3$Maand, temp3$Dag)], ordered = TRUE)
    fNaam <- paste(i,"CPUE", j, sep="_")
    fNaam <- paste(fNaam, ".jpeg", sep="")
    plot <- ggplot(temp3, aes(x=Datum3, y=CPUE)) + 
      geom_bar(stat="identity", aes(fill="red"))
    plot <- plot + ggtitle(paste("Vangst per eenheid van inspanning:", i, " ", j))
    plot <- plot + expand_limits(x = 0, y = 0)
    plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
    plot <- plot + theme(legend.position="none")
    plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
    plot <- plot + xlab("Datum")
    plot <- plot + ylab("CPUE")
    print(plot)
    ggsave(filename = fNaam, path = imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
  }
}

#absoluut per dag
for(j in Jaren){
  temp2 <- subset(temp, Jaar == j)
  Locations <- unique(temp2$Location)
  for(i in Locations){
    temp3 <- subset(temp2, Location == i )
    temp3$Datum3 <- factor(temp3$Datum2, levels = temp3$Datum2[order(temp3$Jaar, temp3$Maand, temp3$Dag)], ordered = TRUE)
    fNaam <- paste(i ,"TOT", j, sep="_")
    #fNaam <- paste("file://Afbeeldingen/", fNaam, sep="")
    fNaam <- paste(fNaam, ".jpeg", sep = "")
    plot <- ggplot(temp3, aes(x=Datum3, y=Totaal)) + geom_bar(stat="identity", aes(colour="dark grey"))
    plot <- plot + ggtitle(paste("Absolute vangsten:", i, " ", j))
    plot <- plot + expand_limits(x = 0, y = 0)
    plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
    plot <- plot + theme(legend.position="none")
    plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
    plot <- plot + xlab("Datum")
    plot <- plot + ylab("Totaal")
    print(plot)
    ggsave(fNaam, path= imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
  }
}

####Grafieken alle Jaren ####
#CPUE per dag
temp2 <- subset(GRA_Brondata, !is.na(Location))

for(i in Locations){
  temp3 <- subset(temp2, Location == i )
  o <- n_distinct(temp3$Jaar)
  if(o > 1){
    #temp3$Datum2 <- paste(temp3$Dag, temp3$Maand, temp3$Jaar, sep="/")
    temp3$Datum3 <- factor(temp3$Datum2, levels = temp3$Datum2[order(temp3$Jaar, temp3$Maand, temp3$Dag)], ordered = TRUE)
    jmax <- max(temp3$Jaar)
    jmin <- min(temp3$Jaar)
    fNaam <- paste(i,"CPUE",jmin, jmax, sep="_")
    fNaam <- paste(fNaam, ".jpeg", sep="")
    plot <- ggplot(temp3, aes(x=Datum3, y=CPUE)) + 
      geom_bar(stat="identity", aes(fill="red"))
    plot <- plot + ggtitle(paste("Vangst per eenheid van inspanning:", i, " ", jmin, "-", jmax))
    plot <- plot + expand_limits(x = 0, y = 0)
    plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
    plot <- plot + theme(legend.position="none")
    plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
    plot <- plot + xlab("Datum")
    plot <- plot + ylab("CPUE")
    plot <- plot + facet_wrap(~Jaar, scales = "free_x")
    print(plot)
    ggsave(filename = fNaam, path = imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
  }
  else{
    print(paste(i, "heeft slechts 1 jaar data"))
  }
}
#absoluut per dag
for(i in Locations){
  temp3 <- subset(temp2, Location == i )
  o <- n_distinct(temp3$Jaar)
  if(o > 1){
    #temp3$Datum2 <- paste(temp3$Dag, temp3$Maand, temp3$Jaar, sep="/")
    temp3$Datum3 <- factor(temp3$Datum2, levels = temp3$Datum2[order(temp3$Jaar, temp3$Maand, temp3$Dag)], ordered = TRUE)
    jmax <- max(temp3$Jaar)
    jmin <- min(temp3$Jaar)
    temp3$Datum3 <- factor(temp3$Datum2, levels = temp3$Datum2[order(temp3$Jaar, temp3$Maand, temp3$Dag)], ordered = TRUE)
    fNaam <- paste(i ,"TOT",jmin, jmax, sep="_")
    #fNaam <- paste("file://Afbeeldingen/", fNaam, sep="")
    fNaam <- paste(fNaam, ".jpeg", sep = "")
    plot <- ggplot(temp3, aes(x=Datum3, y=Totaal)) + geom_bar(stat="identity", aes(colour="dark grey"))
    plot <- plot + ggtitle(paste("Absolute vangsten:", i, " ", jmin, "-", jmax))
    plot <- plot + expand_limits(x = 0, y = 0)
    plot <- plot + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0,NA),expand = c(0, 0))
    plot <- plot + theme(legend.position="none")
    plot <- plot + theme(axis.line = element_line(color="black", size = 0.5))
    plot <- plot + xlab("Datum")
    plot <- plot + ylab("Totaal")
    plot <- plot + facet_wrap(~Jaar, scales = "free_x")
    print(plot)
    ggsave(fNaam, path= imagepath, width=10.5, height=5, units = c("in"), dpi = 300)
  }
  else{
    print(paste(i, "heeft slechts 1 jaar data"))}
}


#### tabel duur ####
library(chron)
#Select Data
temp_duur <- Brondata[c("Datum","Locatie","Startuur", "Einduur", "Sample_Type", "Aantal fuiken (Totaal)", "Aantal fuiken geplaatst")]

#Remove startup catches due to abnormally long duration of action.
#Arendonk 26-4-2016 & 27-4-2016
temp_duur1 <- subset(temp_duur, Locatie == "Arendonk") 
temp_duur1 <- subset(temp_duur1, Datum != "26-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Arendonk") 
temp_duur <- rbind(temp_duur1,temp_duur2)
temp_duur1 <- subset(temp_duur, Locatie == "Arendonk") 
temp_duur1 <- subset(temp_duur1, Datum != "27-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Arendonk") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Kasterlee 26-4-2016 & 27-4-2016
temp_duur1 <- subset(temp_duur, Locatie == "Kasterlee") 
temp_duur1 <- subset(temp_duur1, Datum != "26-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Kasterlee") 
temp_duur <- rbind(temp_duur1,temp_duur2)
temp_duur1 <- subset(temp_duur, Locatie == "Kasterlee") 
temp_duur1 <- subset(temp_duur1, Datum != "27-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Kasterlee") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Hoogstraten 28-4-2016 & 29-4-2016
temp_duur1 <- subset(temp_duur, Locatie == "Hoogstraten") 
temp_duur1 <- subset(temp_duur1, Datum != "28-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Hoogstraten") 
temp_duur <- rbind(temp_duur1,temp_duur2)
temp_duur1 <- subset(temp_duur, Locatie == "Hoogstraten") 
temp_duur1 <- subset(temp_duur1, Datum != "29-4-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Hoogstraten") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Nijlen 25-7-2016 & 26-7-2016
temp_duur1 <- subset(temp_duur, Locatie == "Nijlen") 
temp_duur1 <- subset(temp_duur1, Datum != "25-7-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Nijlen") 
temp_duur <- rbind(temp_duur1,temp_duur2)
temp_duur1 <- subset(temp_duur, Locatie == "Nijlen") 
temp_duur1 <- subset(temp_duur1, Datum != "26-7-2016")
temp_duur2 <- subset(temp_duur, Locatie != "Nijlen") 
temp_duur <- rbind(temp_duur1,temp_duur2)

#Hoogstraten 19-4-2017
temp_duur1 <- subset(temp_duur, Locatie == "Hoogstraten") 
temp_duur1 <- subset(temp_duur1, Datum != "19-4-2017")
temp_duur2 <- subset(temp_duur, Locatie != "Hoogstraten") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Arendonk 26-4-2017
temp_duur1 <- subset(temp_duur, Locatie == "Arendonk") 
temp_duur1 <- subset(temp_duur1, Datum != "26-4-2017")
temp_duur2 <- subset(temp_duur, Locatie != "Arendonk") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Kasterlee 26-4-2017
temp_duur1 <- subset(temp_duur, Locatie == "Kasterlee") 
temp_duur1 <- subset(temp_duur1, Datum != "26-4-2017")
temp_duur2 <- subset(temp_duur, Locatie != "Kasterlee") 
temp_duur <- rbind(temp_duur1,temp_duur2)
#Nijlen 10-5-2017
temp_duur1 <- subset(temp_duur, Locatie == "Nijlen") 
temp_duur1 <- subset(temp_duur1, Datum != "10-5-2017")
temp_duur2 <- subset(temp_duur, Locatie != "Nijlen") 
temp_duur <- rbind(temp_duur1,temp_duur2)

temp_duur$secs <- (temp_duur$Einduur-temp_duur$Startuur)
temp_duur$mins <- temp_duur$secs/60
temp_duur$hours <- temp_duur$mins/60
temp_duur$Aantal_Fuiken <- ifelse(!is.na(temp_duur$`Aantal fuiken (Totaal)`),temp_duur$`Aantal fuiken (Totaal)`, temp_duur$`Aantal fuiken geplaatst`)
temp_duur$Aantal_Fuiken <- as.numeric(temp_duur$Aantal_Fuiken)

temp_duur <- subset(temp_duur, !is.na(Aantal_Fuiken))
temp_duur$h_per_fuik <- temp_duur$hours/temp_duur$Aantal_Fuiken

tijdsconsumtie_pLoc <- ddply(temp_duur, c("Sample_Type", "Locatie"), summarise,
                             Gemiddelde = mean(h_per_fuik, na.rm=TRUE),
                             N = length(h_per_fuik),
                             sd   = sd(h_per_fuik, na.rm=TRUE),
                             max= max(h_per_fuik),
                             min= min(h_per_fuik),
                             error = qnorm(0.975)*sd/sqrt(N))

ggplot(tijdsconsumtie_pLoc, aes(x=as.factor(Sample_Type))) + 
  geom_point(aes (y=Gemiddelde), size=4) +
  geom_errorbar(aes(ymin = Gemiddelde-error, ymax = Gemiddelde+error), width=0, size=1) + 
  theme_bw() +
  facet_wrap(~Locatie)+
  #theme_inbo(13) +
  ylab("Gemiddelde (+/-fout)")+
  xlab("Sample_Type")
#ggsave(file = "./Output/Onderkaken_licentiejacht_2017_error.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi = 300)

tijdsconsumtie <- ddply(temp_duur, c("Sample_Type"), summarise,
                        Gemiddelde = mean(h_per_fuik, na.rm=TRUE),
                        N = length(h_per_fuik),
                        sd   = sd(h_per_fuik, na.rm=TRUE),
                        max= max(h_per_fuik),
                        min= min(h_per_fuik),
                        error = qnorm(0.975)*sd/sqrt(N))

ggplot(tijdsconsumtie, aes(x=as.factor(Sample_Type))) + 
  geom_point(aes (y=Gemiddelde), size=4) +
  geom_errorbar(aes(ymin = Gemiddelde-error, ymax = Gemiddelde+error), width=0, size=1) + 
  theme_bw() +
  #theme_inbo(13) +
  ylab("Gemiddelde (+/-fout)")+
  xlab("Sample_Type")

ggplot(temp_duur, aes(x=Aantal_Fuiken, y=hours, shape = factor(temp_duur$Locatie)))+
  geom_point()+
  facet_wrap(~Sample_Type)+
  
  
  
  #Succes => Voorlopig niet
  temp_succes <- GRA_Brondata
temp_succes$Totaal <- ifelse(!is.na(temp_succes$Totaal),temp_succes$Totaal,0)
Locations <- unique(temp_succes$Location)
temp3 <- data.frame()
temp2 <- data.frame(x)
for(l in Locations){
  temp <- subset(temp_succes, Location == l)
  temp2$Totaal <- sum(temp$Totaal)
  temp2$Location <- l
  temp2$m_CPUE <- mean(temp$CPUE)
  temp3 <- rbind(temp3, temp2)
}

Succesrate <- temp3
Succesrate$t_vangst <- Succesrate$m_CPUE/Succesrate$Totaal
t_vangst <- subset(Succesrate, !is.na(t_vangst))
print("Aantal eenheden van inspanning gemiddeld nodig voor het detecteren van 1 individu")
print(m_t_vangst <- mean(t_vangst$t_vangst))
temp_UE <- subset(tijdsconsumtie, Sample_Type == c("Plaatsen van fuiken", "Afvangst"))
temp_UE$m_hours <- as.numeric(temp_UE$m_hours)
TPUE <- sum(temp_UE$m_hours)

#### Bereken GSL ####

Jaren <- unique(GRA_Brondata$Jaar)

remove(temp6)
temp6 <- data.frame()
GRA_Brondata2 <- subset(GRA_Brondata, !is.na(Location))


for (a in Jaren){
  temp6 <- data.frame()
  temp <- subset(GRA_Brondata2, Jaar == a) #=> Opgedeeld per jaar
  Locations <- unique(temp$Location)
  GSL <- data.frame(X = Locations)
  Locations <- sort(Locations)
  for(i in Locations){
    temp2 <- subset(temp, Location == i ) #=> Opgedeeld per locatie
    number <- count(temp2)                #=> #records
    print(i)
    print(number)
    temp3 <- head(temp2, n=1)             #=> Eerste record
    temp7 <- tail(temp2, n=1)
    temp5 <- data.frame(x=1)
    if(number>=3){
      temp4 <- tail(temp2, n=3)
      temp5$MeanCPUE <- mean(temp4$CPUE)
    }else{
      temp4 <- temp2
      temp5$MeanCPUE <- mean(temp4$CPUE)
    }
    temp5$LastCapture <- temp7$Totaal
    temp5$Datum_LastCapture <- temp7$Datum
    temp5$LastCapture <- ifelse(!is.na(temp5$LastCapture), temp5$LastCapture, 0)
    temp5$L00 <- ifelse(!is.na(temp7$L00), temp7$L00, 0)
    temp7$M1 <- ifelse(is.na(temp7$M1), 0, temp7$M1)
    temp7$M2 <- ifelse(is.na(temp7$M2), 0, temp7$M2)
    temp7$AM <- ifelse(is.na(temp7$AM), 0, temp7$AM)
    temp7$AV <- ifelse(is.na(temp7$AV), 0, temp7$AV)
    temp5$PostMetamorf2 <- temp7$M1 + temp7$M2 + temp7$AM + temp7$AV
    temp5$Location <- i
    temp5$StartCPUE <- temp3$CPUE
    temp5$Uitgevoerd <- number$n
    if(is.na(temp5$MeanCPUE)){
      print(i)
      print(temp4)
      break}
    temp6 <- rbind(temp6, temp5)
  }
  
  temp6$x <- NULL
  GSL <- temp6
  
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
  
  #Juiste kolommen kiezen
  GSL <- GSL[,c("Location","StartCPUE", "StartGSL", "MinVangst_Start", "MaxVangst_Start", "Uitgevoerd", "Resterend_Min", "Resterend_Max", "LastCapture","Datum_LastCapture", "L00", "PostMetamorf2","MeanCPUE", "HuidigGSL", "MinDoelBereikt", "MaxDoelBereikt", "MinVangst_Huidig", "MaxVangst_Huidig")]
  
  GSL$x <- NULL
  
  
  GSLfNaam <- paste("Geschat startaantal larven",a, sep="_")
  GSLfNaama <- paste(imagepath, GSLfNaam, sep="/" )
  GSLfNaama <- paste(GSLfNaama, ".csv", sep="")
  
  #Uitvoeren
  #test <- gs_new(title = "Geschat startaantal larven", ws_title = "GSL", input = GSL, trim = TRUE)
  #file.create(file=GSLfNaama, overwrite=T)
  write.csv(GSL, GSLfNaama)
  print(GSLfNaama)
}