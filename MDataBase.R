melanomendata <- function(directory, accesstime = 1, doorlooptijd = 1) {
  
  if (directory == "MelanomenData") {
    setwd("/Users/lukas/Documents/Uni Maas/Medicine/Jaar  6/Onderzoek_Dermatologie/MelanomenData/")
    print("Getting the results of the 2016 Database")
    melanomen2016 <- read.xls("2016.xls", method = "tab", na.strings = "-")      
  } else {
    print("Directory not found")
  }
  ## lt_mean <- list()
  ## lt_sd <- list()
  
  if (accesstime == "toegangstijd") {
      accesstime <- melanomen2016[,14]
      a_mean <- mean(accesstime, na.rm = TRUE)
      a_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
      print(c("De toegangstijd is gemiddeld:", a_mean, "dagen"))
      print(c("De standard deviatie van de toegangstijd is:", a_sd, "dagen"))
  } 
  
  if (doorlooptijd == "diagn excisie") {
    accesstime <- melanomen2016[,15]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "histol uitslag") {
    accesstime <- melanomen2016[,16]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "radiol onderzoek") {
    accesstime <- melanomen2016[,17]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "nucl onderzoek") {
    accesstime <- melanomen2016[,18]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "uitslaggesprek") {
    accesstime <- melanomen2016[,19]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "chirurg") {
    accesstime <- melanomen2016[,20]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "ther excisie") {
    accesstime <- melanomen2016[,21]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  if (doorlooptijd == "SN procedure") {
    accesstime <- melanomen2016[,22]
    lt_mean <- mean(accesstime, na.rm = TRUE)
    lt_sd <- round(sd(accesstime, na.rm = TRUE), digits = 3)
    print(c("De doorlooptijd is gemiddeld:", lt_mean, "dagen"))
    print(c(" De standarddeviattie van de doorlooptijd is:", lt_sd, "dagen"))
  } 
  
  ## melanomen2016
}