melanomendata <- function(directory) {
  
  if (directory == "MelanomenData") {
    setwd("/Users/lukas/Documents/Uni Maas/Medicine/Jaar  6/Onderzoek_Dermatologie/MelanomenData/")
    library(gdata)
    library(plyr)
    print("Getting the results of the 2015-2016 Databases")
    melanomen2015 <- read.xls("2015.xlsx", method = "tab", na.strings = "-") 
    melanomen2016 <- read.xls("2016.xlsx", method = "tab", na.strings = "-")
  } else {
    print("Directory not found")
  }
  
  ## calculation n for 2015
  nna <- !is.na(melanomen2015[,14])
  n <- sum(nna == TRUE)
  print(n)
  ## calculation 2015 mean
  listmean2015 <- lapply(melanomen2015[,c(14:22,27)], mean, na.rm = TRUE)
  print("2015 Data Mean")
  str(listmean2015, digits.d = 4)
  
  ## calculation 2016 mean
  listmean2016 <- lapply(melanomen2016[,c(14:22,27)], mean, na.rm = TRUE)
  print("2016 Data Mean")
  str(listmean2016, digits.d = 4)
  
  ## calculation 2015 sd
  listsd2015 <- lapply(melanomen2015[,c(14:22,27)], sd, na.rm = TRUE)
  print("2015 Data SD")
  str(listsd2015, digits.d = 4)
  
  ## calculation 2016 sd
  listsd2016 <- lapply(melanomen2016[,c(14:22,27)], sd, na.rm = TRUE)
  print("2016 Data SD")
  str(listsd2016, digits.d = 4)
  
  ## outcomes for functions
  meltable15 <- table(melanomen2015[,25])
  names(meltable15) <- c("Männer", "Frauen")
  frequencies <- prop.table(meltable15)
  print("Prozentsätze, Männer und Frauen in der 2015 Studie:")
  print(frequencies)
  ## manwomen <- summary(melanomen2015[,25], na.rm = TRUE)
  ## print(manwomen)
  
  meltable16 <- table(melanomen2016[,25])
  names(meltable16) <- c("Männer", "Frauen")
  frequencies <- prop.table(meltable16)
  print("Prozentsätze, Männer und Frauen in der 2016 Studie:")
  print(frequencies)
  
  ## 2015 TNM tumor stadium
  tnmtable15 <- table(melanomen2015[,26])
  print(tnmtable15)
  
  ## 2016 TNM tumor stadium
  tnmtable16 <- table(melanomen2016[,26])
  print(tnmtable16)
  
  
  ## list to cache outcome of the loops
  print("Die Ergebnisse der statistischen Tests:")
  testresult <- list()
  
  for (i in c(14:16, 18:22)) {
    shapiro2016 <- shapiro.test(melanomen2016[,i])
    shapiro2016p <- shapiro2016$p.value
    shapiro2015 <- shapiro.test(melanomen2015[,i])
    shapiro2015p <- shapiro2015$p.value
    hist(melanomen2015[,i])
    hist(melanomen2016[,i])
    
    if (shapiro2016p > 0.05 & shapiro2015p > 0.05) {
      testresult[[i]] <- t.test(melanomen2015[,i], melanomen2016[,i], na.rm = TRUE)
    } else {
      testresult[[i]] <- wilcox.test(melanomen2015[,i], melanomen2016[,i], na.rm = TRUE, conf.int = TRUE, exact = FALSE)
    }

  }
  
  nulltest <- Filter(Negate(is.null), testresult)
  print(nulltest)
    
  ## Einseitiger Wilcox rank sum test 2015 toegangstijden
  ttrichtlijn2015 <- wilcox.test(melanomen2015[,14], alternative = "less", mu = 14)
  print("2015 resultat vergleich mit landelijker richtlijn")
  print(ttrichtlijn2015)
  
  ## Einseitiger Wilcox rank sum test 2016 toegangstijden
  ttrichtlijn2016 <- wilcox.test(melanomen2016[,14], alternative = "less", mu = 14, exact = FALSE)
  print("2016 resultat vergleich mit landelijker richtlijn")
  print(ttrichtlijn2016)
  
  ## Einseitiger Wilcox rank sum test 2015
  ugrichtlijn2015 <- wilcox.test(melanomen2015[,19], alternative = "less", mu = 14)
  print("2015 resultat vergleich mit landelijker richtlijn doorlooptijden uitslaggesprek")
  print(ugrichtlijn2015)
  
  ## Einseitiger Wilcox rank sum test 2016
  ugrichtlijn2016 <- wilcox.test(melanomen2016[,19], alternative = "less", mu = 14, exact = FALSE)
  print("2016 resultat vergleich mit landelijker richtlijn doorlooptijden uitslaggesprek")
  print(ugrichtlijn2016)
  
  ## Einseitiger Wilcox rank sum test 2015 tot therapeutische excisie
  exrichtlijn2015 <- wilcox.test(melanomen2015[,21], alternative = "less", mu = 42)
  print("2015 resultat vergleich mit landelijker richtlijn doorlooptijden uitslaggesprek")
  print(exrichtlijn2015)
  
  ## Einseitiger Wilcox rank sum test 2016 tot therapeutische excisie
  exrichtlijn2016 <- wilcox.test(melanomen2016[,21], alternative = "less", mu = 42, exact = FALSE)
  print("2016 resultat vergleich mit landelijker richtlijn doorlooptijden uitslaggesprek")
  print(exrichtlijn2016)
  
  ## einseitiger t-test für de vergleich der SN prozedur 2015
  snrichtlijn2015 <- t.test(melanomen2015[,22], alternative = "less", mu = 42)
  print("2016 resultat vergleich mit landelijker richtlijn doorlooptijden sn")
  print(snrichtlijn2015)
  
  ## einseitiger t-test für de vergleich der SN prozedur 2016
  snrichtlijn2016 <- t.test(melanomen2016[,22], alternative = "less", mu = 42)
  print("2016 resultat vergleich mit landelijker richtlijn doorlooptijden sn")
  print(snrichtlijn2016)
}







