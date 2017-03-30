melanomendata <- function(directory) {
  
  if (directory == "MelanomenData") {
    setwd("/Users/lukas/Documents/Uni Maas/Medicine/Jaar  6/Onderzoek_Dermatologie/MelanomenData/")
    library(gdata)
    print("Getting the results of the 2015-2016 Databases")
    melanomen2015 <- read.xls("2015.xlsx", method = "tab", na.strings = "-") 
    melanomen2016 <- read.xls("2016.xlsx", method = "tab", na.strings = "-")
  } else {
    print("Directory not found")
  }
  
  listmean2015 <- lapply(melanomen2015[,14:22], mean, na.rm = TRUE)
  listmean2016 <- lapply(melanomen2016[,14:22], mean, na.rm = TRUE)
  print("2015 Data Mean")
  str(listmean2015, digits.d = 4)
  print("2016 Data Mean")
  str(listmean2016, digits.d = 4)
  listsd2015 <- lapply(melanomen2015[,14:22], sd, na.rm = TRUE)
  listsd2016 <- lapply(melanomen2016[,14:22], sd, na.rm = TRUE)
  print("2015 Data SD")
  str(listsd2015, digits.d = 4)
  print("2016 Data SD")
  str(listsd2016, digits.d = 4)
  
  ## str(listsd)
  ## print(round(listsd$Toegangstijd, digits = 2))
  ## allttests <- list()
  
  ## shapirolist <- list(shapiro2016)
  ## print(shapirolist)
  
  testresult <- list()
  
  for (i in 14:22) {
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
    
  ## hist(melanomen2016[,17], main = "Toegangstijd tot 1e afspraak", xlab = "Aantal", ylab = "Tijd")
  
  ## head(allttests)
}