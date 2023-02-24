####### Aliquot list, ADNI only (part1) #######

library('stringr')
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//aliquotlist//New folder//results.csv")
d$Global.Specimen.ID <- substr(d$Global.Specimen.ID,1,nchar(d$Global.Specimen.ID)-3)
d1 <- unique(d[,c(5,9)])
d1 <- d1[d1$Derivative=="BLD",]

for(i in seq(nrow(d1)))
{
  d <- d[!grepl(d1[i,2], d$Global.Specimen.ID),]
}
rm(d1)

d1 <- data.frame(matrix(nrow = 0, ncol = 22))

for(i in seq(length(unique(d$ID1))))
{
  t <- d[d$ID1 == unique(d$ID)[i],]
  for(j in seq(length(unique(t$Collection.Date))))
  {
    t1 <- t[t$Collection.Date == unique(t$Collection.Date)[j],]
    t2 <- data.frame(matrix(nrow = 0, ncol = 22))
    t2[1,1] <- unique(t1$ID1)
    t2[1,2] <- unique(t1$Collection.Date)
    
    if(nrow(t1[t1$Primary == "BLD" & t1$Additive == "EDT",])>0)
    {
      if(nrow(t1[t1$Primary=="BLD" & t1$Additive=="EDT",])==1)
      {
        t2[1,3] <- floor(t1[t1$Primary=="BLD" & t1$Additive=="EDT",]$Available.Volume/0.5)
      } else {
        t2[1,3] <- nrow(t1[t1$Primary=="BLD" & t1$Additive=="EDT" & t1$Derivative == "PLA",]) 
      }
      t2[1,4] <- max(t1[t1$Primary=="BLD" & t1$Additive=="EDT",]$Available.Volume)
      t2[1,5] <- sum((t1[t1$Derivative == "PLA",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,6] <- sum(grepl("used|hcys|isoprostane|rest|pool",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE) - grepl("wrong label",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,19] <- sum(grepl("disappear|dispose|missing",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE))
      # regexpr(" used ", t1[t1$Derivative == "PLA",]$Specimen.Comments) > 0
    } else {
      t2[1,3:6] <- 0
    }
    
    if(nrow(t1[t1$Primary == "BLD" & t1$Additive == "NON",])>0)
    {
      if(nrow(t1[t1$Primary=="BLD" & t1$Additive=="NON",])==1)
      {
        t2[1,7] <- floor(t1[t1$Primary=="BLD" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,7] <- nrow(t1[t1$Primary=="BLD" & t1$Additive=="NON" & t1$Derivative == "SER",])
      }
      t2[1,8] <- max(t1[t1$Primary=="BLD" & t1$Additive=="NON",]$Available.Volume)
      t2[1,9] <- sum((t1[t1$Derivative == "SER",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,10] <- sum(grepl("used|isoprostane|rest|pool",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE) - grepl("wrong label",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,20] <- sum(grepl("disappear|dispose|missing",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,7:10] <- 0
    }
    
    if(nrow(t1[t1$Primary == "CSF" & t1$Additive == "NON",])>0)
    {
      if(nrow(t1[t1$Primary=="CSF" & t1$Additive=="NON",])==1)
      {
        t2[1,11] <- floor(t1[t1$Primary=="CSF" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,11] <- nrow(t1[t1$Primary=="CSF" & t1$Additive=="NON" & t1$Derivative == "CSF",])
      }
      t2[1,12] <- max(t1[t1$Primary=="CSF" & t1$Additive=="NON",]$Available.Volume)
      t2[1,13] <- sum((t1[t1$Derivative == "CSF",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,14] <- sum(grepl("used|isoprostane|rest|pool",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE) - grepl("wrong label",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,21] <- sum(grepl("disappear|dispose|missing",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,11:14] <- 0
    }
    
    if(nrow(t1[t1$Primary == "URN" & t1$Additive == "NON",])>0)
    {
      if(nrow(t1[t1$Primary=="URN" & t1$Additive=="NON",])==1)
      {
        t2[1,15] <- floor(t1[t1$Primary=="URN" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,15] <- nrow(t1[t1$Primary=="URN" & t1$Additive=="NON" & t1$Derivative == "URN",])
      }
      t2[1,16] <- max(t1[t1$Primary=="URN" & t1$Additive=="NON",]$Available.Volume)
      t2[1,17] <- sum((t1[t1$Derivative == "URN",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,18] <- sum(grepl("used|isoprostane|rest|pool",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE) - grepl("wrong label",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,22] <- sum(grepl("disappear|dispose|missing",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,15:18] <- 0
    }
    if(sum(t2[3:18])!=0)
    {
      d1 <- rbind(d1,t2)
    }
  }
}

names(d1) <- c("RID","CollectDate",
               "PLA","PLAVOL","PLA_SENT","PLA_USED",
               "SER","SERVOL","SER_SENT","SER_USED",
               "CSF","CSFVOL","CSF_SENT","CSF_USED",
               "URN","URNVOL","URN_SENT","URN_USED",
               "PLA_missing","SER_missing","CSF_missing","URN_missing")

d1[d1$PLA == 1,]$PLA <- d1[d1$PLA == 1,]$PLAVOL/0.5
d1[d1$SER == 1,]$SER <- d1[d1$SER == 1,]$SERVOL/0.5
d1[d1$URN == 1,]$URN <- d1[d1$URN == 1,]$URNVOL/0.5
d1[is.na(d1$PLA_missing),]$PLA_missing <- 0
d1[is.na(d1$SER_missing),]$SER_missing <- 0
d1[is.na(d1$CSF_missing),]$CSF_missing <- 0
d1[is.na(d1$URN_missing),]$URN_missing <- 0
d1$PLA <- d1$PLA-d1$PLA_SENT-d1$PLA_USED-d1$PLA_missing
d1$SER <- d1$SER-d1$SER_SENT-d1$SER_USED-d1$SER_missing
d1$CSF <- d1$CSF-d1$CSF_SENT-d1$CSF_USED-d1$CSF_missing
d1$URN <- d1$URN-d1$URN_SENT-d1$URN_USED-d1$URN_missing

d1 <- read.csv("C://Users//wanya//Desktop//aliquotlist//New folder//results_v1.csv")

d2 <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")[,c(3:8)]
d2 <- d2[d2$TABLE == "biomark",]
d2$EXAMDATE <- mdy(d2$EXAMDATE)
d1$CollectDate <- dmy(d1$CollectDate)

for(i in seq(nrow(d1)))
{
  t <- d2[d2$RID==d1$RID[i],]
  if(nrow(t)==0)
  {
    d1$VISCODE[i] <- NA
    d1$VISCODE2[i] <- NA
    next
  }
  
  t$diff <- abs(t$EXAMDATE-d1$CollectDate[i])
  if(min(t$diff)>90)
  {
    d1$VISCODE[i] <- NA
    d1$VISCODE2[i] <- NA
    next
  } else {
    t <- t[t$diff == min(t$diff),]
  }
  
  if(nrow(t)==1)
  {
    d1$VISCODE[i] <- t$VISCODE
    d1$VISCODE2[i] <- t$VISCODE2
  } else if ("init" %in% t$VISCODE) {
    t <- t[t$VISCODE == "init",]
    d1$VISCODE[i] <- t$VISCODE[1]
    d1$VISCODE2[i] <- t$VISCODE2[1]
  } else {
    d1$VISCODE[i] <- t$VISCODE[1]
    d1$VISCODE2[i] <- t$VISCODE2[1]
  }
}



