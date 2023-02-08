####### Aliquot list, ADNI only (part1) #######

library('stringr')

d <- read.csv("C://Users//wanya//Desktop//aliquotlist//results.csv")
d$Global.Specimen.ID <- substr(d$Global.Specimen.ID,1,nchar(d$Global.Specimen.ID)-3)
d1 <- unique(d[,c(5,9)])

for(i in seq(nrow(d1)))
{
  if(d1$Derivative[i]=="BLD")
  {
    d <- d[!grepl(d1[i,2], d$Global.Specimen.ID),]
  }
}
rm(d1)

d1 <- data.frame(matrix(nrow = 0, ncol = 18))

for(i in seq(length(unique(d$ID1))))
{
  t <- d[d$ID1 == unique(d$ID)[i],]
  for(j in seq(length(unique(t$Collection.Date))))
  {
    t1 <- t[t$Collection.Date == unique(t$Collection.Date)[j],]
    t2 <- data.frame(matrix(nrow = 0, ncol = 18))
    t2[1,1] <- unique(t1$ID1)
    t2[1,2] <- unique(t1$Collection.Date)
    
    if("BLD" %in% t1$Primary & "EDT" %in% t1$Additive)
    {
      if(nrow(t1[t1$Primary=="BLD" & t1$Additive=="EDT",])==1)
      {
        t2[1,3] <- floor(t1[t1$Primary=="BLD" & t1$Additive=="EDT",]$Available.Volume/0.5)
      } else {
        t2[1,3] <- nrow(t1[t1$Primary=="BLD" & t1$Additive=="EDT" & t1$Derivative == "PLA",])
      }
      t2[1,4] <- max(t1[t1$Primary=="BLD" & t1$Additive=="EDT",]$Available.Volume)
      t2[1,5] <- sum((t1[t1$Derivative == "PLA",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,6] <- sum(grepl("used|hcys",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE))
      # regexpr(" used ", t1[t1$Derivative == "PLA",]$Specimen.Comments) > 0
    } else {
      t2[1,3:6] <- 0
    }
    
    if("BLD" %in% t1$Primary & "NON" %in% t1$Additive)
    {
      if(nrow(t1[t1$Primary=="BLD" & t1$Additive=="NON",])==1)
      {
        t2[1,7] <- floor(t1[t1$Primary=="BLD" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,7] <- nrow(t1[t1$Primary=="BLD" & t1$Additive=="NON" & t1$Derivative == "SER",])
      }
      t2[1,8] <- max(t1[t1$Primary=="BLD" & t1$Additive=="NON",]$Available.Volume)
      t2[1,9] <- sum((t1[t1$Derivative == "SER",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,10] <- sum(grepl("used",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,7:10] <- 0
    }
    
    if("CSF" %in% t1$Primary & "NON" %in% t1$Additive)
    {
      if(nrow(t1[t1$Primary=="CSF" & t1$Additive=="NON",])==1)
      {
        t2[1,11] <- floor(t1[t1$Primary=="CSF" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,11] <- nrow(t1[t1$Primary=="CSF" & t1$Additive=="NON" & t1$Derivative == "CSF",])
      }
      t2[1,12] <- max(t1[t1$Primary=="CSF" & t1$Additive=="NON",]$Available.Volume)
      t2[1,13] <- sum((t1[t1$Derivative == "CSF",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,14] <- sum(grepl("used",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,11:14] <- 0
    }
    
    if("URN" %in% t1$Primary & "NON" %in% t1$Additive)
    {
      if(nrow(t1[t1$Primary=="URN" & t1$Additive=="NON",])==1)
      {
        t2[1,15] <- floor(t1[t1$Primary=="URN" & t1$Additive=="NON",]$Available.Volume/0.5)
      } else {
        t2[1,15] <- nrow(t1[t1$Primary=="URN" & t1$Additive=="NON" & t1$Derivative == "URN",])
      }
      t2[1,16] <- max(t1[t1$Primary=="URN" & t1$Additive=="NON",]$Available.Volume)
      t2[1,17] <- sum((t1[t1$Derivative == "URN",]$Has.Shipped == "Yes") | grepl("sent|shipped",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE) == TRUE)
      t2[1,18] <- sum(grepl("used",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE))
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
               "URN","URNVOL","URN_SENT","URN_USED")


