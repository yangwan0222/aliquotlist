####### Aliquot list, ADNI only (part1) #######

library('stringr')

d <- read.csv("C://Users//wanya//Desktop//aliquotlist//results.csv")

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
    
    if("PLA" %in% t1$Derivative)
    {
      t2[1,3] <- nrow(t1[t1$Derivative == "PLA",])
      t2[1,4] <- t1[t1$Global.Specimen.ID == str_replace(t1[t1$Derivative == "PLA",]$Global.Specimen.ID[1],'-01','-00'),]$Available.Volume
      t2[1,5] <- sum(grepl("sent|shipped",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,6] <- sum(grepl("used|hcys",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "PLA",]$Specimen.Comments,ignore.case = TRUE))
      # regexpr(" used ", t1[t1$Derivative == "PLA",]$Specimen.Comments) > 0
    } else {
      t2[1,3] <- 0
      t2[1,4] <- 0
      t2[1,5] <- 0
      t2[1,6] <- 0
    }
    
    if("SER" %in% t1$Derivative)
    {
      t2[1,7] <- nrow(t1[t1$Derivative == "SER",])
      t2[1,8] <- t1[t1$Global.Specimen.ID == str_replace(t1[t1$Derivative == "SER",]$Global.Specimen.ID[1],'-01','-00'),]$Available.Volume
      t2[1,9] <- sum(grepl("sent|shipped",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,10] <- sum(grepl("used",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "SER",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,7] <- 0
      t2[1,8] <- 0
      t2[1,9] <- 0
      t2[1,10] <- 0
    }
    
    if("CSF" %in% t1$Derivative)
    {
      t2[1,11] <- nrow(t1[t1$Derivative == "CSF",])
      t2[1,12] <- t1[t1$Global.Specimen.ID == str_replace(t1[t1$Derivative == "CSF",]$Global.Specimen.ID[1],'-01','-00'),]$Available.Volume
      t2[1,13] <- sum(grepl("sent|shipped",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,14] <- sum(grepl("used",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "CSF",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,11] <- 0
      t2[1,12] <- 0
      t2[1,13] <- 0
      t2[1,14] <- 0
    }
    
    if("URN" %in% t1$Derivative)
    {
      t2[1,15] <- nrow(t1[t1$Derivative == "URN",])
      t2[1,16] <- t1[t1$Global.Specimen.ID == str_replace(t1[t1$Derivative == "URN",]$Global.Specimen.ID[1],'-01','-00'),]$Available.Volume
      t2[1,17] <- sum(grepl("sent|shipped",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE))
      t2[1,18] <- sum(grepl("used",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE)-
                       grepl("unused",t1[t1$Derivative == "URN",]$Specimen.Comments,ignore.case = TRUE))
    } else {
      t2[1,15] <- 0
      t2[1,16] <- 0
      t2[1,17] <- 0
      t2[1,18] <- 0
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


