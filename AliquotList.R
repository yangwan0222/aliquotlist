####### Aliquot list, ADNI only #######

d <- read.csv("C://Users//wanya//Desktop//aliquotlist//results.csv")

d1 <- data.frame(matrix(nrow = 1, ncol = 18))

names(d1) <- c("RID","CollectDate",
               "PLA","PLAVOL","PLA_SENT","PLA_USED",
               "SER","SERVOL","SER_SENT","SER_USED",
               "CSF","CSFVOL","CSF_SENT","CSF_USED",
               "URN","URNVOL","URN_SENT","URN_USED")

for(i in seq(length(unique(d$ID1))))
{
  t <- d[d$ID1 == unique(d$ID)[i],]
  for(j in seq(length(unique(t$Collection.Date))))
  {
    t1 <- t[t$Collection.Date == unique(t$Collection.Date)[i],]
    
  }
}

