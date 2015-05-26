#make dummmy variable
j <- 1
n <- 1
c <- 1
x <- 2
#create matrix to populate
matrix3 <- matrix(ncol=3, nrow=1144)
matrix5 <- data.frame(matrix3)
#convert country factor into a string
i <- sapply(Just.HDI, is.factor)
Just.HDI[i] <- lapply(Just.HDI[i], as.character)
for(n in 1:187)
{
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,2]
  matrix5[j,3] <- 1990
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,3]
  matrix5[j,3] <- 2000
  j <- j +1  
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,4]
  matrix5[j,3] <- 2005
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,5]
  matrix5[j,3] <- 2008
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,6]
  matrix5[j,3] <- 2010
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,7]
  matrix5[j,3] <- 2011
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,8]
  matrix5[j,3] <- 2012
  j <- j +1
  matrix5[j,1] <- Just.HDI[n,1]
  matrix5[j,2] <- Just.HDI[n,9]
  matrix5[j,3] <- 2013
  j <- j +1
}
#now let's delete the stuff in the other data set
GDP <- UNdata_Export_20150525_182847080
GDP$Year <- as.numeric(GDP$Year)
i <- sapply(GDP, is.factor)
GDP[i] <- lapply(GDP[i], as.character)
fetcher <- function(x){
  y <- GDP$Year&&GDP$Country.or.Area&&GDP$Value[which(match(GDP$Country.or.Area,x)==TRUE)]
  return(y)
}
sapply(matrix5$X1,function(x) fetcher(x))
write.csv(matrix5, file = "matrix.csv", row.names = FALSE)
