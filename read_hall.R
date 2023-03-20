# Create .csv files for Hall projective planes and their duals

# Paul's Suggestion for Encoding:
# Assuming that the file name has a ".csv" extension,
# the numbering for points and lines will commence at 1.
# Assuming that the file name has a ".txt" extension,
# the numbering for points and lines will commence at 0.
hall.read <- function(fname){
  if (substr(fname,nchar(fname)-2,nchar(fname)) == "txt"){
    df <- read.csv(fname,header = FALSE,sep = " ",colClasses = "numeric")
    df <- df+1
  }
  if (substr(fname,nchar(fname)-2,nchar(fname)) == "csv"){
    df <- read.csv(fname)
  }
  colnames(df) <- NULL
  m <- as.matrix(df)
  return (m)
}
#df <- hall.read("hall_d.txt");head(df)

#hall.write <- function(df,fname){
  #write.csv(df,fname,row.names = FALSE)
#}

#hall.write(df,"hall_d.csv")


hall.dualize <- function(df){
  m <- vector("list",91)
  for (i in (1:91)){
    for(j in (1:10)){
      k <- df[i,j]
      m[[k]] <- c(m[[k]],i)
    }
  }
  dfdual <- matrix(nrow = 91, ncol = 10)
  for (i in (1:91)){
    dfdual[i,] <- m[[i]]
  }
  return(dfdual)
}
dfd <- hall.dualize(df);head(dfd)

hall.check <- function(df) {
  v <- vector("list",0)
  for (i in (1:90)){
    for (j in ((i+1):91)) {
      if (length(intersect(df[i,],df[j,])) != 1){
        v$result <- FALSE;v$i <- i; v$j <- j
        return(v)
      }
    }
  }
  v$result <- TRUE;v$i <- 0; v$j <- 0
  return(v)
}
hall.check(df)  
hall.check(dfd)

#These use the four systems listed in Hall's 1943 article
#Addition table for the elementary abelian group of order 9
addtbl1 <- matrix(rbind(c(0,1,2,3,4,5,6,7,8),
                        c(1,2,0,4,5,3,7,8,6),
                        c(2,0,1,5,3,4,8,6,7),
                        c(3,4,5,6,7,8,0,1,2),
                        c(4,5,3,7,8,6,1,2,0),
                        c(5,3,4,8,6,7,2,0,1),
                        c(6,7,8,0,1,2,3,4,5),
                        c(7,8,6,1,2,0,4,5,3),
                        c(8,6,7,2,0,1,5,3,4)),nrow=9)

#Various multiplication tables for rings of order 9
#System R from Hall, page 273
pen00R <- matrix(rbind(c(0,0,0,0,0,0,0,0,0),
                       c(0,1,2,3,4,5,6,7,8),
                       c(0,2,1,6,8,7,3,5,4),
                       c(0,3,6,2,7,4,1,8,5),
                       c(0,4,8,5,2,6,7,3,1),
                       c(0,5,7,8,3,2,4,1,6),
                       c(0,6,3,1,5,8,2,4,7),
                       c(0,7,5,4,6,1,8,2,3),
                       c(0,8,4,7,1,3,5,6,2)),
                 nrow=9)

pen00S <- matrix(rbind(c(0,0,0,0,0,0,0,0,0),
                       c(0,1,2,3,4,5,6,7,8),
                       c(0,2,1,6,8,7,3,5,4),
                       c(0,3,6,4,1,8,5,2,7),
                       c(0,4,8,7,5,1,2,6,3),
                       c(0,5,7,1,6,3,8,4,2),
                       c(0,6,3,8,2,4,7,1,5),
                       c(0,7,5,2,3,6,4,8,1),
                       c(0,8,4,5,7,2,1,3,6)),
                 nrow=9)

pen00T <- matrix(rbind(c(0,0,0,0,0,0,0,0,0),
                       c(0,1,2,3,4,5,6,7,8),
                       c(0,2,1,6,8,7,3,5,4),
                       c(0,3,6,7,5,1,8,4,2),
                       c(0,4,8,1,6,3,5,2,7),
                       c(0,5,7,4,1,8,2,6,3),
                       c(0,6,3,5,7,2,4,8,1),
                       c(0,7,5,8,2,4,1,3,6),
                       c(0,8,4,2,3,6,7,1,5)),
                 nrow=9)

pen00U <- matrix(rbind(c(0,0,0,0,0,0,0,0,0),
                       c(0,1,2,3,4,5,6,7,8),
                       c(0,2,1,6,8,7,3,5,4),
                       c(0,3,7,5,1,6,4,8,2),
                       c(0,4,6,8,5,2,1,3,7),
                       c(0,5,8,2,6,4,7,1,3),
                       c(0,6,5,7,2,3,8,4,1),
                       c(0,7,4,1,3,8,5,2,6),
                       c(0,8,3,4,7,1,2,6,5)),
                 nrow=9)

hall.make.incidenceTable <- function(multbl, addtbl = addtbl1) {
  header <- c(1,10,19,28,37,46,55,64,73,82)
  m <- matrix(nrow = 91, ncol = 10)
  #First make the affine plane
  for (j in 0:8)
    m[j+1,1:9] <- (9*j+1):(9*j+9)
  for (i in 0:8) {
    for (j in 0:8){
      for (k in 1:9){
        m[10+9*i+j,k] <- header[k]+addtbl[multbl[i+1,k]+1,j+1]
      }
    }
  }
  #Add a point at infinity to each row
  for(i in 0:9)
    for(j in 1:9)
      m[9*i+j,10] <- 82+i
  #Add the line at infinity
  m[91,] <- 82:91
  return (m)
}

df <- hall.make.incidenceTable(pen00R,addtbl1);head(df)
hall.check(df)
hall.write(df,"hall_r.csv")

df <- hall.make.incidenceTable(pen00S,addtbl1);head(df)
hall.check(df)
hall.write(df,"hall_s.csv")

df <- hall.make.incidenceTable(pen00T,addtbl1);head(df)
hall.check(df)
hall.write(df,"hall_t.csv")

df <- hall.make.incidenceTable(pen00U,addtbl1);head(df)
hall.check(df)
hall.write(df,"hall_u.csv")
