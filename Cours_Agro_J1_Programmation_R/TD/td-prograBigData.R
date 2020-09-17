install.packages("roxygen2")
install.packages("devtools")
install.packages("microbenchmark")


#require(roxygen2)
#require(devtools)
require(microbenchmark)

setwd("~/M2/Cours_Agro_J1_Programmation_R/TD")


factFor = function(n){
  x = 1
  for (k in 2:n){
    x = k * x 
  }
  return(x)
}


factWhile = function(n){
  k=1
  res = 1
  while (k != n){
    k = k+1
    res = res * k
  } 
  return (res)
}


factReapeat = function(n){
  k = 1 
  res = 1 
  repeat{
    k = k+1 
    res = res * k 
    if (k == n){
      break
    }
  }
  return(res)
}


#' calculate n fctorial
#'
#' @param n A integer
#' @param type a way of calcul
#' @return n factorial
#' @examples
#' compareFact(8, "while")
#' compareFact(3, "rep")
compareFact = function(n,type = "for"){
  if (n < 0){
    stop ("n must be positive")
  } 
  if (n != round(n)){
    stop("n must be an integer")
  }
  if (n == 0){
    res = 1
  }
  else {
    type = tryCatch( match.arg(type, c("for", "while", "repeat")), error = function(e)("for"))
    res = switch(type,
                 "for" = factFor(n),
                 "while" = factWhile(n),
                 "repeat" = factReapeat(n))
  }
  return(res)
}

compareFact(5)
compareFact(5, type = "while")
compareFact(5, type = "repeat")
compareFact(5, type = "frf")
compareFact(0)
compareFact(-1)
compareFact("a")



microbenchmark(compareFact(100000, type = "for"))
microbenchmark(compareFact(100000, type = "while"))
microbenchmark(compareFact(100000, type = "repeat"))

#####  exo Apply 

data <- read.table("flights14.csv", sep = ",", header = TRUE)
head(data)

typeCol = sapply(data, class)

#mean
sapply(data,mean)
apply(data[, typeCol == "integer"],2,mean)

data$arr_time[1:10] <- NA
apply(data[, typeCol == "integer"],2,mean, na.rm=TRUE)




multi = function(x, na.remove = TRUE){
  return(c(min = min(x, na.rm = na.remove), max = max(x, na.rm = na.remove)))
}

apply(data[, typeCol == "integer"],2, multi, na.remove = TRUE)

apply(data[, typeCol == "integer"],2 , function(x, ...) c(min = min(x), max = max(x), mean = mean(x)))

sapply(data[, typeCol == "integer"], function(x, ...) c(min = min(x), max = max(x), mean = mean(x)))

# question 6
data$itinary = paste(data$origin, data$dest)

# question 7
res = tapply(data$air_time, INDEX = list(data$itinary), FUN = function(x) c(std = sd(x), mean = mean(x), count = length(x), cv = sd(x)/mean(x)))

cv = sapply(res,function(x) x[3])
cv[which.max(cv)]


require(parallel)
nb.cores <- detectCores() # 8
nb.cores

# mieux vaut éviter d'utiliser toutes les ressources
cl <- makeCluster(nb.cores - 5)
res <- clusterApply(cl, 1:7, function(x){ rnorm(x)})
str(res)
stopCluster(cl)



