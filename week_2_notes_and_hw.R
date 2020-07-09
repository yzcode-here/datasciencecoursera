add2 <- function(x, y) {
  x + y
}

add2(5, 5)

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above10(c(1,2,3,4,5,22,23))

above10_2 <- function(x) {
  x[x > 10]
}

above10_2(c(1,2,3,4,5,22,23,10,11))


above <- function(x, n) {
  use <- x > n
  x[use]
}

above(c(1,2,3,4,5,22,23), 20)

columnmean <- function(x, removeNA = TRUE) {
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(x[, i], na.rm = removeNA)
  }
  means
}

columnmean(cars)

printfun <- function(x) {
  line1 <- paste0("The input was ", x)
  xlen <- nchar(x)
  line2 <- paste0("The input was ", xlen, " units long")
  print(line1)
  print(line2)
}

printfun("nosy")
printfun(22)

## SWIRL week 2
ls() 
rm(list = ls()) # clearing the workspace

swirl()

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)
