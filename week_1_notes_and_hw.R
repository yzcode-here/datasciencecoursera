data("cars")
y <- cars[1:15,]
dput(y, file = 'test_file.R')
new.y <- dget("test_file.R")
new.y

x <- list(foo = 1:4, bar = 0.6)
x$foo[x$foo < 3]

x[[1]][[2]] # same as x[[c(1,2)]]

x <- c(4, TRUE)
class(x)

x <- c(1,3, 5)
y <- c(3, 2, 10)
z <- rbind(x, y)
class(z)
z

x <- list(2, "a", "b", TRUE)
y <- x[[2]]
y
class(y)

x <- 1:4
y <- 2:3
z <- x + y
class(z)

x <- c(3, 5, 1, 10, 12, 6) 
x[x <=5] <- 0
x[x < 6] <- 0
x[x %in% 1:5] <- 0

# Q 11-20 of the Week 1 Homework
hw1_data <- read.csv("hw1_data.csv")

# 11
names(hw1_data)

# 12
hw1_data[1:2,]
head(hw1_data, n = 2)

# 13
dim(hw1_data)

# 14
tail(hw1_data, n = 2)

# 15
hw1_data$Ozone[47]

# 16
summary(hw1_data)
length(which(is.na(hw1_data$Ozone)))

# 17
mean(hw1_data$Ozone, na.rm = T)

# 18
sub <- subset(hw1_data, Ozone > 31 & Temp > 90)
mean(sub$Solar.R, na.rm = T)

# 19
sub_2 <- subset(hw1_data, Month == 6)
mean(sub_2$Temp, na.rm = T)

# 20
sub_3 <- subset(hw1_data, Month == 5)
max(sub_3$Ozone, na.rm = T)


