getwd()

# Lab: Introduction to R


## Basic Commands

###
x <- c(1, 3, 2, 5)
x
###
x = c(1, 6, 2)
x
y = c(1, 4, 3)
###
length(x)
length(y)
x + y
###
ls()
rm(x, y)
ls(u
###
rm(list = ls())
###
?matrix
###
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
###
x <- matrix(c(1, 2, 3, 4), 2, 2)
###
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
###
sqrt(x)
x^2
###
x <- rnorm(50)
y <- x + rnorm(50, mean = 50, sd = .1)
cor(x, y)
###
set.seed(1303)
rnorm(50)
###
set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

## Graphics

###
x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y, xlab = "this is the x-axis",
    ylab = "this is the y-axis",
    main = "Plot of X vs Y")
###
pdf("./data/Figure.pdf")
plot(x, y, col = "black")
dev.off()
###
x <- seq(1, 10)
x
x <- 1:10
x
x <- seq(-pi, pi, length = 50)
###
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))


     x <- 1:9; names(x) <- x
     # Multiplication & Power Tables
     x %o% x
x %*% x
     y <- 2:8; names(y) <- paste(y,":", sep = "")
     outer(y, x, `^`)
     
     outer(month.abb, 1999:2003, FUN = paste)
     
     ## three way multiplication table:
     x %o% x %o% y[1:3]

     # simple scalar multiplication
( M <- matrix(1:6, ncol = 2) )
kronecker(4, M)
diag(2, 3)
# Block diagonal matrix:
kronecker(diag(1, 3), M)

# ask for dimnames

fred <- matrix(1:12, 3, 4, dimnames = list(LETTERS[1:3], LETTERS[4:7]))
bill <- c("happy" = 100, "sad" = 1000)
kronecker(fred, bill, make.dimnames = TRUE)

bill <- outer(bill, c("cat" = 3, "dog" = 4))
kronecker(fred, bill, make.dimnames = TRUE)

boxplot(x,y,f)
hist(f)
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)
###
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

## Indexing Data

###
A <- matrix(1:16, 4, 4)
A
###
A[2, 3]
###
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
###
A[1, ]
###
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
###
dim(A)

# 向量乘法：内积、外积、叉积 和 kronecker 积
# 向量乘法中，叉积等价于内积结果为标量，外积

a = c(3,6)
b = c(9,10)

# 内积
a %*% b
# 外积
a %o% b # 或
outer(a, b)
# 叉积
crossprod(a,b) # 或
t(a) %*% b # 或
# kronecker 积
# kronecker(a,b)
kronprod = kronecker(x,y)

# 矩阵乘法： 内积、外积、叉积和 kronecher 积

x=matrix(c(2,6,9,2,7,3),nrow=2,ncol=3,byrow=T)
y=matrix(c(7,5,6,3,12,3),nrow=2,ncol=3,byrow=T)

innerprod =x %*% y
outerprod =x %o% y
crossprod = crossprod(x,y)
kronprod = kronecker(x,y)

outer(a,b,FUN ='/')


## Loading Data

###
Auto <- read.table("./data/Auto.data")
View(Auto)
head(Auto)
###
Auto <- read.table("./data/Auto.data", header = T, na.strings = "?", stringsAsFactors = T)
View(Auto)
###
Auto <- read.csv("./data/Auto.csv", na.strings = "?", stringsAsFactors = T)
View(Auto)
dim(Auto)
Auto[1:4, ]
###
Auto <- na.omit(Auto)
dim(Auto)
###
names(Auto)

## Additional Graphical and Numerical Summaries

###
plot(cylinders, mpg)
###
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
###
cylinders <- as.factor(cylinders)
###
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T,
    horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T,
    xlab = "cylinders", ylab = "MPG")
###
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
hist(mpg, col = 2, breaks = 30)
###
pairs(Auto)
pairs(
    ~ mpg + displacement + horsepower + weight + acceleration,
    data = Auto
  )
###
plot(horsepower, mpg)
identify(horsepower, mpg, name)
###
summary(Auto)
###
summary(mpg)
###

college <- read.csv("./data/College.csv", header = T, stringsAsFactors = T)
head(college)
View(college)
rownames(college)  <- college[,1]
college <- college[,-1]
pairs(college[,1:10])
attach(college)
plot(college$Private,college$Outstate)

Elite <- rep("No",nrow(college))
Elite[college$Top10perc >50]  <-  "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college,Elite)
View(college)

summary(college$Elite)
plot(Elite,Outstate)
par(mfrow=c(2,2))
hist(Outstate)
hist(S.F.Ratio)
hist(Grad.Rate)
hist(PhD)

Auto<- read.table("./data/Auto.data", header = T, stringsAsFactors = T)
View(Auto)
range(Auto)



