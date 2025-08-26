
# example code ------------------------------------------------------------


x <- c(1, 2)
x

y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

# exercise ----------------------------------------------------------------

# create vector with a length of 10 ---------------------------------------


z<-c(1:10)

# create numeric matrix w 2 rows and 2 columns. assign to m ---------------

m<-matrix(data=1:4,nrow=2,ncol=2,byrow=FALSE,dimnames=NULL)
m

# data frame --------------------------------------------------------------

data.frame(name=c("smith","john","kate","akira"),height=c(154,170,156,175))
