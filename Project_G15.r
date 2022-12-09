setwd("~/Desktop/Project_G15")

rawData=read.csv("DailyDelhiClimateTrain.csv", header=T)

library(ggplot2)
library(gmp)
head(rawData, 10)

trainData <- subset (rawData, select = -date)
head(trainData, 10)

b_value <- function(x,y){
  mean_x= mean(x)
  mean_y = mean(y)
  b_numer = sum((x-mean_x)*(y-mean_y))
  b_denom = sum((x-mean_x)^2)
  b= b_numer/b_denom
  return(b)
}

a_value <- function(x,y,b){
  x_mean = mean(x)
  y_mean = mean(y)
  a= y_mean - b*x_mean
  return(a)
}

plotting_regression_line <- function(x,y,a,b){
  print(a)
  print(b)
  y_predict = a + b*x
  return(y_predict)
}

R2_value <- function(y,y_predict){
  y_mean= mean(y)
  sst = sum((y-y_mean)^2)
  print(sst)
  sse = sum((y-y_predict)^2)
  print(sse)
  r2 = 1-(sse/sst)
  r2
  return(r2)
}

x=trainData$meantemp
y=trainData$humidity
# for finding for differnet parameters, change the x and y values to your desired parameters
b <- b_value(x,y)
b
a <- a_value(x,y,b)
a
y_predict<-plotting_regression_line(x,y,a,b)
R2 <- R2_value(y,y_predict)
R2



linear=lm(y~x)
summary(linear)
r1 <- summary(linear)$r.squared

ggplot(trainData, aes(x = meantemp, y = humidity)) +
  geom_point(color = "blue") +
  geom_smooth(method = "", se = FALSE, color = "red")


ggplot(trainData, aes(x = meantemp, y = humidity)) +
  geom_point(color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "red")


A <- rbind(c(sum((x)^4), sum((x)^3),sum((x)^2)),
           c(sum((x)^3), sum((x)^2), sum(x)),
           c(sum((x)^2), sum(x), length(x)))
B <- c(sum(((x)^2)*y), sum(x*y),sum(y)  )
q=solve(A,B)
q
y_predict2 <- q[3] + q[2]*x + q[1]*x**2
R2 <- R2_value(y,y_predict2)
R2

ggplot(trainData, aes(x = meantemp, y = humidity)) +
  geom_point(color = "blue") +
  stat_smooth(method = lm, formula = y ~ poly(x,2,raw = TRUE), color = "red")

C <- rbind(c(sum((x)^6), sum((x)^5), sum((x)^4),sum((x)^3)),
           c(sum((x)^5), sum((x)^4), sum((x)^3),sum((x)^2)),
           c(sum((x)^4), sum((x)^3), sum((x)^2), sum(x)),
           c(sum((x)^3), sum((x)^2), sum(x), length(x)))
D <- c(sum(((x)^3)*y),sum(((x)^2)*y), sum(x*y),sum(y)  )
p=solve(C,D)
p
y_predict3 <- p[4] + p[3]*x + p[2]*x**2 + p[1]*x**3
R2 <- R2_value(y,y_predict3)
R2


ggplot(trainData, aes(x = meantemp, y = humidity)) +
  geom_point(color = "blue") +
  stat_smooth(method = lm, formula = y ~ poly(x,3,raw = TRUE), color = "red")


model2 <- lm(y ~ poly(x,degree = 2,raw = T))
summary(model2)
r2 <- summary(model2)$r.squared

model3 <- lm(y ~ poly(x,degree = 3,raw = T))
summary(model3)
r3 <- summary(model3)$r.squared



model4 <- lm(y ~ poly(x,degree = 4,raw = T))
summary(model4)
r4 <- summary(model4)$r.squared

model5 <- lm(y ~ poly(x,degree = 5,raw = T))
summary(model5)
r5 <- summary(model5)$r.squared

model6 <- lm(y ~ poly(x,degree = 6,raw = T))
summary(model6)
r6 <- summary(model6)$r.squared

model7 <- lm(y ~ poly(x,degree = 7,raw = T))
summary(model7)
r7 <- summary(model7)$r.squared

model8 <- lm(y ~ poly(x,degree = 8,raw = T))
summary(model8)
r8 <- summary(model8)$r.squared

model9 <- lm(y ~ poly(x,degree = 9,raw = T))
summary(model9)
r9 <- summary(model9)$r.squared

model10 <- lm(y ~ poly(x,degree = 10,raw = T))
summary(model10)
r10 <- summary(model10)$r.squared


x_lab <- 1:10
r_graph = c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
plot(x_lab, r_graph, xlab= "Degree", type = "l", col="red", lwd=3)







