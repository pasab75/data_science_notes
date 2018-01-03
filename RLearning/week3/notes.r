str(tapply)
x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)

p <- tapply(x,f,mean)

str(split)

str(gl)
y <- split(x, f)


# s <- split(airquality, airquality$Month) #split air quality by month
# monthlyMeans <- sappy(s, function(x) colMeans(x["Ozone","Solar.R","Wind"]))

