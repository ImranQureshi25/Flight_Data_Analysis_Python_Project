df <- read.csv("C:/Users/iqure/Downloads/airlines_flights_data.csv")
summary(df)
hist.price <- hist(df$price[df$class == "Economy"]) ## Heavily Right Skewed ## 
hist.duration <- hist(df$duration[df$class == "Economy"]) ## Mildly Right Skewed ##

## Logarithmic Price Transformation ##
## Data is right-skewed so logarithmic method needed ##
price.log <- log(df$price[df$class == "Economy"])
hist(price.log)

## Square Root Duration Transformation ##
## Data is mildly right-skewed so square root method needed ##
duration.sqrt <- sqrt(df$duration[df$class == "Economy"])
hist(duration.sqrt)

corr <- cor(price.log, duration.sqrt)
corr ## Correlation is .3868, a moderate positive correlation ##

## Both variables have normal distributions, now let's plot a scatter plot ##
## Let's have duration predict price ##

plot(price.log ~ duration.sqrt,
     main = "Scatter Plot of Log-Transformed Price vs. Sqrt-Transformed Duration",
     xlab = "Sqrt-Transformed Duration (hours)",
     ylab = "Log-Transformed Price (INR)",
     col = "blue", pch = 19)

# Adding regression line
abline(lm(price.log ~ duration.sqrt), col = "red", lwd = 2)

## Let's get our scatter plot equation ##
model <- lm(price.log ~ duration.sqrt)
summary(model) 

## Equation is 8.0453 + .18789 * duration.sqrt ##

## When exponentiating .18789 to account for square root normalization, 
## we get 1.2066, meaning for every 1 unit increase in sqrt duration, 
## price increases by 20.66% ##

## When exponentiating 8.0453 to account for log normalization,
## we get 3123.45, the base price is 3,123.45 INR ##