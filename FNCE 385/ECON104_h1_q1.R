a <-  rnorm(25)

## ------------------------------------------------------------------------
# Have a look at what is in "a" now
print(a)
# Basic statistical summaries of "a"
summary(a)

dist <- rbinom(225,1,0.91)
num_tickets <- sum(dist)
expected_rev <- (num_tickets*300+ (225-num_tickets)*75) 

# There are two outcomes in which the airline would be making losses - 226 or 227 passengers show up, the probability of these events is:
p_226 <- choose(227,226)*0.91^226*0.09
p_227 <- 0.91^227

expected_rev2 <- (1-p_226-p_227)*expected_rev

profit_t <- vector(length = 35)
for (x in 225:260){
  profit_t[x-224] = 0
  for (i in 1:x) {
  prob_i  <- choose(x,i)*0.91^i*0.09^(x-i)
    if(i<= 225){
    profit_i = prob_i * (300*i + 75*(x-i))
  }
    else{
    profit_i = prob_i * (300*225 + (x- i)*75 -(i-225) * 600)
  }
  profit_t[x-224] = profit_t[x-224] + profit_i
  }
}
plot(profit_t)
max(profit_t)
pmax(profit_t)
print(67714/63000)
# [1] 67713.86
#> pmax(profit_t)
#[1] 62943.75 63223.50 63503.25 63783.00 64062.75 64342.49 64622.23 64901.91 65181.43 65460.57 65738.78 66015.05 66287.59 66553.53
#[15] 66808.64 67047.18 67261.96 67444.65 67586.41 67678.64 67713.86 67686.48 67593.38 67434.13 67210.95 66928.32 66592.39 66210.34
#[29] 65789.70 65337.81 64861.37 64366.23 63857.25 63338.30 62812.36 62281.65
#[1] 1.074825

# from the analyses above we can determine that the profit maximizing quantity of tickets sold is 247. 
# The expected profit under this assumption is 67714, which is an increase of 7.48%

