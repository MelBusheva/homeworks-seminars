# Problem 1
for(i in 1:10) {
  print(i*3)
}

# Problem 2
for(i in rnorm(10, mean = 0, sd = 1)) {
if(i >= 1) {
  print(i) }}

# Problem 3
We will denote the males with 1 and the females with 0
a <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
ResultsVector <- NULL
ResultsVector2 <- NULL
for(i in 1:50000){
    ResultsVector <- c(sample(a, 5, replace = FALSE))
  if(sum(ResultsVector) == 3){
    ResultsVector2 = c(ResultsVector2, 1)
  } else {
    ResultsVector2 = c(ResultsVector2, 0)
  }
}
sum(ResultsVector2) / length(ResultsVector2)

# Problem 4
StrikePrice <- 120
sum.Profit <- 0
Results <- 100
for(k in 1:1000){
  Results <- 100
  for(i in 1:100){
    Results <- c(Results + rnorm(1, mean = 0, sd = 7))}
  if(Results > 120){
    sum.Profit = sum.Profit + Results - StrikePrice}
}
sum.Profit / 1000

