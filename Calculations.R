
days <- as.Date("2017/11/21", format="%Y/%m/%d")-as.Date("1987/12/31", format="%Y/%m/%d")
days


alaska <- 1481337
RhoseIsland <- 2707

alaska/RhoseIsland

#Simulate 

hh <- replicate(n = 1e4, expr = {
  x <- sample(1:6, 3, replace = T)
  while(prod(tail(x,3) == c(6,6,6)) != 1)  x <- c(x,sample(1:6, 1))
  length(x)
})

kk <- replicate(n = 1e4, expr = {
  x <- sample(1:6, 3, replace = T)
  while(prod(tail(x,3) == c(6,6,6)) != 1)  x <- append(x, sample(1:6,1))
  length(x)
})

hist(hh, breaks = 100)
mean(hh); 6^3+6^2+6^1

barplot(table(hh))
