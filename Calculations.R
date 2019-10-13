
days <- as.Date("2017/11/21", format="%Y/%m/%d")-as.Date("1987/12/31", format="%Y/%m/%d")
days
 y <- 1

 
 
 ##################### QUESTION 4  - us states 
 
integrate(f = Vectorize(function(x)
  integrate(f = Vectorize(function(y)
    integrate(function(z) x^y^z,lower = 1, upper = 3)$value
  ), lower = 1, upper = 3)$value
),lower = 1, upper = 3) 

  


##################### QUESTION 5  - us states 
alaska <- 1481337
RhoseIsland <- 2707

alaska/RhoseIsland


##################### QUESTION 6  - OL records



##################### QUESTION 7 - DICE 6's
#Simulate 

#Very slow
hh <- replicate(n = 1e4, expr = {
  x <- sample(1:6, 4, replace = T)
  while(prod(tail(x,4) == c(6,6,6,6)) != 1)  x <- c(x,sample(1:6, 1))
  length(x)
})
beepr::beep()
hist(hh, breaks = 100)
mean(hh); 6^4+6^3+6^2+6^1




#Better way:
N_sample_step <- 1e4


kk <- replicate(n = 1e4, expr = {
  cnd <- TRUE
  counter <- 0
  while(cnd){
    x <- sample(1:6, N_sample_step , replace = T)
    obj <- rle(x)
    
    if(any(obj$length == 4)){
      if(any(obj$values[obj$length == 4] == 6)){
        
        match <- match(1, obj$lengths == 4 & obj$values == 6)
        occurrence <- sum(obj$lengths[1:match])
        cnd <- FALSE
        
      }
    } 
    if(cnd) counter <- counter + 1
  }
  
  occurrence + counter * N_sample_step

})
  
hist(kk, breaks = 100)
mean(kk); 6^4 + 6^3+6^2+6^1

##################### QUESTION 8 - lowest tallest 

tmp <- data.frame(Argentina = 6960 ,
           Bolivia = 6542 ,
           Brazil = 2995 ,
           Chile = 6893 ,
           Colombia = 5700 ,
           Ecuador = 6267 ,
           Guyana = 2772 ,
           Paraguay = 842 ,
           Peru = 6768 ,
           Suriname = 1230 ,
           Uruguay = 514 ,
           Venezuela = 4978 )
min(tmp[1,])

# FOR APP -----------------------------------------------------------------

df_Table_Scores <-   data.frame(
    Team = character(0L), 
    Question = character(0L), 
    Try = character(0L), 
    Points = character(0L)
  )

df_Table_Scores <- rbind(df_Table_Scores, 
                         data.frame(
  Team = 'svin', 
  Question = '8', 
  Try = "1", 
  Points = "x"
)
)

df_Table_Scores[df_Table_Scores$Team == "svin",]
tmp <- df_Table_Scores[df_Table_Scores$Team == "svin" &
                  df_Table_Scores$Question == 8, ]
tmp
