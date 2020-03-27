Score <- function(InputList = SvarHoldC){
  GoodGuesses <- 0
  Score <- 10
  PointVec <- rep("x", length(InputList))
  for(i in seq_along(InputList)){
    correct <- (SvarVec[i] >= InputList[[i]][1] & SvarVec[i] <= InputList[[i]][2])
    GoodGuesses <- GoodGuesses + correct
    if(correct) {Score <- Score + floor(InputList[[i]][2]/InputList[[i]][1])
    PointVec[i] <- floor(InputList[[i]][2]/InputList[[i]][1])
    }
  }
  
  FinalScore <- Score * 2^(length(InputList) - GoodGuesses )
  list(FinalScore = FinalScore,
       PointDataFrame = data.frame(Spm = 1:length(InputList),Point = PointVec))
}


########################################## AAAAA JAIS RUNE TROELS
SvarHoldA <- list(spm1 =  c(650,850), 
                  spm2 =  c(10,50),   
                  spm3 =  c(12000,23999), 
                  spm4 =  c(12000,23999), 
                  spm5 =  c(6000000,23999000),
                  spm6 =  c(0,0),   
                  spm7 =  c(60,119), 
                  spm8 =  c(6000,11999), 
                  spm9 =  c(150,449), 
                  spm10 = c(0,0), 
                  spm11 = c(100000000,190000000))
Score(SvarHoldA)



########################################## BBBBB THYGE GORM HUMMEL
SvarHoldB <- list(spm1 =  c(630,750), 
                  spm2 =  c(5,250),   
                  spm3 =  c(1000,5000), 
                  spm4 =  c(18,39), 
                  spm5 =  c(81000000,120000000),
                  spm6 =  c(3500000,4000000),   
                  spm7 =  c(40,75), 
                  spm8 =  c(4800,6100), 
                  spm9 =  c(2,5), 
                  spm10 = c(0,0), 
                  spm11 = c(149000000000,150000000000))
Score(SvarHoldB)



########################################## CCCC SIMON SIMON SIMON
SvarHoldC <- list(spm1 =  c(500,999), 
                  spm2 =  c(25,49),   
                  spm3 =  c(1000,5000), 
                  spm4 =  c(30,59), 
                  spm5 =  c(1200000,7000000),
                  spm6 =  c(100,199),   
                  spm7 =  c(50,99), 
                  spm8 =  c(5400,10799), 
                  spm9 =  c(60,119), 
                  spm10 = c(0,0), 
                  spm11 = c(100000000000,199000000000))
Score(SvarHoldC)



########################################## DDDDD NORDLAND THOMAS
SvarHoldD <- list(spm1 =  c(660,840), 
                  spm2 =  c(50,500),   
                  spm3 =  c(75000,150000), 
                  spm4 =  c(35,50), 
                  spm5 =  c(10*10^6,30*10^6),
                  spm6 =  c(200000,2000000),   
                  spm7 =  c(40,80), 
                  spm8 =  c(5040,8100), 
                  spm9 =  c(3,9), 
                  spm10 = c(2*10^8,4*10^11), 
                  spm11 = c(2.5*10^9,3.5*10^10))
Score(SvarHoldD)










# SVAR --------------------------------------------------------------------

SvarVec <- c(683, 
             17,
             86100,
             38,
             15748000,
             1267069,
             74,
             5757,
             144,
             169,
             149597870700)
#tid
25*60
3*60 + #rhubarb
120 + #journey in   
90 + #kick it
240 +20  + #Talking heads
140 + #heart dont stand a chance  
140 + #Auditorium
130 + #STUNTZ
120  + #System blower
60 + #Damage
  50 + #Rape me
  3*60 + 20  #voodoo 

