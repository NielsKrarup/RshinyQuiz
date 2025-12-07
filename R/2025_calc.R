
# Create a list with birth and death dates for each individual
dates_list <- list(
  Adolf = c(
    as.Date("1889-04-20", format = "%Y-%m-%d"),
    as.Date("1945-04-30", format = "%Y-%m-%d")
  ),
  MargretheII = c(
    as.Date("1940-04-16", format = "%Y-%m-%d"),
    NA
  ),
  Salem_al_Hazmi = c(
    as.Date("1981-02-02", format = "%Y-%m-%d"),
    as.Date("2001-09-11", format = "%Y-%m-%d")
  ),
  Xii = c(
    as.Date("2020-05-04", format = "%Y-%m-%d"),
    NA
  )
)

overlap_foo <- function(va = dates_list[[1]], vb = dates_list[[2]], fix_end_date = Sys.Date(), browse = FALSE){
  if (browse) browser()
  #if both alive
  if (is.na(va[2]) & is.na(vb[2])){
    print("if both alive")
    diff <- fix_end_date - max(va[1], vb[1]) #Time from youngest (latest born till now)
  }
  #if A is dead and B is alive
  if (!is.na(va[2]) & is.na(vb[2])){
    print("if A is dead and B is alive")
    if(vb[1] > va[2]){ #b born after a died, no overlap
      diff <- 0
    }  else if (vb[1] > va[1]){# B born after A born: B birth to A death
      diff <- va[2] - vb[1] 
    } else        diff <- va[2] - va[1] #B born before A born, Total person a's life time
  }
  #if B is dead and A is alive
  if (is.na(va[2]) & !is.na(vb[2])){
    print("if B is dead and A is alive")
    if(va[1] > vb[2]){ #a born after B died, no overlap
      diff <- 0
    }  else if (va[1] > vb[1]){# A born after B born = A birth to B death
      diff <- vb[2] - va[1] 
    } else        diff <- vb[2] - vb[1] #A born before B born, Total person B's life time
  }
  #if both dead
  if (!is.na(va[2]) & !is.na(vb[2])){
    print("if both dead")
    if ( max(va[1], vb[1]) < min(va[2], vb[2]) ){ #if last birth is before first death
      diff <- min(va[2], vb[2]) - max(va[1], vb[1]) 
    }  else diff <- 0
  }
  #return diff in years
  as.numeric(diff) / 365.24

}
#### Test 
overlap_foo(va = dates_list[[3]], vb = dates_list[[4]], browse = !T)

overlap_foo(vb = dates_list[[1]])
tot_years <- 0

for( i in seq_along(dates_list)) {
  for (j in i:length(dates_list)){
    if (j == i) NULL else {
      cat(names(dates_list[i]), "vs", names(dates_list[j]), 
             "res:", overlap_foo(va = dates_list[[i]], vb = dates_list[[j]]))
      tot_years <- tot_years + overlap_foo(va = dates_list[[i]], vb = dates_list[[j]])
    }
  }
}

tot_years

a <- overlap_foo(va = dates_list[[i]], vb = dates_list[[j]])
a
# Print the list
print(dates_list)


# Q4 ----------------------------------------------------------------------

2026^2 - 2024^2


# Q5 ----------------------------------------------------------------------
sum(c(
  2.36, #high jump
  5.95, #pole
  69.89, #Disc
  82.91, #hammer
  86.50  #Javelin
))

