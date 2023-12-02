eminem_born_data <- as.Date("October 17, 1972", format = "%B %d, %Y")
picasso_death_date <- as.Date("8 April 1973", format = "%d %B %Y")

picasso_death_date-eminem_born_data  


# oil spill ---------------------------------------------------------------

bp <- as.Date("April 20, 2010", format = "%B %d, %Y")
ixtol <- as.Date("9 June 1979", format = "%d %B %Y")

difftime(bp, ixtol, units = "weeks")


# WWII death buddies ------------------------------------------------------

library(dplyr)
d_vec <- 
c(ppm = as.Date("22 March 1963",    format = "%d %B %Y"),
  wtb = as.Date("22 November 1963", format = "%d %B %Y"),
  ahd = as.Date("10 July 1964",     format = "%d %B %Y"),
  bfs = as.Date("4 December 1964",  format = "%d %B %Y"),
  hep = as.Date("6 August 1965", format = "%d %B %Y"),
  rub = as.Date("3 December 1965", format = "%d %B %Y"),
  rev = as.Date("5 August 1966", format = "%d %B %Y"),
  sgt = as.Date("26 May 1967", format = "%d %B %Y"),
  whi = as.Date("22 November 1968", format = "%d %B %Y"),
  yel = as.Date("13 January 1969", format = "%d %B %Y"),
  abb = as.Date("26 September 1969", format = "%d %B %Y"),
  let = as.Date("8 May 1970", format = "%d %B %Y")
)
d_vec
dist(d_vec)[dist(d_vec) == 52]
min(dist(d_vec), na.rm = T )
hclust()
diff(c(1,5,6,8))
