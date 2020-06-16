### make period lifetables 
## uses methods from Preston, Heuveline, and Guillot (2001)
## and German Rodriguez (code) http://data.princeton.edu/eco572/periodlt.html
## uses separation factors from Keyfitz and Flieger (via Rodriguez)

make_life_table<-function(nat_dat, deaths){
  nat_dat<-nat_dat%>%
    ungroup()
  ### create proportions of pop with outcome by age/year
  nat_dat<-nat_dat%>%
    mutate(m = deaths / pop)
  
  ### convert to probability 
  ### age_period (n) != 1 for all cases
  ### assume mid-point average time of death for within-period deaths
  ### n = 1 if age == "0", 
  ### n = 10 (arbitrary, assumes death by 95) if age == "85+",
  ### n = 4 if age == "1-4"
  ### else n = 5 for all other age groups
  nat_dat<-nat_dat%>%
    mutate(n = case_when(
      age==0 ~ 1,
      age==1 ~ 4,
      age==85 ~ 0,
      ! age %in% c(1, 4, 85) ~ 5
    ))
  
  nat_dat<-nat_dat%>%
    mutate(q = ### set to probability = 1 for 85+, nQx otherwise
             ifelse(age<85,
               n * m / (1 + (n - nax) * m),
               1),
           p = 1 - q)
  ### make cumulative survival
  nat_dat<-nat_dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p[-nrow(nat_dat)])))
  
  ### deaths
  nat_dat<-nat_dat%>%
    mutate(d = c(-diff(lx),lx[nrow(nat_dat)]))
  ## person-years in each group
  nat_dat<-nat_dat%>%
    mutate(L = (lx - d) * n + d * nax,
           t = sum(L)- cumsum(L) + L)
  ## life expectancy
  nat_dat<-nat_dat%>%
    mutate(e = t/lx)
  ### cum prevalence
  nat_dat<-nat_dat%>%
    mutate(c = 1-lx/1e5)
  return(nat_dat)
}

