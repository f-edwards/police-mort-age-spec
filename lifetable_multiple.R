### make multiple decrement period lifetables 
## uses methods from Preston, Heuveline, and Guillot (2001)
## and German Rodriguez (code) http://data.princeton.edu/eco572/periodlt.html
## uses separation factors from Keyfitz and Flieger (via Rodriguez)
## requires cause of death data, deaths column, and period life table (total mort)
## Use radix 1e8 to match sim draws
edit.na <- function(x, value) { x[is.na(x)] <- value; x}


# #### FOR DEBUGGING, use observed (imputed) and sims to check results
# # nat_dat<-fe_imp%>%
# #   filter(.imp==1)%>%
# #   filter(year ==2017)%>%
# #   filter(race=="African American", sex=="Male")%>%
# #   rename(deaths = officer_force)
# # 
# nat_dat<-fe_postpred%>%
#   filter(race=="African American", sex=="Male")%>%
#   rename(deaths = median)
# 
# life_table<-nvss_lifetable%>%
#   filter(race=="African American", sex=="Male")
#######

make_life_table_multiple<-function(nat_dat, life_table){
  nat_dat<-nat_dat%>%
    ungroup(nat_dat)%>%
    left_join(life_table%>%
                select(age, race, sex, deaths, d, lx, q, nax, m, n, e, L)%>%
                rename(D = deaths,
                       d_x = d,
                       q_x = q,
                       a = nax))
  
  nat_dat<-nat_dat%>%
    mutate(q_xi = q_x * (deaths / D),
           d_i = q_xi * lx,
           m_i = d_i / L)
  
  ### cause deleted decomposition
  nat_dat<-nat_dat%>%
    mutate(R = (d_x - d_i)/ d_x)
  
  nat_dat<-nat_dat%>%
    mutate(pd = (1-q_x)^R,
           ld = 1e5 * cumprod(c(1, pd[-length(pd)])))
  
  nat_dat<-nat_dat%>%
    mutate(dd = edit.na(ld - lead(ld),
                        tail(ld,1)),
           qd = dd/ld,
           ad = ifelse(age<10|age==80,
                       n + R * (q_x/qd) * (a-n),
                       ifelse(age>=10 & age<=75,
                              ((-5/24) * lag(dd) + 2.5 * dd + (5/24) * lead(dd))/dd,
                              a/R)))
  
  nat_dat<-nat_dat%>%
    mutate(Ld = edit.na(dd * ad + (ld-dd) * n,
                        tail(ld*ad, 1)))
  
  nat_dat<-nat_dat%>%
    mutate(td = sum(Ld) - cumsum(Ld) + Ld,
           ed = td/ld)
  
  return(nat_dat)
}


