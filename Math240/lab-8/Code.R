library(tidyverse)
library(nleqslv)


death.dat = read_csv("Death-Data/API_SP.DYN.CDRT.IN_DS2_en_csv_v2_76451.csv")

death.dat = death.dat |>
  select("Country Name", "2022")|>
  rename("Proportion of Deaths per 1,000 People" = "2022") |>
  mutate("Proportion of Deaths per 1,000 People" = 
           get("Proportion of Deaths per 1,000 People")/1000) |>
  filter(!is.na(`Proportion of Deaths per 1,000 People`))
  


### Method of Moments Estimator function

beta.MOM = function(data, par) {
  a = par[1]
  b = par[2]
  
  Ex1 = a/(a+b)
  Ex2 = ((a+1)*a)/((a+b+1)*(a+b))
  
  m1 = mean(data, na.rm = T)
  m2 = mean(data^2, na.rm = T)
  
  return(c(Ex1 - m1, Ex2 - m2))
}

(moms<- nleqslv(x = c(1, 1),
                fn = beta.MOM,
                data=death.dat$`Proportion of Deaths per 1,000 People`))

### MOM estimators
 
(alpha.hat.mom = moms$x[1])
(beta.hat.mom = moms$x[2])

### MLE estimator function

beta.MLE <- function(data, par, neg=F){
  a = par[1]
  b = par[2]
  
  loglik <- sum(log(dbeta(x=data, shape1 = a, shape2 = b)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

(pars = optim(par = c(1, 1), 
      fn = beta.MLE,
      data=death.dat$`Proportion of Deaths per 1,000 People`,
      neg=T))

### MLE Estimators

(alpha.hat.mle = pars$par[1])
(beta.hat.mle = pars$par[2])

### Plots

ggplot()+
  geom_histogram(data = death.dat, aes(x = `Proportion of Deaths per 1,000 People`,
                                       y = after_stat(density)),
                 binwidth = 0.001, color = "lightgrey", fill = "maroon") +
  geom_line(data = )


