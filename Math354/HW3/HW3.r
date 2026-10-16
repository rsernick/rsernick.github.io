## 4.1
library(patchwork)
reps = 1000
n = 30
coef = tibble(round = 1:reps)
b0 = rep(0, reps)
b1 = rep(0, reps)
for(i in 1:reps){
  dat = tibble(x = runif(n, 0, 10),
               e = rnorm(n, 0, 1),
               y = 2 + 1.5*x + e)
  mod = lm(y ~ x, dat)
  b0[i] = mod$coefficients[1]
  b1[i] = mod$coefficients[2]
}
coef = coef |>
  mutate(b0 = b0, b1 = b1)

ggplot(data = coef)+
  geom_histogram(aes(x = b0, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b0))+
  labs(x = "b0", y = "Density")+
  theme_minimal()+
ggplot(data = coef)+
  geom_histogram(aes(x = b1, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b1))+
  labs(x = "b1", y = "Density")+
  theme_minimal()

## 4.2
library(tolerance)

reps = 1000
n = 30
coef = tibble(round = 1:reps)
b0 = rep(0, reps)
b1 = rep(0, reps)
for(i in 1:reps){
  dat = tibble(x = runif(n, 0, 10),
               e = tolerance::r2exp(n, 1, -1),
               y = 2 + 1.5*x + e)
  mod = lm(y ~ x, dat)
  b0[i] = mod$coefficients[1]
  b1[i] = mod$coefficients[2]
}
coef = coef |>
  mutate(b0 = b0, b1 = b1)

ggplot(data = coef)+
  geom_histogram(aes(x = b0, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b0))+
  geom_vline(xintercept = 2, color = "green")+
  labs(x = "b0", y = "Density")+
  theme_minimal()+
ggplot(data = coef)+
  geom_histogram(aes(x = b1, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b1))+
  geom_vline(xintercept = 1.5, color = "green")+
  labs(x = "b1", y = "Density")+
  theme_minimal()

## 4.3
reps = 1000
n = 30
coef = tibble(round = 1:reps)
b0 = rep(0, reps)
b1 = rep(0, reps)
for(i in 1:reps){
  dat = tibble(x = runif(n, 0, 10),
               e = rcauchy(30, 0, 1),
               y = 2 + 1.5*x + e)
  mod = lm(y ~ x, dat)
  b0[i] = mod$coefficients[1]
  b1[i] = mod$coefficients[2]
}
coef = coef |>
  mutate(b0 = b0, b1 = b1)

ggplot(data = coef)+
  geom_histogram(aes(x = b0, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b0))+
  geom_vline(xintercept = 2, color = "green")+
  labs(x = "b0", y = "Density")+
  xlim(-6, 10)+
  theme_minimal()+
ggplot(data = coef)+
  geom_histogram(aes(x = b1, y = after_stat(density)), bins = 10, color = "grey")+
  geom_density(aes(x = b1))+
  geom_vline(xintercept = 1.5, color = "green")+
  labs(x = "b1", y = "Density")+
  xlim(-1, 4)+
  theme_minimal()
