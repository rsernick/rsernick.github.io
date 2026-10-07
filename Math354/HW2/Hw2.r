library(tidyverse)
library(rstatix)
## 1.1
n = 30
k = 5


significance.dat = tibble(simulation = R, unadjusted = rep(NA, 1000), adjusted = rep(NA, 1000))

R = 1:1000
for(i in R){
  dat = tibble(
    value = rnorm(n * k),
    group = as.factor(rep(1:k, each = n))
  )
  unadjusted = pairwise.t.test(dat$value, dat$group, p.adjust.method = 'none')
  adjusted = rstatix::tukey_hsd(dat, value ~ group)

  significance.dat$unadjusted[i] = any(unadjusted$p.value < 0.05, na.rm = TRUE)
  significance.dat$adjusted[i] = any(adjusted$p.adj < 0.05)
}

t1.unadj = length(which(significance.dat$unadjusted == TRUE))/length(R)
t1.adj = length(which(significance.dat$adjusted == TRUE))/length(R)

## 1.2

n = 30
k = 5

BH = rep(NA, 1000)

R = 1:1000
for(i in R){
  dat = tibble(
    value = rnorm(n * k),
    group = as.factor(rep(1:k, each = n))
  )
  bhadjust = pairwise.t.test(dat$value, dat$group, p.adjust.method = 'BH')

  BH[i] = any(bhadjust$p.value < 0.05, na.rm = TRUE)
}

t1.BH = length(which(BH == TRUE))/length(R)

## 2.1

library(pwr)
k = 5
f = 0.25
alpha = 0.05
power = 0.8

anova.power = pwr::pwr.anova.test(k = k, f = f, sig.level = alpha, power = power)

n = ceiling(anova.power$n)
N = n*k

power.dat = tibble(n = 2:75, power = rep(0, 74))

for(i in 2:75){
  power.dat$power[i-1] = pwr::pwr.anova.test(k = k, n = i, f = f, sig.level = alpha)$power
}

ggplot(data = power.dat)+
  geom_line(aes(x = n, y = power))+
  geom_hline(aes(color = "Desired Power", yintercept = 0.8))+
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(power>=0.8,power,0), x = n), fill = "green", alpha = 0.3)+
  scale_color_manual(values = c("Desired Power" = "green", "Desired n" = "blue"))+
  theme_minimal()

## 2.2

N = 150
k = 5
n = N/k
f = 0.25

lambda = N*(f^2)

F = qf(0.95, k-1, N-k)
power = pf(F, k-1, N-k, ncp = lambda, lower.tail = FALSE)

f.dat = tibble(f = seq(0, 10, length.out = 1000)) |>
  mutate(density1 = df(f, k-1, N-k, lambda),
         density2 = df(f, k-1, N-k))

ggplot(data = f.dat)+
  geom_line(aes(x = f, y = density1, color = "Non-Central F"))+
  geom_line(aes(x = f, y = density2, color = "F"))+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(f > F, density1, 0), x = f, fill = "Power"), alpha = 0.3)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(f < F, density1, 0), x = f, fill = "Type 2 Error"), alpha = 0.3)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(f > F, density2, 0), x = f, fill = "Type 1 Error"), alpha = 0.3)+
  geom_hline(yintercept = 0)+
  labs(y = "Density", x = "F")+
  scale_color_manual(values = c("Non-Central F" = "blue", "F" = "black"))+
  scale_fill_manual(values = c("Power" = "purple", "Type 1 Error" = "red", "Type 2 Error" = "yellow"))+
  theme_minimal()


## 2.3
library(PMCMRplus)
n = 30
k = 5

power1 = PMCMRplus::power.tukey.test(n = n, groups = k, delta = 0.25, within.var = 0.77^2)$power
power = 0

while(power < 0.8){
  n = n + 1
  power = PMCMRplus::power.tukey.test(n = n, groups = k, delta = 0.25, within.var = 0.77^2)$power
}

power.dat = rep(0, 299)

pair.power = tibble(n = 2:300)

for(i in pair.power$n){
  power.dat[i-1] = PMCMRplus::power.tukey.test(n = i, groups = k, delta = 0.25, within.var = 0.77^2)$power
}

pair.power = pair.power |>
  mutate(power = power.dat)

ggplot(data = pair.power)+
  geom_line(aes(x = n, y = power,))+
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "green")+
  geom_vline(xintercept = 30, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0)+
  labs(x = "n", y = "Power")+
  theme_minimal()

## 2.4
library(Superpower)
n = 30
k = 5
anova1 = Superpower::ANOVA_design(design = "5b", n = n, mu = c(2.92,3.11,3.36,4.04,4.35), sd = sqrt(0.8))
power1 = Superpower::ANOVA_power(anova1, alpha_level = 0.05)
anova2 = Superpower::ANOVA_design(design = "5b", n = n*2, mu = c(2.92,3.11,3.36,4.04,4.35), sd = sqrt(0.8))
power2 = Superpower::ANOVA_power(anova2, alpha_level = 0.05)

stages = c("Stage 0", "Stage 1", "Stage 2", "Stage 3", "Stage 4")
comparisons = rep("", 10)
index = 1;
for(i in 1:(length(stages)-1)){
  for(k in (i+1):length(stages)){
    comparisons[index] = paste(stages[i], stages[k], sep = " vs ")
    index = index + 1
  }
}


comp.dat = tibble(Comparison = comparisons, Original = power1$pc_results$power, Double = power2$pc_results$power)|>
  add_row(Comparison = "Overall", Original = power1$main_results$power, Double = power2$main_results$power, .before = 1)

underpowered = comp.dat |>
  filter(Original < 80)

## 3

mouse.dat = tibble(Replicate = 1:3, `2M` = c(1.48969, 1.30168, 1.30620), 
                   `6M` = c(4.36000, 4.28807, 3.64184), `16M` = c(7.30182, 7.12416, 8.28764),
                    Treatment = c(4.69304, 2.51326, 2.38019))


mouse.long = mouse.dat |>
  pivot_longer(cols = -Replicate, names_to = "Group", values_to = "Value")

mouse.summary = mouse.long |>
  group_by(Group) |>
  summarise(mean = mean(Value), sd = sd(Value))

## 3.3

anova.dat = rstatix::anova_test(mouse.long, Value ~ Group)

pairwise.test = rstatix::tukey_hsd(mouse.long, Value ~ Group)


power.design = Superpower::ANOVA_design("4b", 3, mouse.summary$mean, mouse.summary$sd)
power.results = Superpower::ANOVA_power(power.design, alpha_level = 0.05)

stages = mouse.summary$Group
comparisons = rep("", 6)
index = 1;
for(i in 1:(length(stages)-1)){
  for(k in (i+1):length(stages)){
    comparisons[index] = paste(stages[i], stages[k], sep = " vs ")
    index = index + 1
  }
}

overall.results = tibble(Comparisons = comparisons, pvalue = pairwise.test$p.adj, power = paste0(power.results$pc_results$power, "%"))
