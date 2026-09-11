library(tidyverse)

## Question 1
indirect = c(1.0000, 1.0000, 0.7500, 1.0000, 0.4167, 0.8333, 0.7500, 0.8333, 0.8333, 1.0000)
diff = c(-0.0833, 0.0000, -0.0833, -0.0833, 0.5833, 0.0833, 0.2500, 0.1667, 0.0000, -0.4167)
n = 1:10
dat1 = tibble(n, indirect)
dat2 = tibble(n, diff)

wider = dat2 %>%
  pivot_wider(names_from = n,
              values_from = diff)

summary1 = dat1 %>%
  summarize(
    n = n(),
    mean = mean(indirect),
    sd = sd(indirect),
    median = median(indirect),
    IQR = IQR(indirect)
  )

summary2 = dat2 %>%
  summarize(
    n = n(),
    mean = mean(diff),
    sd = sd(diff),
    median = median(diff),
    IQR = IQR(diff)
  )

test1 = wilcox.test(indirect, alternative = "two.sided", mu = 0.5, conf.level = 0.95, conf.int = TRUE)
test2 = wilcox.test(diff, alternative = "two.sided", mu = 0, conf.level = 0.95, conf.int = TRUE)

test1 = c(as.numeric(test1$statistic), as.numeric(test1$p.value))
test2 = c(as.numeric(test2$statistic), as.numeric(test2$p.value))

tests = tibble(c(NULL, 'W', 'p'), test1, test2)

wilcox = tibble(w = 0:55) %>%
  mutate(probability = dsignrank(w,10))

maxw = 10*(10+1)/2
lower = qsignrank(0.025, 10)
upper = maxw-lower

ggplot(data = wilcox)+
  geom_col(aes(x = w, y = probability), color = 'dark grey', fill = 'grey')+
  geom_line(aes(x = w, y = probability, color = 'Wilcoxon'), linewidth = 1) +
  geom_hline(yintercept = 0) +
  labs(x = 'Wilcoxon Signed Rank Statistic', y = 'Probability')+
  geom_ribbon(aes(x = w, ymin = 0, ymax = ifelse(w <= lower & w >= 0, probability, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_ribbon(aes(x = w, ymin = 0, ymax = ifelse(w >= upper & w <= maxw, probability, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_segment(aes(x = lower, xend = lower, y = 0, yend = dsignrank(lower, 10), color = 'Rejection Region'))+
  geom_segment(aes(x = upper, xend = upper, y = 0, yend = dsignrank(upper, 10), color = 'Rejection Region'))+
  geom_point(aes(x = test1[1], y = 0, color = 'Overhearing'), size = 3)+
  geom_point(aes(x = test2[1], y = 0, color = 'Difference'), size = 3)+
  xlim(0,55)+
  scale_color_manual(values = c('Wilcoxon' = 'black', 'Overhearing' = 'green', 'Difference' = 'blue', 'Rejection Region' = 'red'))+
  theme_minimal()

## Question 2b

x = 10
n = 10
binom_test = binom.test(x = x, n = n, p = 0.5, alternative = "two.sided", conf.level = 0.95)
prob = as.numeric(binom_test$p.value)


successes = 0:10
proportion = dbinom(successes, 10, 0.5)


binom = tibble(successes, proportion)

lower = qbinom(0.025, 10, 0.5)
upper = 10 - lower

ggplot(data = binom, aes(x = successes))+
  geom_hline(yintercept = 0)+
  geom_col(aes(y = proportion, color = 'Binomial Distribution'), fill = 'grey')+
  geom_line(aes(y = proportion, color = 'Binomial Distribution'), linewidth = 1)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(successes <= lower & successes >= 0, proportion, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(successes >= upper & successes <= 10, proportion, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_segment(aes(x = lower, xend = lower, y = 0, yend = proportion[lower+1], color = 'Rejection Region'))+
  geom_segment(aes(x = upper, xend = upper, y = 0, yend = proportion[upper+1], color = 'Rejection Region'))+
  geom_point(aes(x = 10, y = 0, color = 'Observation'), size = 3)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  scale_color_manual(values = c('Binomial Distribution' = 'black', 'Observation' = 'blue', 
                                'Rejection Region' = 'red'))+
  labs(x = 'Successes', y = 'Probability')+
  theme_minimal()


## Question 2c

n = 10
x = 10
p = x/n
p0 = 0.5
mu0 = n*p0
sd0 = sqrt(n*p0*(1-p0))

approx = 2*pnorm(x-0.5, mean = mu0, sd = sd0, lower.tail = FALSE)
gauss.successes = 0:10
gauss.prob = dnorm(gauss.successes, mean = mu0, sd = sd0)
gauss.dat = tibble(gauss.successes, gauss.prob)

lower = floor(qnorm(0.025, mu0, sd0))
upper = n - lower

ggplot(data = gauss.dat, aes(x = gauss.successes, y = gauss.prob))+
  geom_hline(yintercept = 0)+
  geom_col(aes(color = 'Gaussian Approximation'), fill = 'grey')+
  geom_line(aes(color = 'Gaussian Approximation'), linewidth = 1)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(gauss.successes <= lower & gauss.successes >= 0, gauss.prob, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_ribbon(aes(ymin = 0, ymax = ifelse(gauss.successes >= upper & gauss.successes <= 10, gauss.prob, NA)),
                  fill = 'red', alpha = 0.3)+
  geom_segment(aes(x = lower, xend = lower, y = 0, yend = gauss.prob[lower+1], color = 'Rejection Region'))+
  geom_segment(aes(x = upper, xend = upper, y = 0, yend = gauss.prob[upper+1], color = 'Rejection Region'))+
  geom_point(aes(x = 10, y = 0, color = 'Observation'), size = 3)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  scale_color_manual(values = c('Gaussian Approximation' = 'black', 'Observation' = 'blue', 
                                'Rejection Region' = 'red'))+
  labs(x = 'Successes', y = 'Probability')+
  theme_minimal()
