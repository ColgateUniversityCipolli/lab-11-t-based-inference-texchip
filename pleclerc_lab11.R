
#####################################
###### Task 1 #######################
#####################################

library(pwr)
set.seed(5656)

(pwr.t.test(d = 0.65, # large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample"))

# n = 20.58039 => 21 observations needed.

#####################################
###### Task 2 #######################
#####################################

library(tidyverse)

farther <- read_csv("farther.csv",col_names = F)
closer <- read_csv("closer.csv",col_names = F)

fig.2g.data <- tibble(g.Farther_vals = farther[[1]],
                      g.Closer_vals = closer[[1]]) |>
  mutate(difference = g.Closer_vals - g.Farther_vals)
#view(fig.2g.data)

#####################################
###### Task 3 #######################
#####################################

library(e1071)

# a) farther

farther.summary <- fig.2g.data |>
  summarize(mean       = mean(g.Farther_vals),
            sd         = sd(g.Farther_vals),
            median     = median(g.Farther_vals),
            IQR        = IQR(g.Farther_vals),
            skewness   = skewness(g.Farther_vals),
            exkurtosis = kurtosis(g.Farther_vals))

(farther.summary)

# b) closer

closer.summary <- fig.2g.data |>
  summarize(mean       = mean(g.Closer_vals),
            sd         = sd(g.Closer_vals),
            median     = median(g.Closer_vals),
            IQR        = IQR(g.Closer_vals),
            skewness   = skewness(g.Closer_vals),
            exkurtosis = kurtosis(g.Closer_vals))

(closer.summary)

# c) paired differences

difference.summary <- fig.2g.data |>
  summarize(mean       = mean(difference),
            sd         = sd(difference),
            median     = median(difference),
            IQR        = IQR(difference),
            skewness   = skewness(difference),
            exkurtosis = kurtosis(difference))

(difference.summary)

#####################################
###### Task 4 #######################
#####################################

library(effectsize)

# a) Closer

mu0 <- 0
x <- fig.2g.data$g.Closer_vals
xbar <- mean(x)
s <- sd(x)
n <- length(x)
(t.stat.a <- (xbar - mu0)/(s/sqrt(n)))
(p.val <- 2*pt(q=-abs(t.stat.a), df = n-1))
t.test(x=x, mu = mu0, alternative = "greater")
(hedges_g(x = x, mu = mu0, alternative = "greater"))
interpret_hedges_g(1.61)
t.test(x=x, mu = mu0, alternative = "two.sided")

# b) Farther

mu0 <- 0
x <- fig.2g.data$g.Farther_vals
xbar <- mean(x)
s <- sd(x)
n <- length(x)
(t.stat.b <- (xbar - mu0)/(s/sqrt(n)))
(p.val <- 2*pt(q=-abs(t.stat.b), df = n-1))
t.test(x=x, mu = mu0, alternative = "less")
(hedges_g(x = x, mu = mu0, alternative = "less"))
interpret_hedges_g(-1.51)
t.test(x=x, mu = mu0, alternative = "two.sided")

# c) Difference

mu0 <- 0
x <- fig.2g.data$difference
xbar <- mean(x)
s <- sd(x)
n <- length(x)
(t.stat.c <- (xbar - mu0)/(s/sqrt(n)))
(p.val <- 2*pt(q=-abs(t.stat.c), df = n-1))
t.test(x=x, mu = mu0, alternative = "two.sided")
(hedges_g(x = x, mu = mu0, alternative = "two.sided"))
interpret_hedges_g(1.65)

#####################################
###### Task 5 #######################
#####################################

# a) Closer

mu0 <- 0
x <- fig.2g.data$g.Closer_vals
xbar <- mean(x)
s <- sd(x)
n <- length(x)

ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t = t.stat.a, 
                    y = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, 0, 
              qt(0.95, df = n-1), 14,  # rejection region (right)
              t.stat.a)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat.a), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="white", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="blue")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2))) +
  ylab("Density")+
  ggtitle("T Test for Closer Values Compared to Null T Distribution")

# b) Farther

mu0 <- 0
x <- fig.2g.data$g.Farther_vals
xbar <- mean(x)
s <- sd(x)
n <- length(x)

ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t = t.stat.b, 
                    y = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-14, qt(0.05, df = n-1), # rejection region (left)
              0, 5,
              t.stat.b)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat.b), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="white", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="blue")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2))) +
  ylab("Density")+
  ggtitle("T Test for Farther Values Compared to Null T Distribution")

# c) Difference

mu0 <- 0
x <- fig.2g.data$difference
xbar <- mean(x)
s <- sd(x)
n <- length(x)

ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t = t.stat.c, 
                    y = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 14,  # rejection region (right)
              t.stat.c)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat.c), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="white", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="blue")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2))) + 
  ylab("Density")+
  ggtitle("T Test for Difference Compared to Null T Distribution")
