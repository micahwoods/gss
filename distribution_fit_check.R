# do a check of gss data

source("r/libraries.R")
source("r/functions.R")
source("r/read_data.R")

nAtc <- length(atc$ID)
nPace <- length(pace$ID)

x <- rnorm(50, 50, 2)
y <- rnorm(50, 50, 2)
# Do x and y come from the same distribution?
ks.test(x, y)

plotdist(d$KM3, "lnorm", demp = TRUE,
         para = list(meanlog = mean(log(d$KM3)), sdlog = sd(log(d$KM3))))

descdist(d$KM3)

# satisfy both > 3000 and pH > 7.7
gssCa <- filter(d, CaM3 <= 3000 | pH <= 7.7)

# variously set these vectors as mlsn data or as gss data
potassium <- d$KM3
phosphorus <- d$PM3
calcium <- gssCa$CaM3
magnesium <- d$MgM3
sulfur <- d$SM3

# look at this for the mlsn and gss datasets
mk.ll <- fitdist(potassium, distr = 'llogis')
mp.ll <- fitdist(phosphorus, distr = 'llogis')
mca.ll <- fitdist(calcium, distr = 'llogis')
mmg.ll <- fitdist(magnesium, distr = 'llogis')
ms.ll <- fitdist(sulfur, distr = 'llogis')

mk.ll <- fitdist(potassium, distr = 'lnorm')
mp.ll <- fitdist(phosphorus, distr = 'lnorm')
mca.ll <- fitdist(calcium, distr = 'lnorm')
mmg.ll <- fitdist(magnesium, distr = 'lnorm')
ms.ll <- fitdist(sulfur, distr = 'lnorm')

mk.ll <- fitdist(potassium, distr = 'norm')
mp.ll <- fitdist(phosphorus, distr = 'norm')
mca.ll <- fitdist(calcium, distr = 'norm')
mmg.ll <- fitdist(magnesium, distr = 'norm')
ms.ll <- fitdist(sulfur, distr = 'norm')

mk.ll <- fitdist(potassium, distr = 'exp')
mp.ll <- fitdist(phosphorus, distr = 'exp')
mca.ll <- fitdist(calcium, distr = 'exp')
mmg.ll <- fitdist(magnesium, distr = 'exp')
ms.ll <- fitdist(sulfur, distr = 'exp')

mk.ll <- fitdist(potassium, distr = 'gamma')
mp.ll <- fitdist(phosphorus, distr = 'gamma')
mca.ll <- fitdist(calcium, distr = 'gamma')
mmg.ll <- fitdist(magnesium, distr = 'gamma')
ms.ll <- fitdist(sulfur, distr = 'gamma')

mk.ll <- fitdist(potassium, distr = 'unif')
mp.ll <- fitdist(phosphorus, distr = 'unif')
mca.ll <- fitdist(calcium, distr = 'unif')
mmg.ll <- fitdist(magnesium, distr = 'unif')
ms.ll <- fitdist(sulfur, distr = 'unif')

mk.ll <- fitdist(potassium, distr = 'weibull')
mp.ll <- fitdist(phosphorus, distr = 'weibull')
mca.ll <- fitdist(calcium, distr = 'weibull')
mmg.ll <- fitdist(magnesium, distr = 'weibull')
ms.ll <- fitdist(sulfur, distr = 'weibull')

ph.ln <- fitdist(d$OM360, distr = 'lnorm')
ph <- gofstat(ph.ln)
ph$ks
ph$kstest

k <- gofstat(mk.ll)
k$ks
k$kstest

p <- gofstat(mp.ll)
p$ks
p$kstest

ca <- gofstat(mca.ll)
ca$ks
ca$kstest

mg <- gofstat(mmg.ll)
mg$ks
mg$kstest

s <- gofstat(ms.ll)
s$ks
s$kstest

# for a particular distribution, this will give the median of the KS statistic
# median for lognormal is lowest for GSS data; median of loglogistic is lowest for MLSN data
median(c(k$ks, p$ks, ca$ks, mg$ks, s$ks))

round(ln.mlsn(potassium), 0)
sum(potassium < 31) / length(potassium)

round(ln.mlsn(phosphorus), 0)
sum(phosphorus < 23) / length(phosphorus)

round(ln.mlsn(calcium), 0)
sum(calcium < 256) / length(calcium)

round(ln.mlsn(magnesium), 0)
sum(magnesium < 36) / length(magnesium)

round(ln.mlsn(sulfur), 0)
sum(sulfur < 8) / length(sulfur)

hello <- fitdist(potassium, distr = 'lnorm')
qlnorm(c(0.01, 0.5, .99),  hello$estimate[1], hello$estimate[2], lower.tail = TRUE)

hello <- fitdist(phosphorus, distr = 'lnorm')
qlnorm(c(0.01, 0.5, .99),  hello$estimate[1], hello$estimate[2], lower.tail = TRUE)

hello <- fitdist(calcium, distr = 'lnorm')
qlnorm(c(0.01, 0.5, .99),  hello$estimate[1], hello$estimate[2], lower.tail = TRUE)

hello <- fitdist(magnesium, distr = 'lnorm')
qlnorm(c(0.01, 0.5, .99),  hello$estimate[1], hello$estimate[2], lower.tail = TRUE)

hello <- fitdist(sulfur, distr = 'lnorm')
qlnorm(c(0.01, 0.5, .99),  hello$estimate[1], hello$estimate[2], lower.tail = TRUE)

qlnorm()

gof.mlsn <- gofstat(list(mk.ll, mp.ll, mca.ll, mmg.ll, ms.ll),
                    fitnames = c("k", "p", "ca", "mg", "s"))

# check all the statistics, find and replace the data vectors example k, then p, etc
mk.lnorm <- fitdist(phosphorus, distr = 'lnorm')
mk.gamma <- fitdist(phosphorus, distr = 'gamma')
mk.norm <- fitdist(phosphorus, distr = 'norm')
mk.llogis <- fitdist(phosphorus, distr = 'llogis')
mk.wei <- fitdist(phosphorus, distr = 'weibull')
mk.cauchy <- fitdist(phosphorus, distr = 'cauchy')
mk.exp <- fitdist(phosphorus, distr = 'exp')



gof <- gofstat(list(mk.lnorm, mk.gamma, mk.norm, mk.llogis, mk.wei, mk.cauchy, mk.exp),
        fitnames = c("lnorm", "gamma", "norm", "llogis", "weibull", "cauchy", "exponential"))

print(gof)
summary(gof)
gof$kstest

potassium <- d$KM3
summary(potassium)

hello <- fitdist(potassium, distr = 'lnorm')
ksim <- rlnorm(1000, hello$estimate[1], hello$estimate[2])

hello <- fitdist(potassium, distr = 'lnorm')
ksim <- rlnorm(1000, hello$estimate[1], hello$estimate[2])

p <- ggplot(data = d, aes(x = potassium))
p + # geom_density() +
  geom_density(data = as.data.frame(ksim), aes(x = ksim), colour = 'red',
              fill = 'red', alpha = 0.3) +
  geom_density(data = as.data.frame(mlsn_kdata), aes(x = mlsn_kdata), 
               colour = 'blue', fill = 'blue', alpha = 0.3)

mlsn.k <- mlsn(potassium)
mlsn2.k <- mlsn2(potassium)
mlsnln.k <- ln.mlsn(potassium)

