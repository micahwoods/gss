# this makes a table that shows mean and sd of the distributions
# first describe the parameters
# K to S

source('r/libraries.R')
source('r/functions.R')
source('r/read_data.R')

#ph.ln <- fitdist(d$pH, distr = 'lnorm')
#om.ln <- fitdist(d$OM360, distr = 'lnorm')
k.ln <- fitdist(d$KM3, distr = 'lnorm')
p.ln <- fitdist(d$PM3, distr = 'lnorm')
ca.ln <- fitdist(gssCa$CaM3, distr = 'lnorm')
mg.ln <- fitdist(d$MgM3, distr = 'lnorm')
s.ln <- fitdist(d$SM3, distr = 'lnorm')

krow <- cbind('K', k.ln$estimate[1], k.ln$estimate[2])
prow <- cbind('P', p.ln$estimate[1], p.ln$estimate[2])
carow <- cbind('Ca', ca.ln$estimate[1], ca.ln$estimate[2])
mgrow <- cbind('Mg', mg.ln$estimate[1], mg.ln$estimate[2])
srow <- cbind('S', s.ln$estimate[1], s.ln$estimate[2])

ln.table <- as.data.frame(rbind(krow, prow, carow, mgrow, srow))

colnames(ln.table) <- c("Soil parameter", "μ", "σ")

ln.table$μ <- round(as.numeric(as.character(ln.table$μ)), 3)
ln.table$σ <- round(as.numeric(as.character(ln.table$σ)), 4)

options(xtable.comment = FALSE)
options(xtable.booktabs = TRUE)
ln.table <- xtable(ln.table, caption = "Parameters for the lognormal distribution fit to the Global Soil Survey Mehlich 3 data in \\SI{}{\\mg\\per\\kg}, where μ is the mean of the logarithm and σ is the standard deviation of the logarithm.", label = "tab:lntable")
#digits(ln.table) <- c(0, 0, 3, 3)
print(ln.table, include.rownames = FALSE)

