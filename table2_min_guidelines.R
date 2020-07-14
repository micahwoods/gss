# make table 2 for gss article

# these scripts load libraries and define functions
# then read the data and organize it, deal with below detection values by estimation, etc
source('r/libraries.R')
source('r/functions.R')
source('r/read_data.R')

# the mlsn data are from the read_data.R file formatted as
# vectors such as k_mlsn, p_mlsn, etc

# these are the gss vectors for K to S
KM3 <- d$KM3
PM3 <- d$PM3
CaM3 <- gssCa$CaM3
MgM3 <- d$MgM3
SM3 <- d$SM3

# calculates comparison values
# x is the gss vector to fit lnorm, x2 is the mlsn vector to fit llogis, high is SLAN no response level
comp.x <- function(x, x2, high) {
    fit.x <- fitdist(x, distr = 'lnorm')
    gss10 <- qlnorm(0.1, fit.x$estimate[1], fit.x$estimate[2])
    fit.mlsn <- vglm(x2 ~ 1, fisk)
    coef.mlsn <- Coef(fit.mlsn)
    k10 <- qfisk(0.1, coef.mlsn[1], coef.mlsn[2])
  return(c(gss10, k10, high))
}

krow <- comp.x(KM3, k_mlsn, 117)
prow <- comp.x(PM3, p_mlsn, 55)
carow <- comp.x(CaM3, ca_mlsn, 751)
mgrow <- comp.x(MgM3, mg_mlsn, 121)
srow <- comp.x(SM3, s_mlsn, 41)

t3 <- rbind.data.frame(krow, prow, carow, mgrow, srow)
colnames(t3) <- c('GSS10', 'MLSN10', 'SLAN')
t3$element <- c('K', 'P', 'Ca', 'Mg', 'S')

t3 <- t3[, c(4, 1:3)]

options(xtable.comment = FALSE)
options(xtable.booktabs = TRUE)
compare.table <- xtable(t3, caption = "compare table", label = "tab:comparetable")
digits(compare.table) <- c(0, 0, 0, 0, 0)
print(compare.table, include.rownames = FALSE)

# for the conclusion

medium <- subset(d, KM3 < 117 |
                   PM3 < 55 |
                   CaM3 < 751 |
                   MgM3 < 121 |
                   SM3 < 41)

# 160 of 162 are less than high in some category

160/162

low <- subset(d, KM3 < 50 |
                PM3 < 26 |
                CaM3 < 500 |
                MgM3 < 60 |
                SM3 < 15)

# 129 of 162 are less than medium in some category

129/162

