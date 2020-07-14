# define functions

# figure out how to assign uniform distribution
# to below detection values

# OM360, min = 0.1
# Cl min = 1
# M3P 1
# Bray, appears 3?
# Cu, B 0.2
# Zn 0.4
# NO3 and NH4, 0.5

fix_below_detection <- function(v, limit) {
  fix_length <- length(v)
  filled_v <- ifelse(v == 0 | is.na(v),
                      runif(fix_length, 0, limit),
                      v)
  
  return(filled_v)
}

# run the sim data as functions
# then print the charts individually for ease of changing axis labels and so on

sim_mlsn <- function(parameter, n) {
  # first get rid of NA's
  # and in the case of elements set to 0 for below detection, set as NA
  
  d <- ifelse(parameter == 0, NA, parameter)
  d <- d[is.na(d) == FALSE]
  
  fit.mlsn <- vglm(d ~ 1, fisk)
  z <- Coef(fit.mlsn)
  
  m_sim_data <- rfisk(n, z[1], z[2])
  
  m_sim_data <- as.data.frame(m_sim_data)
  
  max_mlsn <- qfisk(0.97, z[1], z[2])
  
  return(list(m_sim_data, max_mlsn))
}

sim_gss <- function(parameter, n) {
  # first get rid of NA's
  # and in the case of elements set to 0 for below detection, set as NA
  
  d <- ifelse(parameter == 0, NA, parameter)
  d <- d[is.na(d) == FALSE]
  
  gss.fit <- fitdist(d, distr = 'lnorm', lower = c(0, 0))
  
  g_sim_data <- rlnorm(n, gss.fit[["estimate"]][1], gss.fit[["estimate"]][2])
  g_sim_data <- as.data.frame(g_sim_data)
  
  max_gss <- max_gss <- qlnorm(0.97, gss.fit[["estimate"]][1], gss.fit[["estimate"]][2])
  
  return(list(g_sim_data, max_gss))
}

# fit a lognormal to a vector of GSS data
# return the dlnorm values as a df and return the 0.97 quantile as a max
g_dist <- function(x) {
  
  d <- ifelse(x == 0, NA, x)
  d <- d[is.na(d) == FALSE]
  
  gss.fit <- fitdist(d, distr = 'lnorm')
  
  values <- gss.fit[['estimate']]
  
  g97 <- qlnorm(0.97, values[1], values[2])
  
  g99 <- round(qlnorm(0.999, values[1], values[2]))
  
  x.values <- seq(0.01, g99, g99/500)
  
  y.values <- dlnorm(x.values, values[1], values[2])
  
  out.frame <- cbind.data.frame(x.values, y.values)
  
  return(list(out.frame, g97))
  
}

# fit a loglogistic to a vector of MLSN data
# return the llogis values as a df and return the 0.97 quantile as a max
m_dist <- function(x) {
  
  d <- ifelse(x == 0, NA, x)
  d <- d[is.na(d) == FALSE]
  
  fit.mlsn <- vglm(d ~ 1, fisk)
  z <- Coef(fit.mlsn)
  
  m97 <- qfisk(0.97, z[1], z[2])
  
  m99 <- round(qfisk(0.999, z[1], z[2]))
  
  x.values <- seq(0.01, m99, m99/500)
  
  y.values <- dfisk(x.values, z[1], z[2])
  
  out.frame <- cbind.data.frame(x.values, y.values)
  
  return(list(out.frame, m97))
  
}

# function returns log normal mlsn
ln.mlsn <- function(x) {
  hello <- fitdist(x, distr = 'lnorm')
  mlsn <- qlnorm(0.1, hello$estimate[1], hello$estimate[2], lower.tail = TRUE)
  return(mlsn)
}

# generates an mlsn value for a vector using log logistic distribution
mlsn <- function(x) {
  fit.x <- vglm(x ~ 1, fisk)
  z <- Coef(fit.x)
  new.mlsn <- qfisk(0.1, z[1], z[2])
  return(round(new.mlsn, 0))
}

# function returns a log logistic mlsn with log and exp
mlsn2 <- function(x) {
  
  # updated on 20160708, wouldn't fit Ca so take log of
  # the input vector before fitting
  forFit <- log(x)
  fit.x <- vglm(forFit ~ 1, fisk)
  z <- Coef(fit.x)
  new.mlsn <- qfisk(0.1, z[1], z[2])
  return(round(exp(new.mlsn), 0))
}
