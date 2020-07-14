# amake a summary table of GSS data

source('r/libraries.R')
source('r/functions.R')
source('r/read_data.R')

# calculates values for a parameter for use in summary table
sum.x <- function(x) {
  Count <- length(x)
  Minimum <- min(x)
  Median <- median(x)
  Mean <- mean(x)
  Maximum <- max(x)
 # MLSN <- mlsn2(x)
  return(c(Count, Minimum, Median, Mean, Maximum))
}

gss <- d

# sum.x function will calculate n, min, max, mean, median
# do this for a table to include pH, OM, K, P, Ca, Mg, S

pH <- sum.x(gss$pH)
pH[4] <- "NA"
# pH <- as.numeric(pH)
OM <- sum.x(gss$OM360 * 10)
K <- sum.x(gss$KM3)
P <- sum.x(gss$PM3)
Ca <- sum.x(gssCa$CaM3)
Mg <- sum.x(gss$MgM3)
S <- sum.x(gss$SM3)

parameters <- c("pH", "OM %", "K ppm",
                "P ppm", "Ca ppm", "Mg ppm", "S ppm")

sum.table <- as.data.frame(rbind(pH, OM, K, P, Ca, Mg, S))

sum.table <- cbind(parameters, sum.table)

colnames(sum.table) <- c("Soil parameter", "n", "Min", 
                         "Median", "Mean", "Max")

sum.table$Median <- prettyNum(as.numeric(as.character(sum.table$Median)), digits = 2)
sum.table$Mean <- prettyNum(as.numeric(as.character(sum.table$Mean)), digits = 2)
sum.table$Max <- prettyNum(as.numeric(as.character(sum.table$Max)), digits = 3)

options(xtable.comment = FALSE)
options(xtable.booktabs = TRUE)
sum.table <- xtable(sum.table, caption = "Summary of Global Soil Survey data from September 2013 through August 2016. Five calcareous samples with pH > 7.7 and Mehlich 3 Ca > \\SI{3000}{\\mg \\per \\kg} were omitted from the summary analysis of Ca because the Mehlich 3 extractant dissolves some of the calcium carbonate in such samples.", label = "tab:sumtable")
digits(sum.table) <- c(0, 0, 0, 0, 0, 0, 0)
print(sum.table, include.rownames = FALSE)


