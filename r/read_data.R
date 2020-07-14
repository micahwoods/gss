# read data and
# make data vectors for mlsn and gss data

# run the libraries.R and functions.R files first to set it up
source("r/libraries.R")
source("r/functions.R")

# read the GSS data
d <- read.csv('data/gss_data.csv',
              header = TRUE, stringsAsFactors = FALSE)

# fix below detection values by assigning them a value from a uniform
# distribution with limits of 0 and the detection limit
# if this adjustment it not made, it skews the distributions high because
# below detection data would be omitted; alternatively setting below detection to 0
# biases slightly low, because the values aren't 0, they are below detection
d$Cl <- fix_below_detection(d$Cl, 1)
d$BM3 <- fix_below_detection(d$BM3, 0.2)
d$PB2 <- fix_below_detection(d$PB2, 3)
d$NO3N <- fix_below_detection(d$NO3N, 0.5)
d$NH4N <- fix_below_detection(d$NH4N, 0.5)

# make a cut for Ca only, to eliminate apparent calcareous at CaM3 > 3000
# this is <= 3000 or pH <= 7.7; the opposite, or what is filtered out, must
# satisfy both > 3000 and pH > 7.7
gssCa <- filter(d, CaM3 <= 3000 | pH <= 7.7)

# read the mlsn data for reference too
atc <- read.csv("https://raw.githubusercontent.com/micahwoods/2016_mlsn_paper/master/data/atc_samples.csv",
                header = TRUE, stringsAsFactors = FALSE)

# cut to get the data with desired values of pH and TEC
atc <- filter(atc, pH >= 5.5 & pH <= 8.5 & TECM3 <= 6)

pace <- read.csv("https://raw.githubusercontent.com/micahwoods/2016_mlsn_paper/master/data/pace_samples.csv", 
                 sep = ",", header = TRUE)

# cut to get the data with desired values of pH and TEC
pace <- filter(pace, pH >= 5.5 & pH <= 8.5 & TECM3 <= 6)

# make data vectors for 
ph_mlsn <- c(pace$pH, atc$pH)
om_mlsn <- c(pace$OM360, atc$OM360)

# if there are NAs not from test not done, but from below detection, 
# or if below detection have been set to 0, fix
om_mlsn <- fix_below_detection(om_mlsn, 0.1)

k_mlsn <- c(pace$KM3, atc$KM3)
k_mlsn <- fix_below_detection(k_mlsn, 4)

p_mlsn <- c(pace$PM3, atc$PM3)

bray_mlsn <- c(pace$PB2, atc$PB2)
bray_mlsn <- fix_below_detection(bray_mlsn, 3)

# NAs in this were all or almost all not run
# so don't make an adjustment to these NAs
op_mlsn <- c(pace$PO, atc$PO)

ca_mlsn <- c(pace$CaM3, atc$CaM3)
mg_mlsn <- c(pace$MgM3, atc$MgM3)

s_mlsn <- c(pace$SM3, atc$SM3)
s_mlsn <- fix_below_detection(s_mlsn, 2)

fe_mlsn <- c(pace$FeM3, atc$FeM3)

mn_mlsn <- c(pace$MnM3, atc$MnM3)
mn_mlsn <- fix_below_detection(mn_mlsn, 1)

# NAs in this were all (or almost all) not run
cl_mlsn <- pace$ClM3

b_mlsn <- c(pace$BM3, atc$BM3)
b_mlsn <- fix_below_detection(b_mlsn, 0.2)

cu_mlsn <- c(pace$CuM3, atc$CuM3)
cu_mlsn <- fix_below_detection(cu_mlsn, 0.2)

zn_mlsn <- c(pace$ZnM3, atc$ZnM3)
zn_mlsn <- fix_below_detection(zn_mlsn, 0.4)

no3_mlsn <- c(pace$NO3N, atc$NO3N)
no3_mlsn <- fix_below_detection(no3_mlsn, 0.5)

nh4_mlsn <- c(pace$NH4N, atc$NH4N)
nh4_mlsn <- fix_below_detection(nh4_mlsn, 0.5)

# NAs in this were not run
ec_mlsn <- c(pace$EC12, atc$EC)

