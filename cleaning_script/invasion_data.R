## Generating invasions data, using stuff from various sources
# Places into model_data2 directory
# Generates:
# A - socioeconomic data array
# C - first sightings matrix
# D - pairwise non-temporal data array
# Tr - pairwise temporal array

rm(list=ls());gc()
graphics.off()

setwd("D:/Environment Honor Thesis/global_model/cleaned_data")

#### LIBRARIES ####

library(abind) # For abind
library(tidyr) # For spread()


#### SWITCHES ####

# Plot histograms for all variables in A/C/D
# In "../figs/3_newData/"
sw.plotvars = TRUE

sw.transform = TRUE # Log-transform
# Which variables to log. If NA, don't log any
sw.logvars = c(#NA 
  "trade_export_sum",
  "trade_import_sum",
  "gdp", 
  "population"
)

#### BODY ####

### Generating C ####
### FIRST INVASIONS MATRIX
C.raw = readRDS("sp.rds")

C = C.raw

# Remove first column ('Country')
rownames(C) = C[,1]
C <- C[, -c(1)]
# Convert to matrix
C <- as.matrix(C)

C[which(C <= 1995)] <- 1995 # Set everything before 1995 as possible source.
C[which(C > 2016)] <- NA # Dont have pred data past 2018 at the moment, so treat as 'not yet invaded'

# See which species have not invaded Europe, and exclude
not_pres=apply(C,2,function(x){all(is.na(x))})
C=C[,-which(not_pres)]

# Modify for 1995=1 to 2016=22
byr = min(C, na.rm = TRUE)
C = C - byr + 1



#### TRADE DATA
# Read in BACI, to get total import/export (annual)
# (We already have 'Tr' so we don't need to do it again)
Tr = readRDS("bi_tra.rds")
#Tr = `Tr-baci1995to2018-array`
# Aggregate Tr (assuming this is an array of array of i,j,year)
# Is this Weight?
Tr.exp = apply(Tr, c(1,3), sum, na.rm = TRUE)
Tr.imp = apply(Tr, c(2,3), sum, na.rm = TRUE)



### Generating A ####
# A: country-year data
# Array of dimensions: year, country, variable
# Convert everything into years since b_yr (index)
# I.e., 1995 => 1 to 2018 => 24

### SOCIO-ECONOMIC DATA (non-temporal)
A = list()
#### GDP/Population data
# Read in GDP and Population (WorldBank)
# Note that I had to modify the original CSV file to remove the header
gdp = readRDS("gdp.rds")
pop = readRDS("pop.rds")


# Combine Tr.exp and Tr.imp into a single array
A.Tr = abind(Tr.exp, Tr.imp, along = 0)
dimnames(A.Tr)[[1]] = c("trade_export_sum", "trade_import_sum")
dimnames(A.Tr)[[3]] = as.character(1:22)
A.Tr = aperm(A.Tr, c(3,2,1))
# # Exclude the countries not in A
# A.Tr = A.Tr[,dimnames(A.Tr)[[2]] %in% dimnames(A)[[2]],]

# Add A.Tr to A
A = A.Tr

# Generate array of dimension (nyr, nc, var) using gdp, pop
rownames(gdp) = gdp$Country.Code
gdp = gdp[dimnames(A)[[2]],]
gdp = as.matrix(gdp[,paste0("X",1995:2016)])

rownames(pop) = pop$Country.Code
pop = pop[dimnames(A)[[2]],]
pop = as.matrix(pop[,paste0("X",1995:2016)])

A.wdi = abind(gdp, pop, along = 0)
dimnames(A.wdi)[[1]] = c("gdp", "population")
dimnames(A.wdi)[[3]] = as.character(1:22)
A.wdi = aperm(A.wdi, c(3,2,1))

# Add to A
A = abind(A, A.wdi, along = 3)

#### GENERATING D ####
# Country pair data, most normalized
# Non-temporal
D.raw = readRDS("bi_dis.rds")
# Columns of interest
# What is `overlap`?
D.raw.cols = c("phys_dist")

# Generates matrices of row countries i to columns country j
# D = lapply(D.raw.cols, FUN = function(vv){
#   out = D.raw[,c("Country_A","Country_B",vv)]
#   out = spread(out, key = "Country_B", value = vv)
#   rownames(out) = out[,1]
#   return(out[,-1])
# })

# abind for array
D = abind(D.raw, along = 0)
dimnames(D)[[1]] = D.raw.cols
# 2 = i, 3 = j, 1 = var
# Re-order to nc_i, nc_j, var
D = aperm(D, c(2,3,1))


# Plot histograms
if(sw.plotvars){
  # Plot A
  fout = "../cleaned_data_figs/data_v1/"
  temp.vars = dimnames(A)[[3]]  
  for(dd in temp.vars){
    print(dd)
    png(paste0(fout,"A-",dd,".png"), 500, 500)
    hist(A[,,dd], main = dd)
    dev.off()
  }
  
  # Plot D
  temp.vars = dimnames(D)[[3]]
  for(dd in temp.vars){
    print(dd)
    png(paste0(fout,"D-",dd,".png"), 500, 500)
    hist(D[,,dd], main = dd)
    dev.off()
  }
}

## Transform the data now
if(sw.transform){
  # Log-transform gdp/trade/population
  if(!all(is.na(sw.logvars))){
    A[,,sw.logvars] = log(A[,,sw.logvars])
  }
  # Normalize A
  for(i in 1:dim(A)[3]){
    print(i)
    A[,,i] = scale(A[,,i])
  }
  # Normalize D
  for(i in 1:dim(D)[3]){
    print(i)
    D[,,i] = scale(D[,,i])
  }
}

# Modify Tr
dimnames(Tr)[[3]] = as.character(1:22)

# Write out (let's call this version 2)
saveRDS(A, "../model/model_data/A-socioEcoDat-array.rds")
saveRDS(C, "../model/model_data/C-firstSightings-matrix.rds")
saveRDS(D, "../model/model_data/D-pairwiseData-array.rds")
saveRDS(Tr, "../model/model_data/Tr-baci1995to2018-array.rds")


# Generate a table for variable switches
pp.vars = c(paste0(dimnames(A)[[3]],".src"), paste0(dimnames(A)[[3]],".dst"), dimnames(D)[[3]], "bilateral_trade")
Var = data.frame(
  # No species variables yet
  type = c(rep("pp.vars", length(pp.vars)), "sp.vars", "time.vars"),
  source = c(rep("A", length(dimnames(A)[[3]])*2), rep("D", length(dimnames(D)[[3]])), "Tr", "B", NA),
  variable = c(pp.vars, NA, "t"),
  include = FALSE, # Switch to 'TRUE' when including in the model
  starting.value = 0
)
write.csv(Var, "../model/model_data/Var-variable_switches.csv", row.names = FALSE)

