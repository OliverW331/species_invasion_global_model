## Generating invasions data, using stuff from various sources
# Places into model_data2 directory
# Generates:
# A - socioeconomic data array
# C - first sightings matrix
# D - pairwise non-temporal data array
# Tr - pairwise temporal array

rm(list=ls());gc()
graphics.off()

setwd("C:/McGill/Environment Honor Thesis/Euro Model/invasions_eu-main/invasions_eu-main/data")

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
C.raw = readRDS("./newdata_122022/YearFirstInv_matrix2.rds")

C = C.raw

# Remove first column ('Country')
rownames(C) = C[,1]
C <- C[, -c(1)]
# Convert to matrix
C <- as.matrix(C)

C[which(C <= 1995)] <- 1995 # Set everything before 1995 as possible source.
C[which(C > 2018)] <- NA # Dont have pred data past 2018 at the moment, so treat as 'not yet invaded'

# See which species have not invaded Europe, and exclude
not_pres=apply(C,2,function(x){all(is.na(x))})
C=C[,-which(not_pres)]

# Modify for 1995=1 to 2018=24
byr = min(C, na.rm = TRUE)
C = C - byr + 1

### Generating A ####
# A: country-year data
# Array of dimensions: year, country, variable
# Convert everything into years since b_yr (index)
# I.e., 1995 => 1 to 2018 => 24

### SOCIO-ECONOMIC DATA (non-temporal)

A.raw = readRDS("./newdata_122022/country_for_dat.rds")
colnames(A.raw)[which(colnames(A.raw)=="ias_herps_mainlain")] = "ias_herps_mainland"
# Columns of interest
A.raw.cols = c("react_manage", "proact_manage", "total_manage", paste0("PC",1:6),
              "surface_area", "prot_area", "n_neighb_count", "total_land_bord",
              "ias_all_mainland", "ias_plants_mainland", "ias_ants_mainland",
              "ias_spiders_mainland", "ias_herps_mainland","ias_birds_mainland",
              "ias_mammals_mainland", "surv_eff_plants", "surv_eff_amphib",
              "surv_eff_amphib", "surv_eff_mammals", "surv_eff_birds", 
              "survey_effort")

# A is already in long form, so we rep and combine to generate an array
row.names(A.raw) = A.raw$country
A = abind(lapply(1:24, FUN = function(x){
                                          return(as.matrix(A.raw[,A.raw.cols]))
                                        }), along=0)
# FYI: indices 1:24 are 1995:2018
dimnames(A)[[1]] = as.character(1:24)


#### TRADE DATA
# Read in BACI, to get total import/export (annual)
# (We already have 'Tr' so we don't need to do it again)
Tr = readRDS("./baci_1995to2018_array.rds")
Tr = `Tr-baci1995to2018-array`
# Aggregate Tr (assuming this is an array of array of i,j,year)
# Is this Weight?
Tr.exp = apply(Tr, c(1,3), sum, na.rm = TRUE)
Tr.imp = apply(Tr, c(2,3), sum, na.rm = TRUE)


#### GDP/Population data
# Read in GDP and Population (WorldBank)
# Note that I had to modify the original CSV file to remove the header
gdp = read.csv("./newdata_122022/gdp/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4701247.csv", 
                stringsAsFactor = FALSE)
pop = read.csv("./newdata_122022/pop/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv", 
                stringsAsFactor = FALSE)
# Subset to 1995-2018
gdp = gdp[,c("Country.Name", "Country.Code", paste0("X",1995:2018))]
pop = pop[,c("Country.Name", "Country.Code", paste0("X",1995:2018))]
# Subset to countries of interest
# Note that 'Slovakia' is also 'Slovak Republic' (so we change it!)
gdp$Country.Name[which(gdp$Country.Name=='Slovak Republic')] = 'Slovakia'
pop$Country.Name[which(pop$Country.Name=='Slovak Republic')] = 'Slovakia'
gdp = gdp[gdp$Country.Name %in% rownames(C),]
pop = pop[pop$Country.Name %in% rownames(C),]

# Combine Tr.exp and Tr.imp into a single array
A.Tr = abind(Tr.exp, Tr.imp, along = 0)
dimnames(A.Tr)[[1]] = c("trade_export_sum", "trade_import_sum")
dimnames(A.Tr)[[3]] = as.character(1:24)
A.Tr = aperm(A.Tr, c(3,2,1))
# Exclude the countries not in A
A.Tr = A.Tr[,dimnames(A.Tr)[[2]] %in% dimnames(A)[[2]],]

# Add A.Tr to A
A = abind(A, A.Tr, along = 3)

# Generate array of dimension (nyr, nc, var) using gdp, pop
rownames(gdp) = gdp$Country.Name
gdp = gdp[dimnames(A)[[2]],]
gdp = as.matrix(gdp[,paste0("X",1995:2018)])

rownames(pop) = pop$Country.Name
pop = pop[dimnames(A)[[2]],]
pop = as.matrix(pop[,paste0("X",1995:2018)])

A.wdi = abind(gdp, pop, along = 0)
dimnames(A.wdi)[[1]] = c("gdp", "population")
dimnames(A.wdi)[[3]] = as.character(1:24)
A.wdi = aperm(A.wdi, c(3,2,1))

# Add to A
A = abind(A, A.wdi, along = 3)


#### GENERATING D ####
# Country pair data, most normalized
# Non-temporal
D.raw = readRDS("./newdata_122022/pairs_for_dat.rds")
# Columns of interest
# What is `overlap`?
D.raw.cols = c("phys_dist", "shared_border", "overlap","ias_all_comm_sim",
              "ias_plants_comm_sim", "ias_inverts_comm_sim", 
              "ias_vertebr_comm_sim")

# Generates matrices of row countries i to columns country j
D = lapply(D.raw.cols, FUN = function(vv){
  out = D.raw[,c("Country_A","Country_B",vv)]
  out = spread(out, key = "Country_B", value = vv)
  rownames(out) = out[,1]
  return(out[,-1])
})

# abind for array
D = abind(D, along = 0)
dimnames(D)[[1]] = D.raw.cols
# 2 = i, 3 = j, 1 = var
# Re-order to nc_i, nc_j, var
D = aperm(D, c(2,3,1))


# Plot histograms
if(sw.plotvars){
  # Plot A
  fout = "../figs/3_newData/"
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
Tr = Tr[dimnames(A)[[2]],dimnames(A)[[2]],]
dimnames(Tr)[[3]] = as.character(1:24)

# Write out (let's call this version 2)
saveRDS(A, "./model_data2/A-socioEcoDat-array-v2.rds")
saveRDS(C, "./model_data2/C-firstSightings-matrix-v2.rds")
saveRDS(D, "./model_data2/D-pairwiseData-array-v2.rds")
saveRDS(Tr, "./model_data2/Tr-baci1995to2018-array.rds")


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
write.csv(Var, "./model_data2/Var-variable_switches.csv", row.names = FALSE)