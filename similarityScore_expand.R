#Adapated from:
#Doucet GE, Moser DA, Rodrigue A, Bassett DS, Glahn DC, Frangou S (2019): Person-Based Brain Morphometric
#Similarity is Heritable and Correlates With Biological Features. Cereb Cortex 1991 29: 852–862.

#HC control
#PT patient

HC_file <- '/PATH/TO/HC.csv'
PT_file <- '/PATH/TO/PT.csv'

# 1. Calculate PBSI_HC --------------
M_HC <- read.csv(HC_file, header = F)

SIMS_HC <- cor(t(M_HC), use= "everything", method = "spearman")
v_HC <- c()

for (i in 1:ncol(SIMS_HC)){
  mat_HC <- sum(SIMS_HC[which(!is.na(SIMS_HC[,i])), i])
  v_HC <- c(v_HC, mat_HC)
}

PBSI_HC <- (v_HC-1)/((dim(SIMS_HC)[1])-1)
PBSI_HC <- t(PBSI_HC)

# Check for outliers
mHOMS_HC <- mean(PBSI_HC)
sHOMS_HC <- sd(PBSI_HC)
outliersN_HC <- mHOMS_HC - 2*sHOMS_HC # - 2SD
outliersP_HC <- mHOMS_HC + 2*sHOMS_HC # + 2SD

outliers_HC <- length(which(PBSI_HC < outliersN_HC)) + 
  length(which(PBSI_HC > outliersP_HC))
if (outliers_HC != 0){
  cat('\n Check your subjects, you seem to have', outliers_HC, 'HC outliers.')
}

# 2. Calculate PBSI_PT --------------
M_PT <- read.csv(PT_file, header = F)

SIMS_PT <- cor(t(M_PT), use= "everything", method = "spearman")
v_PT <- c()

for (j in 1:ncol(SIMS_PT)){
  mat_PT <- sum(SIMS_PT[which(!is.na(SIMS_PT[,j])), j])
  v_PT <- c(v_PT, mat_PT)
}

PBSI_PT <- (v_PT-1)/((dim(SIMS_PT)[1])-1)
PBSI_PT <- t(PBSI_PT)

# Check for outliers
mHOMS_PT <- mean(PBSI_PT)
sHOMS_PT <- sd(PBSI_PT)
outliersN_PT <- mHOMS_PT - 2*sHOMS_PT # - 2SD
outliersP_PT <- mHOMS_PT + 2*sHOMS_PT # + 2SD

outliers_PT <- length(which(PBSI_PT < outliersN_PT)) + 
  length(which(PBSI_PT > outliersP_PT))
if (outliers_PT != 0){
  cat('\n Check your subjects, you seem to have', outliers_PT, 'PT outliers.')
}

# 3. Calculate PBSI_PT_HC --------------

PBSI_PT_HC <- c()
for (k in 1:nrow(M_PT)){
  
  subj_PT <- M_PT[k,]
  
  SIMS_PT_HC <- cor(x = t(M_HC), y = t(subj_PT), use= "everything", method = "spearman")
  v_PT_HC <- c()
  
  for (i in 1:ncol(SIMS_PT_HC)){
    mat_PT_HC <- sum(SIMS_PT_HC[which(!is.na(SIMS_PT_HC[,i])), i])
    v_PT_HC <- c(v_PT_HC, mat_PT_HC)
  }
  
  PBSI_PT_HC <- c(PBSI_PT_HC, (v_PT_HC-1)/((dim(SIMS_PT_HC)[1])-1))
}

PBSI_PT_HC <- t(PBSI_PT_HC)

# Check for outliers
mHOMS_PT_HC <- mean(PBSI_PT_HC)
sHOMS_PT_HC <- sd(PBSI_PT_HC)
outliersN_PT_HC <- mHOMS_PT_HC - 2*sHOMS_PT_HC # - 2SD
outliersP_PT_HC <- mHOMS_PT_HC + 2*sHOMS_PT_HC # + 2SD

outliers_PT_HC <- length(which(PBSI_PT_HC < outliersN_PT_HC)) + 
  length(which(PBSI_PT_HC > outliersP_PT_HC))
if (outliers_PT_HC != 0){
  cat('\n Check your subjects, you seem to have', outliers_PT_HC, 'PT-HC outliers.')
}
