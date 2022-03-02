# $$$$$$$$$$$$$$$$ $$$$$$$ $$$$$$$$$$$$$$$$
# $$$$$$$$$$$$$$$$ PolyFun $$$$$$$$$$$$$$$$
# $$$$$$$$$$$$$$$$ $$$$$$$ $$$$$$$$$$$$$$$$
# https://github.com/omerwe/polyfun

##-------------------------------------------------------------
# There are three ways to run PolyFun:

# 1. Using precomputed prior causal probabilities of 19 million imputed UK Biobank SNPs with MAF>0.1%, based on a meta-analysis of 15 UK Biobank traits. This is the simplest approach, but it may not include all your SNPs of interest (especially when analyzing non-European populations) and the prior causal probabilities may not be optimal for some traits.

# 2. Computing prior causal probabilities via an L2-regularized extension of stratified LD-score regression (S-LDSC). This is a relatively simple approach, but the prior causal probabilities may not be robust to modeling misspecification.

# 3. Computing prior causal probabilities non-parametrically. This is the most robust approach, but it is computationally intensive and requires access to individual-level genotypic data from a large reference panel (optimally >10,000 population-matched individuals).
##-------------------------------------------------------------


# GitHub Notes:
## How to pull changes from original repo into the forked repo
# https://digitaldrummerj.me/git-syncing-fork-with-original-repo/


# %%%%%%%%%%%%%%%% PolyFun approach 1 %%%%%%%%%%%%%%%%
## Using precomputed prior causal probabilities based on a meta-analysis of 15 UK Biobank traits


# %%%%%%%%%%%%%%%% PolyFun approaches 2 & 3 %%%%%%%%%%%%%%%%
## 2. Computing prior causal probabilities via an L2-regularized extension of S-LDSC
## 3. Computing prior causal probabilities non-parametrically


# %%%%%%%%%%%%%%%% Run PolyFun+SUSIE %%%%%%%%%%%%%%%%
