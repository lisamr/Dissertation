# pca script
# 

source("R/lmms_with_s_p_nutrients.R")
library(vegan)
library(ggbiplot) # must be installed via devtools::install_github("vqv/ggbiplot")


pca13 = princomp(decostand(dplyr::select(glmm13,
                                  Bare,
                                  Litter,
                                  Crypto,
                                  AIG,
                                  AIF,
                                  ANF,
                                  PNF,
                                  PNG,
                                  Shrubs
                                  ), "standardize"), scores=T)
ggbiplot(pca13, labels =  factor(glmm13$Site.type)) +
  theme_bw() +
  ggtitle("2013")

pca16 = princomp(decostand(dplyr::select(glmm16,
                                         Bare,
                                         Litter,
                                         Crypto,
                                         AIG,
                                         AIF,
                                         ANF,
                                         PNF,
                                         PNG,
                                         Shrubs
                                         ), "standardize"), scores=T)
ggbiplot(pca16, labels =  factor(glmm13$Site.type)) +
  theme_bw() +
  ggtitle("2016")

pca_all = princomp(decostand(dplyr::select(all,
                                         Bare,
                                         Litter,
                                         Crypto,
                                         AIG,
                                         AIF,
                                         ANF,
                                         PNF,
                                         PNG,
                                         Shrubs
), "standardize"), scores=T, cor = FALSE)
ggbiplot(pca_all, labels =  factor(all$Site.type)) +
  theme_bw() +
  ggtitle("all")
loadings(pca_all)
all$pca1_inv <- pca_all$scores[,1]
