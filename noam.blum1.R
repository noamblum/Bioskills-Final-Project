#!/usr/bin/env Rscript
# Bioskills course 2022 final project by Noam Blum - noam.blum1@mail.huji.ac.il
# Script used to generate both graphs present in the project's PDF
# Last edited - 16/07/2022

load("scaled_tcell_score.rdata")
load("scaled_bcell_score.rdata")
unique(cf_lymph_conditions)
nrow(scaled_tcell_score[scaled_tcell_score$condition == "healthy",])
nrow(scaled_tcell_score[scaled_tcell_score$condition == "covid",])
nrow(scaled_tcell_score[scaled_tcell_score$condition == "sclc",])
# Exploratory graph - show the signature score of all conditions normalized by
#                     the median of healthy samples - will show the percentage of activation
# Explanatory graph - compare normalized t-cell and b-cell signature of covid to
#                     those of sclc (lung cancer) to determine if there are differences
#                     This is meant to compare a viral lung disease to lung cancer