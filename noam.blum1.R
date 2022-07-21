#!/usr/bin/env Rscript
# Bioskills course 2022 final project by Noam Blum - noam.blum1@mail.huji.ac.il
# Script used to generate both graphs present in the project's PDF
# Last edited - 21/07/2022
library(tidyverse)
library(ggplot2)
library(RColorBrewer)



# Load data
load("scaled_tcell_score.rdata")
load("scaled_bcell_score.rdata")

# Standardize data
desired_conditions <- c(  "healthy", 
                          "sport", 
                          "AIH",
                          "ibd",
                          "sclc",
                          "melanoma",
                          "covid",
                          "transplant"
                          )

tcell <- scaled_tcell_score[,c("condition", "sig1_score")]
tcell$type <- "T-Cell"

bcell <- scaled_bcell_score[,c("condition", "sig1_score")]
bcell$type <- "B-Cell"

unified <- rbind(tcell, bcell)
unified <- unified %>% rename(score = sig1_score)
unified <- unified[unified$condition %in% desired_conditions,]


# Exploratory graph - show the signature score of all conditions as a percentage
#                     of the median signature of healthy samples

# Calculate median
unified.healthy <- unified[unified$condition == "healthy",] 
tcell.median <- median(unified.healthy$score[unified.healthy$type=="T-Cell"])
bcell.median <- median(unified.healthy$score[unified.healthy$type=="B-Cell"])

# Create normalized dataframe
unified.normalized <- unified
unified.normalized$score[unified.normalized$type == "T-Cell"] <- 
  (unified.normalized$score[unified.normalized$type == "T-Cell"] / tcell.median)
unified.normalized$score[unified.normalized$type == "B-Cell"] <- 
  (unified.normalized$score[unified.normalized$type == "B-Cell"] / bcell.median)

# Fraction to percentage
unified.normalized$score <- unified.normalized$score * 100
cond_display_names <- c(  "Healthy", 
                          "Post Sport", 
                          "Autoimmune Hepatitis",
                          "Inflammatory Bowel Disease",
                          "Small Cell Lung Cancer",
                          "Melanoma",
                          "COVID-19",
                          "Transplant")

unified.normalized$condition <- factor(as.character(unified.normalized$condition),
                           levels = desired_conditions)
levels(unified.normalized$condition) <- cond_display_names

# Colors imported and adapted from the Paired set in brewer
cond_cols <- c("#33a02c", 
               "#97de59",
               "#5bb3e3",
               "#1f78b4",
               "#ff7f00",
               "#fcb04c",
               "#e3393c",
               "#ba81d6")

exploratory <-
  ggplot(unified.normalized, aes(x=type,y=score, color=condition, fill=condition))+
  theme_bw()+
  geom_jitter(height = 0, width = 0.25,alpha=0.5)+
  geom_boxplot(alpha=0.5,outlier.color = NA)+
  theme(legend.position="none")+
  scale_color_manual(values = cond_cols)+
  scale_fill_manual(values = cond_cols)+
  ggtitle("Abundace of T-Cells and B-Cells in samples with various conditions")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Cell type")+
  ylab("Abundace of cells relative to healthy median (%)") +
  geom_hline(yintercept=100, color=cond_cols[1], linetype="dashed")+
  facet_wrap(~condition, scales = "free_y")

# Explanatory graph - compare normalized t-cell and b-cell signature of covid to
#                     those of sclc (lung cancer) to determine if there are differences
#                     This is meant to compare a viral lung disease to lung cancer

# Create dataframe with medians
conds.subset <- c("healthy", "sclc", "covid")
unified.subset <- unified[unified$condition %in% conds.subset,]
unified.subset.medians <- unified.subset %>% group_by(type, condition) %>%
                          summarise(median=median(score))
subset_inds <- match(conds.subset, desired_conditions)

# Generate the graph
cond_display_names.subset <- cond_display_names[subset_inds]

unified.subset.medians$condition <- factor(as.character(unified.subset.medians$condition),
                                       levels = conds.subset)
levels(unified.subset.medians$condition) <- cond_display_names.subset

cond_cols.subset = cond_cols[subset_inds]

explanatory <- ggplot(unified.subset.medians, aes(x=type,y=median, color=condition, fill=condition))+
  theme_bw()+
  geom_bar(stat="identity", alpha=0.75)+
  theme(legend.position="none")+
  scale_color_manual(values = cond_cols.subset)+
  scale_fill_manual(values = cond_cols.subset)+
  ggtitle("Abundance of B-Cells and T-Cells compared between
          lung cancer and a viral lung disease")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Cell type")+
  ylab("Median of signature score") +
  facet_wrap(~condition, scales = "fixed")

ggsave("exploratory.png", exploratory, width = 6, height = 5)
ggsave("explanatory.png", explanatory, width = 6, height = 5)
