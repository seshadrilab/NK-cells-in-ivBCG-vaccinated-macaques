# Load the necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggpubr)
library(rstatix)  
library(ggpubr)  # For easy data-visualization

# Set wd
setwd("C:/Users/Steven Makatsa/OneDrive - UW/Shared Documents - SeshadriLab/Manuscripts/InPreparation/2023_IMPAcTB_CyTOF/Figures")

# Read in the CSVs
Fig3BC <- read.csv("text/Fig3BC.csv")

#####
### BAL Subsets
Fig3BC.bal <- Fig3BC %>% filter(STIM == "Unstim", Tissue == "BAL") %>% arrange(TimePoint)

a <- ggboxplot(Fig3BC.bal, x = "TimePoint", y = "NK_cells",
               ylim = c(0, 20)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% NK cells")
a_test <- Fig3BC.bal %>% dunn_test(NK_cells ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### PBMC Subsets
Fig3BC.pbmc <- Fig3BC %>% filter(STIM == "Unstim", Tissue == "PBMC") %>% arrange(TimePoint)

c <- ggboxplot(Fig3BC.pbmc, x = "TimePoint", y = "NK_cells",
               ylim = c(0, 15)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% NK cells")
c_test <- Fig3BC.pbmc %>% dunn_test(NK_cells ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c +
  stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

