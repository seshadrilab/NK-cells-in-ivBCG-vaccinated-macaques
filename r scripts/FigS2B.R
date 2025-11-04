
# Load the necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggpubr)
library(rstatix)  
library(ggpubr)  # For easy data-visualization

# Read in the CSVs
FigS2B <- read.csv("text/Supp/FigS2B.csv")

#####
### CD4 T cells

a <- ggboxplot(FigS2B, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 3, ylim = c(0, 40)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS2B %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### CD8ab T cells

b <- ggboxplot(FigS2B, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ncol = 3, ylim = c(0, 40)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS2B %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

### CD8αβ+ T cells

c <- ggboxplot(FigS2B, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ncol = 3, ylim = c(0, 40)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS2B %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

