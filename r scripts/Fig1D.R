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
Fig1D <- read.csv("text/Fig1D.csv")

#####
### CD4
Fig1D_CD4 <- Fig1D %>% filter(Subset == "CD4 T cells") %>% arrange(TimePoint)

a <- ggboxplot(Fig1D_CD4, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 80)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
a_test <- Fig1D_CD4 %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### CD8aa
Fig1D_CD8aa <- Fig1D %>% filter(Subset == "CD8aa T cells") %>% arrange(TimePoint)

b <- ggboxplot(Fig1D_CD8aa, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 50)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
b_test <- Fig1D_CD8aa %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

### CD8ab
Fig1D_CD8ab <- Fig1D %>% filter(Subset == "CD8ab T cells") %>% arrange(TimePoint)

c <- ggboxplot(Fig1D_CD8ab, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 50)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
c_test <- Fig1D_CD8ab %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### MAIT
Fig1D_MAITs <- Fig1D %>% filter(Subset == "MAITs") %>% arrange(TimePoint)

d <- ggboxplot(Fig1D_MAITs, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 8)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
d_test <- Fig1D_MAITs %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <- d + stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp

### GD
Fig1D_GD <- Fig1D %>% filter(Subset == "GD T cells") %>% arrange(TimePoint)

e <- ggboxplot(Fig1D_GD, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 1.5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
e_test <- Fig1D_GD %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ep <- e + stat_pvalue_manual(e_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ep

### NK
Fig1D_NK <- Fig1D %>% filter(Subset == "NK cells") %>% arrange(TimePoint)

f <- ggboxplot(Fig1D_NK, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
f_test <- Fig1D_NK %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
fp <- f + stat_pvalue_manual(f_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
fp