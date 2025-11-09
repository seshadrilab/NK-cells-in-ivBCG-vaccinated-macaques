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
FigS1B <- read.csv("text/Supp/FigS1B.csv")

#####
### Leukocytes
### B cells
FigS1B_B <- FigS1B %>% filter(Subset == "B cells") %>% arrange(TimePoint)

a <- ggboxplot(FigS1B_B, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
a_test <- FigS1B_B %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### Macrophages
FigS1B_Macs <- FigS1B %>% filter(Subset == "Macrophages") %>% arrange(TimePoint)

b <- ggboxplot(FigS1B_Macs, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
b_test <- FigS1B_Macs %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp


### Monocytes
FigS1B_Mono <- FigS1B %>% filter(Subset == "Monocytes") %>% arrange(TimePoint)

c <- ggboxplot(FigS1B_Mono, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 25)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
c_test <- FigS1B_Mono %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### Myeloid DCs
FigS1B_mDC <- FigS1B %>% filter(Subset == "Myeloid DCs") %>% arrange(TimePoint)

d <- ggboxplot(FigS1B_mDC, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.8,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
d_test <- FigS1B_mDC %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <- d + stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp

### Plasmacytoid DCs
FigS1B_pDC <- FigS1B %>% filter(Subset == "Plasmacytoid DCs") %>% arrange(TimePoint)

e <- ggboxplot(FigS1B_pDC, x = "TimePoint", y = "Leukocytes",
               facet.by = "Subset", ncol = 2, ylim = c(0, 1.5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD45+ cells")
e_test <- FigS1B_pDC %>% group_by(Subset) %>% dunn_test(Leukocytes ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ep <- e + stat_pvalue_manual(e_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ep


