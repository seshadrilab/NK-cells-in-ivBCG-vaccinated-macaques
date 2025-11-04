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
nk.phenotype <- read.csv("text/v2/Fig3E_J.csv")

#####
### PBMC Phenotype
phenotype.pbmc.unstim <- nk.phenotype %>% filter(STIM == "Unstim", Tissue == "PBMC") %>% arrange(TimePoint)

a <- ggboxplot(phenotype.pbmc.unstim, x = "TimePoint", y = "CD16",
               scales = "free") +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD16+ NK cells")
a_test <- phenotype.pbmc.unstim %>% t_test(CD16 ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <-  a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### CD69-GZMB+
b <- ggboxplot(phenotype.pbmc.unstim, x = "TimePoint", y = "CD69_GranB",
               scales = "free") +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD69-GZMB+ NK cells")
b_test <- phenotype.pbmc.unstim %>% t_test(CD69_GranB ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <-  b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

### CD69+GZMB+
c <- ggboxplot(phenotype.pbmc.unstim, x = "TimePoint", y = "CD69GranB",
               scales = "free") +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD69+GZMB+ NK cells")
c_test <- phenotype.pbmc.unstim %>% t_test(CD69GranB ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <-  c +
  stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### CD69+GZMB-
d <- ggboxplot(phenotype.pbmc.unstim, x = "TimePoint", y = "CD69GranB_",
               scales = "free") +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD69+GZMB- NK cells")
d_test <- phenotype.pbmc.unstim %>% t_test(CD69GranB_ ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <-  d +
  stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp


### BAL Phenotype GranzymeB co-expression with CD69
# Read in the CSVs
GranBCD69 <- read.csv("text/v2/Fig3HI.csv")

GranBCD69.bal <- GranBCD69 %>% filter(STIM == "Unstim", Tissue == "BAL") %>% arrange(Subset)
GranBCD69.pbmc <- GranBCD69 %>% filter(STIM == "Unstim", Tissue == "PBMC") %>% arrange(Subset)

a <- ggboxplot(GranBCD69.pbmc, x = "Subset", y = "NK",
               facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)) + ylab("% Freq. of NK cells")
a_test <- GranBCD69.pbmc %>% group_by(TimePoint) %>% t_test(NK ~ Subset)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Subset")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

a <- ggboxplot(GranBCD69.pbmc, x = "Subset", y = "NK",
              facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)) + ylab("% Freq. of NK cells")
a_test <- GranBCD69.pbmc %>% group_by(TimePoint) %>% t_test(NK ~ Subset)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Subset")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### End
