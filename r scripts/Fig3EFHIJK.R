
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
Fig3EFHI <- read.csv("text/Fig3EFHI.csv")
Fig3JK <- read.csv("text/Fig3JK.csv")

#####
### BAL Phenotype
Fig3EFHI.bal.unstim <- Fig3EFHI %>% filter(STIM == "Unstim", Tissue == "BAL") %>% arrange(TimePoint)

a <- ggboxplot(Fig3EFHI.bal.unstim, x = "TimePoint", y = "CD16",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.3,
    position = position_dodge(0.8)) + ylab("% CD16+ NK cells")
a_test <- Fig3EFHI.bal.unstim %>% t_test(CD16 ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <-  a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(Fig3EFHI.bal.unstim, x = "TimePoint", y = "CD69",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD69+ NK cells")
b_test <- Fig3EFHI.bal.unstim %>% t_test(CD69 ~ TimePoint, ref.group = "No. Vax")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <-  b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

### CD69-GZMB+
h <- ggboxplot(Fig3EFHI.bal.unstim, x = "TimePoint", y = "CD69_GranB",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.3,
    position = position_dodge(0.8)) + ylab("% CD69-GZMB+ NK cells")
h_test <- Fig3EFHI.bal.unstim %>% dunn_test(CD69_GranB ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
hp <-  h +
  stat_pvalue_manual(h_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
hp

### CD69+GZMB+
c <- ggboxplot(Fig3EFHI.bal.unstim, x = "TimePoint", y = "CD69GranB",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD69+GZMB+ NK cells")
c_test <- Fig3EFHI.bal.unstim %>% dunn_test(CD69GranB ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <-  c +
  stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### CD69+GZMB-
d <- ggboxplot(Fig3EFHI.bal.unstim, x = "TimePoint", y = "CD69GranB_",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% CD69+GZMB- NK cells")
d_test <- Fig3EFHI.bal.unstim %>% dunn_test(CD69GranB_ ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <-  d +
  stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp

### BAL Phenotype GranzymeB co-expression with CD69
# Read in the CSVs
Fig3JK <- read.csv("text/Fig3JK.csv")

Fig3J.pbmc <- Fig3JK %>% filter(STIM == "Unstim", Tissue == "PBMC") %>% arrange(Subset)

a <- ggboxplot(Fig3J.pbmc, x = "Subset", y = "NK",
               facet.by = "TimePoint", ncol = 4, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("% NK cells")
a_test <- Fig3J.pbmc %>% group_by(TimePoint) %>% wilcox_test(NK ~ Subset)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Subset")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

Fig3K.bal <- Fig3JK %>% filter(STIM == "Unstim", Tissue == "BAL") %>% arrange(Subset)

b <- ggboxplot(Fig3K.bal, x = "Subset", y = "NK",
               facet.by = "TimePoint", ncol = 4, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.5,
    position = position_dodge(0.8)) + ylab("NK cells")
b_test <- Fig3K.bal %>% group_by(TimePoint) %>% wilcox_test(NK ~ Subset)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Subset")
bp <- b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

a <- ggboxplot(GranBCD69.pbmc, x = "Subset", y = "NK",
               facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)) + ylab("% Freq. of NK cells")
a_test <- GranBCD69.pbmc %>% group_by(TimePoint) %>% wilcox_test(NK ~ Subset)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Subset")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap


# End
