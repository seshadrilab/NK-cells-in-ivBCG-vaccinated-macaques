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
FigS3 <- read.csv("text/FigS3.csv")

#####
### PBMC Phenotype
FigS3.pbmc.unstim <- FigS3 %>% filter(STIM == "Unstim", Tissue == "PBMC") %>% arrange(TimePoint)

a <- ggboxplot(FigS3.pbmc.unstim, x = "TimePoint", y = "CD16",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.7,
    position = position_dodge(0.8)) + ylab("% CD16+ NK cells")
a_test <- FigS3.pbmc.unstim %>% dunn_test(CD16 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <-  a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

### CD69-GZMB+
b <- ggboxplot(FigS3.pbmc.unstim, x = "TimePoint", y = "CD69_GranB",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% CD69-GZMB+ NK cells")
b_test <- FigS3.pbmc.unstim %>% dunn_test(CD69_GranB ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <-  b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

### CD69+GZMB+
c <- ggboxplot(FigS3.pbmc.unstim, x = "TimePoint", y = "CD69GranB",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.,
    position = position_dodge(0.8)) + ylab("% CD69+GZMB+ NK cells")
c_test <- FigS3.pbmc.unstim %>% dunn_test(CD69GranB ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <-  c +
  stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

# BAL 
FigS3.bal.unstim <- FigS3 %>% filter(STIM == "Unstim", Tissue == "BAL") %>% arrange(TimePoint)

e <- ggboxplot(FigS3.bal.unstim, x = "TimePoint", y = "CD69CD103",
               ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD69+CD103+ NK cells")
e_test <- FigS3.bal.unstim %>% dunn_test(CD69CD103 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ep <-  e +
  stat_pvalue_manual(e_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ep
ylim = c(0, 15)

f <- ggboxplot(FigS3.bal.unstim, x = "TimePoint", y = "CD57",
               ylim = c(0, 12.5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.3,
    position = position_dodge(0.8)) + ylab("% CD57+ NK cells")
f_test <- FigS3.bal.unstim %>% dunn_test(CD57 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
fp <-  f +
  stat_pvalue_manual(f_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
fp

g <- ggboxplot(FigS3.bal.unstim, x = "TimePoint", y = "CD158",
               ylim = c(0, 12.5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.3,
    position = position_dodge(0.8)) + ylab("% CD158+ NK cells")
g_test <- FigS3.bal.unstim %>% dunn_test(CD158 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
gp <-  g +
  stat_pvalue_manual(g_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
gp

FigS3_CD57 <- FigS3 %>% filter(STIM == "Unstim", !(TimePoint == "W2")) %>% arrange(TimePoint)

a <- ggboxplot(FigS3_CD57, x = "TimePoint", y = "CD57",
               fill = "Tissue", color = "Tissue", palette = c("#006D2C", "#1F4C89"), alpha = 0.5, ylim = c(0, 10)) +
  geom_dotplot(
    aes(fill = Tissue, color = Tissue),
    binaxis='y', stackdir='center', dotsize = 0.5,
    position = position_dodge(0.8)) + ylab("% CD57+ NK cells")
a_test <- FigS3_CD57 %>% group_by(TimePoint) %>% wilcox_test(CD57 ~ Tissue)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap


### End

