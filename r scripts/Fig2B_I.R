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
Fig2BCEFHI <- read.csv("text/Fig2BCEFHI.csv")
Fig2BCEFHI.unstim <- Fig2BCEFHI %>% filter(STIM == "Unstim") %>% arrange(TimePoint)

#### NK phenotype

a <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "GZMB",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2.2,
    position = position_dodge(0.8)) + ylab("% GZMB+ NK cells")
a_test <- Fig2BCEFHI.unstim %>% dunn_test(GZMB ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "GZMK",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% GZMK+ NK cells")
b_test <- Fig2BCEFHI.unstim %>% dunn_test(GZMK ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp


c <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "PERF",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.6,
    position = position_dodge(0.8)) + ylab("% PERF+ NK cells")
c_test <- Fig2BCEFHI.unstim %>% dunn_test(PERF ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

d <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "CD16",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.4,
    position = position_dodge(0.8)) + ylab("% CD16+ NK cells")
d_test <- Fig2BCEFHI.unstim %>% dunn_test(CD16 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <- d + stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp

e <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "CD69",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 5,
    position = position_dodge(0.8)) + ylab("% CD69+ NK cells")
e_test <- Fig2BCEFHI.unstim %>% dunn_test(CD69 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ep <- e + stat_pvalue_manual(e_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ep

f <- ggboxplot(Fig2BCEFHI.unstim, x = "TimePoint", y = "CXCR3",
                ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2.4,
    position = position_dodge(0.8)) + ylab("% CXCR3+ NK cells")
f_test <- Fig2BCEFHI.unstim %>% dunn_test(CXCR3 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
fp <- f + stat_pvalue_manual(f_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
fp


ggarrange(a,b,c,d,e,f,
          ncol = 3, nrow = 2, common.legend = FALSE)
