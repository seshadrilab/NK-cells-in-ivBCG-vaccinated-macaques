
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
Fig2JKL <- read.csv("text/Fig2JKL.csv")
Fig2JKL.unstim <- Fig2JKL %>% filter(STIM == "Unstim") %>% arrange(TimePoint)

#### UNSTIM ONLY
a <- ggboxplot(Fig2JKL.unstim, x = "TimePoint", y = "GZMB",
               fill = "CD69", color = "CD69", palette = c("#006D2C", "#1F4C89"), alpha = 0.5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = CD69, color = CD69),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% GZMB+ NK cells")
a_test <- Fig2JKL.unstim %>% group_by(TimePoint) %>% wilcox_test(GZMB ~ CD69)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(Fig2JKL.unstim, x = "TimePoint", y = "GZMK",
               fill = "CD69", color = "CD69", palette = c("#006D2C", "#1F4C89"), alpha = 0.5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = CD69, color = CD69),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% GZMK+ NK cells")
b_test <- Fig2JKL.unstim %>% group_by(TimePoint) %>% wilcox_test(GZMK ~ CD69)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(Fig2JKL.unstim, x = "TimePoint", y = "PERF",
               fill = "CD69", color = "CD69", palette = c("#006D2C", "#1F4C89"), alpha = 0.5, ylim = c(0, 100)) +
  geom_dotplot(
    aes(fill = CD69, color = CD69),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% PERF+ NK cells")
c_test <- Fig2JKL.unstim %>% group_by(TimePoint) %>% wilcox_test(PERF ~ CD69)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp
