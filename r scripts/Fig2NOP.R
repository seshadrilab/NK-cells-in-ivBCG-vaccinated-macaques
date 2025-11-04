
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
Fig2N <- read.csv("text/Fig2N.csv")
Fig2OP <- read.csv("text/Fig2OP.csv")

## Total Nk cells

a <- ggboxplot(Fig2N, x = "TimePoint", y = "IFNg",
               ylim = c(0, 40)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% IFN-γ+ NK cells")
a_test <- Fig2N %>% dunn_test(IFNg ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(Fig2N, x = "TimePoint", y = "CD107a",
               ylim = c(0, 25)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD107a+ NK cells")
b_test <- Fig2N %>% dunn_test(CD107a ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

## CD69pos vs neg
a <- ggboxplot(Fig2OP, x = "TimePoint", y = "IFNg",
               fill = "CD69", color = "CD69", palette = c("#006D2C", "#1F4C89"), alpha = 0.5,  ylim = c(0, 40)) +
  geom_dotplot(
    aes(fill = CD69, color = CD69),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% IFN-γ+ NK cells")
a_test <- Fig2OP %>% group_by(TimePoint) %>% wilcox_test(IFNg ~ CD69)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(Fig2OP, x = "TimePoint", y = "CD107a",
               fill = "CD69", color = "CD69", palette = c("#006D2C", "#1F4C89"), alpha = 0.5,  scales = "free") +
  geom_dotplot(
    aes(fill = CD69, color = CD69),
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)) + ylab("% CD107a+ NK cells")
b_test <- Fig2OP %>% group_by(TimePoint) %>% wilcox_test(CD107a ~ CD69)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp


