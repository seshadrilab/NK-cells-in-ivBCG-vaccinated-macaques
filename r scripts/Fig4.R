
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
Fig4 <- read.csv("text/Fig4.csv")
Fig4$TimePoint <- factor(Fig4$TimePoint,levels = c("preBCG", "W2", "W4", "W8", "W12/13"))

### Fig ABC
#### Total NK cells

a <- ggboxplot(Fig4, x = "TimePoint", y = "NK", id = "NHP", size = 0.2,
               ylim = c(0, 10)) + geom_jitter(width=0.24, height=0.6) +
  ylab("% NK cells")
a_test <- Fig4 %>% t_test(NK ~ TimePoint, paired = TRUE, ref.group = "preBCG")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a +
  stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

#### CD16+ NK cells

b <- ggboxplot(Fig4, x = "TimePoint", y = "CD16", id = "NHP", size = 0.2,
               ylim = c(0, 100)) + geom_jitter(width=0.24, height=0.6) +
  ylab("% CD16+ NK cells")
b_test <- Fig4 %>% t_test(CD16 ~ TimePoint, paired = TRUE, ref.group = "preBCG")  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <-  b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

#### CD69- NK cells

c <- ggboxplot(Fig4, x = "TimePoint", y = "CD69neg", id = "NHP", size = 0.2,
               ylim = c(0, 100)) + geom_jitter(width=0.24, height=0.6) +
  ylab("% CD69- NK cells")
c_test <- Fig4 %>% dunn_test(CD69neg ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <-  c +
  stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

#### CD8+ NK cells

d <- ggboxplot(Fig4, x = "TimePoint", y = "CD8", id = "NHP", size = 0.2,
               ylim = c(0, 100)) + geom_jitter(width=0.24, height=0.6) +
  ylab("% CD8α+ NK cells")
d_test <- Fig4 %>% dunn_test(CD8 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
dp <-  d +
  stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp
### Protection
#### Total NK cells
d <- ggboxplot(Fig4, x = "Protection", y = "NK",
               fill = "Protection", color = "Protection", palette = c("#D2691E", "#800000"), alpha = 0.2, facet.by = "TimePoint", ncol = 5, ylim = c(0, 7)) +
  geom_dotplot(aes(fill = Protection, color = Protection),
               binwidth = 1, binaxis='y', stackdir='center', dotsize = 0.1,
               position = position_jitter(0.2)) + ylab("% NK cells")
d_test <- Fig4 %>% group_by(TimePoint) %>% wilcox_test(NK ~ Protection)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Protection")
dp <- d +
  stat_pvalue_manual(d_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dp

#### CD16+ NK cells
e <- ggboxplot(Fig4, x = "Protection", y = "CD16",
               fill = "Protection", color = "Protection", palette = c("#D2691E", "#800000"), alpha = 0.2, facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(aes(fill = Protection, color = Protection),
               binwidth = 1, binaxis='y', stackdir='center', dotsize = 2.5,
              position = position_jitter(0.2)) + ylab("% CD16+ NK cells")
e_test <- Fig4 %>% group_by(TimePoint) %>% wilcox_test(CD16 ~ Protection)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Protection")
ep <- e +
  stat_pvalue_manual(e_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ep

#### CD69- NK cells
f <- ggboxplot(Fig4, x = "Protection", y = "CD69neg",
               fill = "Protection", color = "Protection", palette = c("#D2691E", "#800000"), alpha = 0.2, facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(aes(fill = Protection, color = Protection),
               binwidth = 1, binaxis='y', stackdir='center', dotsize = 2.5,
               position = position_jitter(0.2)) + ylab("% CD69- NK cells")
f_test <- Fig4 %>% group_by(TimePoint) %>% wilcox_test(CD69neg ~ Protection)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Protection")
fp <- f +
  stat_pvalue_manual(f_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
fp

#### CD8a NK cells
f <- ggboxplot(Fig4, x = "Protection", y = "CD8",
               fill = "Protection", color = "Protection", palette = c("#D2691E", "#800000"), alpha = 0.2, facet.by = "TimePoint", ncol = 5, ylim = c(0, 100)) +
  geom_dotplot(aes(fill = Protection, color = Protection),
               binwidth = 1, binaxis='y', stackdir='center', dotsize = 2.5,
               position = position_jitter(0.2)) + ylab("% CD8a+ NK cells")
f_test <- Fig4 %>% group_by(TimePoint) %>% wilcox_test(CD8 ~ Protection)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Protection")
fp <- f +
  stat_pvalue_manual(f_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
fp
### End
b <- ggboxplot(Fig4, x = "Protection", y = "NK_count",
               fill = "Protection", color = "Protection", palette = c("#D2691E", "#800000"), alpha = 0.2, facet.by = "TimePoint", ncol = 5, ylim = c(0, 85000)) +
  geom_dotplot(
    aes(fill = Protection, color = Protection),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("NK Count")
b_test <- Fig4 %>% group_by(TimePoint) %>% wilcox_test(NK_count ~ Protection)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "Protection")
bp <- b +
  stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

bp
