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
FigS2C <- read.csv("text/Supp/FigS2C.csv")

### CD107a

FigS1C_CD107a <- FigS2C %>% filter(Subset == "CD107a") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_CD107a, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 6, ylim = c(0, 5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_CD107a %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_CD107a, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_CD107a %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_CD107a, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 5)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_CD107a %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### CD154

FigS1C_CD154 <- FigS2C %>% filter(Subset == "CD154") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_CD154, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ylim = c(0, 35)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_CD154 %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_CD154, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 18)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_CD154 %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_CD154, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 6)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.3,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_CD154 %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### IFNg

FigS1C_IFNg <- FigS2C %>% filter(Subset == "IFNg") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_IFNg, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 6, ylim = c(0, 50)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_IFNg %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_IFNg, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 60)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_IFNg %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_IFNg, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 45)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_IFNg %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### TNF

FigS1C_TNF <- FigS2C %>% filter(Subset == "TNF") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_TNF, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 6, ylim = c(0, 25)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_TNF %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_TNF, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 22)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_CD154 %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_TNF, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 18)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_TNF %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### IL-2

FigS1C_IL2 <- FigS2C %>% filter(Subset == "IL-2") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_IL2, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 6, ylim = c(0, 10)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_IL2 %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_IL2, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 4)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_IL2 %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_IL2, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_IL2 %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp

### IL-17A

FigS1C_IL17A <- FigS2C %>% filter(Subset == "IL-17A") %>% arrange(TimePoint)

a <- ggboxplot(FigS1C_IL17A, x = "TimePoint", y = "CD4",
               facet.by = "Subset", ncol = 6, ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1.2,
    position = position_dodge(0.8)) + ylab("% CD4+ T cells")
a_test <- FigS1C_IL17A %>% group_by(Subset) %>% dunn_test(CD4 ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
ap <- a + stat_pvalue_manual(a_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
ap

b <- ggboxplot(FigS1C_IL17A, x = "TimePoint", y = "CD8aa",
               facet.by = "Subset", ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 1,
    position = position_dodge(0.8)) + ylab("% CD8αα+ T cells")
b_test <- FigS1C_IL17A %>% group_by(Subset) %>% dunn_test(CD8aa ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
bp <- b + stat_pvalue_manual(b_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
bp

c <- ggboxplot(FigS1C_IL17A, x = "TimePoint", y = "CD8ab",
               facet.by = "Subset", ylim = c(0, 2)) +
  geom_dotplot(
    aes(fill = NULL, color = NULL),
    binaxis='y', stackdir='center', dotsize = 2,
    position = position_dodge(0.8)) + ylab("% CD8αβ+ T cells")
c_test <- FigS1C_IL17A %>% group_by(Subset) %>% dunn_test(CD8ab ~ TimePoint)  %>%
  adjust_pvalue() %>%  add_significance("p.adj") %>% add_xy_position(x = "TimePoint")
cp <- c + stat_pvalue_manual(c_test, label = "p", , tip.length = 0.01, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
cp