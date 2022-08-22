library("readxl")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("tidyverse")
library("ggpubr")
library("grid")
library("cowplot")
library("ggimage")
library("tibble")
library("stringr")
library("openxlsx")
library("png")
library("rstudioapi")
library("forcats")
library("gtools")
library("XLConnect")
library("xlsx")
library("gt")
library("mmtable2")
library("patchwork")
library("utile.visuals")
library("ggforce")
library("flextable")
library("writexl")

WES_DM_Path <- choose.files()

WES_DM_Data_Import_Only <- read_excel(WES_DM_Path, sheet = 1, col_names = TRUE)

names(WES_DM_Data_Import_Only)

WES_Wrangled <- 
  WES_DM_Data_Import_Only %>% 
  filter(`Cell Line` %in% c("NIH1", "NIH2", "NIH3", "AIRMw1c1")) %>% 
  group_by(`Freeze/Campaign/lot`) %>% 
  ggplot(aes(x = `Cell Line`, fill = `SNV Data Comments`)) +
  geom_bar()








