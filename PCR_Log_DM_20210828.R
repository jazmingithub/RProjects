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
library("esquisse")

PCR_Data_Mine <- "C:/Users/A4030204/Box/Jazmin Garcia/Core Tech DB clean up Project/PCR screen log.xlsx"


view(PCR_Data_Mine)

Sheet_Names <- excel_sheets(PCR_Data_Mine) 




PCR_Log_sheet <- 
  PCR_Data_Mine %>% 
excel_sheets() %>% 
  set_names() %>% 
  map( ~read_excel(path = data1, sheet = 2))


