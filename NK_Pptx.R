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
library("officer")
library("rmarkdown")


setwd("C:/Users/A4030204/Documents/R/RnD_NK_DM")



#Choose excel file with NK Prduction and Tracker-NK Cells are joined by Diff Rum number
NK_Metadata_Path <- choose.files()
#Create data frama
NK_Metadata_Import <- read_excel(NK_Metadata_Path, sheet = 3)
NK_Metadata_df <- as.data.frame(NK_Metadata_Import)

names(NK_Metadata_df)


#Get user input- experiment number 
Experiment.Number <- readline(prompt="Enter Experiment number: ") 


#create clextable for single experiment name
NK_Metadata_Flextable <- 
  NK_Metadata_df %>% 
  filter(`NK_Ms_Teams.Experiment Number` %in% Experiment.Number) %>%
  flextable() %>% 
  fontsize(size = 10, part = "all") %>%
  autofit()

NK_Metadata_Flextable


#NK_Presentation_Path <- read_pptx("UCells Expteriment Template Internal v06.01.2022.pptx") 


#Create and print pptx with inserted flextable 
NK_Presentation <- read_pptx("UCells Expteriment Template Internal v06.01.2022.pptx")%>% 
  #remove_slide(index = 1) %>%
  add_slide(layout = "Slide with mininal content", master = "Astellas Master") %>% 
  ph_with(value = NK_Metadata_Flextable, location = ph_location_label(ph_label = "NK_Metadata")) %>% 
  ph_with(value = 'Example RXXXX: Name', location = ph_location_label(ph_label = "Title 1")) %>%
  ph_with(value = 'DATA', location = ph_location_label(ph_label = "Content Placeholder 2")) %>%
  ph_with(value = '1', location = ph_location_label(ph_label = "Slide Number Placeholder 5")) 

  
print(NK_Presentation, glue::glue("{Experiment.Number} Summary {Sys.Date()}.pptx"))
  
  














  


