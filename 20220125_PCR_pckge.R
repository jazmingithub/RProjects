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



PCR_Summary_Path <- choose.files()

#Importing Confluency Data from PCR Summary
Confluency_Data_Import_Only <- read_excel(PCR_Summary_Path, sheet = 3, range = "A5:K5765", col_names = TRUE)

Confluency_Data_df <- 
  as.data.frame(Confluency_Data_Import_Only) %>% 
  na.omit(`Microplate ID`) %>% 
  mutate("Sample ID" = paste0(`Plate No. (for reference)`, Well))  %>%
    mutate(CFU_Pos = case_when(
    `CFU count` > 0 ~ "*",
    FALSE ~ " ")) %>% 
  mutate(CFU_Single = case_when(
    `CFU count` == 1 & Notes %in% c("*", "**", "***") ~ "*",
    FALSE ~ " ")) 

names(Confluency_Data_df)
view(Confluency_Data_df)




#Importing Anlayzer Data from PCR Summary
Anlyzer_Data_Import_Only <- read_excel(PCR_Summary_Path, sheet = 3, skip = 5, range = cell_cols("N:T"), col_names = TRUE)

Anlyzer_Data_df <- 
  as.data.frame(Anlyzer_Data_Import_Only) %>% 
  mutate(Copies = replace(Copies, Copies %in% c("NOTDETECTED", "NOT DETECTED"), 0)) %>% 
  na.omit(`Experiment Name`)

view(Anlyzer_Data_df)
names(Anlyzer_Data_df)


#Merge Confluency Data with Analyzer Data 
PCR_Summary_Date_Merged_df <- 
  left_join(Anlyzer_Data_df, Confluency_Data_df, 
                                     by = c("Sample ID")) 

names(PCR_Summary_Date_Merged_df)    
view(PCR_Summary_Date_Merged_df)




#Summary Table 



Summary_Results <- 
  PCR_Summary_Date_Merged_df %>% 
  drop_na(`Microplate ID`) %>% 
  count(CFU_Pos, CFU_Single) %>% 
  flextable() %>%
  add_header_row(
    colwidths = c(2,2), 
    values = c("PCR Results", "Campaign, Sample, Genotype")
  ) %>% 
  theme_vanilla() %>% 
  add_footer_lines("Campaign info") %>% 
  set_caption("Capaign, Lot, Editing Step, Edit PCR Summary") %>% 
  
  
  
  
  
  
  
  
  
  
  
  count(CFU_Pos, CFU_Single)
  



  
  #Total # 1 CFU wells
  
  #Total # 1 CFU containing wells screened
  
  #Percent of 1 CFU wells edited (#PCR pos. clones from single CFU wells/ total # single CFU wells)
  
  #Total # single cell 1 CFU wells

  #Total # single cell 1 CFU wells containing wells screened
  
  #Total # single cell 1 CFU wells positive for edit by PCR

  #Percent of single cell 1 CFU wells edited (#PCR pos. clones from single CFU wells/ total # single CFU wells)

  #Recommended wells = "See PCR Summary slide" 

  
  




## THIS NEXT SECTION IS TO GRAPH THE WRANGGLED DATA
#Create CFU layers for sub table 
CFU_Count_df <- 
  PCR_Summary_Date_Merged_df %>%
  select(c(`Lysis Plate ID`, `Sample ID`, CFU_Pos, CFU_Single)) %>% 
  pivot_longer(c( CFU_Pos, CFU_Single), names_to = "CFU", values_to = "*") %>% 
  group_by(`Lysis Plate ID`) %>% 
  filter(`Lysis Plate ID` %in% "4B")

names(CFU_Count_df)
view(CFU_Count_df)


# Sub table under graph 
CFU_Pos_tbl <- 
  CFU_Count_df  %>% 
  ggplot(aes(color = CFU)) + 
  geom_text(data=CFU_Count_df, mapping=aes(x=as_factor(`Sample ID`), y = factor(CFU, c("CFU_Pos", "CFU_Single")), label = CFU_Count_df$`*`)) +
  scale_y_discrete(limits = rev, drop = TRUE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle=90, size = 6, hjust=0, vjust=0.2),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1,"pt"), 
        strip.text = element_blank(),
  ) + 
  scale_x_discrete(expand = c(0,0), breaks = as_factor(CFU_Count_df$`Sample ID`)) +
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio =1)


#Use for trouble shooting
Test_Graph <-
  PCR_Summary_Date_Merged_df %>%
  filter(`Lysis Plate ID` %in% "4B") %>%
  #filter(`Sample ID` %in% c("1", "10", "100", "1000", "10000", "100000", "H2O", "WT")) %>% 
  ggplot(aes(x = as_factor(`Sample ID`), y = as.double(Copies), fill = `Target Name`)) +
  geom_col(position = "dodge2", width = 0.4, alpha = 1, show.legend = TRUE) + 
  # add if statement for CFU pos wells #geom_tile(fill = "white", alpha = .4, color = "black") +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000), expand = c(0,0),
                labels = c("1", "10", "100", "1000", "10000", "100000")) +
  coord_cartesian(ylim = c(1, 100000)) +
  
  #ggtitle(paste("Plate", levels(as_factor(PCR_plate_data$`Lysis Plate ID`))[i]), 
          #subtitle = "Campaing Info") +
  #facet_grid(.~ PCR_plate_data$`Sample Type`, scales = "free_x", space="free_x") +
  theme_bw() +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle=90, size = 6, hjust=0, vjust=0.2), 
         legend.position = "none",
         #legend.position = "bottom", legend.box = "horizontal", legend.title=element_blank(),
         #plot.margin = margin(10, 5, 10, 5),  
         plot.caption = element_text(hjust=1),
         plot.caption.position = "plot", 
         panel.spacing=unit(1,"pt"),
         strip.text = element_blank(),
         axis.ticks.x = element_blank()
  ) + 
  scale_x_discrete(expand = c(0,0)) +
  annotate("segment", x = "10000", xend = "H2O", y = 40000, yend = 40000, size = 1) + 
  annotate("text", x = 3, y = 60000, label = "Controls", size = 4) +
  labs(x = NULL, y = NULL) 


#This works for aligning table below graph 
plot_cmba <- append_table(
  plot = Test_Graph,
  table = CFU_Pos_tbl, 
  extract.legend = FALSE,
)


grid.draw(plot_cmba)



#Use for loop 
listgraphs <- list()
for(i in 1:nlevels(as_factor(PCR_plate_data$`Lysis Plate ID`))) {
  listgraphs[[i]] <- 
  PCR_plate_data %>%

  group_by(PCR_plate_data$`Lysis Plate ID`) %>% 
    
  
  ggplot(aes(x = as_factor(PCR_plate_data$`Sample ID`), y = PCR_plate_data$`Nmbr Copies`, group = PCR_plate_data$`Short Target Name`)) +
  
    #barplot(stat =  "identity") + 

  
  geom_col(position = position_dodge(0.9), alpha = 1, show.legend = TRUE) + 
  ggtitle(paste("Plate", levels(as_factor(PCR_plate_data$`Lysis Plate ID`))[i]), 
          subtitle = "Campaing Info") +
  #facet_grid(.~ PCR_plate_data$`Sample Type`, scales = "free_x", space="free_x") +
    theme_bw() +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(angle=90, size = 6), #add vjust. hjust # remove ticks 
           legend.position = "bottom", legend.box = "horizontal", legend.title=element_blank(),
           plot.margin = margin(10, 5, 10, 5),  
           plot.caption = element_text(hjust=1),
           plot.caption.position = "plot", 
           panel.spacing=unit(1,"pt"),
           strip.text = element_blank()
           ) + 
  scale_x_discrete(expand = c(0,0))) +
  scale_y_continuous(limits = c(1, 100000), expand = c(0,0), trans = 'log10') +
  labs(x = NULL, y = NULL, caption= paste("Plate", levels(as_factor(PCR_plate_data$`Lysis Plate ID`))[i])) 
  
  print(listgraphs[[i]])
  
  #ggsave(listgraphs[[i]], file = paste0("PCR_Graph_Plate_", levels(as_factor(PCR_plate_data_1$Plate))[i], ".png"),
  #width = 26, height = 5, units = "cm")
}


############## 


---
  for(i in 1:nlevels(as_factor(PCR_plate_data$`Lysis Plate ID`))) {
    listgraphs[[i]] <- 
      PCR_plate_data %>%
      mutate(Sample_Type = PCR_plate_data$`Sample ID` %in% c("10000", "1000", "100","10", "WT","H2O"),
             Sample_Type = if_else(Sample_Type == TRUE, "Control", "PD_Sample")) %>%  
      group_by(PCR_plate_data$`Lysis Plate ID`) %>% 
      ggplot(aes(x = PCR_plate_data$`Sample ID`, y = PCR_plate_data$Copies, group = PCR_plate_data$`Short Target Name`)) +
      geom_col(position = position_dodge(0.9), vjust=-0.25, alpha = 1, show.legend = TRUE) + 
      ggtitle(paste("Plate", levels(as_factor(PCR_plate_data$`Lysis Plate ID`))[i]), 
              subtitle = "Campaing Info") +
      facet_grid(.~ Sample_Type, scales = "free_x", space="free_x") +
      Graph_Details+ 
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(limits = c(1, 100000), expand = c(0,0), trans = 'log10') +
      labs(x = NULL, y = NULL, caption= paste("Plate", levels(as_factor(PCR_plate_data$`Lysis Plate ID`))[i])) 
    
    
    print(listgraphs[[i]])
    
    #ggsave(listgraphs[[i]], file = paste0("PCR_Graph_Plate_", levels(as_factor(PCR_plate_data_1$Plate))[i], ".png"),
    #width = 26, height = 5, units = "cm")
  }
---


####ATEMPTS TO REORDER X-AXIS####### 
#mutate(Clone1 = if_else(Sample_Type %in% "PD_Sample", mixedsort(Clone1), Clone1)) #works in table but not ggplot 
#Well[order(levels= Clone)]
#x= fct_inorder(Clone)
#x= factor(Sample_Type, levels = c( "Control", "PD_Sample")
#x = Clone1[order(nchar(Clone1), Clone1)]

######################################### OLD### DONT USE ###########

PCR_plate_data_1 <- PCR_plate_data %>%
  mutate(Sample_Type = Clone %in% c("H2O", "WT", "10000", "1000", "100","10"),
                           Sample_Type = if_else(Sample_Type == TRUE, "Control", "PD_Sample"))

view(PCR_plate_data_1)
names(PCR_plate_data)
str(PCR_plate_data)
#PCR_df <- data.frame(PCR_plate_data$Run,  
                     #PCR_plate_data$Plate,
                     #PCR_plate_data$Target,
                     #PCR_plate_data$Clone,
                     #PCR_plate_data$Copies)
str(PCR_df)
view(PCR_df)


#Control Graphs 
PCR_Controls <- as_grob(PCR_plate_data %>% 
  na.omit(Clone) %>%
  group_by(Run) %>%
  #To make filter for controls 
  filter(Clone %in% c("H2O", "WT", "10000", "1000", "100","10")) %>% 
  arrange(Clone) %>%  
  #mutate(Target = recode(Target,)) #needs work 
  ggplot(aes(x= Clone, y = Copies, fill = Target))+
  geom_col(position = "dodge", alpha = 1, show.legend = TRUE) +
  ggtitle("Control_Title_input") +
  #facet_wrap(~Plate)+
  theme_bw() +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()
         )+
  theme(
    axis.text.x = element_text(angle=90, size = 12), 
    axis.ticks.x = element_blank(),
    legend.justification = "left"
    ) + 
  xlab(NULL) +
  #theme(plot.margin = margin(10, 5, 10, 5)) + #look into this more
  theme_half_open() +
  scale_x_discrete(expand = expansion(add = .6)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
)
  #ylab() + 
  #scale_x_discrete(expand = expansion(add = .6))
  #scale_y_continuous(expand = expansion(mult = c(0, .1)))

  
as_factor(PCR_plate_data$Plate)


PCR_Plate_1B <- as_grob(PCR_plate_data %>%
  na.omit(Clone) %>%
  group_by(Run) %>%
  #To make filter for controls 
  #filter(Plate) %in% Run %>% #filter will be usefull whith more than one run 
  filter(Run == "1B_3B_4B_5B", Plate %in% c("1B")) %>%    #add i for loop here 
  arrange(Clone) %>%  
  #mutate(Target = recode(Target,)) #needs work 
  ggplot(aes(x= Clone, y = Copies, group = Target, fill = Target)) +
  geom_col(position = "dodge", alpha = 1, show.legend = FALSE) + 
  ggtitle("Plate_ID_input") +
  #facet_wrap(as_factor(PCR_plate_data$Plate), ncol =1, scales = "free_x")+
  scale_x_discrete(expand = expansion(add = .6)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  ylab(NULL) +
  theme_bw() +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle=90, size = 12, vjust=0)
         ) 
)

#USE THIS to arragne control and plate plot together 
ggarrange(as_grob(PCR_Controls), as_grob(PCR_Plate_1B), nrow = 1, ncol = 2)

#works but needs work 
ggdraw() + 
  draw_plot(PCR_Controls, x = 0, y = .5, width = .5, height = .5)+  
  draw_plot(PCR_Plate_1B, x = .5, y = .5, width = .5, height = .5)


view(PCR_facet)
#USE THIS ONE to print controls and plate samples in one graph 
  #should make a plot list? 

#ALSO works ? 
plot_grid(PCR_Controls, PCR_Plate_1B,
          align = "h",
          axis = "l",
          greedy = TRUE)

#graph onto new page 
multi.page <- ggarrange(PCR_Controls, PCR_Plate_1B, nrow=1, ncol =1)



view(PCR_plate_data_1)

PCR_graph_new <-  PCR_plate_data_1 %>%
              na.omit(Clone) %>%
              mutate(Sample_Type = Clone %in% c("H2O", "WT", "10000", "1000", "100","10"),
                  Sample_Type = if_else(Sample_Type == TRUE, "Control", "PD_Sample")) %>%
              group_by(Run) %>%
              filter(Sample_Type %in% c("Control") | Plate %in% c("1B")) %>%    #add i for loop here 
              arrange(Sample_Type) %>% 
                ggplot(aes(x= Clone, y = Copies, group = Target, fill = Target)) +
                          geom_col(position = "dodge", alpha = 1, show.legend = TRUE) + 
                          ggtitle("Title_input") +
                          #facet_wrap(as_factor(PCR_plate_data_1$Sample_Type))
                          #facet_wrap(as_factor(Sample_Type), ncol =1, scales = "free_x") +
                          #facet_grid(PCR_plate_data_1$Plate ~ PCR_plate_data_1$Sample_Type)+
                          theme_bw() +
                          theme( panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 axis.text.x = element_text(angle=90, size = 12, vjust=0)
                          )  +
                          scale_x_discrete(expand = expansion(add = .6)) +
                          scale_y_continuous(expand = expansion(mult = c(0, .1))) +
                          ylab(NULL)  
  




listgraphs1 <- list() 



print(listgraphs1)


#DONT USE THIS 
for(i in 1:nlevels(as_factor(PCR_plate_data_1$Plate))) {
  listgraphs1[[i]] <-
    #Sample_Type %in% c("Control")
    #filter(Run == "1B_3B_4B_5B", Sample_Type %in% c("Control") |Plate %in% c("1B")) %>%    #add i for loop here 
    #filter(Sample_Type %in% c("Control") | Plate %in% c(i)) %>%
    #subset(Plate == levels(Plate)[i] | Sample_Type %in% c("Control"))
    #arrange(Sample_Type) %>% 
    PCR_plate_data_1 %>%
    na.omit(Clone) %>%
    group_by(Run) %>%
    filter(Sample_Type %in% c("Control") | Plate %in% c("1B")) %>% 
      ggplot(aes(x= Clone, y = Copies, group = Target, fill = Target)) +
    geom_col(position = "dodge", alpha = 1, show.legend = FALSE) + 
    ggtitle(levels(PCR_plate_data_1$Plate)[i]) +
    #facet_wrap(as_factor(PCR_plate_data_1$Sample_Type))
    #facet_wrap(as_factor(Sample_Type), ncol =1, scales = "free_x") +
    #facet_grid(PCR_plate_data_1$Plate ~ PCR_plate_data_1$Sample_Type)+
    theme_bw() +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(angle=90, size = 12, vjust=0)
    )  +
    scale_x_discrete(expand = expansion(add = .6)) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    ylab(NULL) 
}








  
  
#xlab("")
#get #color for edits 



#graph function 
bar_fun = function(x,y){
  ggplot(PCR_plate_data, aes_string(x= x, y = y, fill = Target), group = Target) +
    geom_bar(stat = "identity", position = position_dodge(), width = 2, alpha = 1) + 
    theme_bw() +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           legend.justification = "bottom",
           axis.text.x = element_text(angle=90, size = 12, vjust=0), 
           plot.margin = margin(10, 5, 10, 5) #look into this more
    ) +
    ggtitle("Control_Title_input") +
    scale_x_discrete(expand = expansion(add = .6)) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    ylab(NULL) +
    xlab("")
}
  
  
PCR_plots = map(PCR_plate_data, ~bar_fun(nlevels(PCR_plate_data$Plate, PCR_plate_data$Copies)))
  
  
  




#Align plots - out put table not plot  
plots <- align_plots(
  PCR_Controls_grobb,
  PCR_Plate_1B_grobb,
  align = "h", 
  axis = "b",
  greedy = TRUE)
print(plots)







#ggsave 
#ggsave(file = paste(PCR_plate_data, "png",  width = 10, height = 1.36))

listgraphs2 <- list()
#loop to make several grtaphs- needs work 
for(i in 1:nlevels(as_factor(PCR_plate_data_1$Plate))){
  data1 = PCR_plate_data[PCR_plate_data_1$Plate == levels(as_factor(PCR_plate_data_1$Plate))[i] ,]
  listgraphs2[[i]] <- ggplot(data1, aes(Clone, Copies),group = Target, fill= Target) + 
    geom_bar(stat = "identity", position=position_dodge())+  
    scale_x_discrete()
  print(listgraphs2[[i]])
}

# to reorder xaxis- needs work 
#scale_x_discrete(limits = c("H2O", "WT", "10000", "1000", "100","10")


#subview graph 
#PCR_Plate_1B + geom_subview(x=.7, y=.78, subview= PCR_Controls)
