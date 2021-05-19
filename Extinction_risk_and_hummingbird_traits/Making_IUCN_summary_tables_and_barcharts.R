## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(RColorBrewer)


## ----get_iucn_data_order_factors------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#These are levels that I want in summary tables/plots
redlist_levels <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Data Deficient")
pop_levels <- c("Increasing", "Stable", "Decreasing", "Unknown")
clade_levels <- c("Hermits", "Topazes", "Mangoes", "Brilliants", "Coquettes", "Patagona", "Mountain Gems", "Bees", "Emeralds") #In order of evolutionary age according to McGuire et al. 2014

#Get data, order factor levels
iucn_data <- read.csv( "C:\\Users\\leimberk\\Box\\Biol_Reviews_Analyses\\IUCN_status_and_traits\\1_Compiling_IUCN_and_trait_data\\export\\Hummingbird_IUCN_and_trait_data_with_derived_vars_20210518.csv") %>%
  select(Clade, Species_name, Redlist_category, Population_trend, Forest_depend_status) %>% #Species_name = species name according to Jetz phylogeny, not IUCN species name
  
  #Scoring forest dependency as in Betts et al. 2017
  mutate(Forest_depend_Betts = case_when(Forest_depend_status == "High" ~ "Forest-exclusive", #This is equivalent to 'Forest specialist' in phylogenetic regression
                                         Forest_depend_status == "Medium" ~ "Forest-optional",
                                         Forest_depend_status == "Low" ~ "Forest-optional",
                                         Forest_depend_status == "Non-forest" ~ "Non-forest",
                                         TRUE ~ as.character(Forest_depend_status))) %>%
  #Order factors
  mutate(Redlist_category = factor(Redlist_category, ordered = TRUE, levels = redlist_levels),
         Population_trend = factor(Population_trend, ordered = TRUE, levels = pop_levels),
         Clade = factor(Clade, ordered = TRUE, levels = clade_levels),
         Forest_depend_Betts = ifelse(Forest_depend_Betts == "NA" | is.na(Forest_depend_Betts), "Unknown", Forest_depend_Betts))


## ----summarize_extinction_risk_by_clade-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Redlist category/status ('Threatened' in phylogenetic regression)
clade_status_sum <- iucn_data %>%
  group_by(Clade, Redlist_category) %>%
  summarise(Num_spp = n()) %>%
  ungroup() %>%
  spread(key = Redlist_category, value = Num_spp, fill = 0)

#Population trends ('Decreasing' in phylogenetic regression)
clade_pop_sum<-iucn_data %>%
  group_by(Clade, Population_trend) %>%
  summarise(Num_spp = n()) %>%
  ungroup() %>%
  spread(key = Population_trend, value = Num_spp, fill = 0)


## ----summarize_extinction_risk_by_forest_dependency-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Redlist category/status ('Threatened' in phylogenetic regression)
fd_status_sum <- iucn_data %>%
  group_by(Forest_depend_Betts, Redlist_category) %>%
  summarise(Num_spp = n()) %>%
  ungroup() %>%
  spread(key = Redlist_category, value = Num_spp, fill = 0)

#Population trends ('Decreasing' in phylogenetic regression)
fd_pop_sum <- iucn_data %>%
  group_by(Forest_depend_Betts, Population_trend) %>%
  summarise(Num_spp = n()) %>%
  ungroup() %>%
  spread(key = Population_trend, value = Num_spp, fill=0)


## ----export_summary_tables------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/leimberk/Box/Biol_Reviews_Analyses/IUCN_status_and_traits/2_Making_IUCN_summary_tables")
#write.csv(fd_status_sum, "export/IUCN_Status_Summary_20200407.csv")
#write.csv(fd_pop_sum, "export/IUCN_Population_Summary_20200407.csv")
#write.csv(clade_status_sum, "export/IUCN_Status_Summary_by_Clade_20200904.csv")
#write.csv(clade_pop_sum, "export/IUCN_Population_Summary_by_Clade_20200904.csv")


## ----function_to_make_stacked_barchart------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Writing function to make stacked bar plots
make_stacked_plots <- function(data, x_var, x_label, y_label, fill_var, fill_label, rotate_x_text){

  fill_col <- data[[deparse(ensym(fill_var))]] #deparse() converts symbols to strings
  palette_length <- length(unique(fill_col))
  if(palette_length == 3){palette_cols = c("#2c7bb6", "#ffffbf", "#d7191c")} #color brewer colors for RdYlBu
  if(palette_length == 5){palette_cols = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")} #color brewer colors for RdYlBu
 
  base_plot  <-  ggplot(data, aes(fill = !!as.name(fill_var), x = !!as.name(x_var))) +
    geom_bar(position=position_stack(reverse=TRUE), width=0.5, colour="black", size = 0.15) + #To look at total number of birds, reversed order of colors
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = x_label, y = y_label, fill = fill_label) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_fill_manual(values = palette_cols)

  if(rotate_x_text == FALSE){
    
    plot  <-  base_plot +
      scale_x_discrete(labels = c("Mountain Gems" = "Mountain \nGems", "Forest-exclusive" = "Exclusive", "Forest-optional" = "Optional")) #Need to shorten/move to diff lines if don't rotate
    
   }
  
  if(rotate_x_text == TRUE){
    plot  <-  base_plot +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=55, hjust=1)) + #Rotated labels
      scale_x_discrete(labels = c("Mountain Gems" = "Mtn Gems", "Forest-exclusive" = "Forest-\nexclusive", "Forest-optional" = "Forest-\noptional"))
  }
  
  return(plot)
    
}


## ----make_forest_depend_plots---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fd_plot_data <- iucn_data %>%
    filter(Forest_depend_Betts != "Unknown")

fd_status_stacked_plot <- make_stacked_plots(data = fd_plot_data %>% filter(Redlist_category != "Data Deficient"),
                                             x_var = "Forest_depend_Betts",
                                             x_label = "",
                                             y_label = "",
                                             fill_var="Redlist_category",
                                             fill_label = "",
                                             rotate_x_text = TRUE)
fd_status_stacked_plot

fd_pop_stacked_plot <- make_stacked_plots(data = fd_plot_data %>% filter(Population_trend != "Unknown"),
                                          x_var = "Forest_depend_Betts",
                                          x_label = "\nForest dependency", #Line break before x-axis label moves it down
                                          y_label = "",
                                          fill_var = "Population_trend",
                                          fill_label = "",
                                          rotate_x_text = TRUE) 
fd_pop_stacked_plot

#--------------
#Combine plots
fd_stacked_multiplot <- ggpubr::ggarrange(fd_status_stacked_plot, fd_pop_stacked_plot, ncol = 2, nrow = 1, align = "h", common.legend = FALSE, legend = "right")
fd_stacked_multiplot

#Export
#ggsave(filename = "export/IUCN_Extinction_risk_Forest_depend_20200407.png", plot = stacked_multiplot, device = "png", width = 11, height = 8.5, units = "in")


## ----make_clade_plots-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clade_plot_data <- iucn_data %>%
    filter(Clade != "Patagona") #Remove Patagona (only 1 species)

clade_status_stacked_plot <- make_stacked_plots(data = clade_plot_data %>% filter(Redlist_category != "Data Deficient"),
                                                x_var = "Clade",
                                                x_label = "",
                                                y_label = "Number of species",
                                                fill_var = "Redlist_category",
                                                fill_label = "",
                                                rotate_x_text = TRUE)
clade_status_stacked_plot

clade_pop_stacked_plot <- make_stacked_plots(data = clade_plot_data %>% filter(Population_trend != "Unknown"),
                                             x_var = "Clade",
                                             x_label = "\nClade",
                                             y_label = "Number of species",
                                             fill_var = "Population_trend",
                                             fill_label = "",
                                             rotate_x_text = TRUE)
clade_pop_stacked_plot

#--------------
#Combine plots
clade_stacked_multiplot <- ggpubr::ggarrange(clade_status_stacked_plot, clade_pop_stacked_plot, ncol = 2, nrow = 1, align = "h", common.legend = FALSE, legend = "right")
clade_stacked_multiplot

#Export
#ggsave(filename = "export/IUCN_Extinction_risk_Forest_depend_20200407.png", plot = stacked_multiplot, device = "png", width = 11, height = 8.5, units = "in")


## ----combine_plots_into_single_figure-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Combine plots for RedList category (will be plots A & B)
fd_clade_status_multiplot <- ggpubr::ggarrange(clade_status_stacked_plot, fd_status_stacked_plot, ncol = 2, nrow = 1, align = "h", common.legend = TRUE, legend = "top", labels = c("A", "B"), font.label = list(size = 18))
fd_clade_pop_multiplot <- ggpubr::ggarrange(clade_pop_stacked_plot, fd_pop_stacked_plot, ncol = 2, nrow = 1, align = "h", common.legend = TRUE,  legend = "top", labels = c("C", "D"), font.label = list(size = 18))

#Combine plots for population trend (will be plots C & D)
fd_clade_status_multiplot <- ggpubr::annotate_figure(fd_clade_status_multiplot, top = text_grob("Red List category", face = "bold", size = 18))
fd_clade_pop_multiplot <- ggpubr::annotate_figure(fd_clade_pop_multiplot, top = text_grob("Population trend", face = "bold", size = 18))

fd_clade_status_multiplot
fd_clade_pop_multiplot

#Combine plots above to create 4-panel plot (A-D)
fd_clade_multiplot <- ggpubr::ggarrange(fd_clade_status_multiplot, fd_clade_pop_multiplot, ncol = 1, nrow = 2, align = "v", common.legend = FALSE)
fd_clade_multiplot

#Export
setwd("C:/Users/leimberk/Box/Biol_Reviews_Analyses/IUCN_status_and_traits/2_Making_IUCN_summary_tables")
#ggsave(filename = "export/IUCN_extinction_risk_multiplot_rdylbu_20201030.png", plot = fd_clade_multiplot, device = "png", width = 10, height = 12, units = "in")


## ----number_of_species_per_panel------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Info for figure caption
panel_A_sum <- clade_plot_data %>% filter(Redlist_category != "Data Deficient") %>% summarise(Num_spp = n())
panel_C_sum <- clade_plot_data %>% filter(Population_trend != "Unknown") %>% summarise(Num_spp = n())

panel_B_sum <- fd_plot_data %>% filter(Redlist_category != "Data Deficient") %>% summarise(Num_spp = n())
panel_D_sum <- fd_plot_data %>% filter(Population_trend != "Unknown") %>% summarise(Num_spp = n())

