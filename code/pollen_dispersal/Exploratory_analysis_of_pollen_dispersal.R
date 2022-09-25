## ----setup, include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(ggpubr)

set.seed(0)


## ----import_digitized_data---------------------------------------------------------------------------------------------------------------------------------------------------------------------
digitized_points <- read.csv("data/pollen_dispersal/Digitized_pollen_dispersal_data.csv")


## ----prepare_data_for_analysis-----------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- digitized_points %>%
  
  mutate_at(vars(X, Y), ~round(., digits=0)) %>% #Round X & Y values- don't actually have as much precision as numbers from digitization imply
  mutate(Y = Y/100) %>% #Convert percent to proportion so that y ranges between 0 and 1

  #Prepare species names for plotting
  separate(Plant_species, into = c("Genus", "Species"), remove = FALSE) %>% #Remove underscore from plant names for plotting (NEW COLUMN = "SPECIES_NAME")
  mutate(Species_name = paste(Genus, Species, sep = " ")) %>%
  select(-Genus, -Species) %>%

  #Split Trinidad studies from Tobago studies for plotting/analysis
  mutate(Species_name = ifelse(grepl("Trinidad", Replicate), paste(Species_name, "(Trinidad)", sep = " "), Species_name))%>%
  mutate(Species_name = ifelse(grepl("Tobago", Replicate), paste(Species_name, "(Tobago)", sep = " "), Species_name))


## ----fit_exponential_decay_curves--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Figured out starting parameter for alpha by experimenting with stats::SSasymp (SS = self starting)
plots_per_species <- data %>%
  group_by(Species_name, Dye_source_defense) %>%
  do(plot = ggplot(., aes(x = X, y = Y)) +
       geom_point() +
       stat_smooth(method = "nls", formula = y ~ exp(-alpha*x), method.args = list(start = c(alpha = 0.001)), se = FALSE) +
       labs(title = paste(.$Species_name, .$Dye_source_defense, sep = ": "))) %>%
  ungroup()

plots_per_species$plot

#All the species have fits!


## ----make_table_of_fit_parameters--------------------------------------------------------------------------------------------------------------------------------------------------------------
model_fits <- data %>%
  group_by(Species_name, Dye_source_defense)%>%
  do(fit = nls(formula = Y ~ exp(-alpha*X), data = ., start = list(alpha = 0.001)))

model_fits$fit

#Rate parameter alpha
rate_params <- model_fits %>%
  nest_by(Species_name, Dye_source_defense, fit) %>%
  summarise(broom::tidy(fit)) %>%
  ungroup() %>%
  select(Species_name, Dye_source_defense, term, estimate) %>% 
  spread(term, estimate) %>%
  rename(rate_constant = alpha) %>%
  ungroup()

#Residual sum of squares
#There is probably a more direct way to extract this from the fit objects, but this works
rss <- tibble(model = model_fits$fit) %>%
  mutate(residuals = purrr::map(model, resid),
         squared_residuals = purrr::map(residuals, ~.x^2),
         model_num = 1:length(model)) %>%
  group_by(model_num) %>%
  summarise(sum_squared_residuals = sum(unlist(squared_residuals))) %>%
  ungroup()

#Join together..
fit_summary <- cbind(rate_params, rss) %>%
  select(model_num, everything())


## ----summarize_dispersal_distances-------------------------------------------------------------------------------------------------------------------------------------------------------------
dispersal_distance_sum <- data %>%
  filter(Y > 0) %>% #Interested in how far dye got, so not interested in distances that didn't have any dye found (i.e., Y = 0)
  group_by(Study, Species_name, Dye_source_defense) %>%
  summarise(max = max(X, na.rm = TRUE),
            min = min(X, na.rm = TRUE),
            mean = mean(X, na.rm = TRUE),
            median = median(X, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(max)


## ----calculate_distance_with_50%_initial_value-------------------------------------------------------------------------------------------------------------------------------------------------
#Calculate distance at which dye proportion is 50% of initial value
#First, calculate initial dye transfer proportion, and calculate its half value
#This is a bit complicated because, initial values are not necessarily max value (1)
#Also need to account for there being multiple initial values for studies/species with multiple replicates (days)
first_x_values <- data %>%
  group_by(Study, Species_name, Dye_source_defense) %>%
  arrange(X) %>%
  summarise(first_X = first(X)) %>%
  ungroup() %>%
  mutate(first_x_ID = paste(Species_name, first_X, sep = "_"))

initial_y_values <- data %>%
  filter(Y > 0) %>%
  mutate(first_x_ID = paste(Species_name, X, sep = "_")) %>%
  filter(first_x_ID %in% first_x_values$first_x_ID) %>%
  group_by(Study, Species_name, Dye_source_defense) %>%
  summarise(initial_y = max(Y)) %>% #For species with > 1 value (i.e., if multiple replicates) - pick max initial value
  ungroup()
  
half_y_values <- initial_y_values %>%
  mutate(half_y = initial_y/2)

#At what distance does the curve hit the 'half_y' value?
#With user-defined starting values
half_y_predicted_distances <- fit_summary %>%
  left_join(half_y_values) %>%
  mutate(half_y_distance = log(half_y)/(-rate_constant)) #Solve for x, from following equation: y = exp(-alpha * x)


## ----calculate_max_search_distance-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculate max distance that pollen was searched for (can include zero)
max_search_distance_sum <- data %>%
  group_by(Study, Species_name, Dye_source_defense)%>%
  summarise(max_search_dist = max(X))


## ----combine_summaries_of_pollen_dispersal-----------------------------------------------------------------------------------------------------------------------------------------------------
library(stringr)
pollen_dispersal_sum <- half_y_predicted_distances %>%
  left_join(dispersal_distance_sum) %>%
  left_join(max_search_distance_sum) %>%
  mutate_if(is.numeric, ~round(., digits=2)) %>%
  separate(col = Study, into = c("Authors", "Study_year"), sep = "_(?=[[:digit:]])", remove = FALSE) %>%
  select(model_num, Study_year, Study, Species_name, Dye_source_defense, rate_constant, sum_squared_residuals, initial_y, half_y_distance, min, max, mean, median, everything()) %>%
  arrange(Study_year, Study, Species_name)

#Median search distance
median(pollen_dispersal_sum$max_search_dist)
  
#Export
write.csv(pollen_dispersal_sum, "results/pollen_dispersal/Dispersal_curves_summary.csv")

## ----make_boxplots-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function to make boxplots comparing summary statistics for defended vs. undefended dye sources
#Metrics to compare:
#1. Maximum dispersal distance

make_boxplot<-function(data, var){
  
  if(var == "max"){
    title_label <- "Maximum \ndispersal distance (m)\n" #\n adds a line afterwards; cheat to get label farther away from axis
  }
  
  if(var == "median"){
    title_label <- "Median \ndispersal distance (m)\n"
  }
  
  if(var == "rate_constant"){
    title_label <- "Decay rate\n"
  }
  
  if(var == "half_y_distance"){
    #title_label<-"Distance at which proportion of flowers \nwith dye reaches 50% of initial value (m)"
    title_label<-"50% decay distance (m)\n"
  } 
  
  boxplot <- ggplot(data = data, aes(x = Dye_source_defense, y = .data[[var]], fill = Dye_source_defense))+
             geom_boxplot(show.legend = FALSE, alpha = 0.5, width = 0.5)+
             geom_point(size = 2, shape = 21) +
             labs(y = title_label, x = "") +
             theme_bw() +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x = element_text(size = 14),
                   axis.text.y = element_text(size = 14),
                   axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   plot.margin = unit(c(1.5, 1.5, 1.5, 2.5), "lines"), #Default plot margins in theme_bw are 1, 1, 0.5, 0.5 (top, right, bottom, left); adjusting for spacing in ggarrange when combine plots
                   legend.position = "none") +
             scale_fill_grey()

  return(boxplot)
}

max_boxplot <- make_boxplot(data = pollen_dispersal_sum, var = "max")
max_boxplot

median_boxplot <- make_boxplot(data = pollen_dispersal_sum, var = "median")
median_boxplot

decay_boxplot <- make_boxplot(data = pollen_dispersal_sum, var = "rate_constant")
decay_boxplot

half_y_boxplot <- make_boxplot(data = pollen_dispersal_sum, var = "half_y_distance")
half_y_boxplot

#Decided not to try and do any stats because sample size is T I N Y


## ----define_colors_for_dispersal_curves--------------------------------------------------------------------------------------------------------------------------------------------------------
defend_data <- filter(data, Dye_source_defense == "Defended")
undefend_data <- filter(data, Dye_source_defense == "Undefended")

num_defend_colors <- length(unique(paste(defend_data$Species_name, defend_data$Dye_source_defense, sep = "_")))
red_colors <- colorRampPalette(rev(brewer.pal(6, "YlOrRd")))(num_defend_colors + 3) #For defended species. Plus 3 gets rid of super light colors

num_undefend_colors <- length(unique(paste(undefend_data$Species_name, undefend_data$Dye_source_defense, sep = "_")))
blue_colors <- colorRampPalette(rev(brewer.pal(6, "YlGnBu")))(num_undefend_colors + 3) #For undefended species. Plus 3 gets rid of super light colors


## ----make_dispersal_curve_plots----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function to make plots (same base plot, different data)
make_curve_plots<-function(data, defended){
  
  if(defended == "Y"){
    
    plot_data <- data %>% filter(Dye_source_defense == "Defended")
    title_label <- c("Source plant defended")
    legend_labels <- defend_plant_labels
    colors <- red_colors
    position <- c(0.75, 0.65)

  }
  
  if(defended == "N"){
    
     plot_data <- data %>% filter(Dye_source_defense == "Undefended")
     title_label <- c("Source plant not defended")
     legend_labels <- undefend_plant_labels
     colors <- blue_colors
     position <- c(0.75, 0.6)
  }
  
  plot <- plot_data %>%
    ggplot(aes(x = X, y = Y, colour = Species_name)) +
      geom_point(size = 1) +
      theme_bw() +
      xlim(0, 325) +
      labs(colour = "Plant species", x = "\nDistance from source plant (m)\n", y = "Proportion of flowers with dye\n", title = title_label) +
      geom_line(stat = "smooth", method = "nls", formula = y~exp(-alpha*x), method.args = list(start = c(alpha = 0.001)), se = FALSE, alpha = 1, size = 1) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 16), #can also set size here
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.text.align = 0, #Using expression makes legend text right-aligned for some reason
            legend.background = element_blank(),
            legend.title = element_text(face = "bold", size = 14),
            legend.position = position) + #the coordinates for legend.position are x- and y- offsets from the bottom-left of the plot, ranging from 0 - 1.
      scale_color_manual(values = colors, labels = legend_labels)

  return(plot)
}

#Reorder and italicize plant names in legend. Want color gradient to match decay rate.
temp <- pollen_dispersal_sum %>% arrange(Dye_source_defense, desc(rate_constant))

defend_plant_order <- c("Heliconia latispatha", "Justicia secunda (Trinidad)", "Hansteinia blepharorachis", "Heliconia imbricata", "Justicia spp", "Echeveria gibbiflora")
defend_plant_labels <- c(expression(italic("Heliconia latispatha")), expression(italic("Justicia secunda")*" (Trinidad)"), expression(italic("Hansteinia blepharorachis")), expression(italic("Heliconia imbricata")), expression(italic("Justicia")~"spp."), expression(italic("Echeveria gibbiflora")))

undefend_plant_order <- c("Mandevilla hirsuta (Trinidad)", "Heliconia acuminata", "Justicia secunda (Trinidad)", "Rasizea spicata", "Heliconia tortuosa", "Justicia secunda (Tobago)", "Mandevilla hirsuta (Tobago)")
undefend_plant_labels <- c(expression(italic("Mandevilla hirsuta")*" (Trinidad)"), expression(italic("Heliconia acuminata")), expression(italic("Justicia secunda")*" (Trinidad)"), expression(italic("Rasizea spicata")), expression(italic("Heliconia tortuosa")), expression(italic("Justicia secunda")*" (Tobago)"), expression(italic("Mandevilla hirsuta")*" (Tobago)"))

defend_plot <- make_curve_plots(data = data %>% mutate(Species_name = factor(Species_name, levels = defend_plant_order)), defended = "Y")
defend_plot

undefend_plot <- make_curve_plots(data = data %>% mutate(Species_name = factor(Species_name, levels = undefend_plant_order)), defended = "N")
undefend_plot


## ----combine_plots-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Make a composite figure with pollen dispersal curves *and* boxplots
#Pollen dispersal curves
curves_multiplot <- ggpubr::ggarrange(defend_plot, undefend_plot,
                                    labels = c("A", "B"),
                                    font.label = list(size = 14),
                                    ncol = 1, nrow = 2,
                                    align = "v")
curves_multiplot

#Boxplots of summary metrics
boxplot_multiplot <- ggpubr::ggarrange(max_boxplot, median_boxplot, half_y_boxplot, decay_boxplot,
                                 labels = c("C", "D", "E", "F"), #Lettering starts at 'C' because curves are 'A'+'B"
                                 font.label = list(size = 14),
                                 hjust = -0.5, vjust = 1.5, #Default label positions
                                 ncol = 2, nrow = 2,
                                 align = "v")

boxplot_multiplot

#Pollen dispersal curves plus boxplots
curves_boxplot_multiplot <- ggpubr::ggarrange(curves_multiplot, boxplot_multiplot, ncol = 1, nrow = 2, heights = c(1.5, 1), align = "v") #With boxplots below
curves_boxplot_multiplot

#Export
ggsave(filename = "results/pollen_dispersal/Pollen_dispersal_curves_metrics.png", plot = curves_boxplot_multiplot, device = "png", width = 8.5, height = 14, units = "in", dpi = 300, bg = "white")

