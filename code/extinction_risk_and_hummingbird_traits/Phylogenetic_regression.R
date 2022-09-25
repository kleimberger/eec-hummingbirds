## ----setup----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(ggplot2)
library(GGally)
library(ape)
library(phylolm)

#Bootstraps within phyloglm() take forever! But you can remedy this by running >1 processor at a time.
#From phyloglm help..."Bootstrapping can be parallelized using the future package on any future compatible back-end. For example, run library(future); plan(multiprocess)), after which bootstrapping will automatically occur in parallel. See plan for options."
library(future)
plan(multiprocess)

#Set seed to make bootstrap results totally reproducible
set.seed(0)


## ----get_hummingbird_data-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Data with derived variables (i.e., migrant/forest dependency/hermit scores already calculated, variables already log-transformed)
hbird_data <- read.csv("data/extinction_risk_and_hummingbird_traits/Hummingbird_IUCN_and_trait_data_with_derived_vars_20210518.csv") %>%
  mutate_at(vars(Migratory_score, Forest_depend_score, Hermit), ~as.factor(.))

#Create separate datasets to use later
threat_data <- hbird_data %>%
  filter(!is.na(Threatened) & Threatened != "NA")

decrease_data <- hbird_data %>%
  filter(!is.na(Decreasing) & Decreasing != "NA")

#Lists of predictor variables, to use later
vars <- c("Body_mass", "Bill_length", "Range_size", "Elevation_range", "Human_footprint", "Migratory_score", "Forest_depend_score", "Hermit") #All variables
vars_log <- c("Body_mass_log", "Bill_length_log", "Range_size_log", "Elevation_range", "Human_footprint", "Migratory_score", "Forest_depend_score", "Hermit") #All variables, some ln-transformed
vars_pretty_names <- c("Ln Body mass", "Ln Bill length", "Ln Range size", "Elevational range", "Human footprint within range", "Migrant", "Forest specialist", "Hermit")


## ----get_tree-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tree_mcc <- read.nexus("data/extinction_risk_and_hummingbird_traits/BEAST_MCC_10000_tree.tre")
tree_mcc_hbirds <- drop.tip(tree_mcc, tip = c("Aegotheles_insignis", "Hemiprocne_mystacea")) #Without the outgroups (nightjar + tree swift)

unique(Ntip(tree_mcc)) #Number of species (tips) included in the tree (N=336)
unique(Ntip(tree_mcc_hbirds)) #Number of species (tips) included in the tree (N=334)
plot(tree_mcc_hbirds, cex = 0.3, no.margin = TRUE) #The tree exists!

#List of species included in the tree
species_list <- data.frame(tree_mcc_hbirds$tip.label) #only look at first tree of 1000
names(species_list) <- "species"


## ----explore_iucn_data----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Number of species with data for RedList category ('Threatened') and each predictor variable
threat_by_predict <- threat_data %>%
  group_by(Threatened) %>%
  summarise_at(all_of(vars), list(~sum(!is.na(.) & . != "NA"))) %>%
  ungroup() %>%
  mutate(Threatened = as.character(Threatened))

threat_sum <- threat_data %>%
  summarise_at(all_of(vars), list(~sum(!is.na(.) & . != "NA"))) %>%
  mutate(Threatened = "All") %>%
  bind_rows(threat_by_predict)

#Number of species with data on population trend ('Decreasing') and each predictor variable
decrease_by_predict <- decrease_data %>%
  group_by(Decreasing) %>%
  summarise_at(all_of(vars), list(~sum(!is.na(.) & . != "NA"))) %>%
  ungroup() %>%
  mutate(Decreasing = as.character(Decreasing))

decrease_sum <- decrease_data %>%
  summarise_at(all_of(vars), list(~sum(!is.na(.) & . != "NA"))) %>%
  mutate(Decreasing = "All") %>%
  bind_rows(decrease_by_predict)

#Summary plot of the response variables
(threat_hist <- ggplot(data = hbird_data, aes(x = as.factor(Threatened))) +
    geom_histogram(stat = "count") + 
    labs(x = "Species threatened"))

(decrease_hist <- ggplot(data = hbird_data, aes(x = as.factor(Decreasing))) +
    geom_histogram(stat = "count") +
    labs(x = "Population decreasing"))


## ----explore_predictors---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Want to look at dataset close to what will be used in analysis. Remove species without complete set of predictor variables...
threat_vars <- threat_data %>% select(all_of(vars) | all_of(vars_log))
threat_data_complete <- threat_vars[complete.cases(threat_vars), ]

decrease_vars <- decrease_data %>% select(all_of(vars) | all_of(vars_log))
decrease_data_complete <- decrease_vars[complete.cases(decrease_vars), ]

#Response: Threatened
#With un-logged variables
(threat_pairs <- GGally::ggpairs(dplyr::select(threat_data_complete, all_of(vars)), 
                        upper = list(continuous = wrap("cor", size = 4, display_grid = FALSE), combo = "na", discrete = "na"),
                        lower = list(continuous = "points", combo = "box_no_facet", discrete = "facetbar")))


#With log-transformed variables. Distributions are much less skewed for morphological measurements, range size
(threat_pairs_log <- GGally::ggpairs(dplyr::select(threat_data_complete, all_of(vars_log)), 
                        upper = list(continuous = wrap("cor", size = 4, display_grid = FALSE), combo = "na", discrete = "na"),
                        lower = list(continuous = "points", combo = "box_no_facet", discrete = "facetbar")))

#Response: Decreasing
#With un-logged variables
(decrease_pairs <- GGally::ggpairs(dplyr::select(decrease_data_complete, all_of(vars)), 
                        upper = list(continuous = wrap("cor", size = 4, display_grid = FALSE), combo = "na", discrete = "na"),
                        lower = list(continuous = "points", combo = "box_no_facet", discrete = "facetbar")))


#With log-transformed variables. Distributions are much less skewed for morphological measuresments, range size
(decrease_pairs_log <- GGally::ggpairs(dplyr::select(decrease_data_complete, all_of(vars_log)), 
                        upper = list(continuous = wrap("cor", size = 4, display_grid = FALSE), combo = "na", discrete = "na"),
                        lower = list(continuous = "points", combo = "box_no_facet", discrete = "facetbar")))


#Export for better viewing
#ggsave("results/extinction_risk_and_hummingbird_traits/Threat_pairs_plots.pdf", threat_pairs_log, width=11, height=8.5, unit=c("in"))
#ggsave("results/extinction_risk_and_hummingbird_traits/Decrease_pairs_plots.pdf", decrease_pairs_log, width=11, height=8.5, unit=c("in"))

## ----functions_for_modeling-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Subset data to complete cases of response, predictors
subset_data <- function(y, vars, data){
  
  #Subset data to complete observations of a given response variable (for my purposes, this will either be 'Threatened' or 'Decreasing')
  complete_y <- filter(data, !is.na(!!as.name(y)) & !!as.name(y) != "NA")
  
  #Subset data to complete observations of predictor variables of interest
  complete_data <- tidyr::drop_na(complete_y, vars)
  complete_data <- as.data.frame(complete_data) #Convert to data frame instead of tibble to use row.names
  row.names(complete_data) <- complete_data$Species_name
  return(complete_data)
  
}

#Standardize continuous predictor variables
#scale() transforms data to mean=0, SD=1
scale_data <- function(vars, data){
  
  cols_to_scale <- data %>% 
    select(vars) %>%
    mutate_if(is.numeric, scale) #Note: this replaces the data in the original column but doesn't change name!
  
  scaled_data <- data %>%
    select(-vars) %>%
    cbind(cols_to_scale)
  
  row.names(scaled_data) <- data$Species_name #Lost rownames during mutate, putting them back
  
  return(scaled_data)
  
}

#Prune phylogenetic tree 
#Only want tree to include the species that are in the complete dataset
prune_tree <- function(tree, data){
  
  pruned_tree <- drop.tip(tree, tip = tree$tip.label[!(tree$tip.label %in% data$Species_name)])
  return(pruned_tree)
  
}


#Subset data to species in phylogenetic tree
#Subset dataset -- again! -- because some species are not in the phylogenetic tree (tree only has 334 species; full dataset has 360+)
subset_data_to_tree <- function(pruned_tree, data){
  
  tree_data <- filter(data, Species_name %in% pruned_tree$tip.label)
  return(tree_data)
  
}

#Create model formula from variables of interest
create_model_formula<-function(y, vars){

  predictor_list <- paste(vars, collapse=" + ")
  model_formula <- formula(paste(y, "~", predictor_list))
  return(model_formula)
  
}


#Overall function to run models 
#Inputs: variables of interest, phylogenetic tree, full dataset.
#Arguments: bootstrap (TRUE/FALSE)
run_model <- function(y, vars, tree, data, bootstrap){
  
    complete_data <- subset_data(y, vars, data) #Subset data to rows with complete observations (no missing responses, no missing predictors)
    pruned_tree <- prune_tree(tree, complete_data) #Prune phylogenetic tree to only include species in the complete dataset
    pruned_tree_data <- subset_data_to_tree(pruned_tree, complete_data) #Subset data -- again -- to only include species that are in the phylogenetic tree. There shouldn't be any mismatch, but just to make sure.
    scaled_data <- scale_data(vars, pruned_tree_data) #Scale the continuous predictor variables (original columns overwritten)

    formula <- create_model_formula(y, vars) #Create model formula
    
    if(bootstrap == TRUE){boot_value = 1000}
    if(bootstrap == FALSE){boot_value = 0}

    #Default log.alpha.bound is 4, default btol is 10. Will not run with logistic_MPLE.
    model <- phyloglm(formula, data = scaled_data, phy = pruned_tree, method = "logistic_IG10", boot = boot_value, btol = 25) 
      
    return(model)  
}

#Testing these functions
#With RedList category...
complete_data_test <- subset_data(y = "Threatened", vars = vars_log, data = hbird_data)
pruned_tree_test <- prune_tree(tree = tree_mcc_hbirds, data = complete_data_test)
unique(Ntip(pruned_tree_test)) #Number of species (tips) included in the tree
pruned_tree_data_test <- subset_data_to_tree(pruned_tree = pruned_tree_test, data = complete_data_test)
scaled_data_test <- scale_data(vars = vars_log, data = pruned_tree_data_test)
formula_test <- create_model_formula(y = "Threatened", vars = vars_log)
model_test <- run_model(y = "Threatened", vars = vars_log, tree = tree_mcc_hbirds, data = scaled_data_test, bootstrap = FALSE)
summary(model_test)

which(!(rownames(complete_data_test) %in% pruned_tree_test$tip.label)) #Which species are in data but not in tree. Want this to be "integer (empty)".
which(!(pruned_tree_test$tip.label %in% rownames(complete_data_test))) #Which species are in tree but not in data. Want this to be "integer (empty)".

#With population trend...
complete_data_test <- subset_data(y = "Decreasing", vars = vars_log, data = hbird_data)
pruned_tree_test <- prune_tree(tree = tree_mcc_hbirds, data = complete_data_test)
unique(Ntip(pruned_tree_test)) #Number of species (tips) included in the tree
pruned_tree_data_test <- subset_data_to_tree(pruned_tree = pruned_tree_test, data = complete_data_test)
scaled_data_test <- scale_data(vars = vars_log, data = pruned_tree_data_test)
formula_test <- create_model_formula(y = "Threatened", vars = vars_log)
model_test <- run_model(y = "Threatened", vars = vars_log, tree = tree_mcc_hbirds, data = scaled_data_test, bootstrap = FALSE)
summary(model_test)

which(!(rownames(complete_data_test) %in% pruned_tree_test$tip.label)) #Which species are in data but not in tree. Want this to be "integer (empty)".
which(!(pruned_tree_test$tip.label %in% rownames(complete_data_test))) #Which species are in tree but not in data. Want this to be "integer (empty)".


## ----run_threat_model-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#With bootstrapped confidence intervals (takes really long time to run!)
# threat_model_boot <- run_model(y = "Threatened",
#                                vars = vars_log,
#                                tree = tree_mcc_hbirds,
#                                data = hbird_data,
#                                bootstrap = TRUE)
# 
# summary(threat_model_boot)
# threat_model_boot$alphaWarn #Check to see if alpha parameter is near bounds (0 = not near bounds, 1 = near upper bound, 2 = near lower bound)


## ----run_decrease_model---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#With bootstrapped confidence intervals (takes really long time to run!)
# decrease_model_boot <- run_model(y = "Decreasing",
#                                  vars = vars_log,
#                                  tree = tree_mcc_hbirds,
#                                  data = hbird_data,
#                                  bootstrap = TRUE)
# 
# summary(decrease_model_boot)
# decrease_model_boot$alphaWarn #Check to see if alpha parameter is near bounds (0 = not near bounds, 1 = near upper bound, 2 = near lower bound)


## ----export_bootstraps----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#write_rds(threat_model_boot, "results/extinction_risk_and_hummingbird_traits/Threat_model_output_20210513.rds")
#write_rds(decrease_model_boot, "results/extinction_risk_and_hummingbird_traits/Decrease_model_output_20210513.rds")

#Reimport bootstrapped models to don't have to run again if need to re-make figures, etc.
threat_model_boot <- read_rds("results/extinction_risk_and_hummingbird_traits/Threat_model_output_20210513.rds")
decrease_model_boot <- read_rds("results/extinction_risk_and_hummingbird_traits/Decrease_model_output_20210513.rds")


## ----vif_function---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#car::vif() does not work with phyloglm objects. Get the following error: "Error in terms.default: no terms component nor atribute".
#To use car::vif, need an object that responds to coef, vcov, and model.matrix, such as an lm or glm object.
#Original code from https://github.com/cran/car/blob/master/R/vif.R                   
#On 04/01/2020, KGL edited the vif.default() functon to be compatible with phyloglm() model object. 

calculate_vif <- function(mod, ...) {
    if (any(is.na(coef(mod)))) 
        stop ("there are aliased coefficients in the model")
    v <- mod$vcov #KGL replaced vcov(mod) with mod$vcov
    assign <- attr(mod$X, "assign") #KGL replaced model.matrix(mod) with mod$X
    if (names(coefficients(mod)[1])  ==  "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    }
    else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(formula(mod))) #KGL replaced mod with formula(mod)
    n.terms <- length(terms)
    if (n.terms < 2) stop("model contains fewer than 2 terms")
    R <- cov2cor(v) #cov2cor scales a covariance matrix into the corresponding correlation matrix (R)
    detR <- det(R) #determinant of correlation matrix
    result <- matrix(0, n.terms, 3) #making an empty matrix for the results
    rownames(result) <- terms #making an empty matrix for the results
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))") #making an empty matrix for the results
    for (term in 1:n.terms) {
        subs <- which(assign  ==  term)
        result[term, 1] <- det(as.matrix(R[subs, subs])) *
            det(as.matrix(R[-subs, -subs])) / detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2]  ==  1)) result <- result[, 1] #if column 2 (Df) is 1, then just return the first column (GVIF) - skip the columns for Df and GVIF^(1/(2*Df))
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    result
}


## ----calculate_vif--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function to check for collinearity, make summary table of VIF
make_vif_table <- function(model, y){

      vif_table <- calculate_vif(model) %>%
             data.frame() %>%
             rownames_to_column(var = "Variable") %>%
             rename(VIF = ".") %>% #Rename column with VIF value (currently named ".")
             mutate(Response = !!y) %>%
             mutate(Variable = case_when(
                    Variable  ==  "(Intercept)" ~ "Intercept",
                    Variable  ==  "Body_mass_log" ~ "Ln Body mass",
                    Variable  ==  "Bill_length_log" ~ "Ln Bill length",
                    Variable  ==  "Range_size_log" ~ "Ln Range size",
                    Variable  ==  "Elevation_range" ~ "Elevational range",
                    Variable  ==  "Human_footprint" ~ "Human footprint within range",
                    Variable  ==  "Migratory_score1" ~ "Migrant",
                    Variable  ==  "Forest_depend_score1" ~ "Forest specialist",
                    Variable  ==  "Hermit1" ~ "Hermit",
                    TRUE ~ as.character(Variable))) %>%
            mutate_at(vars(VIF), list(~round(., digits =  2)))
  
  return(vif_table)
      
}

#Models with bootstrapped CI
threat_vif_boot <- make_vif_table(model = threat_model_boot, y = "Threatened") %>% mutate(CI_type = "Bootstrap")
decrease_vif_boot <- make_vif_table(model = decrease_model_boot, y = "Decreasing") %>% mutate(CI_type = "Bootstrap")

#Combine VIF output
vif_table <- threat_vif_boot %>%
  bind_rows(decrease_vif_boot)

#Export
write.csv(vif_table, "results/extinction_risk_and_hummingbird_traits/Table_of_VIF_values.csv")

## ----export_results-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function to get summary output from phyloglm object
get_summary_output <- function(model){
  
  # output <- summary(model)[[2]] %>%
  #   data.frame() %>%
  #   rownames_to_column(var = "Variable") %>%
  #   mutate(Alpha=model$alpha)
  
  confint <- model$bootconfint95 %>%
    t() %>%
    data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    rename(LowerCI = "X2.5.", UpperCI = "X97.5.")
    
  alpha <- data.frame(Variable = "alpha", Estimate = model$alpha)
  
  output <- model$coefficients %>%
    data.frame() %>%
    rename(Estimate = ".") %>%
    rownames_to_column(var = "Variable") %>%
    bind_rows(alpha) %>%
    left_join(confint)
  
  return(output)
  
}

#Models with bootstrapped CI
threat_summary_boot <- get_summary_output(threat_model_boot) %>% mutate(Response = "Threatened", CI_type = "Bootstrap")
decrease_summary_boot <- get_summary_output(decrease_model_boot)  %>% mutate(Response = "Decreasing", CI_type = "Bootstrap")

#Combine summary output
summary_output <- threat_summary_boot %>%
  bind_rows(decrease_summary_boot) %>%
  select(CI_type, Response, Variable, everything())

#Function to make summary tables for plotting, export
make_table <- function(starter_table){
  
  starter_table <- starter_table %>%
    mutate(Response = factor(Response, levels = c("Threatened", "Decreasing"), labels = c("Threatened", "Decreasing population")))
  
  #Will this row out temporarily, add back in later
  alpha <- starter_table %>%
    filter(Variable == "alpha")
  
  #Step 1: Rename factor levels
  table1 <- starter_table %>%
    filter(Variable != "alpha") %>%
    mutate(Variable = case_when(
                    Variable == "(Intercept)" ~ "Intercept",
                    Variable == "Body_mass_log" ~ "Ln Body mass",
                    Variable == "Bill_length_log" ~ "Ln Bill length",
                    Variable == "Range_size_log" ~ "Ln Range size",
                    Variable == "Elevation_range" ~ "Elevational range",
                    Variable == "Human_footprint" ~ "Human footprint within range",
                    Variable == "Migratory_score1" ~ "Migrant",
                    Variable == "Forest_depend_score1" ~ "Forest specialist",
                    Variable == "Hermit1" ~ "Hermit",
                    TRUE ~ as.character(Variable))) %>%
    mutate(Variable = factor(Variable, levels = c("Intercept", vars_pretty_names), ordered = TRUE))

  #return(table1)
  
  #Step 2: Calculate CI & backtransform coefficients
  table2 <- table1 %>%
    
    #Remove z and p-vals for bootstrap (not relevant)
    mutate(z.value = ifelse(CI_type == "Bootstrap", NA, z.value)) %>%
    mutate(p.value = ifelse(CI_type == "Bootstrap", NA, p.value)) %>% 

    #Backtransform coefficients
    mutate_at(vars(Estimate, LowerCI, UpperCI), funs(odds = exp(.))) %>% #Log-odds to odds
    mutate_at(vars(Estimate, LowerCI, UpperCI), funs(prob = plogis(.))) %>% #Log-odds to probabilities
      
    #Calculating fold-changes for logged variables.
    #For logits, increase in X is associated with change in the odd by a multiplicative factor of k^coefficient (Statistical Sleuth p. 645)
    mutate_at(vars(Estimate, LowerCI, UpperCI), funs(odds2x = 2^., odds0.5x = 0.5^.)) %>%  
    mutate_at(vars(contains('0.5x'), contains('2x')), funs(ifelse(grepl('Ln', Variable), ., NA))) %>% #Removing fold-changes for non-logged variables
    rename(LowerCI_odds0.5x = UpperCI_odds0.5x, UpperCI_odds0.5x = LowerCI_odds0.5x) #Switch the upper and lower CIs for 0.5-fold change. Because 0.5^X = (1/2)^X = 1/(2^X). So the lower (and smaller) CI will actually cause larger CI       
  
   #return(table2)
  
   #Step 3: Add column for significant level + round to 2 digits 
   table3 <- table2 %>%
     mutate(Signif_level = ifelse(CI_type == "Bootstrap", ifelse(LowerCI < 0 & UpperCI > 0, "P > 0.05", "0 < P < 0.05"), NA)) %>% #For bootstrap, p-values based on CI
     mutate_if(is.numeric, funs(round(., digits = 2))) %>%
     select(CI_type:Estimate_odds0.5x, LowerCI_odds0.5x, UpperCI_odds0.5x, everything()) %>%
     bind_rows(alpha) %>% #Add alpha back in
     arrange(CI_type, Response)

    return(table3)
}

results_summary_table <- make_table(summary_output) 

#Export
write.csv(results_summary_table, "results/extinction_risk_and_hummingbird_traits/Table_of_model_results.csv")

## ----make_results_figure--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function to make coefficient plots
make_coef_plot <- function(summary_data){
  
  #Remove alpha from dataframe
  summary_data <- summary_data %>%
    filter(Variable != "alpha") %>%
    filter(Variable != "Intercept") %>%
    mutate(Variable = factor(Variable, levels = c("Intercept", vars_pretty_names), ordered = TRUE))
  
  #Some of this code originally from Christopher Wolf
  plot <- ggplot(data = summary_data, aes(x = Variable, y = Estimate, shape = Response, alpha = Signif_level)) +
    #coord_flip(ylim = c(-3,3)) + #if want coefficient estimates on x axis 
    geom_point(position = position_dodge(.5), size = 3) +
    geom_errorbar(position = position_dodge(.5), aes(ymax = UpperCI, ymin = LowerCI), width = 0.25, size = 1) + # error bars show 95% confidence intervals
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) + # add a line at 0 (no effect)
    theme_bw(base_size = 20) +
    theme(axis.text.x = element_text(angle = 55, hjust = 1),
          legend.position = "top", legend.justification = "left",
          plot.margin = unit(c(1, 1, 0.5, 10), "lines"), #Default plot margins in theme_bw are 1, 1, 0.5, 0.5 (top, right, bottom, left); adjusting for spacing in ggarrange when combine plots
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Order for plot margin is top, right, bottom, left
    labs(x = "", y = "Standardized coefficient", colour = "Response", linetype = "Response", alpha = "P-value") +
    scale_alpha_manual(values = c(1, 0.5), guide = FALSE) + #No legend - decided to just write something in caption about significant values being bolded
    annotate("segment", x = -.6, xend = -.6, y = 0.5, yend = 3.5, col = "black", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
    annotate("segment", x = -.6, xend = -.6, y = -0.5, yend = -3.5, col = "black", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
    annotate("text",  x = -1.6, y = 2, size = 5, angle = 0, color = "black", label = c("Higher risk with \nlarger value \nof predictor")) +
    annotate("text",  x = -1.6, y = -2, size = 5, angle = 0, color = "black", label = c("Higher risk with \nsmaller value \nof predictor")) +
    coord_cartesian(ylim = c(-4, 4), xlim = c(1, 8), clip = "off")
  
  return(plot)
}


#Plot of coefficient estimates and bootstrapped CI
#Significance based on whether CI overlaps zero
(bootstrap_coef_plot <- make_coef_plot(filter(results_summary_table, CI_type  ==  "Bootstrap")))

#Export
ggsave("results/extinction_risk_and_hummingbird_traits/Standardized_coefs_bootstrapCI_plot.png", bootstrap_coef_plot, device = "png", width = 10, height = 8.5, dpi = 300, units = "in", bg = "white")

