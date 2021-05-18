## ----setup--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(UpSetR)


## ----import_web_of_science_search_results-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
papers = read.csv("C:\\Users\\leimberk\\Box\\Biol_Reviews_Analyses\\Papers_over_time\\export\\Web_of_science_search_results_20200904.csv")


## ----summarize_papers_per_year------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Summary by category, year
papers_sum <- papers %>%
  mutate(topic = factor(topic, levels = c("hummingbirds", "ecology", "evolution", "conservation", "networks"), labels = c("Plants \nand hummingbirds", "Ecology \nand behaviour", "Evolution", "Conservation", "Networks"))) %>%
  group_by(topic, year) %>%
  summarise(num_papers = n()) %>%
  ungroup() %>%
  complete(year = 1966:2019, nesting(topic), fill = list(num_papers = 0)) %>% #Fill in missing years with explicit zeroes
  arrange(topic, year)

#Summary by category, across years
papers_sum_overall <- papers_sum %>%
  group_by(topic) %>%
  summarise(num_papers = sum(num_papers)) %>%
  ungroup()


## ----explore_overlap_between_topics-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#How much overlap between topics/categories? LOTS
length(unique(papers$title)) #total articles (N=1098)

overlap_sum <- papers %>%
  group_by(title) %>%
  summarise(num_topics = n())

#Calculate overlap for pairwise comparisons of topics
#Function to calculate overlap
calculate_overlap <- function(data, topic1_name, topic2_name){

  dataset1 <- data %>% filter(topic == topic1_name)
  dataset2 <- data %>% filter(topic == topic2_name)
  overlap <- dataset1 %>% filter(title %in% dataset2$title) #topic1 %in% topic2
  num_overlap_articles <- dim(overlap)[1]
  
  return(num_overlap_articles)
  
}

#Function to calculate total number of papers in each category
calculate_total <- function(data, topic1_name){
  
  total <- data %>% filter(topic == topic1_name)
  num_total_articles <- dim(total)[1]

}

#Create different combinations of topics
topic1 <- unique(papers$topic)
topic2 <- unique(papers$topic)

overlap_base_table <- crossing(topic1, topic2) %>%
  filter(topic1 != topic2) %>% #Not interested in overlap between same topic
  filter(topic1 != "hummingbirds" & topic2 != "hummingbirds") #Interested in overlap between subcategories, not between all papers 

#Calculate how much overlap between each category
overlap_results <- overlap_base_table %>%
  mutate(num_overlap_articles = map2(topic1, topic2, ~calculate_overlap(data = papers, topic1_name = .x, topic2_name = .y))) %>%
  mutate(num_overlap_articles = unlist(num_overlap_articles)) %>%
  mutate(num_total_articles = map(topic1, ~calculate_total(data = papers, topic1_name = .))) %>%
  mutate(num_total_articles = unlist(num_total_articles)) %>%
  arrange(topic1, desc(num_total_articles))

#Visualize overlap between subcategories with UpSetR package (alternative to Venn diagram)
data_for_upsetr <- papers %>%
  mutate(presence = 1) %>%
  select(topic, presence, title) %>%
  pivot_wider(names_from = topic, values_from = presence, values_fill = list(presence = 0)) %>%
  data.frame()

upset_plot<- upset(data_for_upsetr, sets = c("ecology", "evolution", "conservation", "networks"), mb.ratio = c(0.55, 0.45), keep.order = "true", order.by = "freq", text.scale = 2)
upset_plot
#mb.ratio is ratio between matrix plot and main bar

# setwd("C:/Users/kleim/Box/Biol_Reviews_Analyses/Papers_over_time/export")
# pdf(file="Intersection_plot_20201019.pdf", onefile=FALSE, width = 11, height = 8.5) # or other device
# upset_plot
# dev.off()
# 
# png(file="Intersection_plot_20201019.png", width = 11, height = 8.5, units = "in", res = 300) # or other device
# upset_plot
# dev.off()


## ----plot_papers_over_time----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Lines labeled at end
plot_line_with_labels<-ggplot() +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_smooth(data = papers_sum, aes(x = year, y = num_papers, colour = topic), method = "loess", span = 0.15, method.args = list(degree=1), se = FALSE, size = 1.2) +
  geom_text(data = papers_sum %>% filter(year == last(year)), aes(x = year, y = num_papers * 1.08 + 0.4, label = topic), hjust = 0, nudge_x = 0.5, size = c(6, 4.5, 4.5, 4.5, 4.5)) +
  scale_x_continuous(breaks = seq(1965, 2020, 5), expand = expansion(mult = c(0.05, .3))) + #expand pads your data (multiplicatively and additively) to calculate the scale limits. Makes room for text labels.
  labs(x = "\nYear", y = "Number of publications\n", color = "Topic", linetype = "Topic") +
  scale_color_grey(start = 0, end = 0.75) #Ending at 0.75 prevents super light values for last category (networks)
 
plot_line_with_labels

#Export
setwd("C:/Users/leimberk/Box/Biol_Reviews_Analyses/Papers_over_time")
#ggsave("export/Papers_over_time_lines_labeled_gray_20210518.png", plot_line_with_labels, width = 10, height = 7.5, units = c("in"), dpi = 300)

