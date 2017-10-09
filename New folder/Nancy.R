#####################################################################################
#             DATA MANIPULATION AND PLOTS FOR 15N-NH4 - 15N-NO3                     #
# GROUP 3
rm(list = ls())
DE170913 <- read_delim("C:/Users/Daniele Ferraretto/Desktop/DE170913.csv", ";", escape_double = FALSE, trim_ws = TRUE)

DE170913$Code = as.factor(DE170913$Code)
DE170913$compartment = c("d_leaves", "b_cuttings", "a_roots", "c_stems")
DE170913$compartment = as.factor(DE170913$compartment)
#names(DE170913$`15N_excess_perc`) = DE170913$N15_excess_perc

DE170913$form <- rep(c("NH[4]-N","NO[3]-N"), each = 12)
DE170913$form = as.factor(DE170913$form)
DE170913$plant <- rep(c(1,2,3), each = 4)


library(ggplot2)
# DM
ggplot(DE170913, aes(x = compartment, y = DW)) + 
  geom_boxplot(aes(fill = compartment)) +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 11) +
  facet_grid(form ~ ., labeller = label_parsed) +
  scale_fill_manual(values = c('a_roots' = "red4", 'b_cuttings' = "orange", 
                               'c_stems' = "brown2", 'd_leaves' = "green"),
                    labels = c('a_roots' = "roots", 'b_cuttings' = "cuttings", 
                               'c_stems' = "stems", 'd_leaves' = "leaves"), 
                    name = "compartment") +
  ggtitle(expression("Dry mass (mg) in seedlings by compartment")) +
  labs(y = "Dry mass (mg)")


ggplot(DE170913, aes(x = compartment, y = `15N_excess_perc`)) + 
  geom_boxplot(aes(fill = compartment)) +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 11) +
  facet_grid(form ~ ., labeller = label_parsed) +
  scale_fill_manual(values = c('a_roots' = "red4", 'b_cuttings' = "orange", 
                               'c_stems' = "brown2", 'd_leaves' = "green"),
                    labels = c('a_roots' = "roots", 'b_cuttings' = "cuttings", 
                               'c_stems' = "stems", 'd_leaves' = "leaves"), 
                    name = "compartment") +
  ggtitle(expression({}^15*N~~excess~"(%) in seedlings by compartment")) +
  labs(y = expression({}^15*N~~excess~"(%)"))

# Roots vs leaf area

roots_leaves <- read_delim("C:/Users/Daniele Ferraretto/Desktop/roots_leaves.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(roots_leaves, aes(x = DW, y = Leaf_area)) + 
  geom_point() +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 11) +
  facet_grid(form ~ ., labeller = label_parsed) +
  ggtitle("Comparison between roots dried mass and leaf area per seedling") +
  labs(x = "roots dried biomass (g)", y = expression(paste("leaf area"," ", "(",mg^2,")")))
  
expression(paste(delta, {15},"N (\u2030)"))

  # Roots vs leaf area
  DM_leaf_surf <- read_delim("C:/Users/Daniele Ferraretto/Desktop/DM_leaf_surf.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  
  ggplot(DM_leaf_surf, aes(x = DW, y = Leaf_area)) + 
    geom_point() +
    theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    theme_bw(base_size = 11) +
    facet_grid(form ~ ., labeller = label_parsed) +
    ggtitle("Comparison between seedling total mass and leaf area") +
    labs(x = "roots dried biomass (g)", y = expression(paste("leaf area", " ", "(",mg^2,")")))
          
  