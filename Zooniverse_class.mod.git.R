# Load Libraries
library(lme4)
library(tidyverse) # data manipulation (includes ggplot and forcats)
library(lubridate) # dates 
library(dplyr)

# Read-in modeling data
model_data = data.table::fread(file = "Data/model_data.csv", data.table = FALSE)

# Now lets model how effective the current retirement rules are-----------------
# We will first summarise by subject_id

retire_data = model_data %>% 
  group_by(subject_id) %>% 
  summarise(expertID = unique(expertID),
            our_rt = unique(our_rt),
            final_tag = unique(final_tag))

# Then update final correct tags for species groups
retire_data = retire_data %>% 
  mutate(correct = ifelse(expertID == final_tag, 1, 0)) %>% 
  mutate(correct = ifelse(expertID == "Fox squirrel" 
                          & final_tag == "Squirrel (cannot ID)", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "Eastern gray squirrel" 
                          & final_tag == "Squirrel (cannot ID)", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "American mink" 
                          & final_tag == "American mink", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "American mink" 
                          & final_tag == "weasel", 1, as.numeric(correct))) 

# Make retirement classes a factor
retire_data$our_rt <- factor(retire_data$our_rt, levels = c("classification_count",
                                                            "3_nothing_here", "5_nothing_here", "7_species", "Human"))


#Run the model
mod_retire = glm(correct ~ our_rt, family = "binomial"(link = "logit"), data = retire_data)

# Save the model
saveRDS(mod_retire, file = "Data/mod_retire.RDS")

# Open model
mod_retire <- readRDS(
  "Data/mod_retire.RDS"
)


# Now let's use this data to predict the probabilities for each retirement class---
# correct = seq(0,1,by = 1)
our_rt = c("classification_count","3_nothing_here", "5_nothing_here", "7_species", "Human")
new_dat = expand.grid(our_rt = our_rt)
new_dat <- cbind(new_dat, predict(mod_retire, new_dat, type = "response", 
                                  se.fit=TRUE, interval="confidence")) 
new_dat = new_dat %>% select(-residual.scale)

# create new columns for confidence intervals
new_dat = new_dat %>% mutate(fit.lwr = fit - se.fit)
new_dat = new_dat %>% mutate(fit.upr = fit + se.fit)


# pull probabilities for each species, group by retirement type, group by species, 
# then summarize proportion of correct for each species

sp_data = model_data %>% 
  group_by(subject_id, expertID, final_tag, our_rt) %>% 
  summarise() %>% 
  mutate(correct_tag = ifelse(expertID == final_tag, 1, 0))

sp_class_prop = sp_data %>% 
  group_by(our_rt, final_tag) %>% 
  summarise(fit = sum(correct_tag)/length(correct_tag)) %>% 
  drop_na()

# Let's make a plot theme: 
theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.15),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}

# Plotting----------------------------------------------------------------------

sp_class_prop = sp_class_prop %>% 
  mutate(type = case_when(
    fit >= .9 ~ "High accuracy",
    fit < .9 ~ "Low accuracy",
    TRUE ~ as.character("other")))

# Add rare species:
# Species with <200 detection
# Gray fox
# North american beaver
# Woodchuck
# Flying squirrel (cannot ID)
# American mink
# Red fox

sp_class_prop = sp_class_prop %>% 
  mutate(type = case_when(
    final_tag == "Gray fox" ~ "Rare species",
    final_tag == "North american beaver" ~ "Rare species",
    final_tag == "Woodchuck" ~ "Rare species",
    final_tag == "Flying squirrel (cannot ID)" ~ "Rare species",
    final_tag == "American mink" ~ "Rare species",
    final_tag == "Red fox" ~ "Rare species",
    TRUE ~ as.character(type)
  ))


sp_plot = sp_class_prop %>% 
  filter(our_rt == "7_species" | our_rt == "classification_count")
  
ggplot(data = new_dat[c(1,4),], aes(x=our_rt, y=fit)) +
  theme_niwot()+
  xlab("\nRetirement rule") +
  ylab("User accuracy\n")+
  # scale_x_discrete(guide = guide_axis(angle = 50))+
  geom_point(data = sp_plot, aes(x=our_rt, y = fit, label=final_tag, 
                                 color = type), size = 4, shape = 19, position = position_jitter(width = .3, height = 0))+
  scale_color_manual(values = c("High accuracy" = "#269BAD",
                                "Low accuracy" = "#8FA35B",
                                "Rare species" = "#C95B5B"))+
  scale_color_manual(breaks=c("High accuracy", "Low accuracy", "Rare species", "Among-species mean"),
                     values=c("High accuracy" = "#269BAD", "Low accuracy" = "#8FA35B",
                              "Rare species" = "#C95B5B", "Among-species mean" = "#094C59"))+
  theme(legend.position = "bottom")+
  geom_errorbar(aes(ymin=fit.lwr, ymax=fit.upr), size = 1, width=.2, color= "#094C59", shape = 8) +
  geom_point(color= "#094C59", size = 6, shape = 18)
  # guides(color = guide_legend(
  #   override.aes=list(shape = c(19,19,19,18))))


