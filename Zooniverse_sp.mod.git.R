# Load Libraries
library(lme4)
library(tidyverse) # data manipulation (includes ggplot and forcats)
library(lubridate) # dates 
library(dplyr)


# Read-in modeling data
model_data = data.table::fread(file = "model_data.csv", data.table = FALSE)

# Read in location information
response <- readRDS(
  "response.RDS"
)

#Let's run models only for those with a zooniverse account
model_data = model_data %>% 
  filter(!is.na(user_id))

# get count of number of classifications
tmp <- split(
  model_data,
  factor(model_data$user_id)
)
pb <- txtProgressBar(max = length(tmp))
for(i in 1:length(tmp)){
  setTxtProgressBar(pb, i)
  one_df <- tmp[[i]]
  one_df <- one_df[order(one_df$classification_id),]
  one_df$class_num <- 1:nrow(one_df)
  tmp[[i]] <- one_df
}
model_data <- dplyr::bind_rows(tmp)

# Drop columns of species which don't have weights like 'Bird'
model_data = model_data %>% 
  drop_na(animal_weight)

# join data on photoName
response <- response %>% 
  select(photoName, locationAbbr)

model_data <- left_join(model_data, response, by = "photoName")

# Log and scale appropriate variables
model_data$blur_scale = as.numeric(scale(log(model_data$blur)))
model_data$pielou_scale = as.numeric(scale(model_data$pielou))
model_data$animal_weight_scale = as.numeric(scale(log(model_data$animal_weight)))
model_data$sp_prop_scale <- as.numeric(scale(log(model_data$sp_prop)))
model_data$expertID <- factor(model_data$expertID)
model_data$user_id <- factor(model_data$user_id)
model_data$locationAbbr <- factor(model_data$locationAbbr)
model_data$class_num <- as.numeric(scale(model_data$class_num))

# Run the full model
sp_mod_wt <- glmer(correct ~ pielou_scale + blur_scale + class_num + animal_weight_scale + 
                     (1 | expertID) + (1 | user_id) +(1| locationAbbr), family = "binomial", data = model_data)

summary(sp_mod_wt)

# null model
sp_mod_wt_null <- glm(correct ~ 1, data = model_data, family = "binomial")
summary(sp_mod_wt_null)


# Get confidence intervals
library('jtools')
summ(sp_mod_wt, confint = TRUE, digits =3)

# Calculate MacFadden's pseudo-R^2
mcf <- 1-(sp_mod_wt@devcomp$cmp["dev"]/sp_mod_wt_null$deviance)

# Plotting----------------------------------------------------------------------

## Plot and predict impact of animal weight on tagging success------------------

##What are averages for predictor variables?
my_average <- merTools::averageObs(
  sp_mod_wt
)

## get the exact model data
plot_data <- sp_mod_wt@frame

## get the range of the animal weight.
aw_range_real <- range(model_data$animal_weight)
aw_range <- range(model_data$animal_weight_scale)

# prediction sequence
#my_seq <- seq(-0.03, 0.05, 0.01) # for plotting unscaled variables
pseq_real <- seq(aw_range_real[1], aw_range_real[2], length.out = 400) #sequence of real variables
pseq <- seq(aw_range[1], aw_range[2], length.out = 400) #sequence of unscaled variables

# Create the prediction data.frame, including predictions for non-avid users
# The mean of these variables should be close to '0' since we centered and scalesd
for_pred <- data.frame(
  pielou_scale = 0,
  blur_scale = 0,
  animal_weight_scale = pseq, #(pseq - mean(model_data$animal_weight_log)) / sd(model_data$animal_weight_log),
  class_num = 0
  #  sp_prop_scale = 0
)

# this just gets the best fit line, we need to get 95% CI,
# which we can only approximate given the large model.
to_plot <- predict(
  sp_mod_wt, 
  newdata = for_pred,
  re.form = NA
)


# To do this, we use the merTools package, this means
#  we need to add in some random effect stuff for
#  the model predictions. We can locate the "average"
#  with the merTools::averageObs() function.
for_pred_mt <- data.frame(
  pielou_scale = 0,
  blur_scale = 0,
  animal_weight_scale = pseq, #(pseq - mean(model_data$animal_weight_log)) / sd(model_data$animal_weight_log),
  class_num = my_average$class_num,
  expertID = my_average$expertID,
  user_id = my_average$user_id,
  locationAbbr = my_average$locationAbbr
  #sp_prop_scale = my_average$sp_prop_scale
)

# and approximate those intervals, needs lots of simulations
#  to get a smoother line for plotting.
to_plot_mt <- merTools::predictInterval(
  sp_mod_wt,
  newdata = for_pred_mt,
  which = "fixed",
  level = 0.95,
  n.sims = 30000
)

# convert to a probability
to_plot_mt <- apply(
  to_plot_mt,
  2,
  plogis
)

# the lines are still a little jagged, just going to smooth them out a tiny
#  bit.
to_plot_smooth <- apply(
  to_plot_mt, 
  2,
  function(each_col) lowess(x = pseq, y = each_col)$y
)

#saveRDS(to_plot_smooth, file = "Data/to_plot_smooth.RDS")

# get prop success for each species
weight_prop <- model_data %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(expertID) %>% 
  dplyr::summarise(
    pc = mean(correct),
    aws = unique(animal_weight)) %>% 
  data.frame()

# look at model coef from species specific $coef func and look for large magnitude (take abs value of coef)
# sort and look at top 5
# may come out as list (might need to add sp. name (look at levels))
sp.coef = as.data.frame(coef(sp_mod_wt)[2])

sp.intercept = as.data.frame(coef(sp_mod_wt)[2]) %>% 
  select(expertID..Intercept., expertID.animal_weight_scale) 

sp.intercept = sp.intercept %>% 
  rename(intercept = expertID..Intercept.) %>% 
  mutate(abs.intercept = abs(intercept)) 

sp.intercept = sp.intercept %>% 
  arrange(abs.intercept)


model_fix = coef(summary(sp_mod_wt))

# and now we are ready to plot using bbplot, it helps to know the range
#  for the x and y axis (it makes better plots I've found).

tiff(file="sp_mod_wt_weight_updated.tiff", 
     width = 6, height = 6, units = "in", res = 600, 
     compression = "lzw")

{
  bbplot::blank(
    xlim = range(pseq_real),
    ylim = c(0,1),
    bty = "l"
  )
  
  # add generic axes to x axis
  bbplot::axis_blank(1)
  # and to y axis
  bbplot::axis_blank(2)
  
  # add numbers to x axis
  bbplot::axis_text(
    side = 1,
    line = 0.8,
    cex = 1.
  )
  
  # add numbers to y axis
  bbplot::axis_text(
    side = 2,
    line = 0.8,
    cex = 1.,
    las = 1
  )
  
  # add x axis title
  bbplot::axis_text(
    "Animal weight (lbs)",
    side  = 1,
    line = 2.5,
    cex = 1.25
  )
  
  # add y axis title
  bbplot::axis_text(
    "User accuracy of species images",
    side = 2,
    line = 2.5,
    cex = 1.25
  )
  
  # add predictive interval to figure
  bbplot::ribbon(
    x = pseq_real,
    y = to_plot_smooth[,c("upr","lwr")],
    col = c("#9CD1E6"),
    alpha = 0.5
  )
  
  # add estimated line
  lines(
    x = pseq_real,
    y = to_plot_smooth[,"fit"],
    col = c("#269BAD"),
    lwd = 3
  )
  
  points(
    x = weight_prop$aws,
    y = weight_prop$pc,
    pch = 19,
    cex = 1.
  )
  
}

dev.off()

## Plot and predict impact of pielou on tagging success-------------------------

# get the range of pielou scales.
aw_range_real <- range(model_data$pielou)
aw_range <- range(model_data$pielou_scale)

# prediction sequence
#my_seq <- seq(-0.03, 0.05, 0.01) # for plotting unscaled variables
pseq_real <- seq(aw_range_real[1], aw_range_real[2], length.out = 400) #sequence of real variables
pseq <- seq(aw_range[1], aw_range[2], length.out = 400) #sequence of unscaled variables

saveRDS(pseq_real, file = "Data/pseq_real.RDS")
saveRDS(pseq, file = "Data/pseq.RDS")

# create the prediction data.frame, including predictions for non-avid users
for_pred <- data.frame(
  pielou_scale = pseq,
  blur_scale = 0,
  animal_weight_scale = 0,
  class_num  = 0
  #sp_prop_scale = 0
)

# this just gets the best fit line, we need to get 95% CI,
#  which we can only approximate given the large model.
to_plot <- predict(
  sp_mod_wt, 
  newdata = for_pred,
  re.form = NA
)


# To do this, we use the merTools package, this means
#  we need to add in some random effect stuff for
#  the model predictions. We can locate the "average"
#  with the merTools::averageObs() function.

my_average <- merTools::averageObs(
  sp_mod_wt
)

for_pred_mt <- data.frame(
  pielou_scale = pseq,
  blur_scale = 0, #Why is this zero and not another average?
  animal_weight_scale = my_average$animal_weight_scale,
  class_num  = my_average$class_num ,
  expertID = my_average$expertID,
  user_id = my_average$user,
  locationAbbr = my_average$locationAbbr 
  #sp_prop_scale = my_average$sp_prop_scale
)


test = merge(to_plot_smooth[,"fit"], pseq_real) #pseq_real = pielou 


# and approximate those intervals, needs lots of simulations
#  to get a smoother line for plotting.
to_plot_mt <- merTools::predictInterval(
  sp_mod_wt,
  newdata = for_pred_mt,
  which = "fixed",
  level = 0.95,
  n.sims = 30000
)


# convert to a probability
to_plot_mt <- apply(
  to_plot_mt,
  2,
  plogis
)

# the lines are still a little jagged, just going to smooth them out a tiny
#  bit.
to_plot_smooth <- apply(
  to_plot_mt, 
  2,
  function(each_col) lowess(x = pseq, y = each_col)$y
)

saveRDS(to_plot_smooth, file = "Data/to_plot_smooth.RDS")

# and now we are ready to plot using bbplot, it helps to know the range
#  for the x and y axis (it makes better plots I've found).


tiff(file="sp_mod_wt_pielou_updated.tiff", 
     width = 6, height = 6, units = "in", res = 600, 
     compression = "lzw")
{
  bbplot::blank(
    xlim = range(pseq_real),
    ylim = c(0,1),
    bty = "l"
  )
  
  # add generic axes to x axis
  bbplot::axis_blank(1)
  # and to y axis
  bbplot::axis_blank(2)
  
  # add numbers to x axis
  bbplot::axis_text(
    side = 1,
    line = 0.8,
    cex = 1.
  )
  
  # add numbers to y axis
  bbplot::axis_text(
    side = 2,
    line = 0.8,
    cex = 1.,
    las = 1
  )
  
  # add x axis title
  bbplot::axis_text(
    "Evenness",
    side  = 1,
    line = 2.5,
    cex = 1.25
  )
  
  # add y axis title
  bbplot::axis_text(
    "User accuracy of species images",
    side = 2,
    line = 2.5,
    cex = 1.25
  )
  
  # add predictive interval to figure
  bbplot::ribbon(
    x = pseq_real,
    y = to_plot_smooth[,c("upr","lwr")],
    col = c("#8FA35B"),
    alpha = 0.5
  )
  
  # add estimated line
  lines(
    x = pseq_real,
    y = to_plot_smooth[,"fit"],
    col = c("#5A7038"),
    lwd = 3
  )
  
}

dev.off()