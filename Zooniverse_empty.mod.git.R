# Load Libraries
library(lme4)
library(tidyverse) # data manipulation (includes ggplot and forcats)
library(lubridate) # dates 
library(dplyr)

# Read-in modeling data
model_data = data.table::fread(file = "E:/LPZ Coordinator/Zooniverse/Data/model_data.csv", data.table = FALSE)

#Let's run models only for those with a zooniverse account
model_data = model_data %>% 
  filter(!is.na(user_id))

# Filter down to only expertly tagged 'empty' photos
model_data = model_data %>% 
  filter(expertID == "Empty")

# Log and scale appropriate variables
model_data$blur_scale = as.numeric(scale(log(model_data$blur)))
model_data$pielou_scale = as.numeric(scale(model_data$pielou))
model_data$animal_weight_scale = as.numeric(scale(log(model_data$animal_weight)))
model_data$sp_prop_scale <- as.numeric(scale(log(model_data$sp_prop)))
model_data$expertID <- factor(model_data$expertID)
model_data$user_id <- factor(model_data$user_id)

# Run model
empty_mod = glmer(correct ~ pielou_scale + blur_scale + avid_user + (1 | user_id), family = "binomial", data = model_data)

# Save model 
saveRDS(empty_mod, file = "Data/empty_mod.RDS")

# Open model
empty_mod <- readRDS(
  "Data/empty_mod.RDS"
)

summary(empty_mod)

library('jtools')
summ(empty_mod, confint = TRUE, digits =3)

# Get unscaled coefs------------------------------------------------------------
# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?noredirect=1&lq=1

unsc.vars <- subset(model_data, select=c(blur, pielou, animal_weight, sp_prop))
sc.vars <-subset(model_data, select=c(blur_scale, pielou_scale, animal_weight_scale, sp_prop_scale))

# scaled
colMeans(sc.vars)
apply(sc.vars, 2, sd)

#unscaled
cm <- colMeans(unsc.vars)
csd <- apply(unsc.vars, 2, sd)


rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}

(cc <- rescale.coefs(fixef(empty_mod),mu=c(0,cm),sigma=c(1,csd)))



# Probabilities
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression#:~:text=The%20coefficient%20returned%20by%20a,1%2Bexp(logit))%20.

# Plotting----------------------------------------------------------------------

## Plot and predict impact of pielou on empty tagging success-------------------
# get the data
plot_data <- empty_mod@frame

#What are averages for predictor variables?
my_average <- merTools::averageObs(
  empty_mod
)

## Let predict how pielou impacts tagging success of empty photos---------------
# get the range of the realistic and scaled pielou range.
aw_range_real <- range(model_data$pielou)
aw_range <- range(model_data$pielou_scale)

# prediction sequence
#my_seq <- seq(-0.03, 0.05, 0.01) # for plotting unscaled variables
pseq_real <- seq(aw_range_real[1], aw_range_real[2], length.out = 400) #sequence of real variables
pseq <- seq(aw_range[1], aw_range[2], length.out = 400) #sequence of unscaled variables

#then modify plot function with pseq_real

# create the prediction data.frame, including predictions for non-avid users
for_pred <- data.frame(
  pielou_scale = pseq,
  blur_scale = 0,
  avid_user = 0 #Is zero OK for binary variables? Since we don't center/scale?
)


# this just gets the best fit line, we need to get 95% CI,
#  which we can only approximate given the large model.
to_plot <- predict(
  empty_mod, 
  newdata = for_pred,
  re.form = NA
)


# To do this, we use the merTools package, this means
#  we need to add in some random effect stuff for
#  the model predictions. We can locate the "average"
#  with the merTools::averageObs() function.

for_pred_mt <- data.frame(
  pielou_scale = pseq,
  blur_scale = my_average$blur_scale,
  avid_user = my_average$avid_user,
  user_id = my_average$user_id
)

# and approximate those intervals, needs lots of simulations
#  to get a smoother line for plotting.
to_plot_mt <- merTools::predictInterval(
  empty_mod,
  newdata = for_pred_mt,
  which = "fixed",
  level = 0.95,
  n.sims = 30000
)

# convert to a probability
to_plot_smooth <- apply(
  to_plot_mt,
  2,
  plogis
)

pielou_probs = as.data.frame(to_plot_smooth)
mean(pielou_probs$fit)

# Accuracy for evenness = 1 
pielou_probs[1,]
pielou_probs[1,1]

# Accuracy for evenness = 0
pielou_probs[400,]

pielou_probs[1,1]/pielou_probs[400,1]


# the lines are still a little jagged, just going to smooth them out a tiny
#  bit.


# and now we are ready to plot using bbplot, it helps to know the range
#  for the x and y axis (it makes better plots I've found).


# x-axis on real scale

tiff(file="empty_mod_pielou.jpg", 
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
    cex = 1,
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
    "User Accuracy",
    side = 2,
    line = 2.5,
    cex = 1.25
  )
  
  # add axis title
  # bbplot::axis_text(
  #   "Empty Model",
  #   side = 3,
  #   line = 2.75,
  #   cex = 1.25
  # )
  
  # add real interval to figure
  bbplot::ribbon(
    x = pseq_real,
    y = to_plot_smooth[,c("upr","lwr")],
    col = "dark green",
    alpha = 0.5
  )
  
  # add estimated line
  lines(
    x = pseq_real,
    y = to_plot_smooth[,"fit"],
    col = "dark green",
    lwd = 3
  )
  
}

dev.off()

