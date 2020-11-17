#----- Required Packages-----#
library(readr)
library(dplyr)
library(ggplot2)
library(mgcv)


#----- Read in Data -----#
Pitch_Dataset <- read_csv("pitch_data.csv")
View(Pitch_Dataset)


#-------------------------------------------------#
#------lefty, right, & combined strike zones------#
#-------------------------------------------------#
combined_strike_zone <- Pitch_Dataset %>%
  filter(is_swing == 0) %>%
  filter(is_strike == 1) 

lhb_strike_zone <- Pitch_Dataset %>%
  filter(is_lhb == 1) %>%
  filter(is_swing == 0) %>%
  filter(is_strike == 1)

rhb_strike_zone <- Pitch_Dataset %>%
  filter(is_lhb != 1) %>%
  filter(is_swing == 0) %>%
  filter(is_strike == 1)


#-------------------------------------------------#
#-------exploratory scatter plot and t test-------#
#-------------------------------------------------#

# test difference in means
t.test(rhb_strike_zone$px, lhb_strike_zone$px)
#null hypothesis is that there is no difference in means
#p-value < 2.2e-16; we can reject the null hypothesis
#there is a statistically significant difference between our means.

combined_strike_zone$is_lhb <- as.factor(combined_strike_zone$is_lhb)

initial_plot <- ggplot(combined_strike_zone, aes(x = px, y = pz, color = is_lhb)) +
  geom_point() +
  ggtitle("Called Strikes") +
  xlab("x position") + 
  ylab("y position") +
  labs( colour = "Batter Handedness") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Right Handed Batter",
                                "Left Handed Batter"))

ggsave(filename = "initial_strikezone.png", plot = initial_plot)


#----- Build Reference Strike Zone -----#
left_edge <- -0.85     # strike zone measures 17inches horizontally
right_edge <- 0.85
top_edge <- 3.5        # "ballpark" top & bottom of zone 
bottom_edge <- 1.5
reference_zone <- data.frame(x1 = left_edge, x2 = right_edge,
                             y1 = bottom_edge, y2 = top_edge)


#-------------------------------------------------#
#----General Additive Model for Fitting K Zone----#
#-------------------------------------------------#

#---- RHB Called Strikes----#
called_strike_rhb <- Pitch_Dataset %>%
  filter(is_lhb != 1) %>%
  filter(is_swing == 0)


#------ RHB GAM model------#
fit_rhb <- gam(is_strike ~ s(px, pz), family=binomial, data=called_strike_rhb)

#---- Use for plotting ----#
x_rhb <- seq(-1.5, 1.5, length.out=50)
y_rhb <- seq(1, 4, length.out=50)
#produce test data, fill x,y strike zone grid with sample 2,500 sample values
predict_rhb <- data.frame(px = c(outer(x_rhb, y_rhb * 0 + 1)),
                          pz = c(outer(x_rhb * 0 + 1, y_rhb)))

#-------- Predict ---------#
#use the GAM to predict whether the test data observations are balls or strikes
log_predict_rhb <- predict(fit_rhb, predict_rhb) 
predict_rhb$likelihood <- exp(log_predict_rhb) / (1 + exp(log_predict_rhb))

#---------- Plot ----------#
rhb_zone <-
  ggplot() +
  ggtitle("RHB Strike Zone") +
  geom_tile(data=predict_rhb, 
            aes(x=px, y=pz, fill= likelihood)) +
  geom_rect(data=reference_zone, 
            aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha=0.0, color = "black") +
  scale_fill_distiller(palette = "Spectral") +
  geom_path(lwd=1.5, col="black") +
  labs( fill = "Strike Probability", x = "X position", y = "Y position") +
  coord_fixed()

ggsave(filename = "rhb_zone2.png", plot = rhb_zone)



#---- LHB Called Strikes----#
called_strike_lhb <- Pitch_Dataset %>%
  filter(is_lhb == 1) %>%
  filter(is_swing == 0)

#------ LHB GAM model------#
fit_lhb <- gam(is_strike ~ s(px, pz), family=binomial, data=called_strike_lhb)

#---- Use for plotting ----#
x_lhb <- seq(-1.5, 1.5, length.out=50)
y_lhb <- seq(1, 4, length.out=50)
predict_lhb <- data.frame(px = c(outer(x_lhb, y_lhb * 0 + 1)),
                          pz = c(outer(x_lhb * 0 + 1, y_lhb)))

#-------- Predict ---------#
log_predict_lhb <- predict(fit_lhb, predict_lhb)
predict_lhb$likelihood <- exp(log_predict_lhb) / (1 + exp(log_predict_lhb))

#---------- Plot ----------#
lhb_zone <-
  ggplot() +
  ggtitle("LHB Strike Zone") +
  geom_tile(data=predict_lhb, 
            aes(x=px, y=pz, fill= likelihood)) +
  geom_rect(data=reference_zone,
            aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha=0.0, color = "black") +
  labs( colour = "Strike Probability") +
  scale_fill_distiller(palette = "Spectral") +
  geom_path(lwd=1.5, col="black") +
  labs( fill = "Strike Probability", x = "X position", y = "Y position") +
  coord_fixed() 

ggsave(filename = "lhb_zone2.png", plot = lhb_zone)






# catcher framing analysis save
#------------------------------------------------#
#------------Catcher Framing Analysis------------#
#------------------------------------------------#

# First, remove pitches where there was a swing
no_swing_data <- Pitch_Dataset %>%
  filter(is_swing != 1)

#-----Second, split into different data sets-----#
split_data <- function(fulldata, index){
  return_df<- no_swing_data %>%
    filter(catcherid == index)
  return(return_df)
}

catcher_1 <- split_data(no_swing_data, 1)
catcher_2 <- split_data(no_swing_data, 2)
catcher_3 <- split_data(no_swing_data, 3)
catcher_4 <- split_data(no_swing_data, 4)

#-----Create data with only called strikes-----#
called_strikes <- no_swing_data %>%
  filter(is_strike == 1)

#Clean up a rogue observation
called_strikes <- called_strikes %>%
  filter(px != 1.944)

#-----Create Approximate "Edges"-----#
summary(called_strikes$px)
boxplot(called_strikes$px)

summary(called_strikes$pz)
boxplot(called_strikes$pz)
(1.31300 + .4365)/2
# 0.87475
(1.375 + .469)/2
# 0.922

# Lets round both values.
# Home plate in terms of px = [-0.9, 0.9] 

# Lets define the edges as -1.0 -> -.8 and .8 -> 1.0, find all pitches in this zone. 
# which catcher had the highest percentage of these pitches being called strikes?

#-----Function that returns % of successful framing opportunities-----#
frame_rate <- function(catcher_data){
  catcher_edges_data <- catcher_data %>% 
    filter( (px >= -1.0 & px <= -0.8) | (px >= 0.8 & px <= -1.0))
  
  # If I want to add bottom and top of zone, modification :
  # filter( (px >= -1.0 & px <= -0.8) | (px >= 0.8 & px <= -1.0)) |
  #         (pz >= 3.4 & pz <= 3.7) | (pz >= 0.9 & pz <= 1.2) )
  
  frame_success_rate = sum(catcher_edges_data$is_strike)/nrow(catcher_edges_data)
  return(frame_success_rate)
}

catcher_1_framing <- frame_rate(catcher_1)
catcher_2_framing <- frame_rate(catcher_2)
catcher_3_framing <- frame_rate(catcher_3)
catcher_4_framing <- frame_rate(catcher_4)



