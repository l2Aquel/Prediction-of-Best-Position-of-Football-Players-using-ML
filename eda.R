setwd("~/Research")

data <- read.csv('players_22.csv')
dim(data)

head(data)

data$Age <- as.integer(data$Age)
data$Height_cm <- as.integer(data$Height_cm)
data$Weight_kg <- as.integer(data$Weight_kg)
data$Crossing <- as.integer(data$Crossing)
data$Finishing <- as.integer(data$Finishing)
data$Heading_Accuracy <- as.integer(data$Heading_Accuracy)
data$Short_Passing <- as.integer(data$Short_Passing)
data$Volleys <- as.integer(data$Volleys)
data$Dribbling <- as.integer(data$Dribbling)
data$Curve <- as.integer(data$Curve)
data$Free_Kick_Accuracy <- as.integer(data$Free_Kick_Accuracy)
data$Long_Passing <- as.integer(data$Long_Passing)
data$Ball_Control <- as.integer(data$Ball_Control)
data$Acceleration <- as.integer(data$Acceleration)
data$Sprint_Speed <- as.integer(data$Sprint_Speed)
data$Agility <- as.integer(data$Agility)
data$Reactions <- as.integer(data$Reactions)
data$Balance <- as.integer(data$Balance)
data$Shot_Power <- as.integer(data$Shot_Power)
data$Jumping <- as.integer(data$Jumping)
data$Stamina <- as.integer(data$Stamina)
data$Strength <- as.integer(data$Strength)
data$Long_Shots <- as.integer(data$Long_Shots)
data$Aggression <- as.integer(data$Aggression)
data$Interceptions <- as.integer(data$Interceptions)
data$Positioning <- as.integer(data$Positioning)
data$Vision <- as.integer(data$Vision)
data$Penalties <- as.integer(data$Penalties)
data$Composure <- as.integer(data$Composure)
data$Defensive_Awareness <- as.integer(data$Defensive_Awareness)
data$Standing_Tackle <- as.integer(data$Standing_Tackle)
data$Sliding_Tackle <- as.integer(data$Sliding_Tackle)
data$GK_Diving <- as.integer(data$GK_Diving)
data$GK_Handling <- as.integer(data$GK_Handling)
data$GK_Kicking <- as.integer(data$GK_Kicking)
data$GK_Positioning <- as.integer(data$GK_Positioning)
data$GK_Reflexes <- as.integer(data$GK_Reflexes)
data$Pace_Diving <- as.integer(data$Pace_Diving)
data$Shooting_Handling <- as.integer(data$Shooting_Handling)
data$Passing_Kicking <- as.integer(data$Passing_Kicking)
data$Dribbling_Reflexes <- as.integer(data$Dribbling_Reflexes)
data$Defending_Pace <- as.integer(data$Defending_Pace)
data$Physical_Positioning <- as.integer(data$Physical_Positioning)
data$Preferred_foot <- as.factor(data$Preferred_foot)
data$Best_Position <- factor(data$Best_Position, levels = c("CB","RB","LB","RWB","LWB","CDM","CM","RM","LM","CAM","CF","RW","LW","ST","GK"))
data$Weak_Foot <- as.factor(data$Weak_Foot)
data$Attacking_Work_Rate <- factor(data$Attacking_Work_Rate,levels = c("Low","Medium","High"))
data$Defensive_Work_Rate <- factor(data$Defensive_Work_Rate,levels = c("Low","Medium","High"))
data$Body_Type <- as.factor(data$Body_Type)


new_data <- subset(data,select = -c(Name,Age,GK_Diving,GK_Handling,GK_Kicking,GK_Positioning,GK_Reflexes,GK_Speed,Pace_Diving,Shooting_Handling,Passing_Kicking,Dribbling_Reflexes,Defending_Pace,Physical_Positioning))
new_data <- new_data[!(new_data$Best_Position == "GK"),]
new_data$Best_Position <- droplevels(new_data$Best_Position)

str(new_data)
dim(new_data)

library(ggplot2)
library(dplyr)

position_distribution <-  new_data %>%
  group_by(Best_Position) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

position_distribution

# Create the value distribution
ggplot(position_distribution, aes(x = Best_Position, y = percent, fill = Best_Position)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5, size = 3.5) +  
  labs(title = "Percentage of Each Best Position",
       x = "Best Position",
       y = "Percent")

# Create the boxplot
ggplot(new_data, aes(x = Best_Position, y = Height_cm)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Height Distribution by Best Position",
       x = "Best Position",
       y = "Height (cm)") +
  theme_minimal()

ggplot(new_data, aes(x = Best_Position, y = Short_Passing)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Short Passing Distribution by Best Position",
       x = "Best Position",
       y = "Short Passing") +
  theme_minimal()

ggplot(new_data, aes(x = Best_Position, y = Finishing)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Finishing Distribution by Best Position",
       x = "Best Position",
       y = "Finishing") +
  theme_minimal()

new_data$position_category <- dplyr::case_when(
  new_data$Best_Position %in% c("ST", "CF", "RW", "LW") ~ "Attack",
  new_data$Best_Position %in% c("CDM", "CM", "RM", "LM", "CAM") ~ "Midfield",
  new_data$Best_Position %in% c("CB", "LB", "RB", "RWB", "LWB") ~ "Defense",
  TRUE ~ "Other"
)

head(new_data)

#Scatter plot
ggplot(new_data, aes(x = Ball_Control, y = Dribbling,shape = factor(position_category),color = factor(position_category))) +
  geom_point() +
  labs(title = "Ball Control vs Dribbling",
       x = "Ball Control",y = "Dribbling")

ggplot(new_data, aes(x = Sliding_Tackle, y = Standing_Tackle,shape = factor(position_category),color = factor(position_category))) +
  geom_point() +
  labs(title = "Sliding_Tackle vs Standing_Tackle",
       x = "Sliding_Tackle",y = "Standing_Tackle")

ggplot(new_data, aes(x = Finishing, y = Shot_Power,shape = factor(position_category),color = factor(position_category))) +
  geom_point() +
  labs(title = "Finishing vs Shot Power",
       x = "Finishing",y = "Shot Power")
