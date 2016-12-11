
# Read route data cityTocity and display titles
data <- read.csv("Quiz_data.csv")

# Lets first try raw correlation b/w continous variables
cor(data$Fare, data$DISTANCE)
