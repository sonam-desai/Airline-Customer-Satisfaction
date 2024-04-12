install.packages("skimr")
install.packages("stringr")
install.packages("vctrs")
install.packages("plotly")
install.packages("psych")
update.packages(ask = FALSE)
library("readr")
library("skimr")
library("vctrs")
require(skimr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(corrplot)
library(psych)
library(GPArotation)
library(class)
library(randomForest)
library(pROC)
library(FSelector)
library(e1071)
library(rpart)
library(class)
library(corrplot)


data <- read.csv("C:/Users/sonam/OneDrive - San Diego State University (SDSU.EDU)/Sem 3/Big Data Infrastructure/train.csv",stringsAsFactors = F)
head(data)
dim(data)
str(data)
data
## Observations

any(is.na(data))
sum(is.na(data))

data <- na.omit(data)
colnames(data) <- gsub("\\.", "_", colnames(data))
colnames(data) <- tolower(colnames(data)) 
dim(data)
summary(data)
skim(data)

table(data$gender)
table(data$customer_type)
table(data$type_of_travel)
table(data$class)
table(data$satisfaction)
summary(data)

data$arrival_delay_in_minutes <- as.numeric(data$arrival_delay_in_minutes)
data$arrival_delay_in_minutes <- as.numeric(data$departure_delay_in_minutes)
hist(log(data$arrival_delay_in_minutes), , col='pink', main="Histogram of Log Arrival Delay") #Sonam
hist(log(data$departure_delay_in_minutes), col = 'lightblue', main="Histogram of Log Departure Delay") #Sonam



# Handling outliers in departure and arrival delay
data$departure_delay_in_minutes[data$departure_delay_in_minutes < 0] <- 0
data$arrival_delay_in_minutes[data$arrival_delay_in_minutes < 0] <- 0


# Remove unnecessary columns
data <- data[, !(names(data) %in% c("arrival_delay_in_minutes"))]
data <- data[, !(names(data) %in% c("departure_delay_in_minutes"))]
data <- data[, !(names(data) %in% c("id"))]
data <- data[, !(names(data) %in% c("x"))]
# Check for missing values after cleaning
any(is.na(data))
dim(data)
summary(data)
library(psych)
describe(data)
#### Data cleaning and Proper dataset done




#Qualitative variable
library(tidyr)
library(ggplot2)


# Assuming 'ds' is your data frame
ds_num <- data[, sapply(data, is.numeric)]
ds_num_p <- gather(data.frame(ds_num), key = "variable", value = "values")

# Plot boxplots
ggplot(ds_num_p) +
  geom_boxplot(aes(x = variable, y = values), fill = "lightblue") +
  facet_wrap(~variable, ncol = 6, scales = "free") +
  theme(strip.text.x = element_blank(), text = element_text(size = 14)) +
  labs(title = "Boxplots for Numerical Variables")



# Satisfaction variable
ggplot(data, aes(x = recode(satisfaction, '0' = "neutral or dissatisfied", '1' = "satisfied"), fill = satisfaction)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count))) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  ylim(0, 60000) +
  labs(y = "Count", title = "Satisfaction variable in Dataset") +
  theme_bw()

# Gender variable
ggplot(data, aes(x = recode(gender, '0' = "Female", '1' = "Male"), fill = gender)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count))) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  ylim(0, 60000) +
  labs(y = "Count", title = "Gender variable in Dataset") +
  theme_bw()



library(plotly)
# Calculate the counts of each customer type
customer_type_counts <- table(data$customer_type)
values <- customer_type_counts
labels <- names(customer_type_counts)
colors <- c('lightblue', 'pink') # Custom blue colors
# Create the pie chart
fig <- plot_ly(labels = ~labels, values = ~values, type = 'pie', pull = c(0.01, 0.05), hole = 0.45, marker = list(colors = colors))
fig <- fig %>% layout(hoverinfo = 'label+percent', textinfo = 'percent', textfont = list(size = 20))
fig <- fig %>% layout(annotations = list(x = 0.5, y = 0.5, text = 'Customer Type', font = list(size = 18, family = 'Verdana', color = 'black'), showarrow = FALSE))
fig <- fig %>% layout(title = 'Customer Type Distribution', titlefont = list(size = 25, family = 'Verdana'))
fig

# Define the distance groups based on percentiles
distance_group_labels <- c('0-414', '415-586', '587-719', '720-844', '845-1032', '1033-1289', '1290-1744', '1745-4983')

# Cut the 'Flight Distance' column into 8 groups
data$Distance_Group <- cut(data$flight_distance,
                              breaks = c(0, 414, 586, 719, 844, 1032, 1289, 1744, 4983),
                              labels = distance_group_labels,
                              include.lowest = TRUE)

# Create a histogram for Flight Distance
install.packages('hrbrthemes')
library(ggplot2)
library(hrbrthemes)

# Assuming df_copy is your data frame
fig_hist_distance <- ggplot(data, aes(x =flight_distance)) +
  geom_histogram(bins = 30, fill = "lightpink", color = "black") +
  labs(x = "Flight Distance", title = "Flight Distance Distribution") +
  theme_ipsum() +  # You can use other themes or customize as needed
  theme(axis.text.x = element_text(hjust = 0.5)) 

# Print the plot
print(fig_hist_distance)



# Type.of.Travel variable
ggplot(data, aes(x = recode(type_of_travel, '0' = "Business travel", '1' = "Personal Travel"), fill = type_of_travel)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count))) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  ylim(0, 100000) +
  labs(y = "Count", title = "Type of Travel variable in Dataset") +
  theme_bw()

# Class variable
ggplot(data, aes(x = recode(class, '0' = "Business", '1' = "Eco", '2' = "Eco Plus"), fill = class)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count))) +
  scale_fill_manual(values = c("pink", "lightgreen","lightblue")) +
  ylim(0, 60000) +
  labs(y = "Count", title = "Class variable in Dataset") +
  theme_bw()

# Create a categorical plot using ggplot
ggplot(data, aes(x = age, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("blue", "magenta"))+
  labs(y = "Count", title = "Age vs Passenger Satisfaction") +
  theme_bw()

# Calculate satisfaction rate
satisfaction_rate <- data %>%
  group_by(customer_type, satisfaction) %>%
  summarize(count = n()) %>%
  group_by(customer_type) %>%
  mutate(satisfaction_rate = count / sum(count)) %>%
  ungroup()

# Create the bar plot for satisfaction rate vs customer type
ggplot(satisfaction_rate, aes(x = customer_type, y = satisfaction_rate, fill = satisfaction)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_text(aes(label = scales::percent(satisfaction_rate)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5) + 
  scale_fill_manual(values = c("lightblue", "pink")) +
  labs(y = "Satisfaction Rate", title = "Customer Type vs Satisfaction Rate") +
  theme_bw()

# Calculate satisfaction rate
satisfaction_rate1 <- data %>%
  group_by(flight_distance, satisfaction) %>%
  summarize(count = n()) %>%
  group_by(flight_distance) %>%
  mutate(satisfaction_rate1 = count / sum(count)) %>%
  ungroup()

# Create the bar plot for satisfaction rate vs customer type
ggplot(satisfaction_rate1, aes(x = flight_distance, y = satisfaction_rate1, fill = satisfaction)) +
  geom_bar(stat = "identity", position = "dodge", width = 2) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  labs(y = "Satisfaction Rate", title = "Flight Distance vs Satisfaction Rate") +
  theme_bw()


ggplot(data, aes(x = inflight_wifi_service, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.7, color = "black") +
  scale_fill_manual(values = c("lightblue", "pink"))+
  labs(y = "Count", title = "inflight_wifi_service vs Passenger Satisfaction") +
  theme_bw()

ggplot(data, aes(x = ease_of_online_booking , fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("lightblue", "pink"))+
  labs(y = "Count", title = "ease_of_online_booking  vs Passenger Satisfaction") +
  theme_bw()

ggplot(data, aes(x = seat_comfort, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count))) +
  scale_fill_manual(values = c("lightblue", "pink"))+
  labs(y = "Count", title = "seat_comfort vs Passenger Satisfaction") +
  theme_bw()

ggplot(data, aes(x = cleanliness, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.8) +
  scale_fill_manual(values = c("lightblue", "pink"))+
  labs(y = "Count", title = "Cleanliness vs Passenger Satisfaction") +
  theme_bw()

ggplot(data, aes(x = food_and_drink, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.8) +
  scale_fill_manual(values = c("lightblue", "pink"))+
  labs(y = "Count", title = "Food and Drink vs Passenger Satisfaction") +
  theme_bw()

ggplot(data, aes(x = departure_arrival_time_convenient, fill = satisfaction)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("lightblue","pink", "pink"))+
  labs(y = "Count", title = "departure_arrival_time_convenient vs satisfaction") +
  theme_bw()




library(corrplot)
correlation_plot <- cor(ds_num)
summary(correlation_plot)
options(repr.plot.width = 14, repr.plot.height = 8)
corrplot(correlation_plot, na.label=" correlation_plot", tl.cex=1, tl.col="black", method="color", main = "Correlation Plot")



# Calculate the correlation matrix
corr <- cor(ds_num)

# Create a mask for the upper triangle
mask <- upper.tri(corr)

# Set up the plot
par(mar = c(2, 2, 2, 2))  # Adjust margin as needed
corrplot(corr, method = "color", col = colorRampPalette(c("lightblue", "white", "pink"))(100),
         type = "upper", order = "hclust", addCoef.col = "black", tl.col = "black",
         tl.srt = 45, diag = FALSE)
# Assuming your data frame is named 'train'
# Custom Min-Max scaling function
# Custom Min-Max scaling function for numeric columns
min_max_scale <- function(x) {
  if (is.numeric(x)) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else {
    x
  }
}

# Apply Min-Max scaling to each column of the data frame
scaled_data <- as.data.frame(lapply(data, min_max_scale))

# Display the first few rows of the scaled data
head(scaled_data)



# Extract X and y
X <- subset(scaled_data, select = -satisfaction)
y <- scaled_data$satisfaction
info_gain <- information.gain(satisfaction ~ ., data = scaled_data) # Perform feature selection using Information Gain
selected_features <- head(order(-info_gain), 10)  # Select top 10 features
selected_feature_names <- names(scaled_data)[selected_features]
print(selected_feature_names)

# Remove NAs, N/A, duplicates, and blanks from training and testing data
data <- data[complete.cases(data), ]
data <- data[!duplicated(data), ]

library(dplyr)
library(ggplot2)
library(caret)
library(ROCR)
library(corrplot)
# Split the data into training and testing sets (adjust the percentage as needed)
set.seed(123)
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


library(caret)

# Naive Bayes
library(e1071)
naive_bayes_model <- naiveBayes(satisfaction ~ ., data = train_data)
nb_predictions <- predict(naive_bayes_model, test_data)
caret::confusionMatrix(table(nb_predictions, test_data$satisfaction))

# Naive Bayes ROC and AUC Curve
nb_roc <- roc(test_data$satisfaction, as.numeric(predict(naive_bayes_model, test_data, type = "raw")[, 2]))
plot(nb_roc, main = "ROC Curve for Naive Bayes", col = "blue")
auc(nb_roc)  # Print AUC



# Decision Tree
library(rpart)
dt_model <- rpart(satisfaction ~ ., data = train_data, method = "class")
dt_predictions <- predict(dt_model, test_data, type = "class")
confusionMatrix(table(dt_predictions, test_data$satisfaction))

# Decision Tree ROC and AUC Curve
dt_roc <- roc(test_data$satisfaction, as.numeric(dt_predictions))
plot(dt_roc, main = "ROC Curve for Decision Tree", col = "green")
auc(dt_roc)  # Print AUC



# Random Forest
# Convert satisfaction to a factor with two levels
train_data$satisfaction <- as.factor(train_data$satisfaction)

# Check levels of satisfaction variable
print(levels(train_data$satisfaction))
rf_model <- randomForest(satisfaction ~ ., data = train_data)
rf_predictions <- predict(rf_model, test_data)
confusionMatrix(table(rf_predictions, test_data$satisfaction))

# Random Forest ROC and AUC Curve
rf_roc <- roc(test_data$satisfaction, as.numeric(rf_predictions))
plot(rf_roc, main = "ROC Curve for Random Forest", col = "red")
auc(rf_roc)  # Print AUC