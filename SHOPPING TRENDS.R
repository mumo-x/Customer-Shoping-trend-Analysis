library(readxl)
shopping_trends <- read_excel("D:/notes/MASTERS/Computational Statistics and Programming/ASSIGNMENT/shopping_trends.xlsx")
View(shopping_trends)
summary(shopping_trends)

#Convert categorical variables to factors
shopping_trends$Gender <- as.factor(shopping_trends$Gender)
shopping_trends$Location <- as.factor(shopping_trends$Location)
shopping_trends$Season <- as.factor(shopping_trends$Season)
shopping_trends$Subscription_Status <- as.factor(shopping_trends$Subscription_Status)
shopping_trends$Payment_Method <- as.factor(shopping_trends$Payment_Method)
shopping_trends$Shipping_Type <- as.factor(shopping_trends$Shipping_Type)
shopping_trends$Discount_Applied <- as.factor(shopping_trends$Discount_Applied)
shopping_trends$Promo_Code_Used <- as.factor(shopping_trends$Promo_Code_Used)

#Fit a linear regression model
model<- lm(Purchase_Amount ~ Age + Gender + Location + Season + Promo_Code_Used + Subscription_Status + Payment_Method + Shipping_Type + Discount_Applied + Promo_Code_Used , data = shopping_trends)
plot(model)

#Summarize and interpret the model
summary(model)

#extract data
purchase_amounts <- shopping_trends$Purchase_Amount

# Define parameters
set.seed(123)         
n_simulations <- 1000
group_size <- 50      
threshold <- 70      

# Run Monte Carlo simulation
exceed_count <- 0  # Counter for groups exceeding the threshold

for (i in 1:n_simulations) {
  # Randomly sample 'group_size' purchase amounts
  sampled_group <- sample(purchase_amounts, size = group_size, replace = TRUE)
  
  # Calculate the average of the sampled group
  avg_purchase <- mean(sampled_group)
  
  # Check if the average exceeds the threshold
  if (avg_purchase > threshold) {
    exceed_count <- exceed_count + 1
  }
}

# Calculate the probability
probability <- exceed_count / n_simulations
probability

#2nd simulation
# Define parameters
set.seed(123)         
n_simulations <- 5000
group_size <- 50      
threshold <- 70      

# Run Monte Carlo simulation
exceed_count <- 0  # Counter for groups exceeding the threshold

for (i in 1:n_simulations) {
  # Randomly sample 'group_size' purchase amounts
  sampled_group <- sample(purchase_amounts, size = group_size, replace = TRUE)
  
  # Calculate the average of the sampled group
  avg_purchase <- mean(sampled_group)
  
  # Check if the average exceeds the threshold
  if (avg_purchase > threshold) {
    exceed_count <- exceed_count + 1
  }
}

# Calculate the probability
probability <- exceed_count / n_simulations
probability

