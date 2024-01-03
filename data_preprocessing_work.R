#Simple linear regression 
# Importing the datase
# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Load caTools library
library(caTools)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')

set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
regressor = lm(formula=Salary ~ YearsExperience,
               data = training_set)
#Predicting test result set
y_pred = predict(regressor,newdata = test_set)
#visualizing training set results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y =training_set$Salary),
             colour = 'red')  +
  geom_line(aes(x =training_set$YearsExperience, y = predict(regressor, new_data = training_set)),
            colour = 'blue')
  ggtitle('Salary vs Experience(trainingset)')
  xlab('Years of Experience')
  ylab('Salary')
  
  # visualization of test set result 
  ggplot() +
    geom_point(aes(x = test_set$YearsExperience, y =test_set$Salary),
               colour = 'red')  +
    geom_line(aes(x =test_set$YearsExperience, y_pred),
              colour = 'blue')
  ggtitle('Salary vs Experience(testset)')
  xlab('Years of Experience')
  ylab('Salary')
  # visulizing training set(predicted)  and test set
  ggplot() +
    geom_point(aes(x = test_set$YearsExperience, y =test_set$Salary),
               colour = 'red')  +
    geom_line(aes(x =training_set$YearsExperience, y = predict(regressor, new_data = training_set)),
              colour = 'blue')
  ggtitle('Salary vs Experience(testset)')
  
