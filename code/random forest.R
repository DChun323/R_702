ML (random forests)

library(ranger)
library(randomForest)

head(iris)

# Regression RF
m_ranger = lm(Sepal.Length ~ ., data = iris[-1,])
m_ranger

m_ranger_pred = predict(m_ranger, newdata = iris[1,])
iris[1,"Sepal.Length"] # true (observed) value
m_ranger_pred$predictions[1] # Prediction

m_ranger2 = randomForest(Sepal.Length ~ ., data = iris[-1,])
m_ranger2
plot(m_ranger2)

# Classification RF
m_ranger_c = ranger(Species ~ ., data = iris[-1,])
m_ranger_c

m_ranger_c_pred = predict(m_ranger_c, data = iris[1,])
iris[1,"Species"] # true (observed) value
m_ranger_c_pred$predictions[1] # Prediction