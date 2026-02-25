# Instalar solo si no las tienes
install.packages("tidyverse")
install.packages("caret")
install.packages("pROC")

# Cargar librerías
library(tidyverse)
library(caret)
library(pROC)

# Cargar dataset
churn <- read.csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Ver estructura
str(churn)

# Dimensiones
dim(churn)

# Primeras filas
head(churn)

# Revisar Variable Objetivo

table(churn$Churn)
prop.table(table(churn$Churn))

# Revisar NA por columna
colSums(is.na(churn))
summary(churn)

# Convertir a numérico
churn$TotalCharges <- as.numeric(churn$TotalCharges)

# Ver NA generados
sum(is.na(churn$TotalCharges))

# Eliminar na
churn <- churn[!is.na(churn$TotalCharges), ]

dim(churn)

# Convertir Churn a factor
churn$Churn <- as.factor(churn$Churn)
levels(churn$Churn)

#Exploración estratégica
# Churn por tipo de contrato
table(churn$Contract, churn$Churn)

prop.table(table(churn$Contract, churn$Churn), 1)


#Tenure vs Churn
churn %>%
  group_by(Churn) %>%
  summarise(mean_tenure = mean(tenure))

#MonthlyCharges vs Churn
churn %>%
  group_by(Churn) %>%
  summarise(mean_monthly = mean(MonthlyCharges))

# Modelo Predictivo
# Train / Test Split
set.seed(123)

trainIndex <- createDataPartition(churn$Churn, 
                                  p = 0.7, 
                                  list = FALSE)

train <- churn[trainIndex, ]
test  <- churn[-trainIndex, ]

dim(train)
dim(test)

#Construir Modelo de Regresión Logística
# customerID (no es predictiva)
train$customerID <- NULL
test$customerID  <- NULL

#Constuir Modelo
model_logit <- glm(Churn ~ ., 
                   data = train, 
                   family = binomial)

summary(model_logit)

#Predecir Test
# Probabilidades
prob_test <- predict(model_logit, 
                     newdata = test, 
                     type = "response")

# Clasificación con umbral 0.5
pred_test <- ifelse(prob_test > 0.5, "Yes", "No")

# Convertir a factor
pred_test <- as.factor(pred_test)

#Matriz de confusión
confusionMatrix(pred_test, test$Churn)

#Calcular ROC y AUC
roc_obj <- roc(test$Churn, prob_test)

auc(roc_obj)