install.packages("binaryLogic")
library(binaryLogic) 
library(class)
library(cvms)
library(tibble)
library(broom)
library(tidyverse)
library(rpart.plot)
library(e1071)
library(rpart)
library(ggplot2)
library(naivebayes)

q<- read.csv("C:/Users/atrac/Desktop/IST 707/Project/Dataframe.csv") # reading in the data

q[is.na(q)] <- 0
Q <- read.csv("C:/Users/atrac/Desktop/IST 707/Project/Dataframe.csv") # saving off a copy of the original data so it's always available. 


Q[is.na(Q)] <- 0 # setting all na's in the data set to 0
 
Q[3:ncol(Q)] <- b # grabbing all of the errors into a seperate data set b

for (i in 1:24){
b[which(b[,i] > 1),i] <-1

} # this turns all values in the data set that are not 0 to be 1. This makes every column have 1 if the error exist and 0 if it doesn't.
x <- vector() # making empty variable for later
Data <- b # saving off copy of b to make sure not to touch it

for (i in 1:ncol(Data)){
  v <-sum(Data[,i])
  x <- append(x,v)
} # creating list x that will contain all of the sums of the values in each column. This is a list of the counts of the errors. 

y  <- sort(x,decreasing = TRUE,index.return = TRUE) 

y$x # to show the counts

y$ix # to show the corresponding columns

frequency_df <- data.frame(y$x,colnames(Data[,y$ix]))

frequency_df #showing the top frequencies the errors and their 


df <- q[,-1:-2]
df <- df[,-19:-24]
df <- as.data.frame(df)
colnames(df) <- paste("error",colnames(df),sep="_")
df$error_1.6 <- as.numeric(df$error_1.6)
df[is.na(df)] <- 0
df[df>0] <- 1
df <- df %>% mutate_all(as.factor)
df

set.seed(10)
smp_size <- floor(0.75*nrow(df))
train_idx <- sample(seq_len(nrow(df)),size=smp_size)
train <- df[train_idx,]
test <- df[-train_idx,]




nb_model <- naiveBayes(X3.1~., data=train)

pred_nb <- predict(object = nb_model, select(test,-X3.1), type = "class")



conf_mat_nb <- confusion_matrix(targets = test$X3.1, predictions = pred_nb)


plot_confusion_matrix(
  conf_mat_nb$`Confusion Matrix`[[1]],
  font_counts = font(
    size = 10,
    angle = 45,
    color = "red"
  ),
  add_normalized = FALSE,
  add_col_percentages = FALSE,
  add_row_percentages = FALSE,
  
)
# credit for plots and confusion matrix (https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html)

paste("Balanced Accuracy",conf_mat_nb$`Balanced Accuracy`)

model_SVM_sigmoid <-svm(X3.1~.,data=train, kernel = "linear", scale = FALSE, cost = 0.2)

pred_svm_sigmoid <- predict(model_SVM_sigmoid,select(test,-X3.1),type = "class")

conf_mat_sigmoid <- confusion_matrix(targets = test$X1.2, predictions = pred_svm_sigmoid)

t <- pred_svm_sigmoid


table(test$X3.1,t)

plot_confusion_matrix(
  conf_mat_sigmoid$`Confusion Matrix`[[1]],
  font_counts = font(
    size = 10,
    angle = 45,
    color = "red"
  ),
  add_normalized = FALSE,
  add_col_percentages = FALSE,
  add_row_percentages = FALSE,
  
)

paste("Balanced Accuracy",conf_mat_sigmoid$`Balanced Accuracy`)

k <- round(sqrt(nrow(train)))

train_model_k <- knn(train = select(train,-X3.1), test = select(test,-X3.1), cl = train$X3.1, k = k, prob= TRUE)

conf_mat_k <- confusion_matrix(targets = test$X3.1, predictions = train_model_k)

# table_results <- tibble("test" = test[1], "prediction"  = pred)
# 
# b_table <- table(table_results)  
plot_confusion_matrix(
  conf_mat_k$`Confusion Matrix`[[1]],
  font_counts = font(
    size = 10,
    angle = 45,
    color = "red"
  ),
  add_normalized = FALSE,
  add_col_percentages = FALSE,
  add_row_percentages = FALSE,
  
)

paste("Balanced Accuracy",conf_mat_k$`Balanced Accuracy`)


train_model <- rpart(X3.1 ~., data = train, method = "class")

rpart.plot(train_model)


df <- data.frame(imp = train_model$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2[c((nrow(df2)-10):nrow(df2)),]) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() +ggtitle("Error Importance")


pred <- predict (object = train_model,select(test,-X3.1),type = "class")

conf_mat_Dtree <- confusion_matrix(targets = test$X3.1, predictions = pred)

plot_confusion_matrix(
  conf_mat_Dtree$`Confusion Matrix`[[1]],
  font_counts = font(
    size = 10,
    angle = 45,
    color = "red"
  ),
  add_normalized = FALSE,
  add_col_percentages = FALSE,
  add_row_percentages = FALSE,
  
)

paste("Balanced Accuracy",conf_mat_Dtree$`Balanced Accuracy`)

plotcp(train_model)
