# Fit the logit, probit and cloglog-link logistic regression models

log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,

                       family = binomial(link = logit), data = training_set)



log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,

                       family = binomial(link = probit), data = training_set)



log_model_cloglog <-  glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,

                       family = binomial(link = cloglog), data = training_set)





# Make predictions for all models using the test set

predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")

predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")

predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response") 



# Use a cut-off of 14% to make binary predictions-vectors

cutoff <- 0.14

class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)

class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0) 

class_pred_cloglog <-ifelse(predictions_cloglog > cutoff, 1, 0)  



# Make a confusion matrix for the three models

tab_class_logit <- table(true_val,class_pred_logit)

tab_class_probit <- table(true_val,class_pred_probit) 

tab_class_cloglog <- table(true_val,class_pred_cloglog) 



# Compute the classification accuracy for all three models

acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)

acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)

acc_cloglog <-sum(diag(tab_class_cloglog)) / nrow(test_set) 







# The Gini-measure of the root node is given below

gini_root <- 2 * 89 / 500 * 411 / 500



# Compute the Gini measure for the left leaf node

gini_ll <- 2 * 45 / 500 * 401 / 500



# Compute the Gini measure for the right leaf node

gini_rl <- 2 * 10 / 54 * 44 / 54



# Compute the gain

gain <- gini_root - 446 / 500 * gini_ll - 54 / 500 * gini_rl



# compare the gain-column in small_tree$splits with our computed gain, multiplied by 500, and assure they are the same

small_tree$splits

improve <- gain * 500







####UNDERSAMPLING



### Change the code provided such that a decision tree is constructed using the undersampled training set instead of training_set. 

### Additionally, add the argument control = rpart.control(cp = 0.001). cp, which is the complexity parameter, 

### is the threshold value for a decrease in overall lack of fit for any split. If cp is not met, further splits will no longer be pursued. 

### cp's default value is 0.01, but for complex problems, it is advised to relax cp.





# Load package rpart in your workspace.

library(rpart)



# Change the code provided in the video such that a decision tree is constructed using the undersampled training set. Include rpart.control to relax the complexity parameter to 0.001.

tree_undersample <- rpart(loan_status ~ ., method = "class",

                          data =  undersampled_training_set, control = rpart.control(cp = 0.001))



# Plot the decision tree

plot(tree_undersample, uniform=TRUE)



# Add labels to the decision tree

text(tree_undersample)



#### CHANGE PRIOR PROBABILITIES

## As mentioned in the video, you can also change the prior probabilities to obtain a decision tree. 

## This is an indirect way of adjusting the importance of misclassifications for each class. 

## You can specify another argument inside rpart() to include prior probabities. 

## The argument you are looking for has the following form

                    ##parms = list(prior=c(non_default_proportion, default_proportion))





# Change the code below such that a tree is constructed with adjusted prior probabilities.

tree_prior <- rpart(loan_status ~ ., method = "class",

                    data = training_set,

                    parms = list(prior=c(0.3, 0.7)),

                    control = rpart.control(cp = 0.001)

                    )



# Plot the decision tree

plot(tree_prior, uniform = TRUE)



# Add labels to the decision tree

text(tree_prior)



### INCLUDING A LOSS MATRIX

## Thirdly, you can include a loss matrix, changing the relative importance of misclassifying a default as non-default versus a non-default as a default. 

## You want to stress that misclassifying a default as a non-default should be penalized more heavily. 

## Including a loss matrix can again be done in the argument parms in the loss matrix.



## parms = list(loss = matrix(c(0, cost_def_as_nondef, cost_nondef_as_def, 0), ncol=2))

# Change the code below such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.

tree_loss_matrix <- rpart(loan_status ~ ., method = "class",

                          data =  training_set,

                          parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)),

                          control = rpart.control(cp = 0.001)

                          )





# Plot the decision tree

plot(tree_loss_matrix, uniform = TRUE)



# Add labels to the decision tree

text(tree_loss_matrix)





### Pruning the tree with changed prior probabilities



# tree_prior is loaded in your workspace

set.seed(567)



# Plot the cross-validated error rate as a function of the complexity parameter

plotcp(tree_prior)



# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.

printcp(tree_prior)



# Create an index for of the row with the minimum xerror

index <- which.min(tree_prior$cptable [ , "xerror"])



# Create tree_min

tree_min <- tree_prior$cptable[index, "CP"]



#  Prune the tree using tree_min

ptree_prior <- prune(tree_prior, cp = tree_min)



# Use prp() to plot the pruned tree

prp(ptree_prior)





### Pruning the tree with the loss matrix

# set a seed and run the code to construct the tree with the loss matrix again

set.seed(345)

tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,

                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),

                           control = rpart.control(cp = 0.001))



# Plot the cross-validated error rate as a function of the complexity parameter

plotcp(tree_loss_matrix)



# Prune the tree using cp = 0.0012788

ptree_loss_matrix <- prune(tree_loss_matrix, cp = 0.0012788)        

                          

# Use prp() and argument extra = 1 to plot the pruned tree

prp(ptree_loss_matrix, extra = 1)





### One final tree using more options

# set a seed and run the code to obtain a tree using weights, minsplit and minbucket

set.seed(345)

tree_weights <- rpart(loan_status ~ ., method = "class",

                      data = training_set,

                      weights = case_weights,

                      control = rpart.control(minsplit = 5, minbucket =2, cp = 0.001))



# Plot the cross-validated error rate for a changing cp

plotcp(tree_weights)



# Create an index for of the row with the minimum xerror

index <- which.min(tree_weights$cp[ , "xerror"])



# Create tree_min

tree_min <- tree_weights$cp[index, "CP"]



# Prune the tree using tree_min

ptree_weights <- prune(tree_weights, tree_min)



# Plot the pruned tree using the rpart.plot()-package

prp(ptree_weights, extra = 1)





# Make predictions for each of the pruned trees using the test set.

pred_undersample <- predict(ptree_undersample, newdata = test_set,  type = "class")

pred_prior <- predict(ptree_prior, newdata = test_set,  type = "class")

pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set,  type = "class")

pred_weights <- predict(ptree_weights, newdata = test_set,  type = "class")



# construct confusion matrices using the predictions.

confmat_undersample <- table(test_set$loan_status, pred_undersample)

confmat_prior <- table(test_set$loan_status, pred_prior)

confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)

confmat_weights <- table(test_set$loan_status, pred_weights)



# Compute the accuracies

acc_undersample <- sum(diag(confmat_undersample)) / nrow(test_set)

acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)

acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)

acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)





### Computing a bad rate given a fixed acceptance rate



# Make predictions for the probability of default using the pruned tree and the test set.

prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]



# Obtain the cutoff for acceptance rate 80%

cutoff_prior <- quantile(prob_default_prior, 0.8)  



# Obtain the binary predictions.

bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)



# Obtain the actual default status for the accepted loans

accepted_status_prior_80 <- test_set$loan_status[bin_pred_prior_80 == 0]



# Obtain the bad rate for the accepted loans

sum(accepted_status_prior_80)/length(accepted_status_prior_80)





### The strategy table and strategy curve



## strategy_bank function

function(prob_of_def){

cutoff=rep(NA, 21)

bad_rate=rep(NA, 21)

accept_rate=seq(1,0,by=-0.05)

for (i in 1:21){

  cutoff[i]=quantile(prob_of_def,accept_rate[i])

  pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)

  pred_as_good=test_set$loan_status[pred_i==0]

  bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}

table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))

return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))

}









# Have a look at the function strategy_bank

strategy_bank



# Apply the function strategy_bank to both predictions_cloglog and predictions_loss_matrix

strategy_cloglog <- strategy_bank(predictions_cloglog)

strategy_loss_matrix <- strategy_bank(predictions_loss_matrix)



# Obtain the strategy tables for both prediction-vectors

strategy_cloglog$table

strategy_loss_matrix$table



> strategy_loss_matrix$table

     accept_rate cutoff bad_rate

 [1,]        1.00 0.2830   0.1069

 [2,]        0.95 0.1698   0.0798

 [3,]        0.90 0.1698   0.1071

 [4,]        0.85 0.1698   0.1071 B

 [5,]        0.80 0.1698   0.1071

 [6,]        0.75 0.1698   0.1071

 [7,]        0.70 0.1698   0.1071

 [8,]        0.65 0.1698   0.1071

 [9,]        0.60 0.1667   0.0746

[10,]        0.55 0.1332   0.0743

[11,]        0.50 0.1300   0.0719

[12,]        0.45 0.0724   0.0629  A

[13,]        0.40 0.0704   0.0631

[14,]        0.35 0.0704   0.0631

[15,]        0.30 0.0655   0.0584

[16,]        0.25 0.0610   0.0502

[17,]        0.20 0.0584   0.0459

[18,]        0.15 0.0411   0.0362

[19,]        0.10 0.0411   0.0362

[20,]        0.05 0.0411   0.0660

[21,]        0.00 0.0000   0.0800

> 

strategy_cloglog$table

      accept_rate cutoff bad_rate

 [1,]        1.00 0.3679   0.1069

 [2,]        0.95 0.1890   0.1012

 [3,]        0.90 0.1808   0.0983

 [4,]        0.85 0.1707   0.0943 B

 [5,]        0.80 0.1538   0.0905

 [6,]        0.75 0.1434   0.0873

 [7,]        0.70 0.1383   0.0832

 [8,]        0.65 0.1328   0.0803

 [9,]        0.60 0.1255   0.0763

[10,]        0.55 0.1136   0.0741

[11,]        0.50 0.1050   0.0711

[12,]        0.45 0.0975   0.0677 A

[13,]        0.40 0.0938   0.0642

[14,]        0.35 0.0904   0.0600

[15,]        0.30 0.0861   0.0543

[16,]        0.25 0.0779   0.0486

[17,]        0.20 0.0571   0.0454

[18,]        0.15 0.0549   0.0384

[19,]        0.10 0.0529   0.0402

[20,]        0.05 0.0501   0.0389

[21,]        0.00 0.0305   0.0000

> 





# ROC-curves can easily be created using the pROC-package in R. 

# Let's have a look if there is a big difference between ROC-curves for the four logistic regression-models previously used throughout this course. 

# A small heads up:

# predictions_logit contains probability of default (PD) predictions using the default logit link and containing variables age, emp_cat, ir_cat and loan_amnt.

# predictions_probit contains PD-predictions using the probit and containing variables age, emp_cat, ir_cat and loan_amnt.

# predictions_cloglog contains PD-predictions using the cloglog link and containing variables age, emp_cat, ir_cat and loan_amnt.

# predictions_all_full contains PD-predictions using the default logit link and containing all seven variables in the data set.

# You will first draw the ROC-curves for these four models in one plot. Afterwards, you will look at the area under the curve.

# Load the pROC-package

library(pROC)



# Construct the objects containing ROC-information

# using function roc(response, predictor)

ROC_logit <- roc(test_set$loan_status, predictions_logit)

ROC_probit <- roc(test_set$loan_status, predictions_probit)

ROC_cloglog <- roc(test_set$loan_status, predictions_cloglog)

ROC_all_full <- roc(test_set$loan_status, predictions_all_full) 



# Draw all ROCs on one plot

plot(ROC_logit)

lines(ROC_probit, col="blue")

lines(ROC_cloglog, col="red")

lines(ROC_all_full, col="green")



# Compute the AUCs

auc(ROC_logit)

auc(ROC_probit)

auc(ROC_cloglog)

auc(ROC_all_full)



## ROC-curves for comparison of tree-based models

# Construct the objects containing ROC-information

ROC_undersample <- roc(test_set$loan_status, predictions_undersample)

ROC_prior <- roc(test_set$loan_status, predictions_prior)

ROC_loss_matrix <- roc(test_set$loan_status, predictions_loss_matrix)

ROC_weights <- roc(test_set$loan_status, predictions_weights)



# Draw the ROC-curves in one plot

plot(ROC_undersample)

lines(ROC_prior, col="blue")

lines(ROC_loss_matrix, col="red")

lines(ROC_weights, col="green")



# Compute the AUCs

auc(ROC_undersample)

auc(ROC_prior)

auc(ROC_loss_matrix)

auc(ROC_weights)





### Another round of pruning based on AUC

# Build four models each time deleting one variable in log_3_remove_ir

log_4_remove_amnt <- glm(loan_status ~ grade + annual_inc + emp_cat, 

                        family = binomial, data = training_set) 



log_4_remove_grade <- glm(loan_status ~ loan_amnt + annual_inc + emp_cat, 

                        family = binomial, data = training_set) 



log_4_remove_inc <- glm(loan_status ~ loan_amnt + grade + emp_cat, 

                        family = binomial, data = training_set) 



log_4_remove_emp <- glm(loan_status ~ loan_amnt + grade + annual_inc, 

                        family = binomial, data = training_set) 



# Make PD-predictions for each of the models

pred_4_remove_amnt <- predict(log_4_remove_amnt, newdata = test_set, type = "response")

pred_4_remove_grade <- predict(log_4_remove_grade, newdata = test_set, type = "response") 

pred_4_remove_inc <- predict(log_4_remove_inc, newdata = test_set, type = "response")

pred_4_remove_emp <- predict(log_4_remove_emp, newdata = test_set, type = "response")



# Compute the AUCs

auc(test_set$loan_status, pred_4_remove_amnt)

auc(test_set$loan_status, pred_4_remove_grade)

auc(test_set$loan_status, pred_4_remove_inc)

auc(test_set$loan_status, pred_4_remove_emp)





### Further model reduction?

# Build three models each time deleting one variable in log_4_remove_amnt

log_5_remove_grade <- glm(loan_status ~ annual_inc + emp_cat, family = binomial, data = training_set) 



log_5_remove_inc <- glm(loan_status ~ grade + emp_cat, family = binomial, data = training_set) 

log_5_remove_emp <- glm(loan_status ~ grade + annual_inc, family = binomial, data = training_set) 



# Make PD-predictions for each of the models

pred_5_remove_grade <- predict(log_5_remove_grade, newdata = test_set, type = "response")

pred_5_remove_inc <- predict(log_5_remove_inc, newdata = test_set, type = "response")

pred_5_remove_emp <- predict(log_5_remove_emp, newdata = test_set, type = "response")



# Compute the AUCs

auc(test_set$loan_status,pred_5_remove_grade)

auc(test_set$loan_status,pred_5_remove_inc)

auc(test_set$loan_status,pred_5_remove_emp)





# Plot the ROC-curve for the best model here

plot(roc(test_set$loan_status,pred_4_remove_amnt))

