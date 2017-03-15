# cosine similarity for income dataset 
# Mike Zhang (zhang.4660)
# Yiran Cao (cao.805)

#######################################################################
# import the dataset from the terminal 
rm(list=ls())

print("loading dataframe from file...")
income_tr <- read.csv("income_tr.csv",stringsAsFactors = FALSE)

################uncomment the following 2 lines to change the size of training set
#m <- dim(income_tr)[1]
#income_tr <- income_tr[sample(1:m, size = round( m / 4), replace = FALSE, prob = rep(1/m, m)),]

income_te <- read.csv("income_te.csv",stringsAsFactors = FALSE)

N_tr <- nrow(income_tr)
N_te <- nrow(income_te)


print("data preprocessing...")

# seperate numeric data from income dataset
numeric_data <- c("age", "fnlwgt","hour_per_week")

# store value for train and test dataset
income_tr_numeric <- income_tr[numeric_data]
income_te_numeric <- income_te[numeric_data]

# seperate asymmertric binary data from income dataset
asymmetric_data <- c("capital_gain", "capital_loss")

# store value for train and test dataset
income_tr_asymmetric <- income_tr[asymmetric_data]
income_te_asymmetric <- income_te[asymmetric_data]

# seperate ordinal data from income dataset
ordinal_data <- c("education_cat")

# store value for train and test dataset
income_tr_ordinal <- income_tr[ordinal_data]
income_te_ordinal <- income_te[ordinal_data]

# seperate string data from income dataset
string_data <- c("workclass", "marital_status", "occupation", "relationship", "race", "gender","native_country")

# store value for train and test dataset
income_tr_string <- income_tr[string_data]
income_te_string <- income_te[string_data]

#######################################################################
# handle missing data 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# compute mode for numberic attributes in train and test dataset. 
modes_tr <- apply(income_tr, 2, Mode)
modes_te <- apply(income_te, 2, Mode)

# comput e means for numberic attributes in train and test dataset
means_tr <- apply(income_tr_numeric, 2, mean)
means_te <- apply(income_te_numeric, 2, mean)

# use means to fulfill the missing value for numeric attributes
for(var in numeric_data){
  income_tr_numeric[var][income_tr_numeric[var] == " ?"] = means_tr[var]
  income_te_numeric[var][income_te_numeric[var] == " ?"] = means_te[var]
}

# use modes to fultill the missing values for ordinal attributes
for(var in ordinal_data){
  income_tr_ordinal[var][income_tr_ordinal[var] == " ?"] = modes_tr[var]
  income_te_ordinal[var][income_te_ordinal[var] == " ?"] = modes_te[var]
}

# use modes to fultill the missing values for string attributes
for(var in string_data){
  income_tr_string[var][income_tr_string[var] == " ?"] = modes_tr[var]
  income_te_string[var][income_te_string[var] == " ?"] = modes_te[var]
}
income_tr_string <- apply(income_tr_string, 2, as.factor)
income_te_string <- apply(income_te_string, 2, as.factor)
# use modes to fultill the missing values for asymmetric attributes
for(var in asymmetric_data) {
  income_tr_asymmetric[var][income_tr_asymmetric[var] == " ?"] = modes_tr[var]
  income_te_asymmetric[var][income_te_asymmetric[var] == " ?"] = modes_te[var]
}

#######################################################################
# data basic function

# function for calculating the distance of a row
row_length <- function(r) {
  return (sqrt(sum(r^2)))
}

# function for calculating cosine similarity of 2 rows
cos_sim <- function(r1, r2) {
  cos <- sum(r1 * r2) / (row_length(r1) * row_length(r2))
  if(cos<-1){
    cos = -1
  }
  if(cos > 1){
    cos = 1
  }
  return(1 - acos(cos) / pi)
}

# compute the standard deviation 
standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# compute the equivalent string attributes in two column
count_equi <- function(r1, r2) {
  return(sum(r1 == r2))
}

# identify and return income range with given ID
income_range = function(index){
  name = income_tr[index, 16]
  return(name)
}

#####################################################################
# data transformation

# numeric data standardlization
income_tr_numeric_std <- apply(income_tr_numeric, 2, standardization)
income_te_numeric_std <- apply(income_te_numeric, 2, standardization)

#####################################################################
# computing proximity
print("computing proximities...")
# construct proximity matrix for numeric attribute
numeric_prox_matrix <- matrix(0, nrow = N_te, ncol = N_tr)
for (i in 1:N_te) {
  for (j in 1:N_tr) {
    numeric_prox_matrix[i,j] <- cos_sim(income_te_numeric_std[i,], income_tr_numeric_std[j,])
  }
}

# compute number of rows for asymmetric attributes
asymmetric_prox_matrix_cg <- matrix(0, nrow = N_te, ncol = N_tr)
delta_cg <- matrix(0, nrow = N_te, ncol = N_tr)
for (i in 1:N_te) {
  for (j in 1:N_tr) {
    if(income_te_asymmetric[i,1] != 0 && income_tr_asymmetric[j,1] != 0) {
      asymmetric_prox_matrix_cg[i,j] <- 1
      delta_cg[i,j] <- 1
    } else if(income_te_asymmetric[i,1]==0 && income_tr_asymmetric[j,1]==0) {
      asymmetric_prox_matrix_cg[i,j] <- 0
      delta_cg[i, j] <- 0
    } else {
      asymmetric_prox_matrix_cg[i,j] <- 0
      delta_cg[i, j] <- 1
    }
  }
}

asymmetric_prox_matrix_cl <- matrix(0, nrow = N_te, ncol = N_tr)
delta_cl <- matrix(0, nrow = N_te, ncol = N_tr)
for (i in 1:N_te) {
  for (j in 1:N_tr) {
    if(income_te_asymmetric[i,2] != 0 && income_tr_asymmetric[j,2] != 0) {
      asymmetric_prox_matrix_cl[i,j] <- 1
      delta_cl[i,j] <- 1
    } else if(income_te_asymmetric[i,2] == 0 && income_tr_asymmetric[j,2] == 0) {
      asymmetric_prox_matrix_cl[i,j] <- 0
      delta_cl[i,j] <- 0
    } else {
      asymmetric_prox_matrix_cl[i,j] <- 0
      delta_cl[i,j] <- 1
    }
  }
}

# construct proximity matrix for ordinal attribute
ordinal_prox_matrix <- matrix(nrow = N_te, ncol = N_tr)
# compute cosine similarity for string 
for(i in 1:N_te){
  for(j in 1:N_tr){
    ordinal_prox_matrix[i,j] <- 1 - abs(income_te_ordinal[i,] - income_tr_ordinal[j,]) / 
      (max(income_te_ordinal[,1], income_tr_ordinal[,1]) - min(income_te_ordinal[,1],income_tr_ordinal[,1]))
  }
}

# construct proximity matrix for string attribute
string_prox_matrix <- matrix(0, nrow = N_te, ncol = N_tr)

# compute cosine similarity for string 
for(i in 1:N_te){
  for(j in 1:N_tr){
    for(k in 1:ncol(income_te_string)){
      if(income_te_string[i,k]==income_tr_string[j,k]){
        string_prox_matrix[i,j] = string_prox_matrix[i,j] + 1
      } 
    }
  }
}

## combine all types of attribute in one matrix
col_te_numeric <- ncol(income_te_numeric)
col_te_ordinal <- ncol(income_te_ordinal)
col_te_string <- ncol(income_te_string)
total_col <- col_te_numeric + col_te_ordinal + col_te_string

prox_matrix <- (asymmetric_prox_matrix_cg * 1 + asymmetric_prox_matrix_cl * 1 + 
                  numeric_prox_matrix * col_te_numeric + ordinal_prox_matrix * col_te_ordinal + 
                  string_prox_matrix) / (delta_cg + delta_cl + matrix(total_col, nrow = N_te, ncol = N_tr))

# sort the matrix in decreasing order
order_matrix <- matrix(nrow = N_te, ncol = N_tr)
for (i in 1:N_te) {
  order_matrix[i,] = order(prox_matrix[i,], decreasing = TRUE)
}

sort_prox_matrix <- matrix(nrow = N_te, ncol = N_tr)
for(i in 1:N_te) {
  sort_prox_matrix[i,] <- sort(prox_matrix[i,], decreasing = TRUE)
}

# compute income range and store values into class matrix
class_matrix = matrix(nrow = N_te, ncol = N_tr)
for(i in 1:N_te){
  for(j in 1:N_tr){
    class_matrix[i,j] = income_range(order_matrix[i,j])
  }
}

####################################################################
# training the classifier

print("training the classifier....")

N_iter   <- 50
k        <- 1
division <- 100
improve  <- matrix(0, nrow = N_iter, ncol = 2)
tpr      <- matrix(1, nrow = division + 1, ncol = N_iter)
fpr      <- matrix(1, nrow = division + 1, ncol = N_iter)
#################Notice: We manually set tpr and fpr = 1 when threshold = 0######################
for(i in 1:N_iter) {
  improve[i,1] = k
  weights <- matrix(1, nrow = N_te, ncol = k)
  #uncomment the following three lines to use weighted posterior
  for(s in 1:N_te) {
    weights[s,] <-  sort_prox_matrix[s,1:k]
  }
  
  for(j in 1:division){
    threshold = j / division
    result_matrix = matrix(0, nrow = N_te, ncol = 2)
    result_matrix[,1] = income_te[,"class"]
    #compute posterior and get results
    for(l in 1:N_te) {
      posterior = sum((income_tr[order_matrix[l,1:k],"class"] == " >50K") * weights[l,])  / sum(weights[l,])
      ######################## If we use posterior > threshold: manually set tpr and fpr = 1 when threshold = 0 
      ######################## If we use posterior >= threshold: manually set tpr and fpr = 0 when threshold = 1 
      if(posterior > threshold) {
        result_matrix[l,2] = " >50K"
      } else {
        result_matrix[l,2] = " <=50K"
      }
    }
    tp = sum((result_matrix[,1] == " >50K")  * (result_matrix[,2] == " >50K"))
    tn = sum((result_matrix[,1] == " <=50K") * (result_matrix[,2] == " <=50K"))
    fp = sum((result_matrix[,1] == " <=50K") * (result_matrix[,2] == " >50K"))
    fn = sum((result_matrix[,1] == " >50K")  * (result_matrix[,2] == " <=50K"))
    
    tpr[j + 1, i] = tp / (tp + fn)
    fpr[j + 1, i] = fp / (fp + tn)
  }
  ################ Calculate the area of the trapezoid########################
  for(a in 1:(nrow(tpr) - 1)){
    improve[i,2] <- improve[i,2] + (fpr[a, i] - fpr[a+1, i]) * (tpr[a+1, i] + tpr[a, i]) / 2
  }
  # Minus the area of the triangle
  improve[i,2] <- improve[i,2] - 0.5
  k = k + 2
}

# uncomment the following 3 lines to plot a figure to see how the lift changes with different values of k
#plot(spline(improve[,1], improve[,2], n = 1000),
#     type = "l",xlim = c(0,100), ylim = c(0,0.5), xlab = "k", ylab = "Lift",
#     main = "Lift over Random Guess with k changing")

# We want to calculate the area under the line (k = 9)
#plot(fpr[,9], tpr[,9], type = "p", xlim = c(0, 1), ylim = c(0, 1))
#lines(fpr[,9], tpr[,9])

#plot(improve[,1], improve[,2], type = "l", ylim = c(0, 0.4),
#     xlab = "k", ylab = "improvement")

optk = improve[which.max(improve[,2]),1]

eva_matrix <- matrix(0, nrow = division + 1, ncol = 10)
weights <- matrix(1, nrow = N_te, ncol = optk)
for(s in 1:N_te) {
  weights[s,] <-  sort_prox_matrix[s,1:optk]
}
for(j in 1:100){
  threshold = j / 100
  eva_matrix[j+1,1] = threshold
  result_matrix = matrix(0, nrow = N_te, ncol = 2)
  result_matrix[,1] = income_te[,"class"]
  
  
  #compute posterior and get results
  for(l in 1:N_te) {
    posterior = sum((income_tr[order_matrix[l,1:optk],"class"] == " >50K") * weights[l,])  / sum(weights[l,])
    if(posterior > threshold) {
      result_matrix[l,2] = " >50K"
    } else {
      result_matrix[l,2] = " <=50K"
    }
  }
  tp = sum((result_matrix[,1] == " >50K") * (result_matrix[,2] == " >50K"))
  tn = sum((result_matrix[,1] == " <=50K") * (result_matrix[,2] == " <=50K"))
  fp = sum((result_matrix[,1] == " <=50K") * (result_matrix[,2] == " >50K"))
  fn = sum((result_matrix[,1] == " >50K") * (result_matrix[,2] == " <=50K"))
  
  tpr = tp / (tp + fn)
  fpr = fp / (fp + tn)
  
  eva_matrix[j+1,2] = 1 - (tp + tn) / N_te
  eva_matrix[j+1,3] = tpr - fpr
  eva_matrix[j+1,4] = fpr
  eva_matrix[j+1,5] = tpr
  eva_matrix[j+1,6] = fn / (fn + tp)              # FNR
  eva_matrix[j+1,7] = tn / (tn + fp)              # TFR
  eva_matrix[j+1,8] = tp / (tp + fp)              # precision
  eva_matrix[j+1,9] = tp / (tp + fn)              # recall
  eva_matrix[j+1,10]= 2 * tp / (2 * tp + fp + fn) # f-measure
}
eva_matrix[1,2] = 1 - sum(income_te[,"class"] == " >50K") / N_te
eva_matrix[1,3] = 0
eva_matrix[1,4] = 1
eva_matrix[1,5] = 1

# uncomment the following lines to plot ROC curve
#plot(spline(eva_matrix[,4], eva_matrix[,5], n = 1000),
#     type = "l",xlim = c(0,1), ylim = c(0,1), xlab = "FPR", ylab = "TPR",
#     main = "ROC Curve")
#lines(x=c(0,1),y=c(0,1),lty = 2)
#legend(x=0.6,y=0.45, legend=c("kNN","Random Guess"),col=c("black","black"),lty=c(1:2))


##################outputs###########################
print("doing classification...")

result_matrix = matrix(nrow = N_te, ncol = 3)
result_matrix[,1] = income_te[,"class"]
for (i in 1:N_te) {
  table_test <- table(class_matrix[i, 1:optk])
  predict_name <- which.max(table_test)
  result_matrix[i,2] = names(predict_name)
  result_matrix[i,3] = round(max(table_test) / optk, 3)
}
colnames(result_matrix) <- c("actual-class", "predicted-class", "posterior")

print("writing classification result to results_income.csv")
write.table(result_matrix, "results_income.csv", sep = "," ,quote = FALSE, row.names = FALSE)
print("Done!")



