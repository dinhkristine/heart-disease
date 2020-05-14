

source("R/data.R")



rp_par <- c(0.60, 0.70, 0.80, 0.90, 0.99)

single_vars <- setdiff(names(data), c("dx_heart", "rest_bp_bins", "chol_bins", "max_heart_rate_bins", "oldpeak_bins"))

nrounds <- 10

#### EXPLORE MODEL DEVELOPMENT ---- 

#' Bootstrap with GLM
#'
#' @param df dataframe of all variables 
#' @param rp random partition 
#' @param yvar a character string of target variable
#' @param xvars a character string of independent variable 
#' @param nrounds an integer of how many time we want to run the model
#'
BootstrapGLM <- function(df, rp, yvar, xvars, nrounds){
  
  # select varaibles
  df %<>% select(y = yvar, xvars)
  
  all_auc <- c()
  all_aic <- c()
  all_iteration <- list()
  index <- 1
  
  for(each_rp in rp){
    for (i in 1:nrounds){
      # split train and test randomly based on proportion
      n <- floor(each_rp * nrow(df))
      
      train_ind <- sample(seq_len(nrow(df)), size = n)
      
      train <- df[train_ind, ]
      
      test <- df[-train_ind, ]
      
      # build model 
      fit <- glm(y ~ ., data = train, family = "binomial")
      
      # predict 
      preds <- predict(fit, newdata = test, type = "response")
      
      test$preds <- preds
      
      # Validate AUC  
      test_roc <- pROC::roc(response = test$y, predictor = test$preds)
      
      auc <- pROC::auc(test_roc)[[1]]
      
      all_auc <- c(all_auc, auc)
      all_aic <- c(all_aic, fit$aic)
    }
    # iteration log 
    all_iteration[[index]] <- data.frame(iteration_date = Sys.time(), 
                                         model_type = "glm",
                                         distibution = "binomial",
                                         random_partition = each_rp, 
                                         nrow_train = nrow(train), 
                                         nrow_test = nrow(test),
                                         nrounds = nrounds,
                                         target = yvar, 
                                         n_features = length(xvars),
                                         features = paste(xvars, collapse = ","), 
                                         AIC = mean(all_aic),
                                         auc = mean(all_auc))
    index <- index + 1
  }
  all_iteration %<>% bind_rows()
  
  return(all_iteration)
}

SafelyBootstrapGLM <- safely(BootstrapGLM)


#' Loop Bootstrap GLM
#'
#' @param df dataframe of all variables 
#' @param rp random partition 
#' @param yvar a character string of target variable
#' @param xvars a character string of independent variable 
#' @param nrounds an integer of how many time we want to run the model
#' @param list_of_xvars a list 
#' @param number_of_xvars a character vector of independent varaibles
#'
LoopAll <- function(df, rp, yvar, xvars, nrounds, list_of_xvars, number_of_xvars){
  # group variables
  vars <- combn(list_of_xvars, number_of_xvars)
  
  # how many group do we have?
  no_vars <- dim(vars)[[2]]
  
  # initiate print list and index 
  print_list <- list()
  k <- 1
  
  for (i in 1:no_vars) {
    iteration_log <- SafelyBootstrapGLM(df, rp, yvar, xvars = vars[,i], nrounds)
    print_list[[k]] <- iteration_log
    k <- k + 1
  }
  # map all separate log to bind them together into a dataframe 
  iteration_log <- map(print_list, "result") %>% rbind_list()
  
  return(iteration_log)
}


#### bootstrapping ----

print_list_iteration_log <- list()
index <- 1

for (num_of_vars in 1:length(single_vars)){
  iteration_log <- LoopAll(df = data, 
                           rp = rp_par, 
                           yvar = "dx_heart", 
                           nrounds = nrounds, 
                           list_of_xvars = single_vars, 
                           number_of_xvars = num_of_vars)
  print_list_iteration_log[[num_of_vars]] <- iteration_log
}

# bind all iteration log into a single dataframe 

list_iteration_log <- print_list_iteration_log %>% bind_rows()






