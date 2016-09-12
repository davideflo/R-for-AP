###### train glm_fixed_model #######
library(xlsx)
library(dplyr)
library(h2o)
###### from command line: Tools --> shell --> Rscript. Otherwise it doesn't work
h2o.init(nthreads = -1, max_mem_size = '20g')

response <- "y"
missed <- c()

res <- res2 <- data_frame()
count <- 0

for(da in 0:2)
{
  for(rh in 1:24)
  {
    th_a <- c(2:24,1)
    th <- th_a[rh]
    id <- paste0("rhda",rh,"_",da)
    id2 <- paste0("bis_rhda",rh,"_",da)
    r2 <- r22 <- counter <- counter2 <- 0
    tryCatch(
      {
        start <- Sys.time()
        
        name1 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\trainset_completo_rh_",rh,"_th_",th,"_dayahead_",da,".csv")
        name2 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\trainset2_rh_",rh,"_th_",th,"_dayahead_",da,".csv")
        name3 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\testset_rh_",rh,"_th_",th,"_dayahead_",da,".csv")  
        
        train <- data.frame(read.csv2(name1, header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE))
        train2 <- data.frame(read.csv2(name2, header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE))
        test <- data.frame(read.csv2(name2, header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE))
        
        Y <- as.numeric(unlist(test["y"]))
        
        train <- data.matrix(train)
        train2 <- data.matrix(train2)
        test <- data.matrix(test)
        
        trainseth2o <- as.h2o(train)
        trainset2h2o <- as.h2o(train2)
        testseth2o <- as.h2o(test)
        
        predictors <- setdiff(names(trainseth2o), response)
        
        while(r2 <= 0.5 | is.na(r2) | is.nan(r2) | !is.numeric(r2) | counter <= 20)
        {
          
          model <- h2o.glm(x = predictors, y = response, training_frame = trainseth2o, model_id = id, validation_frame = testseth2o, standardize = TRUE,
                           family = "gaussian", alpha = 0, lambda = 0.01, lambda_search = TRUE, intercept = TRUE)

                    
          r2 <- h2o.r2(model, train = FALSE, valid = TRUE)
          print(paste("r2 modello completo:",r2))
          counter <- counter + 1
        }
        
        while(r22 <= 0.5 | is.na(r2) | is.nan(r2) | !is.numeric(r2) | counter <= 20)
        {
          
          model2 <- h2o.glm(x = predictors, y = response, training_frame = trainset2h2o, model_id = id2, validation_frame = testseth2o, standardize = TRUE,
                            family = "gaussian", alpha = 0, lambda = 0.01, lambda_search = TRUE, intercept = TRUE)
          
          
          r22 <- h2o.r2(model2, train = FALSE, valid = TRUE)
          print(paste("r2 modello 2:",r22))
          counter2 <- counter2 + 1
        }
        
        h2o.saveModel(model, "C:\\Users\\utente\\Documents\\PUN\\glm\\models", force = TRUE)
        h2o.saveModel(model2, "C:\\Users\\utente\\Documents\\PUN\\glm\\models", force = TRUE)
        
        df <- data.frame(as.character(id), t(h2o.r2(model, train = TRUE, valid = TRUE)), t(h2o.mse(model, train = TRUE, valid = TRUE)))
        df2 <- data.frame(as.character(id2), t(h2o.r2(model2, train = TRUE, valid = TRUE)), t(h2o.mse(model2, train = TRUE, valid = TRUE)))
        res <- bind_rows(res, df)
        res2 <- bind_rows(res2, df2)
        
        ### ERROR DISTRIBUTION FOR BOOTSTRAP - MODEL COMPLETO - ##########
        pred <- predict(model, testseth2o)
        
        pt <- as.numeric(pred$predict) 
        pt <- as.matrix(pt)
        pt <- unlist(pt[,1])
        
        diff <- Y - pt
        
        xlsx::write.xlsx(data.frame(diff),paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\errors\\completo_distribution_errors_rh_",rh,"_dayahead_",da,".xlsx"))
        ###############################################
        ### ERROR DISTRIBUTION FOR BOOTSTRAP - MODEL 2 - ##########
        pred2 <- predict(model2, testseth2o)
        
        pt2 <- as.numeric(pred2$predict) 
        pt2 <- as.matrix(pt2)
        pt2 <- unlist(pt2[,1])
        
        diff2 <- Y - pt2
        
        xlsx::write.xlsx(data.frame(diff2),paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\errors\\distribution_errors_rh_",rh,"_dayahead_",da,".xlsx"))
        ###############################################
        
        
        h2o.rm(trainseth2o); h2o.rm(testseth2o); h2o.rm(trainset2h2o)
        h2o.rm(model); h2o.rm(model2)
        print(paste("done rh",rh,"day ahead", da, "and removed the files"))
        
        end <- Sys.time()
        print(end-start)
        
        count <- count + 1
        print(paste0("passages left: ",24*3 - count))
        
      }, error = function(cond)
      {
        
        message(cond)
        traceback()
        missed <- c(missed, id)
        print(paste("day ahead", da, "and step", step, "failed"))
        
      }
    )
  }
}

xlsx::write.xlsx(res,"performance_glmFixed_models.xlsx", row.names = TRUE, col.names = TRUE)
write.table(as.data.frame(missed), "missed_glm_models.txt")
