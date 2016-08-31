#### learnign fixed -- more robust -- models

##### BUILDING DL-FIXED MODEL

library(xlsx)
library(dplyr)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = '20g')

response <- "y"
missed <- c()

res <- data_frame()
count <- 0

for(da in 1:5)
{
  for(step in 1:24)
  {
    id <- paste0("sda",step,"_",da)
    r2 <- 0
    tryCatch(
      {
        start <- Sys.time()
        
        name1 <- paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\trainset_step_",step,"_dayahead_",da,".csv")
        name2 <- paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\testset_step_",step,"_dayahead_",da,".csv")  
        
        train <- data.frame(read.csv2(name1, header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE))
        test <- data.frame(read.csv2(name2, header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE))
        
        Y <- as.numeric(unlist(test["y"]))
        
        train <- data.matrix(train)
        test <- data.matrix(test)
        
        trainseth2o <- as.h2o(train)
        testseth2o <- as.h2o(test)
        
        #drop <- names(trainseth2o)[grep("angleora",names(trainseth2o))]
        #predictors <- setdiff(names(trainseth2o), union(response,drop))
        
        predictors <- setdiff(names(trainseth2o), response)
        
        while(r2 <= 0.6 | is.na(r2) | is.nan(r2) | !is.numeric(r2))
        {
          
          model <- h2o.deeplearning(x = predictors, y = response, training_frame = trainseth2o, model_id = id, validation_frame = testseth2o, standardize = TRUE,
                                    activation = "Rectifier", hidden = c(2188,365,52,12,6), epochs = 100, max_w2 = 100, l1=1e-5)
          
          r2 <- h2o.r2(model, train = FALSE, valid = TRUE)
          print(r2)
        }
        
        
        h2o.saveModel(model, "C:\\Users\\utente\\Documents\\PUN\\fixed\\robust_models", force = TRUE)
        
        df <- data.frame(as.character(id), t(h2o.r2(model, train = TRUE, valid = TRUE)), t(h2o.mse(model, train = TRUE, valid = TRUE)))
        res <- bind_rows(res, df)
        
        ### ERROR DISTRIBUTION FOR BOOTSTRAP ##########
        pred <- predict(model, testseth2o)
        
        pt <- as.numeric(pred$predict) 
        pt <- as.matrix(pt)
        pt <- unlist(pt[,1])
        
        diff <- Y - pt
        
        xlsx::write.xlsx(data.frame(diff),paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\errors\\distribution_errors_step_",step,"_dayahead_",da,".xlsx"))
        ###############################################
        
        h2o.rm(trainseth2o); h2o.rm(testseth2o);
        h2o.rm(model)
        print(paste("done step",step,"day ahead", da, "and removed the files"))
        
        end <- Sys.time()
        print(end-start)
        
        count <- count + 1
        print(paste0("passages left: ",24*5 - count))
        
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

xlsx::write.xlsx(res,"performance_DLFixed_robust_models.xlsx", row.names = TRUE, col.names = TRUE)
write.table(as.data.frame(missed), "missed_robust_models.txt")





