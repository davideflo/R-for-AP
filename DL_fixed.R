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
    id <- paste0(step,"|",da)
    tryCatch(
      {
        start <- Sys.time()
        
        name1 <- paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\trainset_step_",step,"_dayahead_",da,".csv")
        name2 <- paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\testset_step_",step,"_dayahead_",da,".csv")  
        
        trainseth2o <- h2o.importFile(name1)
        testseth2o <- h2o.importFile(name2)
        
        predictors <- setdiff(names(trainseth2o), response)
        
        modelname <- paste0("model_",step,"_",da)
        
        model <- h2o.deeplearning(x = predictors, y = response, training_frame = trainseth2o, model_id = id, validation_frame = testseth2o, standardize = TRUE,
                                  activation = "Rectifier", hidden = c(2188,365,52,12,6), epochs = 100, max_w2 = 100, l1=1e-5)
        
        #assign(modelname, model)
        
        h2o.saveModel(model, "C:\\Users\\utente\\Documents\\PUN\\fixed\\models", force = FALSE)

        df <- data.frame(as.character(id), h2o.r2(model, train = TRUE, valid = TRUE), h2o.mse(model, train = TRUE, valid = TRUE))
        res <- bind_rows(res, df)
                
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
        missed <- c(missed, id)
        print(paste("day ahead", da, "and step", step, "failed"))
        
      }
    )
  }
}

xlsx::write.xlsx(res,"performance_DLFixed_models.xlsx", row.names = TRUE, col.names = TRUE)
write.table(as.data.frame(missed), "missed_models.txt")