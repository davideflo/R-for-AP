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

  for(step in 10:24)

  {

    id <- paste0(step,"|",da)

    tryCatch(

      {

        start <- Sys.time()

        

        name1 <- paste0("/Users/davidefloriello/Dataset2-master/trainset_step_",step,"_dayahead_",da,".csv")



        train <- read.csv2(name1, header=TRUE, sep=",", colClasses="character",stringsAsFactor=FALSE)

        trainseth2o <- as.h2o(train)



        

        predictors <- setdiff(names(trainseth2o), response)

        

        modelname <- paste0("model_",step,"_",da)

        

        model <- h2o.deeplearning(x = predictors, y = response, training_frame = trainseth2o, model_id = id, standardize = TRUE,

                                  activation = "Rectifier", hidden = c(2188,365,52,12,6), epochs = 100, max_w2 = 100, l1=1e-5)

        

        #assign(modelname, model)

        

        h2o.saveModel(model, force = FALSE)



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
