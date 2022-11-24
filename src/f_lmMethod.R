#
#
# Script que devuelve la asignación a fase
#
#

# data tienen que tener los datos de los CNS y las tres columnas f1, f2, f3 
# para la asignación a fase.
# limit01 es por si queremos limitar los coeficientes a (0,1)

lmLinePredictor <- function(data
                             , onlyPositives = TRUE, robust = FALSE , limit01 = FALSE
                             , wt = NULL, wp = 1
                             , spv = NULL){
  
  # onlyPositives <- TRUE
  # robust <- FALSE
  # limit01 <- FALSE
  # wt <- NULL
  # wp <- 1
  
  require(dplyr)

  if(is.null(wt)){
    wt = rep(0,nrow(data))
  }
  
  if(!limit01){
    
    f <- as.formula( paste(spv, ' ~ '
                            , paste(colnames(data)[!colnames(data) %in% c(spv) ]
                                    ,collapse=' + '
                            )
                            , '-1' 
                            )
                      )
    
    if(robust){
      require(MASS,include.only = "rlm", quietly = TRUE)
      m <- rlm(f, data, maxit = 1e4, weights = (1-wt)^wp )
      coef = 'Value'
      
    }else{
      m <- lm( f, data, weights = (1-wt)^wp )
      coef = 'Estimate'
    }
    
    if(onlyPositives){
      
      # m
      {
        data1 <- data
        continuef <- TRUE
        mcoeff <- m$coefficients
        mcoeff[which(is.na(mcoeff))] <- 0
        while(! all(mcoeff>=0) & continuef ){
          
          data1 <- data1[,-which(mcoeff < 0)]
          
          if(ncol(data1) > 3){
            
            f <- as.formula( paste(spv, ' ~ '
                                    , paste(colnames(data1)[!colnames(data1) %in% c(spv) ]
                                            ,collapse=' + '
                                    )
                                    , '-1'
            )
            )
            
            if(robust){
              m <- rlm(f, data1, weights = (1-wt)^wp )
              mcoeff <- m$coefficients
              mcoeff[which(is.na(mcoeff))] <- 0
            }else{
              m <- lm(f, data1, weights = (1-wt)^wp )
              mcoeff <- m$coefficients
              mcoeff[which(is.na(mcoeff))] <- 0
            }
            
          }else{
            m <- NULL
            continuef <- FALSE
          }
          
        }
      }

    }
    
    # added significance level
    {
      #m1
      {
        if(!is.null(m)){
          mS <- summary(m)
          mS <- data.frame(rbind(mS$coefficients[,c(coef,"t value")]))
          names(mS) <- c("estimate","t")
          mS$p <- 2 *pt(abs(mS$t), df = nrow(data)-ncol(data)+3 , lower.tail = FALSE)
          mS$sig <- NA                             # Named vector with significance sig
          mS$sig[mS$p < 0.1] <- "."
          mS$sig[mS$p < 0.05] <- "*"
          mS$sig[mS$p < 0.01] <- "**"
          mS$sig[mS$p < 0.001] <- "***"
          mS$sig[is.na(mS$sig)] <- "-"
          mS$cod_meter <- rownames(mS)
        }

      }

    }

    # juntamos todo
    {
      
      lmRes <- data.frame( cod_meter = colnames(data)[!colnames(data) %in% c(spv)] )
      
      if(!is.null(m)){
        
        lmRes <- lmRes %>% 
          left_join(mS, by = "cod_meter") %>% 
          rename(f = estimate
                 , sig = sig) %>% 
          select(-p,-t) 
        
      }else{
        lmRes <- lmRes %>% 
          mutate(f = 0, sig = "-")
      }
      

      lmRes <- lmRes %>% 
        replace_na(list(f = 0, sig = "-")) %>% 
        column_to_rownames("cod_meter")
    }

    return(lmRes)
    
  }
  
  # limit 01
  {
    # if(limit01){
    #   
    #   #glmnet a pelo
    #   library(glmnet)
    #   
    #   nf1 = which(names(data) == "f1")
    #   nf2 = which(names(data) == "f2")
    #   nf3 = which(names(data) == "f3")
    #   
    #   X_train = data[,1:(nf1-1)]
    #   f1 = data[,nf1]
    #   f2 = data[,nf2]
    #   f3 = data[,nf3]
    #   
    #   lambdas <- 10^seq(2, -3, by = -0.01)
    #   
    #   
    #   # this involves tuning a hyperparameter, lambda. The code above runs the 
    #   # glmnet() model several times for different values of lambda.
    #   # We can automate this task of finding the optimal lambda value using the 
    #   # cv.glmnet() function.
    #   {
    #     #
    #     # f1
    #     # 
    #     {
    #       if( any(f1!= 0) ){
    #         
    #         cv_lasso <- cv.glmnet(data.matrix(X_train), f1
    #                               , intercept = FALSE
    #                               , alpha = 0.5
    #                               # , upper.limits	= 1
    #                               , lower.limits = 0
    #                               , lambda = lambdas
    #                               , parallel = FALSE
    #         )
    #         
    #         optimal_lambda <- cv_lasso$lambda.min
    #         optimal_lambda
    #         
    #         
    #         lasso_regf1 = glmnet(X_train, f1
    #                              , family = 'gaussian'
    #                              , intercept = FALSE
    #                              # , nlambda = 100
    #                              , alpha = 0.5
    #                              , lambda = optimal_lambda
    #                              # , upper.limits	= 1
    #                              , lower.limits = 0
    #         )
    #         
    #         coefF1 <- lasso_regf1$beta
    #       }else{
    #         coefF1 <- rep(0,dim(X_train)[2])
    #       }
    #     }
    #     
    #     #
    #     # f2
    #     # 
    #     {
    #       if( any(f2!= 0) ){
    #         
    #         cv_lasso <- cv.glmnet(data.matrix(X_train), f2
    #                               , intercept = FALSE
    #                               , alpha = 0.5
    #                               # , upper.limits	= 1
    #                               , lower.limits = 0
    #                               , lambda = lambdas
    #                               , parallel = FALSE
    #         )
    #         
    #         optimal_lambda <- cv_lasso$lambda.min
    #         optimal_lambda
    #         
    #         
    #         lasso_regf2 = glmnet(X_train, f2
    #                              , family = 'gaussian'
    #                              , intercept = FALSE
    #                              # , nlambda = 100
    #                              , alpha = 0.5
    #                              , lambda = optimal_lambda
    #                              , upper.limits	= 1
    #                              , lower.limits = 0
    #         )
    #         
    #         coefF2 <- lasso_regf2$beta
    #       }else{
    #         coefF2 <- rep(0,dim(X_train)[2])
    #       }
    #     }
    #     
    #     #
    #     # f3
    #     # 
    #     {
    #       if( any(f3!= 0) ){
    #         
    #         cv_lasso <- cv.glmnet(data.matrix(X_train), f3
    #                               , intercept = FALSE
    #                               , alpha = 0.5
    #                               , upper.limits	= 1
    #                               , lower.limits = 0
    #                               , lambda = lambdas
    #                               , parallel = FALSE
    #         )
    #         
    #         optimal_lambda <- cv_lasso$lambda.min
    #         optimal_lambda
    #         
    #         
    #         lasso_regf3 = glmnet(X_train, f3
    #                              , family = 'gaussian'
    #                              , intercept = FALSE
    #                              # , nlambda = 100
    #                              , alpha = 0.5
    #                              , lambda = optimal_lambda
    #                              , upper.limits	= 1
    #                              , lower.limits = 0
    #         )
    #         
    #         coefF3 <- lasso_regf3$beta
    #       }else{
    #         coefF3 <- rep(0,dim(X_train)[2])
    #       }
    #     }
    #     
    #     result <- data.frame(as.matrix(cbind( coefF1, coefF2, coefF3)) )
    #     names(result) <- c("f1","f2","f3") 
    #     
    #     return(round(result,3))
    #     
    #     
    #   }
    #   
    #   # suppressWarnings(
    #   #   rm(nf1,nf2,nf3,X_train,f1,f2,f3
    #   #      ,lasso_regf1,lasso_regf2,lasso_regf3
    #   #      , lambdas,optimal_lambda, cv_lasso
    #   #      , coefF1, coefF2, coefF3)
    # }
  }

  
}