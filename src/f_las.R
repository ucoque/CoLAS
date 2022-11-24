#'
#'
#' @title Script de Asignación de contadores a fase 
#' 
#' @author UCT
#'
#'

LAS <- function( LPdata 
                 , significanceLvl = 0.95
                 , step = 0.05 # increasing allowed difference step size
                 , mda = 0.5   # max difference allowed between coefficientes and 1.
                 , tip_tension = "B02"
                 , c = 1       # coeficiente multiplicador por ajuste de perdidas a la hora de restar lp, sin uso
                 # opciones estimación
                 , onlyPos = TRUE        # fijo
                 , robusto = FALSE       # fijo
){
  
  # # opciones de asignación
  # tip_tension = "B02"
  # significanceLvl = 0.95
  # step = c(0.25,0.125,0.075,0.05,0.025)
  # mda = 0.5
  # # opciones estimación
  # onlyPos = TRUE        # fijo

  significanceLvl <- case_when( significanceLvl == 0.999 ~ list(c("***"))
                                , significanceLvl == 0.99 ~ list(c("**","***"))
                                , significanceLvl == 0.95 ~ list(c("*","**","***")) 
                                , significanceLvl == 0.90 ~ list(c(".","*","**","***")) 
                                , TRUE ~ list(c("***")) )[[1]]
  
  # definimos los pasos que se van a seguir
  {
    if(length(step)>1){
      if(sum(step)<mda){
        message("/!\\ the sum of the steps (",sum(step),") must be higher than mda (",mda,")")
        return(NULL)
      }
    }else{
      step = rep(step,mda/step +1)
    }
  }

  
  source(here("src","f_lmMethod.R"))
  
  #number of SPVs
  nSpv = (ncol(LPdata$SPV)-1)
  spvs = LPdata$spvs
  
  if(tip_tension == "B02"){
    
    regData <- LPdata$CN %>% 
      inner_join(LPdata$SPV, by = "FH") %>% 
      # select(-one_of("METER_ID")) %>%
      column_to_rownames("FH") 
    
  }else{
    # si la tension es B01, dividimos entre dos el consumo porque se conectan a 
    # fase y fase en vez de fase y neutro y la mitad del consumo va para cada fase.
    
    regData <- LPdata$CN %>% 
      mutate_if(is.numeric, funs(. / 2)) %>%  
      inner_join(LPdata$SPV, by = "FH") %>% 
      # select(-one_of("METER_ID")) %>%
      column_to_rownames("FH") 
  }

  cnData <- LPdata$CnData
  cnData$line <- NA

  # si tenemos suficientes registros iteramos.
  if(nrow(regData)>ncol(regData)-nSpv*3){
    
    # variables de iniciación
    {
      m = 1
      allowDif = step[1]
      
      sigLvl <- significanceLvl
      skipLM = FALSE
      rslist = list()
      preds <- list()
    }
    
    while(ncol(regData)>3 & (allowDif<= mda) ){
      
      # tenemos que interar esto para cada sueprvisor y luego juntar y aplicar la logica
      for(spv in spvs){
      
        removeMeters = spvs[-which(spvs %in% spv)]
      
        if(!skipLM){
          pred <- lmLinePredictor(data = regData %>% select(-contains(removeMeters))
                                   , spv = spv
                                   , onlyPositives = onlyPos
                                   )
          preds[[spv]] <- pred
        }
        
        #
        # aplicamos logica de asignación a fase
        #
        asignacion <- preds[[spv]] %>% 
          mutate(asig_f = ifelse((f >= 1-allowDif & f <= 1+allowDif),TRUE,FALSE) & 
                   ifelse(sig %in% sigLvl, TRUE, FALSE) # & ( sig1p >= sig2p & sig1p >= sig3p )
                 
          )
        
        names(asignacion) = paste0(spv,"_",names(asignacion ) )
        
        asignacion$CN = rownames(asignacion)
        
        rslist[[spv]] = asignacion
      
      }
      
      if(length(rslist)==1 ){
        allAsig = rslist[[1]]
        rownames(allAsig) <- NULL
      }else{
        
        allAsig = rslist[[1]]
        
        for(l in 2:length(rslist)){
          allAsig = merge(allAsig,rslist[[l]],by = "CN")
        }
      }

      #
      # sacamos todos los CN que se asignan a alguna fase
      #
      allAsig <- allAsig %>% 
        select(contains("asig"),"CN") %>%
        mutate(sumVar = select(., contains("asig")) %>% rowSums()) %>% 
        rename_all(~str_replace_all(.,"_asig_f","")) %>% 
        filter(sumVar == ifelse(tip_tension == "B02",1,2) ) %>% 
        column_to_rownames("CN") %>% 
        select(-sumVar)

      # cuales van a cada fase
      f_asig = list()
      for(spvFase in colnames(allAsig)){
        f_asig[[spvFase]] = rownames(allAsig)[allAsig[,spvFase]]
      }
      
      
      # si hay alguno...
      if( length(unlist(f_asig))>0 ){
        
        for(spvFase in names(f_asig)){
          # sumamos sus consumos
          {
            spvFaseLP = 0
            
            if( length(f_asig[[spvFase]]) > 1 ){
              spvFaseLP <- rowSums(regData[, f_asig[[spvFase]] ] )
            }else{
              if( length(f_asig[[spvFase]]) > 0 ){
                spvFaseLP <- regData[, f_asig[[spvFase]] ]
              }else{
                spvFaseLP <- 0
              }
            }
          }
          
          # se los restamos a cada fase
          regData[,spvFase] <- regData[,spvFase] - spvFaseLP
          
          # actualizamos en la tabla cnData
          if(length(f_asig[[spvFase]]) > 0){
            cnData$line[which(rownames(cnData) %in% f_asig[[spvFase]]) ] <- 
              ifelse(is.na(cnData$line[which(rownames(cnData) %in% f_asig[[spvFase]]) ] )
                     , spvFase
                     , paste0(cnData$line[which(rownames(cnData) %in% f_asig[[spvFase]]) ],"-",spvFase)
              )
          }
        }
        
        # actualizamos regData
        regData <- regData %>% select(-unlist(f_asig))
        
        message(length(unlist(f_asig))/ifelse(tip_tension=="B01",2,1), " asignados")
        
        m = 1
        allowDif = step[m]
        skipLM = FALSE
        
      }else{
        
        m = m+1
        allowDif = allowDif+step[m]

        message("incrementamos elasticidad: ",round(allowDif,3) )
        skipLM = TRUE
        
      }
      
      # all( regData[,c("f1","f2","f3")]>0 )
    }
    
    rs <- list()
    rs$cnData <- cnData
    rs$endType <- "ok"
    
    return(rs)
    
  }else{
    
    message("/!\\ Not enough samples, there are more Cns than samples")
    
    rs <- list()
    rs$cnData <- cnData
    rs$endType <- "Nok"
    rs$cause <- "needMoreSamples"
    
    return(rs)
  }
  
  
}