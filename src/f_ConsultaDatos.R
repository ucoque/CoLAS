#'
#'
#' @title Script de consulta de datos del stg 
#'
#'
#' @description se trata de una versión modificada del consultaDatos 1 y 2, con 
#' la que se tratan las diferentes fases de os diferentes concentradores en un
#' mismo ct como variables a las que se puede asignar. de modo que si antes 
#' obteníamos f1,f2 y f3, ahora tendremos f1,f2 y f3 x nº de concentradores, y 
#' por tanto las fases posibles se multiplica por el nº de concentradores.
#' Para ello, indicaremos la posicion con un 0.
#' 
#' @author UCT

ConsultaDatos <- function(COD_CT = NULL, posicion = 0
                          , from = '01/01/2021' , to = '01/04/2022'
                          , dynamicFrom = TRUE
                          , maxLoss = 0.5
                          , minLoss = 0.005
                          , allowNegDif = FALSE # para permitir LP_cn > LP_spv
                          , rmTriPhaseNoise = TRUE, rmOnlyTriphaseCn = FALSE
                          # , save = FALSE         # guardamos ficheros?
                          , overWrite = FALSE    # sobreescribimos en caso de q existan?
){

  # parametros Query
  COD_CT = 10200580
  posicion = 0
  dynamicFrom = TRUE
  from = '01/01/2021'
  to = '01/11/2022'
  maxLoss = 0.5
  minLoss = -0.2
  allowNegDif = FALSE
  # limpieza trifasicos
  rmTriPhaseNoise = FALSE
  rmOnlyTriphaseCn = TRUE
  # opciones de guardado
  save = TRUE
  overWrite = FALSE
  
  save = TRUE
  
  # cargamos librerias
  libs = c('tidyverse', 'here','lubridate')
  sapply(libs[!libs %in% installed.packages()], install.packages)
  suppressPackageStartupMessages( sapply(libs, library, character.only = T) )
  
  #checks
  {
    if( is.null(COD_CT) ){
      message("ERROR, write a CT num")
      return(NA)
    }
    if( !is.numeric(COD_CT) ){
      message("ERROR, write a CT num")
      return(NA)
    }
    if( is.null(posicion) ){
      message("ERROR, write a NUM_POSICION")
      return(NA)
    }
    if( !is.numeric(posicion) ){
      message("ERROR, write a NUM_POSICION")
      return(NA)
    }
    if( !is.null(maxLoss) ){
      if( !is.numeric(maxLoss)){
        message("ERROR, Loss should be a number from 0 to 1.")
        return(NA)
      }
      if(maxLoss >1 | maxLoss <0){
        message("ERROR, Loss should be a number from 0 to 1.")
        return(NA)
      }
  }
  
  }
  
  source(here::here("utils","STG.conn.R"))
  
  # calculamos las posiciones que tiene
  if(posicion == 0){
    poss <- tryCatch(
      {
        dbGetQuery(STG
                   , paste0("SELECT distinct num_posicion
                             FROM stg.stg_meter_cgp_ct
                             WHERE nvl(COD_CT_SIC,COD_CT_STG) = ", as.numeric(COD_CT)
                            , " AND num_posicion != 0
                             AND num_posicion is not null") )
      },
      error=function(cond) {
        message("Error when getting CT positions")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },
      warning=function(cond) {
        message("Warning when getting CT positions")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      finally={
        Sys.sleep(2)
      }
    )  

    posSQL <- paste0("(", paste0(poss$NUM_POSICION, collapse = ", "), ")")
    
    
    
  }else{
    posSQL <- posicion
    # poss <- data.frame(NUM_POSICION = posicion)
  }
  
  # calculamos fecha de inicio
  if(dynamicFrom){
    From <- tryCatch(
      {
        dbGetQuery(STG
                   , paste0("SELECT min(trunc(FH,'month')) as FH
                            FROM stg.stg_spv_s52
                            WHERE LVSLINE in (  select cod_meter
                                                from  stg.stg_meter_cgp_ct c
                                                where COD_CT_STG = ", as.numeric(COD_CT), "
                                                  AND num_posicion in ", posSQL,"
                                                  AND num_posicion is not null
                                                  AND tip_meter = 'LVS'
                                            )") )
      },
      error=function(cond) {
        message("Error when getting From date")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },
      warning=function(cond) {
        message("Warning when getting From date")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      finally={
        Sys.sleep(2)
      }
    )  
    
  }
  
  # en cas  o de guardar se define la carpeta donde dejar los ficheros.
  if(save){
    carpetas <- list.files(here::here("Data","CTs"))
    
    # carpeta a crear (cac)
    if(posicion == 0){
      cac <- paste0(COD_CT,"_all")
    }else{
      cac <- paste0(COD_CT,"_",posicion)
    }
    
    if( !(cac %in% carpetas) ){
      dir.create(here::here("Data","CTs",cac))
    }
    
    # definimos la ruta de trabajo para guardar ficheros
    path = here::here("Data","CTs",cac)
    # 
    rm(carpetas,cac)
    
  }
  
  #
  # Cogemos las medidas de los contadores, CN
  #
  {
    if(!dynamicFrom){
      From <- as.POSIXct(from,tryFormats = c("%d/%m/%Y" ))
    }else{
      From <- From$FH
    }
      
    To   <- as.POSIXct(to  ,tryFormats = c("%d/%m/%Y" ))
    
    medidasCNs <- data.frame()
    
    while( From < To ){
      
      if(From + months(1)<To){
        fromAux <- as.character(From, format = "%d/%m/%Y")
        toAux <- as.character(From + months(1), format = "%d/%m/%Y")
        
        fromFN <- paste0(year(From)
                         , paste0(rep(0,2-nchar(month(From))),month(From))
                         , paste0(rep(0,2-nchar(day(From))),day(From)) )
        
        toFN <- paste0(year(From + months(1))
                       , paste0(rep(0,2-nchar(month(From + months(1)))),month(From + months(1)))
                       , paste0(rep(0,2-nchar(day(From + months(1)))),day(From + months(1))) )
        
      }else{
        fromAux <- as.character(From, format = "%d/%m/%Y")
        toAux <- as.character(To, format = "%d/%m/%Y")
        
        fromFN <- paste0(year(From)
                         , paste0(rep(0,2-nchar(month(From))),month(From))
                         , paste0(rep(0,2-nchar(day(From))),day(From)) )
        
        toFN <- paste0(year(To)
                       , paste0(rep(0,2-nchar(month(To))),month(To))
                       , paste0(rep(0,2-nchar(day(To))),day(To)) )
      }
      
      message("Getting Data... from: ",fromAux," --> to: ", toAux)
      
      if(save){
        
        # nombre del fichero para estas fechas
        fileName = paste0(fromFN,"_",toFN,".rds")
        
        # listo los ficheros
        ficheros <- list.files(path)
        
        if(!fileName %in% ficheros | overWrite){
          
          aux <- NULL
          retry <- 0
          while(is.null(aux) & retry < 5){
            
            aux <- tryCatch(
              {
                query <- paste0("
				   -- sacamos los puntos de suministro del CT-pos
            WITH lvs as(
                   SELECT cod_meter, num_posicion
            	FROM stg.stg_meter_cgp_ct
            	WHERE cod_ct_stg = ", COD_CT,"
                       AND num_posicion IN	", posSQL, "
            		AND TIP_METER = 'LVS'
               )
               , ps AS (
            	SELECT COD_SERVICE_POINT AS cups
            			, COD_METER, tip_meter
            			, fec_baja_cn
            			, nvl(FEC_INSTAL_EQUIPO ,FEC_ALTA_CN) FEC_INSTAL_EQUIPO
            	FROM stg.stg_meter_cgp_ct
            	 WHERE ( cod_ct_stg = ", COD_CT,"
            	  OR COD_CT_sic = ", COD_CT," )
            		AND num_posicion IN ", posSQL, "
            		AND COD_SERVICE_POINT IS NOT NULL
            		AND fec_baja_cn > TO_DATE('",toAux,"','dd/mm/yyyy')
            		AND TIP_METER NOT IN ('SP','LVS')
            )
            -- Sacamos cuales son los contadores
            , CNs AS (
            	SELECT h.COD_SERVICE_POINT AS cups
            			, h.COD_METER
            			, NULL AS tip_meter
            			, h.FEC_BAJA AS fec_baja_cn
            			, NULL AS FEC_INSTAL_EQUIPO
            	FROM stg.stg_meter_cgp_ct_his h
            		JOIN ps p
            			ON h.cod_service_point = cups
            	WHERE fec_baja_cn > TO_DATE('",toAux,"','dd/mm/yyyy')
            	UNION 
            	SELECT * 
            	FROM ps
            )
            -- la Curva de carga del supervisor
            , LVS_LP AS (
            	SELECT FH, CUPS, LVSLINE, AI, AE
            		, AI-AE AS line_VAL_AI
            	FROM stg.STG_SPV_S52 s52
            		JOIN lvs lvs
            			ON s52.LVSLINE = lvs.cod_meter
            	WHERE FH >= TO_DATE('",fromAux,"','dd/mm/yyyy') -1/12
            		AND FH < TO_DATE('",toAux,"','dd/mm/yyyy')
            	)
            -- sacamos por fecha la curva de los CN
            , CN_LP AS (
            	SELECT FH
            		, cn.cups
            		, meter_id
            		, VAL_AI
            		, VAL_AE
            	FROM stg.STG_meter_read_lp mlp
            		JOIN CNs cn
            			ON mlp.meter_id = cn.cod_meter
            	AND mlp.FH >= cn.fec_instal_equipo
            	WHERE FH >= TO_DATE('",fromAux,"','dd/mm/yyyy') -1/12
            		AND FH < TO_DATE('",toAux,"','dd/mm/yyyy')
            )
            --agrupamos por fecha
            , gCN_LP AS (
            	SELECT FH, SUM(VAL_AI-VAL_AE) AS CN_VAL_AI_AE
            	FROM CN_LP
            	GROUP BY fh
            )
            , gLVS_LP AS (
            	SELECT FH, SUM(line_VAL_AI) AS line_VAL_AI
            	FROM LVS_LP
            	GROUP BY fh
            )
            /*
            Juntamos gSPV_LP y gCN_LP y filtramos por la diferencia maxima aceptada para 
            conseguir el listado de las fechas 'validas'.
            */
            , FHs AS (
            	SELECT s.FH
            		, ROUND( (line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) ,3 ) AS loss
            	FROM gCN_LP c
            		JOIN gLVS_LP s
            			ON c.FH = s.FH
            	WHERE ",ifelse(allowNegDif ,"abs",""),"(line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) >= ", minLoss, "
            		AND ",ifelse(allowNegDif ,"abs",""),"(line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) <= ", maxLoss, "
            ) 
            -- calculamos las diferencias hora a hora de los SPVs
            , LVS_lagged AS (
            	SELECT s.FH, s.CUPS, s.LVSLINE 
            		, line_VAL_AI - LAG(line_VAL_AI,1) OVER ( PARTITION BY LVSLINE ORDER BY s.FH ) AS dline_VAL_AI
            		, EXTRACT(HOUR FROM f.FH - LAG(f.FH,1) OVER( PARTITION BY LVSLINE ORDER BY f.FH )) AS diffTime
            	FROM LVS_LP s
            		JOIN FHs f
            			ON s.FH = f.FH
            )
            , CN_lagged AS (
            	SELECT f.FH
            		, cups
            		, meter_id
            		, (VAL_AI-VAL_AE) - LAG(VAL_AI-VAL_AE) OVER ( PARTITION BY cups ORDER BY cn.FH ) AS d_ai_ae
            		, EXTRACT(HOUR FROM f.FH - LAG(f.FH,1) OVER ( PARTITION BY cups ORDER BY f.FH)) AS diffTime
            	FROM CN_LP cn
            		JOIN FHs f
            			ON cn.FH = f.FH
            )
            SELECT FH, cups, meter_id, d_ai_ae
            FROM CN_lagged
            WHERE diffTime = 1
            UNION
            SELECT FH, 'lvs' as cups, LVSLINE AS meter_id, dline_VAL_AI AS d_ai_ae
            FROM LVS_lagged
            WHERE difftime = 1 ")
                
                
                dbGetQuery(STG, query )
              }
              , error=function(cond) {
                
                message(" \n Error on querying CN data ")
                message(cond)
                
                # Choose a return value in case of error
                return(NULL)
                
              }
              , finally={
                
                Sys.sleep(2)
                retry <- retry+1
                
                if(retry > 1){
                  DBI::dbDisconnect(STG)
                  Sys.sleep(sample(2:4,1))
                  STG <- STG.conn()
                }
                
                if(retry==4){
                  DBI::dbDisconnect(STG)
                  message("retried 5 times and could not connect")
                  return(NA)
                }
              }
            )
            
            if(!is.null(aux)){
              if(nrow(aux) == 0){
                aux[1,] <- NA
              }
            }
          }
          
          saveRDS(object = aux, file = here::here(path,fileName) )
          
          message(".- file ", fileName," saved")
          
        }else{
          
          message(".- file ",fileName," already exists, we'll read it")
          
          aux <- readRDS(here::here(path,fileName) )
        }
      }
      
      medidasCNs <- rbind(medidasCNs,aux)
      
      From <- From + months(1)
      
      # flush.console()
    }
    
    # quitamos los NA-s de no registros
    medidasCNs <- medidasCNs %>% 
      na.omit() 
    
    if(nrow(medidasCNs) == 0){
      message("ERROR, 0 rows on LP data")
      
      rs <- list()
      
      rs$CN <- medidasCNs
      rs$SPV <- -1
      rs$CnData <- -1
      
      rs$endType <- "Nok"
      rs$cause <- "0 rows"
      
      return(rs)
    }
    
    # any cups have >1 meter_id?
    # en caso de que un cups tenga más de un meter, machacaremos el viejo cn 
    # por el último.
    # finalmente nos deshacemos del cups
    {
      
      cupsMultiMeter <- medidasCNs %>% 
        select(CUPS,METER_ID) %>%
        filter(CUPS != 'lvs') %>%
        distinct() %>%
        group_by(CUPS) %>%
        tally() %>%
        filter(n>1)
      
      if(nrow(cupsMultiMeter)>0){
        for( c in cupsMultiMeter$CUPS){
          
          lastCN <- medidasCNs %>% 
            filter(CUPS == c) %>%
            filter(FH == max(FH)) %>%
            pull(METER_ID)
          
          medidasCNs[medidasCNs$CUPS == c, ]$METER_ID <- lastCN
          
          
        }
      }
      
      medidasCNs$CUPS <- NULL
    }
    
    
    LPdata <- medidasCNs %>% 
      pivot_wider(id_cols = "FH"
                  , names_from = "METER_ID"
                  , values_from = "D_AI_AE", values_fill = 0, values_fn = sum ) 
    

    message("lecturas, Ok")
    
    
  }
  
  #
  # CnData, tenemos un df con el meter, tipo e indicador de si es trifasico (0,1)
  #
  { 
    if('CnData.rds' %in% ficheros ){
      CnData <- readRDS( file = here::here(path,"CnData.rds") )
      
    }else{
      
      CnData <- NULL
      retry <- 0
      while(is.null(CnData) & retry < 5){
        
        CnData <- tryCatch(
          {
            query <- paste0("select COD_METER
                                        , COD_SERVICE_POINT
                                        , TIP_METER
                                        , IND_TRIFASICO
                                        , NUM_POSICION
                                        , NUM_LINEA_BT
                                      FROM stg.stg_meter_cgp_ct ct
                                      WHERE ( cod_ct_stg = ", COD_CT,"
            	                           OR COD_CT_sic = ", COD_CT," )
                                        AND NUM_POSICION IN ",posSQL,"
                                        AND TIP_METER NOT IN ('SP')
                                        AND fec_baja_cn > sysdate
                                        AND cod_service_point is not null
                                        AND FEC_ULT_LECTURA IS NOT NULL
                                        AND nvl(ind_estadio,4) = 4
                                        order by 1")
            
            
            dbGetQuery(STG, query )
          }
          , error=function(cond) {
            
            message(" \n Error on querying CN data ")
            message(cond)
            
            # Choose a return value in case of error
            return(NULL)
            
          }
          , finally={
            
            Sys.sleep(2)
            retry <- retry+1
            
            if(retry > 1){
              DBI::dbDisconnect(STG)
              Sys.sleep(sample(2:4,1))
              STG <- STG.conn()
            }
            
            if(retry==4){
              DBI::dbDisconnect(STG)
              message("retried 5 times and could not connect")
              return(NA)
            }
          }
        )
      }
      

      rownames(CnData) <- CnData[,"COD_METER"]
      CnData$COD_METER <- NULL
      CnData$status <- "ok"
      
      if(save){
        saveRDS(object = CnData, file = here::here(path,'CnData.rds') )
        
      }
    }
    
    message("datos de contadores, Ok")
    
  }
  
  #
  # miramos que los contadores casen en ambos ficheros
  #
  {
    
    ignoreCols <- CnData %>% filter(TIP_METER == 'LVS') %>% rownames()
    # ignoreCols <- LPdata %>% select(starts_with('MRTL', 'CIRL', 'ZIVL', 'ORML')) %>% colnames()
    
    ignoreCols <- c(ignoreCols,'FH')
    
    noLP <- which(!rownames(CnData) %in% names(LPdata))
    
    if(length(noLP)>0){
      CnData$status[noLP] <- "No LP"
      
      for(r in noLP){
        message("removed ",rownames(CnData)[r],", NO LP")
      }
    }
    
    # miramos en LPdata no haya algún contador que no esté en CnData
    noCnData <- which(!names(LPdata) %in% rownames(CnData) &
                        !names(LPdata) %in% ignoreCols  )
    if(length(noCnData)>0){
      
      message("/!\\ There is a cn in LP data but not in CN data")
      
      LPdata <- LPdata[,-noCnData]
      
    }
    
    suppressWarnings( rm(noLP,r,noCnData) )
    
  }
  
  # quitamos las que sean todo 0s
  {
    read0 <- which(! colnames( LPdata ) %in%
                     colnames( LPdata[, colSums(LPdata != 0) >  0]) &
                   ! colnames(LPdata) %in% ignoreCols )
    
    if(length(read0)>0){
      
      zeroLPs <- which(rownames(CnData) %in% names(LPdata)[read0] )
      
      CnData$status[zeroLPs] <- "0 LP"  
      
      LPdata <- LPdata[,-read0]
      
    }
    
    suppressWarnings( rm(read0,zeroLPs ) )
  }
  
  lvss <- CnData %>% filter(TIP_METER == 'LVS') %>% rownames()
  # lvss <- LPdata %>% select(starts_with('MRTL')) %>% colnames()
  
  CN <- LPdata %>% 
    select(-all_of(lvss ))
  SPV <- LPdata %>% 
    select(all_of(lvss ),"FH" )
  CnData <- CnData %>% filter(TIP_METER != 'LVS')
  
  #
  # Limpieza de trifasicos
  #
  # ignoring trifasic meters noise
  # if(rmTriPhaseNoise){
  #   
  #   rmMeters <- rownames(CnData)[which(CnData$IND_TRIFASICO == 1)]
  #   # por si hay algún trifasico que no tiene lecturas y no está en status "ok"
  #   # lo quitamos
  #   rmMeters <- intersect(rmMeters, rownames(CnData)[CnData$status == "ok"] )
  #   
  #   # rmMeters <- intersect(rmMeters, names(CN))
  #   
  #   if(length(rmMeters)>0){
  #     message("/!\\ removing Triphasic  meters")
  #     
  #     # buscamos un limite, "l", que nos cumpla que tengamos tantos registros como CNs
  #     {
  #       useRows = 0
  #       l = 50
  #       
  #       while(! length(useRows) > (ncol(CN)-1) & l < 400 ){
  #         
  #         useRows = 1:nrow(CN)
  #         
  #         for(met in rmMeters){
  #           
  #           aux <- which(CN[,met] >= -l & CN[,met] <= l)
  #           
  #           useRows <- intersect(useRows,aux)
  #           # message("meter ",met, " removed with COD_FASE = ",cnData[met,"COD_FASE"])
  #           
  #         }
  #         l = l+50
  #       }
  #     }
  #     
  #     useRows = 1:nrow(CN)
  #     
  #     for(met in rmMeters){
  #       
  #       aux <- which(CN[,met] >= -l & CN[,met] <= l)
  #       
  #       useRows <- intersect(useRows,aux)
  #       message("meter ",met, " removed with COD_FASE = RST")
  #       
  #       # lo marcamos en CnData
  #       CnData[met,]$status <- "trif"
  #       
  #     }
  #     
  #     CN <- CN %>%
  #       select(-all_of(rmMeters)) %>%
  #       mutate(rowNum = row_number()) %>%
  #       filter(rowNum %in% useRows) %>%
  #       select(-rowNum)
  #     
  #     SPV <- SPV %>%
  #       mutate(rowNum = row_number()) %>%
  #       filter(rowNum %in% useRows) %>%
  #       select(-rowNum)
  #     
  #     if(exists("M")){
  #       M <- M[rownames(CN),]
  #     }
  #     
  #   }
  #   
  #   # de nuevo, como nos quedamos con menos registros, miramos si alguno tiene menos del 5% informado.
  #   {
  #     read0 <- which(!colnames( CN ) %in%
  #                      colnames( CN [, colSums(CN != 0) >  nrow(CN)*0.05]) )
  #     
  #     # CnData$AllLP0 <- FALSE
  #     if(length(read0)>0){
  #       
  #       rmMeters <- colnames(CN)[read0]
  #       
  #       # añadimos comentario en CnData
  #       CnData[rmMeters,]$status <- "High 0 lp"
  #       
  #       for(r in rmMeters){
  #         message("meter ",r," removed. High 0 lp.")
  #       }
  #       
  #       CN <- CN %>% select(-all_of(rmMeters))
  #       
  #     }
  #   }
  #   
  # }
  # 
  # if(rmOnlyTriphaseCn & !rmTriPhaseNoise){
  #   
  #   
  #   rmMeters <- rownames(CnData)[which(CnData$IND_TRIFASICO == 1)]
  #   rmMeters <- intersect(rmMeters, names(CN))
  #   
  #   for(met in rmMeters){
  #     
  #     message("meter ",met, " removed, triphasic meter")
  #     
  #     # lo marcamos en CnData
  #     CnData[met,]$status <- "trif"
  #     
  #   }
  #   
  #   CN <- CN %>%
  #     select(-all_of(rmMeters))
  #   
  # }
  
  
  # miramos que tengamos lecturas de todos los contadores que están en CnData
  if( length(colnames(CN))-1 != length(rownames(CnData[CnData$status == "ok",])) ){
    
    # no volvemos a hacer mención a los que tenían LP 0
    
    indOrig <- which(!rownames(CnData) %in% colnames(CN))
    ind <- rownames(CnData)[indOrig]
    
    siguenOk <- rownames(CnData)[CnData$status == "ok"]
    
    ind <- intersect(ind,siguenOk)
    
    
    if(length(ind)>0){
      message("/!\\ Warning! Not all meters have load curve")
      
      # lo marcamos en CnData
      CnData[ind,]$status <- "NO LP"
      
      for(i in ind){
        message("quitamos el meter ",i,". No tiene lecturas en esas fechas")
        
      }
      
      # CnData <- CnData[-indOrig,]
      
    }
    
    
    
    rm(ind,indOrig)
  }
  
  
  #
  # RETURN
  #
  rs <- list()
  
  rs$CN <- CN
  rs$SPV <- SPV
  rs$spvs <- lvss
  rs$CnData <- CnData
  if(exists("M")){
    rs$M <- M
  }
  rs$endType <- "ok"
  
  return(rs)
}

