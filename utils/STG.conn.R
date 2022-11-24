#Conexion a las tablas del STG de Madrid, ReadOnly
# Tablas bajo los usuarios STG,TITANIUMSTG y TSTGREADINGS
STG.conn <- function(){
  
  # list of the packages we require in this function
  pkgs <- c("DBI",
            "rJava",
            "RJDBC",
            "here")
  
  paketes <- rownames(installed.packages())
  
  index <- which(!pkgs %in% paketes) 
  for (i in index){
    install.packages(pkgs[i])
  }
  
  for (pakete in pkgs){
    suppressMessages(require(pakete, character.only = TRUE))
  }
  

  
  conn <- NULL
  retry <- 0
  while(is.null(conn) & retry < 5){
    conn <- tryCatch(
      {
        
        db <- "jdbc:oracle:thin:@//10.155.141.202:1521/srv_stgibd_readonly"
		
        giltza <- sample( readRDS(here::here("utils","giltzak")) , 1 )

        User <- giltza
        pwd <- giltza
        
        # message(User)
        
        jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath=here("utils","ojdbc8.jar"))
        
        dbConnect(jdbcDriver, db, User, pwd)
      },
      
      error=function(cond) {
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      },
      
      warning=function(cond) {
        message("/!\\ Warning")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      
      finally={
        retry <- retry+1
        if(retry==5){
          message("retried 5 times and could not connect")
          return(NA)
        }
        
      }
    )
  }
    
  
  return(conn)
}
STG <- STG.conn()

# TyC.Query <- "SELECT column_name, owner,table_name 
# FROM all_tab_cols
# WHERE owner in ('STG','TITANIUMSTG','TSTGREADINGS')"
# TyC <-dbGetQuery(STG,TyC.Query)

#to disconnect
# dbDisconnect()