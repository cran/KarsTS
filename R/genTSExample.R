genTSExample <-
function(stationary = TRUE, name = "TS", InKTSEnv = TRUE){
  
  ## Generate stationary time series example
  # Generate dates
  iniDate <- strptime("2015-01-01 00:00:00", 
                      format = "%Y-%m-%d %H:%M", tz = "GMT")
  finDate <- strptime("2016-12-31 18:00:00", 
                      format = "%Y-%m-%d %H:%M", tz = "GMT")
  TSTime <- seq(iniDate, finDate, 21600)
  
  # Generate values
  if( stationary == TRUE){
    
    TSValue <- stats::filter(stats::rnorm(length(TSTime)), 
                             filter=rep(1,3), circular=TRUE)
    
  }else{
    
    TSValue <- stats::diffinv(stats::rnorm(length(TSTime)-1))
    
  }
  
  # Put them together 
  TS <- data.frame(time = TSTime, value = TSValue)
  
  if(InKTSEnv == TRUE){
    # Assign to KTSEnv enviroment
    assign(name, TS, envir = KTSEnv)
  }
  
  TS
  
}
