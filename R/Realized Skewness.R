#' @export Realized_Skewness
#' @importFrom zoo coredata index
Realized_Skewness <- function(LE_contracts,HQreturns,cluster = NULL){
  HQreturns = data_frame(date = zoo::index(HQreturns),return = zoo::coredata(HQreturns))
  if(!is.null(cluster)){
    ans <- foreach(x = 1:length(unique(LE_contracts$date)),.packages = "OptSK",
                   .export = c("LE_contracts","HQreturns"),.combine = "rbind") %dopar%{
    data <- filter(LE_contracts,date == unique(date)[x])
    ans <- data %>% distinct(date,maturity)
    RSK <- foreach(i = 1:length(unique(data$expDate)),.combine = "rbind") %do%{
      tmpdata <- filter(LE_contracts,expDate == unique(data$expDate)[i]& date >= data$date[1])
      tmpdata <- tmpdata %>%
        select(date,maturity,VE) %>%
        mutate(deltaVE = ifelse(maturity == max(maturity),VE,VE-lag(VE))) %>%
        group_by(date) %>%
        mutate(ret = matchRet(HQreturns,date)) %>%
        mutate(rv = 2*(exp(ret)-1-ret)) %>%
        mutate(rs = 3*deltaVE*(exp(ret)-1) + 6*(2-2*exp(ret) + ret + ret*exp(ret))) %>%
        ungroup()
      RVar <- sum(tmpdata$rv)
      RSkewess <- sum(tmpdata$rs)/RVar^(3/2)
      RSK = data_frame(date = unique(tmpdata$date)[1],maturity = unique(tmpdata$maturity)[1],Rvar = RVar,Rskewness = RSkewess)
    }
    ans = left_join(ans,RSK)
  }
  }else{
    ans <- foreach(x = 1:length(unique(LE_contracts$date)),.combine = "rbind") %do%{
                     data <- filter(LE_contracts,date == unique(date)[x])
                     ans <- data %>% distinct(date,maturity)
                     RSK <- foreach(i = 1:length(unique(data$expDate)),.combine = "rbind") %do%{
                       tmpdata <- filter(LE_contracts,expDate == unique(data$expDate)[i]& date >= data$date[1])
                       tmpdata <- tmpdata %>%
                         select(date,maturity,VE) %>%
                         mutate(deltaVE = ifelse(maturity == max(maturity),VE,VE-lag(VE))) %>%
                         group_by(date) %>%
                         mutate(ret = matchRet(HQreturns,date)) %>%
                         mutate(rv = 2*(exp(ret)-1-ret)) %>%
                         mutate(rs = 3*deltaVE*(exp(ret)-1) + 6*(2-2*exp(ret) + ret + ret*exp(ret))) %>%
                         ungroup()
                       RVar <- sum(tmpdata$rv)
                       RSkewess <- sum(tmpdata$rs)/RVar^(3/2)
                       RSK = data_frame(date = unique(tmpdata$date)[1],maturity = unique(tmpdata$maturity)[1],Rvar = RVar,Rskewness = RSkewess)
                     }
                     ans = left_join(ans,RSK)
                   }
  }
}
#------------
# Help function to match the daily return series and the day at which we are calculating realized skewness
#----------
matchRet <- function(ret,Date){
  ans <- ret %>%
    filter(date == Date) %>%
    select(return)
  return(as.numeric(ans))
}
