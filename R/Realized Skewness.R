#' @export Realized_Skewness
#' @importFrom zoo coredata index

GetRsk <- function(daydata,LE_estimates, RlzData){
maturityAvail <- unique(daydata$maturity)
ans <- foreach(i = 1:length(maturityAvail),.combine = "rbind")%do%{
  tempData <- daydata %>% dplyr::filter(maturity == maturityAvail[i])
  periodData <- LE_estimates %>% dplyr::filter(expDate == tempData$expDate,date >= tempData$date)
    if(nrow(periodData) > 2){
    # At day t, change in VE is zero, realized variance and skewness start counting from day t+1
      # Another implicit assumption: Implied variance does not change after the maturity lower-limit.
      # It means that if we set the maturity limit at 5-days, we do not have VE at these day, thus, the change in VE is assumed to be zero
    periodData <- periodData %>% dplyr::mutate(deltaVE = if_else(maturity == max(maturity),0,VE-dplyr::lag(VE))) %>%
      dplyr::left_join(.,RlzData,by = "date") %>% group_by(RlzRet) %>% mutate(RlzRet2 = if_else(is.na(RlzRet),0,RlzRet)) %>% ungroup() %>%
      select(-RlzRet) %>% mutate(RlzRet = RlzRet2) %>% select(-RlzRet2,-RlzVol)%>% dplyr::mutate(Lev = 3*deltaVE*(exp(RlzRet)-1))
    # 15.6.2018: Take cares of the fact that some days we do not have return data but still have option data
    periodRet <- RlzData %>% dplyr::filter(date >= tempData$date & date <= tempData$expDate) %>%
      dplyr::mutate(K = if_else(date == min(date),0,6*(2-2*exp(RlzRet) + RlzRet + RlzRet*exp(RlzRet)))) %>%
      dplyr::mutate(GV = if_else(date == min(date),0,2*(exp(RlzRet)-1-RlzRet)))

    # 15.6.2018: Account for bias: Realized skewness will be upward(downward) biased estimate of stock prices (within the period) is positively
    # (negatively) correlated with the variance risk premium


    RlzVar = sum(periodRet$GV)
    RlzLev = sum(periodData$Lev)
    RlzCub = sum(periodRet$K)
    RlzSkewness = sum(RlzLev + RlzCub)/(RlzVar^(3/2))
    ans = dplyr::data_frame(date = tempData$date,maturity = tempData$maturity,RlzVar = RlzVar,RlzLev = RlzLev,RlzCub = RlzCub,RlzSkewness = RlzSkewness)
    } else {
    ans = dplyr::data_frame(date = tempData$date,maturity = tempData$maturity,RlzVar = NA,RlzLev = NA,RlzCub = NA,RlzSkewness = NA)
    }
  }
}

Realized_Skewness <- function(LE_estimates,RlzData,cluster = NULL){
  dateAvail <- unique(LE_estimates$date)
  LE_estimates$expDate = LE_estimates$date + LE_estimates$maturity
  if(!is.null(cluster)){
    ans <- foreach(x = 1:length(unique(LE_estimates$date)),.packages = "OptSK",
                   .export = c("LE_estimates","RlzData","dateAvail","GetRsk"),.combine = "rbind") %dopar%{
          daydata <- dplyr::filter(LE_estimates,date == dateAvail[x])
          ans <- GetRsk(daydata,LE_estimates,RlzData)
    }
  }else{
    ans <- foreach(x = 1:length(unique(LE_estimates$date)),.packages = "OptSK",
                   .export = c("LE_estimates","RlzData","dateAvail","GetRsk"),.combine = "rbind") %do%{
                     daydata <- dplyr::filter(LE_estimates,date == dateAvail[x])
                     ans <- GetRsk(daydata,LE_estimates,RlzData)
                   }
    }
  ans <- ans %>% dplyr::filter(!is.na(ans$RlzSkewness))
  return(ans)
}
#------------
# Help function to match the daily return series and the day at which we are calculating realized skewness
#----------
matchRet <- function(realized,Ledata){
  tomix <- realized %>% select(date,RlzRet) %>%
    filter(date <= max(Ledata$date)&date >= min(Ledata$date)) %>%
    group_by(date) %>% rowwise() %>%
    mutate(toInclude = ifelse(is.element(date,Ledata$date),TRUE,FALSE)) %>% ungroup()
  ret = vector()
  ret[1] = tomix$RlzRet[1]  #Here, at the day we start calculating realized skewness, we do not taking to account the
  # change in return at that day.
  if(length(tomix$date) >1){
    for(i in 2:length(tomix$date)){
      if(tomix$toInclude[i-1]==TRUE){
        ret[i] = tomix$RlzRet[i]
      } else{
        ret[i] = tomix$RlzRet[i] + ret[i-1]
      }
    }
  }
tomix$ret = ret
  tomix <- tomix %>% select(date,ret)
  return(tomix)
}
