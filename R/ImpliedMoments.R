#' @export ImpliedMoments
#' @import parallel
#' @import foreach
#' @import doParallel
ImpliedMoments <- function(optdta,moments = NULL,method = NULL,cluster = NULL){
  if(is.null(moments)) ExpChoice = c("Var","Skewness","Kurtosis") else ExpChoice = moments
  valid.choices = c("Var","Skewness","Kurtosis")
  if(!as.logical(sum(ExpChoice == valid.choices)))
    stop("\nOptSk-->error: Invalid choices of moments.\n", call. = FALSE)
  if(is.null(method)) method = "Intepolation"
  if(!is.null(cluster)){
    ans = foreach(x = 1:length(unique(optdta$date)),.packages = c("OptSK"),.export = c("optdta","method","ExpChoice"),.combine = "rbind") %dopar% {
      data <- filter(optdta,date == unique(date)[x])
      ans <- data %>% distinct(date,maturity)
      if(!is.na(match("Var",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Var <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Var = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 1))
          ans =left_join(ans,Var,by = c("maturity"))
        } else if(method == "Trapezoidal"){
          Var <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 1)
            Var <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Var = tempans)
          }
          ans =left_join(ans,Var,by = c("maturity","date"))
        }
          }
      if(!is.na(match("Skewness",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Skewness <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Skewness = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 2))
          ans =left_join(ans,Skewness,by = "maturity")
        } else if(method == "Trapezoidal"){
          Skewness <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 2)
            Skewness <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Skewness = tempans)
          }
          ans =left_join(ans,Skewness,by = c("maturity","date"))
        }
      }
      if(!is.na(match("Kurtosis",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Kurtosis <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Kurtosis = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 3))
          ans =left_join(ans,Kurtosis,by = "maturity")
        } else if(method == "Trapezoidal"){
          Kurtosis <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 3)
            Kurtosis <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Kurtosis = tempans)
          }
          ans =left_join(ans,Kurtosis,by = c("maturity","date"))
        }
      }
      }
  } else {
    ans = foreach(x = 1:length(unique(optdta$date)),.combine = "rbind") %do% {
      data <- filter(optdta,date == unique(date)[x])
      ans <- data %>% distinct(date,maturity)
      if(!is.na(match("Var",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Var <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Var = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 1))
          ans =left_join(ans,Var,by = c("maturity"))
        } else if(method == "Trapezoidal"){
          Var <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 1)
            Var <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Var = tempans)
          }
          ans =left_join(ans,Var,by = c("maturity","date"))
        }
      }
      if(!is.na(match("Skewness",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Skewness <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Skewness = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 2))
          ans =left_join(ans,Skewness,by = "maturity")
        } else if(method == "Trapezoidal"){
          Skewness <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 2)
            Skewness <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Var = tempans)
          }
          ans =left_join(ans,Skewness,by = c("maturity","date"))
        }
      }
      if(!is.na(match("Kurtosis",ExpChoice) == 1)) {
        if(method == "Intepolation"){
          Kurtosis <- data %>% group_by(date)%>%
            group_by(maturity) %>%
            summarise(Kurtosis = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 3))
          ans =left_join(ans,Kurtosis,by = "maturity")
        } else if(method == "Trapezoidal"){
          Kurtosis <- foreach(i = 1:length(unique(data$maturity)),.combine = "rbind")%do%{
            tempData <- filter(data,maturity == unique(maturity)[i])
            tempans <- BKM_trapezoidal(tempData,rate = unique(tempData$RfRate),mat = unique(tempData$maturity)/365,out = 3)
            Kurtosis <- data_frame(date = unique(tempData$date),maturity = unique(tempData$maturity),Var = tempans)
          }
          ans =left_join(ans,Kurtosis,by = c("maturity","date"))
        }
      }
    }
  }
  return(ans)
}
