#' @export ImpliedMoments
ImpliedMoments <- function(optdta,moments = NULL,cluster = NULL){
  if(is.null(moments)) ExpChoice = c("Var","Skewness","Kurtosis") else ExpChoice = moments
  valid.choices = c("Var","Skewness","Kurtosis")
  if(!as.logical(sum(ExpChoice == valid.choices)))
    stop("\nOptSk-->error: Invalid choices of moments.\n", call. = FALSE)
  if(!is.null(cluster)){
    foreach(x = 1:length(unique(optdta$date)),.packages = c("OptSK"),.export = "optdta",.combine = "rbind") %dopar% {
      data <- filter(optdta,date == unique(date)[x])
      ans <- optdta %>% distinct(date,maturity)
      if(!is.na(match("Var",ExpChoice) == 1)) {
        Var <- optdta %>% group_by(date)%>%
          group_by(maturity) %>%
          summarise(Var = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 1))
        ans =left_join(ans,Var,by = "maturity")
      }
      if(!is.na(match("Skewness",ExpChoice) == 1)) {
        Skewness <- optdta %>% group_by(date)%>%
        group_by(maturity) %>%
        summarise(Skewness = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 2))
        ans =left_join(ans,Skewness,by = "maturity")
      }
      if(!is.na(match("Kurtosis",ExpChoice) == 1)) {
      Kurtosis <- optdta %>% group_by(date)%>%
        group_by(maturity) %>%
        summarise(Kurtosis = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 3))
      ans =left_join(ans,Kurtosis,by = "maturity")
      }
      }
  } else {
    foreach(x = 1:length(unique(optdta$date)),.packages = c("OptSK"),.export = "optdta",.combine = "rbind") %do% {
      data <- filter(optdta,date == unique(date)[x])
      ans <- optdta %>% distinct(date,maturity)
      if(!is.na(match("Var",ExpChoice) == 1)) {
        Var <- optdta %>% group_by(date)%>%
          group_by(maturity) %>%
          summarise(Var = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 1))
        ans = left_join(ans,Var,by = "maturity")
      }
      if(!is.na(match("Skewness",ExpChoice) == 1)) {
        Skewness <- optdta %>% group_by(date)%>%
          group_by(maturity) %>%
          summarise(Skewness = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 2))
        ans = left_join(ans,Skewness,by = "maturity")
      }
      if(!is.na(match("Kurtosis",ExpChoice) == 1)) {
        Kurtosis <- optdta %>% group_by(date)%>%
          group_by(maturity) %>%
          summarise(Kurtosis = BKM_intepolation(mnes = unique(moneyness),vol = unique(ImpVol),rate = unique(RfRate)[1],mat = unique(maturity)/365,out = 3))
        ans =left_join(ans,Kurtosis,by = "maturity")
      }
    }
    }
}
