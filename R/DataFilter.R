#--------------------
# Data filtering
#--------------------
#' @import dplyr
#' @export datafilter

datafilter <- function(oriData,zerodata,mnesLim = NULL, maturityLim = NULL,OptNumMin= NULL,addImpVol = TRUE,market = "us",cluster = NULL){
  if(is.null(mnesLim)) mnesLim = c(0.8,1.2)
  if(is.null(maturityLim)) maturityLim = c(7,180)
  if(is.null(OptNumMin)) OptNumMin = 4
  if(is.null(market)) stop("The rate market need to be provided with either: us, eu, ftse, smi, kos, nik, hsi\n",call. = FALSE)
  # Filter data with moneyness and maturity limit
  zerorate = zerodata[[market]]
  Optdata <- oriData %>%
    filter(moneyness > mnesLim[1] & moneyness < mnesLim[2]) %>%
    filter(maturity > maturityLim[1] & maturity < maturityLim[2]) %>%
    # add OTM call-put status
    mutate(OTM_C = (moneyness >= 1 & Type == "C")) %>%
    mutate(OTM_P = (moneyness < 1 & Type == "P")) %>%
    filter(OTM_C == "TRUE"| OTM_P == "TRUE") %>%
    group_by(date,maturity) %>%
    mutate(RfRate = interpolate(unique(date),unique(maturity),zerorate)) %>%
    group_by(SpotPrice,StrikePrice,Type,RfRate) %>%
    mutate(arbitragePrice = arbitrageBounds(Type,SpotPrice,StrikePrice,RfRate,maturity/365))
  Optdata = ungroup(Optdata)
  Optdata <- Optdata %>%
    filter(optPrice > arbitragePrice) %>%
    select(-idx,-arbitragePrice, -OTM_C,  -OTM_P)
  Optdata <- Optdata %>%
    count(date,maturity) %>%
    filter(n > OptNumMin) %>%
    semi_join(Optdata,.)
  if(as.logical(addImpVol)) {
    if(is.null(cluster)){
      Optdata <- Optdata %>%
        rowwise()%>%
        mutate(ImpVol =  EuropeanOptionImpliedVolatility(ifelse(Type == "C","call","put"),optPrice,SpotPrice,StrikePrice,0,RfRate,maturity/365,0.2))
      Optdata <- ungroup(Optdata)
      }else{
      clusterEvalQ(cluster,library(RQuantLib))
      clusterExport(cluster,c("Optdata"),envir = environment())
      ImpVol = parSapply(cl = cluster,1:nrow(Optdata),function(x)
        EuropeanOptionImpliedVolatility(ifelse(Optdata$Type[x] == "C","call","put"),Optdata$optPrice[x],Optdata$SpotPrice[x],Optdata$StrikePrice[x],0,Optdata$RfRate[x],Optdata$maturity[x]/365,0.2)
      )
      Optdata$ImpVol = ImpVol
    }
  }
  return(Optdata)
}
