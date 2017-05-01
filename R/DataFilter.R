#--------------------
# Data filtering
#--------------------
#' @import dplyr
#' @import RQuantLib
#' @importFrom pracma pchipfun pchip
datafilter <- function(oriData,zerodata,mnesLim = NULL, maturityLim = NULL,arbitrage = TRUE,market = "us"){
 if(is.null(mnesLim)) mnesLim = c(0.8,1.2)
 if(is.null(maturityLim)) maturityLim = c(7,180)
 if(is.null(market)) stop("The rate market need to be provided with either: us, eu, ftse, smi, kos, nik, hsi\n",call. = FALSE)
# Filter data with moneyness and maturity limit
 zerorate = zerodata[[market]]
  Optdata <- oriData %>%
   filter(moneyness > mnesLim[1] & moneyness < mnesLim[2]) %>%
   filter(maturity > maturityLim[1] & maturity < maturityLim[2]) %>%
# add OTM call-put status
   mutate(OTM_C = (moneyness > 1 & Type == "C")) %>%
   mutate(OTM_P = (moneyness < 1 & Type == "P")) %>%
   filter(OTM_C == "TRUE"| OTM_P == "TRUE") %>%
   group_by(date,maturity) %>%
    mutate(RfRate = intepolate(unique(date),unique(maturity),zerorate)) %>%
    group_by(SpotPrice,StrikePrice,Type,RfRate) %>%
    mutate(arbitragePrice = arbitrageBounds(Type,SpotPrice,StrikePrice,RfRate,maturity/365))
  Optdata = ungroup(Optdata)
  Optdata <- Optdata %>%
    filter(optPrice > arbitragePrice) %>%
    select(-idx,-Type,-arbitragePrice)
  return(Optdata)
 }
intepolate <- function(date,maturity,zerodata){
  zerorate <- filter(zerodata,zerodata$Date == date)
  ans = pchip(zerorate$Maturity,zerorate$Rate,maturity)
  return(ans)
}
arbitrageBounds <- function(type,spot,strike,rate,maturity,dividend = NULL){
  if(is.null(dividend)) dividend = 0 else dividend = dividend
    er = exp(-rate*maturity)
    if(dividend < 1 && dividend >0) dividend = spot * dividend
    if(type == "call" || type == "c" || type == "C"){
      ans = max(0,spot -  strike * er - dividend)
    } else{
      ans = max(0, strike*er + dividend - spot)
      }
  return(ans)
}
