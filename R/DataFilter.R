#--------------------
# Data Filter
#--------------------
#' @import dplyr
#' @export datafilter
#' @export cleanCP
#' @export ImpVolatility
#' @importFrom fOptions GBSOption GBSGreeks GBSVolatility
#-----
# Local funtion used to make equal number of call-put options in each day
#-----
cleanCP <- function(optdta,OptNumMin){
  ans <- foreach(x = 1:length(unique(optdta$maturity)),.combine = "rbind")%do%{
    ans <- dplyr::filter(optdta,maturity == unique(maturity)[x])
    ans <- ans %>%
      count(date,maturity,Type) %>%
      dplyr::filter(n >= OptNumMin) %>%
      semi_join(ans,.,by = c("date","maturity","Type"))
    if(sum(ans$Type == "C") <OptNumMin | sum(ans$Type == "P") <OptNumMin){
      ans <- NULL
    } else{
      ans = ans
    }
  }
  return(ans)
}
#--------------------------
# Function to calculate implied volatility from Black-Scholes model
#----------------------------
ImpVolatility <- function(Type,value,underlying,strike,riskFreeRate,maturity){
  impvol = try(GBSVolatility(price = value,TypeFlag = ifelse(Type == "C","c","p"),S = underlying,
                             X = strike,Time = maturity/365,r = riskFreeRate,b = riskFreeRate),silent = TRUE)
  if(inherits(impvol,"try-error")){
    ans = NA
  } else{
    ans = as.numeric(impvol)
  }
  return(ans)
}
#----------------------
# Function to clean the option data according to chosen criterias
#----------------------

datafilter <- function(oriData,zerodata,mnesLim = NULL, maturityLim = NULL,OptNumMin= NULL,addImpVol = TRUE,volumn = NULL, optPriceLim = NULL, market = NULL,cluster = NULL){
  # Criterias includes:
  # 1. mnesLim: Moneyness bounds
  # 2. maturityLim: Maturity bounds
  # 3. OptNumMin: The minimum number of option contract requires for a day to be retain
  # 4. optPriceLim: The limit of option prices to be retain: Default is 3/8 usd following literature

  if(is.null(mnesLim)) mnesLim = c(0.8,1.2)
  if(is.null(market)) market = "us"
  if(is.null(maturityLim)) maturityLim = c(7,270)
  if(is.null(OptNumMin)) OptNumMin = 2
  if(is.null(optPriceLim)) optPriceLim = 3/8
  if(is.null(volumn)) volumn = FALSE
  if(is.null(market)) stop("The rate market need to be provided with either: us, eu, ftse, smi, kos, nik, hsi\n",call. = FALSE)
  # dplyr::filter data with moneyness and maturity limit
  zerorate = zerodata[[market]]

  # General filter

  Optdata <- oriData %>% dplyr::filter(optPrice >= optPriceLim) %>% dplyr::filter(ask > 0.001) %>% dplyr::filter(bid > 0.001)%>%
    dplyr::filter(moneyness > mnesLim[1] & moneyness < mnesLim[2]) %>%
    dplyr::filter(maturity > maturityLim[1] & maturity < maturityLim[2]) %>% dplyr::filter(ask > bid) %>%
    # add OTM call-put status
    mutate(OTM_C = (moneyness >= 1 & Type == "C")) %>%
    mutate(OTM_P = (moneyness < 1 & Type == "P")) %>%
    dplyr::filter(OTM_C == "TRUE"| OTM_P == "TRUE")
  if(is.logical(volumn)) Optdata <- Optdata %>% dplyr::filter(vol > 0)
  dateAvail = unique(Optdata$date)
  if(!is.null(cluster)){
 # Intepolate risk-free rate and get the arbitrage price.
  ans <- foreach(x = 1:length(dateAvail),.packages = c("OptSK","pracma","fOptions"),
                     .export = c("Optdata","cleanCP","interpolate","arbitrageBounds"),
                     .combine = "rbind")%dopar%{
    tempDate <- dateAvail[x]
    ans <- Optdata %>% dplyr::filter(date == tempDate)
    tempMaturity <- unique(ans$maturity)
    tempRate <- interpolate(tempDate,tempMaturity,zerorate)
    tempMaturity <- data_frame(maturity = tempMaturity,RfRate = tempRate)
    ans <- ans %>% left_join(.,tempMaturity,by = "maturity") %>% dplyr::filter(!is.na(RfRate))
    arbitragePrice = vector()
    if(!is.null(ans)){
    for(i in 1:nrow(ans)){
        arbitragePrice[i] = arbitrageBounds(ans$Type[i],ans$SpotPrice[i],ans$StrikePrice[i],ans$RfRate[i],ans$maturity[i]/365)
    }
    }
    ans <- ans %>% mutate(arbitragePrice = arbitragePrice) %>%
      dplyr::filter(optPrice > arbitragePrice) %>%
      select(-idx,-arbitragePrice, -OTM_C,  -OTM_P)
    ans <- cleanCP(ans,OptNumMin)
      if(!is.null(ans)){
        ImpVol <- vector()
        for(i in 1:nrow(ans)){
          ImpVol[i] = ImpVolatility(Type = ans$Type[i],value = ans$optPrice[i],underlying = ans$SpotPrice[i],
                                 strike = ans$StrikePrice[i],riskFreeRate = ans$RfRate[i],maturity = ans$maturity[i])
        }
        ans <- ans %>% mutate(ImpVol = ImpVol) %>% dplyr::filter(!is.na(ImpVol)&ImpVol>0&ImpVol<1)
      }
  }
  } else{
    ans <- foreach(x = 1:length(dateAvail),.packages = c("OptSK","pracma","fOptions"),
                   .export = c("Optdata","cleanCP","interpolate","arbitrageBounds")
                   ,.combine = "rbind")%do%{
      tempDate <- dateAvail[x]
      ans <- Optdata %>% dplyr::filter(date == tempDate)
      tempMaturity <- unique(ans$maturity)
      tempRate <- interpolate(tempDate,tempMaturity,zerorate)
      tempMaturity <- data_frame(maturity = tempMaturity,RfRate = tempRate)
      ans <- ans %>% left_join(.,tempMaturity,by = "maturity") %>% dplyr::filter(!is.na(RfRate))
      arbitragePrice = vector()
      if(!is.null(ans)){
      for(i in 1:nrow(ans)){
        arbitragePrice[i] = arbitrageBounds(ans$Type[i],ans$SpotPrice[i],ans$StrikePrice[i],ans$RfRate[i],ans$maturity[i]/365)
      }
      }
      ans <- ans %>% mutate(arbitragePrice = arbitragePrice) %>%
        dplyr::filter(optPrice > arbitragePrice) %>%
        select(-idx,-arbitragePrice, -OTM_C,  -OTM_P)
      ans <- cleanCP(ans,OptNumMin)
      if(!is.null(ans)){
        ImpVol <- vector()
        for(i in 1:nrow(ans)){
          ImpVol[i] = ImpVolatility(Type = ans$Type[i],value = ans$optPrice[i],underlying = ans$SpotPrice[i],
                                    strike = ans$StrikePrice[i],riskFreeRate = ans$RfRate[i],maturity = ans$maturity[i])
        }
        ans <- ans %>% mutate(ImpVol = ImpVol) %>% dplyr::filter(!is.na(ImpVol)&ImpVol>0&ImpVol<1)
      }
    }
    }
  return(ans)
}
