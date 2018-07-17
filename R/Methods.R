#' @export interpolate
#' @export arbitrageBounds
#' @export BKM
#' @export LE_contracts
#' @export GeneralVariance
#' @importFrom pracma pchipfun pchip trapz
#' @export ImpliedMoments
#' @import parallel
#' @import foreach
#' @import doParallel
#' @export LE_contracts_intepolate
#-----------------------------
# Utility functions
#-----------------------------

# This function is to interpolate zero rate across available maturities

interpolate <- function(date,maturity,zerodata){
  zerodata$Date <- as.Date(zerodata$Date)
  zerorate <- zerodata %>% mutate(gap =Date - date) %>% dplyr::filter(gap <= 0) %>%
    dplyr::filter(gap == max(gap)) %>% select(-gap)
  rate = try(pchip(zerorate$Maturity,zerorate$Rate,maturity),silent = TRUE)
  if(inherits(rate,"try-error")){
    ans = NA
# Return NA value if we do not have available data of risk free rate on the day
  } else{
    ans = rate
  }
  return(ans)
}

# This function to calculate arbitrage bounds for option prices according to BS model
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

# This function is to calcuate option prices giving the type, spot price, srike price, maturity, riskfree rate and implied volatility

OptPrice <- function(type,spot,strike,maturity,rfrate,iv){
  Price <- try(GBSOption(TypeFlag = ifelse(type == "C","c","p"),S = spot,X = strike,Time = maturity,r = rfrate,b = rfrate,sigma = iv),silent = TRUE)
  if(inherits(Price,"try-error")){
    ans = NA
  } else{
    ans = Price@price
  }
  return(ans)
}

#----------------------------------
# BKM IMPLIED MOMENTS
# Function used to compute Bakshi et al(2003) implied variance, skewness and (ex)-kurtosis.
# Input is daily data of filted options contracts with various maturity
# Two methods allowed including intepolation and trapezoidal
#----------------------------------

BKM <- function(daydata,k=NULL,m = NULL, method = NULL){
  if(is.null(k)) k = 2
  if(is.null(m)) m = 500
  if(is.null(method)) method = "Vilkov"
  maturityAvail <- unique(daydata$maturity)
  ans <- foreach(i = 1:length(maturityAvail),.combine = "rbind")%do%{
    mat = maturityAvail[i]/365
    tempData <- daydata %>% dplyr::filter(maturity == maturityAvail[i]) %>% arrange(moneyness)
    rate = unique(tempData$RfRate)
    er = exp(rate*mat)
    # The "Vilkov" method follows the code provided by Vilkov which apply intepolation and analytical approximation of the integral
    if(method == "Vilkov"){
      u = (1+k)^(1/m)
      mi = -m:m
      ki = u^mi
      mnes <- tempData$moneyness; vol <- tempData$ImpVol
      currspline <- pchipfun(mnes,vol) # Cubric function use to intepolate the volatility surface from 0.3 - 3 with 1001 points
      kmax <- max(mnes) # Maximum available moneyness
      kmin <- min(mnes) # Minumum available moneyness
      ivmax <- first(vol)
      ivmin <- last(vol)
      iv = data_frame(ki = ki, iv = ifelse(ki > kmax,ivmin,ifelse(ki < kmin,ivmax,currspline(ki))),mi = mi)

      iv <- iv %>% group_by(ki,iv)%>%
        mutate(price = ifelse(ki >= 1,OptPrice("C",spot = 1,strike = ki,rfrate = rate,maturity = mat,iv = iv),
                              OptPrice("P",spot = 1,strike = ki,rfrate = rate,maturity = mat,iv = iv)))

      a = 2*(u-1)
      ic = dplyr::filter(iv,ki >= 1)
      ip = dplyr::filter(iv,ki < 1)
      b1 = sum((1 - (log(1+k)/m)*ic$mi)*ic$price/(u^ic$mi))
      b2 = sum((1 - (log(1+k)/m)*ip$mi)*ip$price/(u^ip$mi))
      V = a*(b1 + b2)

      a = 3*(u-1)*log(1+k)/m
      b1 = sum(ic$mi*(2 - (log(1+k)/m)*ic$mi)*ic$price/(u^ic$mi))
      b2 = sum(ip$mi*(2 - (log(1+k)/m)*ip$mi)*ip$price/(u^ip$mi))
      W = a*(b1 + b2)

      a = 4*(u-1)*(log(1+k)/m)^2
      b1 = sum((ic$mi)^2 * (3 - (log(1+k)/m)*ic$mi)*ic$price/(u^ic$mi))
      b2 = sum((ip$mi)^2 * (3 - (log(1+k)/m)*ip$mi)*ip$price/(u^ip$mi))
      X = a*(b1 + b2)
    }else{
      # Else, if we only use current option contracts and apply trapezoidal approache similar to Bali et al (2016)
      S  = unique(tempData$SpotPrice)
      KC <- tempData %>%
        dplyr::filter(Type == "C") %>%
        select(StrikePrice,SpotPrice,optPrice) %>%
        mutate(deltaC = ifelse(StrikePrice == min(StrikePrice),StrikePrice - SpotPrice,StrikePrice - lag(StrikePrice))) %>%
        mutate(vc = .VC(StrikePrice,S)) %>%
        mutate(wc = .WC(StrikePrice,S)) %>%
        mutate(xc = .XC(StrikePrice,S)) %>%
        mutate(V = ifelse(StrikePrice == min(StrikePrice),vc*optPrice*deltaC,0.5*(vc*optPrice + lag(vc)*lag(optPrice))*deltaC)) %>%
        mutate(W = ifelse(StrikePrice == min(StrikePrice),wc*optPrice*deltaC,0.5*(wc*optPrice + lag(wc)*lag(optPrice))*deltaC)) %>%
        mutate(X = ifelse(StrikePrice == min(StrikePrice),xc*optPrice*deltaC,0.5*(xc*optPrice + lag(xc)*lag(optPrice))*deltaC))

      KP <- tempData %>%
        dplyr::filter(Type == "P") %>%
        select(StrikePrice,optPrice)%>%
        arrange(desc(StrikePrice)) %>%
        mutate(deltaP = ifelse(StrikePrice==max(StrikePrice),S - StrikePrice,lag(StrikePrice)-StrikePrice)) %>%
        group_by(StrikePrice) %>%
        mutate(vp = .VP(StrikePrice,S)) %>%
        mutate(wp = .WP(StrikePrice,S)) %>%
        mutate(xp = .XP(StrikePrice,S)) %>%
        mutate(V = ifelse(StrikePrice == max(StrikePrice),vp*optPrice*deltaP,0.5*(vp*optPrice + lag(vp)*lag(optPrice))*deltaP)) %>%
        mutate(W = ifelse(StrikePrice == max(StrikePrice),wp*optPrice*deltaP,0.5*(wp*optPrice + lag(wp)*lag(optPrice))*deltaP)) %>%
        mutate(X = ifelse(StrikePrice == max(StrikePrice),xp*optPrice*deltaP,0.5*(xp*optPrice + lag(xp)*lag(optPrice))*deltaP))
      V = sum(KC$V) + sum(KP$V)
      W = sum(KC$W) - sum(KP$W)
      X = sum(KC$X) + sum(KP$X)
    }
      mu = er - 1 - er*V/2 - er*W/6  - er*X/24
      ImpVar = er*V - mu^2
      ImpSkewness = (er*W - 3*mu*er*V + 2*mu^3)/(er*V - mu^2)^(3/2)
      ImpKurtosis = ((er*X - 4*mu*W + 6*er*mu^2*V - mu^4)/(er*V - mu^2)^2)- 3
      ans <- data_frame(date = tempData$date[i], maturity = tempData$maturity[i],ImpVar = ImpVar, ImpSkewness = ImpSkewness, ImpKurtosis = ImpKurtosis)
  }
  return(ans)
}

#---------------------
# LOG AND ENTROPY VARIANCE
# Function to compute VL and VE contracts
# The approximation is following Kozhan et al (RFS, 2013)
# Input is dailydata of filted options contracts with different maturity
#---------------------

LE_contracts = function(daydata){
  maturityAvail <- unique(daydata$maturity)
  ans <- foreach(i = 1:length(maturityAvail),.combine = "rbind")%do%{
    tempData <- daydata %>% dplyr::filter(maturity == maturityAvail[i])
    mat <- unique(tempData$maturity)/365
    rate <- unique(tempData$RfRate)
    er = exp(mat*rate)
    S  = unique(tempData$SpotPrice)
    B = 1/er
    tempData <- tempData %>%
      arrange(StrikePrice) %>%
      mutate(deltaC = ifelse(StrikePrice == min(StrikePrice),dplyr::lead(StrikePrice)- StrikePrice,
                            ifelse(StrikePrice == max(StrikePrice),StrikePrice-dplyr::lag(StrikePrice),
                                   0.5*(dplyr::lead(StrikePrice)-dplyr::lag(StrikePrice))))) %>%
      mutate(VL = 2*deltaC*(optPrice)/(B*StrikePrice^2)) %>%
      mutate(VE = 2*deltaC*(optPrice)/(StrikePrice*S))
      VL <- sum(tempData$VL)
      VE <- sum(tempData$VE)
      ImpSkewness <- (3*(VE-VL))/(VL^(3/2))
      ans <- data_frame(date = tempData$date[1], maturity = tempData$maturity[1],VL = VL, VE = VE, ImpSkewness = ImpSkewness)
  }
  return(ans)
}

LE_contracts_intepolate = function(daydata,k=NULL,m = NULL){
  if(is.null(k)) k = 2
  if(is.null(m)) m = 500
  maturityAvail <- unique(daydata$maturity)
  ans <- foreach(i = 1:length(maturityAvail),.combine = "rbind")%do%{
    mat = maturityAvail[i]/365
    tempData <- daydata %>% dplyr::filter(maturity == maturityAvail[i]) %>% arrange(moneyness)
    rate = unique(tempData$RfRate)
    er = exp(rate*mat)
    S  = unique(tempData$SpotPrice)
    B = 1/er
    u = (1+k)^(1/m)
    mi = -m:m
    ki = u^mi
    mnes <- tempData$moneyness; vol <- tempData$ImpVol
    currspline <- pchipfun(mnes,vol) # Cubric function use to intepolate the volatility surface from 0.3 - 3 with 1001 points
    kmax <- max(mnes) # Maximum available moneyness
    kmin <- min(mnes) # Minumum available moneyness
    ivmax <- first(vol)
    ivmin <- last(vol)
    iv = data_frame(ki = ki, iv = ifelse(ki > kmax,ivmin,ifelse(ki < kmin,ivmax,currspline(ki))),mi = mi)
    SpotPrice = unique(tempData$SpotPrice)
    iv <- iv %>% group_by(ki,iv)%>%
      mutate(optPrice = ifelse(ki >= 1,SpotPrice*OptPrice("C",spot = 1,strike = ki,rfrate = rate,maturity = mat,iv = iv),
                            SpotPrice*OptPrice("P",spot = 1,strike = ki,rfrate = rate,maturity = mat,iv = iv))) %>%
      mutate(StrikePrice = ki*SpotPrice) %>% ungroup() %>% arrange(StrikePrice) %>%
      mutate(deltaC = ifelse(StrikePrice == min(StrikePrice),dplyr::lead(StrikePrice)- StrikePrice,
                             ifelse(StrikePrice == max(StrikePrice),StrikePrice-dplyr::lag(StrikePrice),
                                    0.5*(dplyr::lead(StrikePrice)-dplyr::lag(StrikePrice))))) %>%
      mutate(VL = 2*deltaC*(optPrice)/(B*StrikePrice^2)) %>%
      mutate(VE = 2*deltaC*(optPrice)/(StrikePrice*S))
    VL <- sum(iv$VL)
    VE <- sum(iv$VE)
    ImpSkewness <- (3*(VE-VL))/(VL^(3/2))
    ans <- data_frame(date = tempData$date[1], maturity = tempData$maturity[1],VL = VL, VE = VE, ImpSkewness = ImpSkewness)
  }
  return(ans)
}

#---------------
# RISK-NEUTRAL MOMENTS
# Given a data_frame of filted option data, compute the implied volatility, skewness and kurtosis
# Method available: Interpolation by Vilkov or Trapzoidal by Bali
#---------------

ImpliedMoments <- function(Optdta, method = NULL,cluster = NULL){
  # Check input
  if(is.null(method)) method = "Vilkov" #Method to calculate implied moment, could be Vilkov or Trapz
  dateAvail <- unique(Optdta$date)
  if(!is.null(cluster)){
    ans = foreach(x = 1:length(dateAvail),.packages = c("OptSK"),.export = c("Optdta","method","dateAvail"),.combine = "rbind") %dopar% {
      daydata <- dplyr::filter(Optdta,date == dateAvail[x])
      ans <- BKM(daydata = daydata,method = method)
    }
  } else {
    ans = foreach(x = 1:length(dateAvail),.combine = "rbind") %do% {
      daydata <- dplyr::filter(Optdta,date == dateAvail[x])
      ans <- BKM(daydata = daydata,method = method)
    }
  }
  return(ans)
}

GeneralVariance <- function(Optdta,method = NULL, cluster = NULL){
  dateAvail <- unique(Optdta$date)
  if(is.null(method)) method = "Kozhan"
  if(!is.null(cluster)){
    ans = foreach(x = 1:length(dateAvail),.packages = c("OptSK","pracma","fOptions"),.export = c("Optdta","dateAvail","LE_contracts",
                                                                             "OptPrice","LE_contracts_intepolate"),.combine = "rbind") %dopar% {
      daydata <- dplyr::filter(Optdta,date == dateAvail[x])
      if(method == "Kozhan"){
        ans <- LE_contracts(daydata = daydata)
      } else{
        ans <- LE_contracts_intepolate(daydata = daydata)
      }
    }
  } else {
    ans = foreach(x = 1:length(dateAvail),.combine = "rbind") %do% {
      daydata <- dplyr::filter(Optdta,date == dateAvail[x])
      if(method == "Kozhan"){
        ans <- LE_contracts(daydata = daydata)
      } else{
        ans <- LE_contracts_intepolate(daydata = daydata)
      }
    }
  }
}

#-----------------------------------------
# Help functions for trapezoidal approach
#-----------------------------------------
.VC = function(K,S){
  return((2*(1 - log(K/S)))/(K^2))
}
.VP = function(K,S){
  return((2*(1 + log(S/K)))/(K^2))
}
.WC = function(K,S){
  return((6*log(K/S) - 3*log(K/S)^2)/(K^2))
}
.WP = function(K,S){
  return((6*log(S/K) + 3*log(S/K)^2)/(K^2))
}
.XC = function(K,S){
  return((12*log(K/S)^2 - 4*log(K/S)^3)/(K^2))
}
.XP = function(K,S){
  return((12*log(S/K)^2 + 4*log(S/K)^3)/(K^2))
}
