#' @export interpolate
#' @export arbitrageBounds
#' @export BKM_intepolation
#' @export BKM_trepezoidal
#' @import RQuantLib
#' @importFrom pracma pchipfun pchip
interpolate <- function(date,maturity,zerodata){
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
BKM_intepolation <- function(mnes,vol,k=NULL,m = NULL,rate,mat,out){
  if(is.null(k)) k = 2
  if(is.null(m)) m = 500
  u = (1+k)^(1/m)
  mi = -m:m
  ki = u^mi
  currspline <- pchipfun(mnes,vol)
  kmax <- max(mnes)
  kmin <- min(mnes)
  ivmax <- first(vol)
  ivmin <- last(vol)
  er = exp(rate*mat)
  iv = data_frame(ki = ki, iv = ifelse(ki > kmax,ivmin,ifelse(ki < kmin,ivmax,currspline(ki))),mi = mi)
  iv <- iv %>% group_by(ki,iv)%>%
    mutate(price = ifelse(ki >= 1,as.numeric(EuropeanOption("call",underlying = 1,strike = ki,dividendYield = 0,riskFreeRate = rate,maturity = mat,volatility = iv)),
                          as.numeric(EuropeanOption("put",underlying = 1,strike = ki,dividendYield = 0,riskFreeRate = rate,maturity = mat,volatility = iv))))
  a = 2*(u-1)
  ic = filter(iv,ki >= 1)
  ip = filter(iv,ki < 1)
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

  mu = er - 1 - er*V/2 - er*W/6  - er*X/24
  Var = er*V - mu^2
  Skew = (er*W - 3*mu*er*V + 2*mu^3)/(er*V - mu^2)^(3/2)
  Kurt = (er*X - 4*mu*W + 6*er*mu^2*V - mu^4)/(er*V - mu^2)^2
  ans <- ifelse(out == 1,Var,ifelse(out == 2,Skew,Kurt))
  return(ans)
}

BKM_trepezoidal = function(optdta,rate,mat,out){
  er = exp(rate*mat)
  S  = unique(testdtaM$SpotPrice)
  KC <- testdtaM %>%
    filter(Type == "C") %>%
    select(StrikePrice,optPrice) %>%
    mutate(deltaKC= ifelse(StrikePrice == min(StrikePrice),StrikePrice - S,StrikePrice-lag(StrikePrice))) %>%
    group_by(StrikePrice) %>%
    mutate(vc = .VC(StrikePrice,S)) %>%
    mutate(wc = .WC(StrikePrice,S)) %>%
    mutate(xc = .XC(StrikePrice,S)) %>%
    mutate(VC = ifelse(StrikePrice == min(StrikePrice),vc*optPrice*deltaKC,0.5*(vc*optPrice + lag(vc)*lag(optPrice))*deltaKC))%>%
    mutate(WC = ifelse(StrikePrice == min(StrikePrice),wc*optPrice*deltaKC,0.5*(wc*optPrice + lag(wc)*lag(optPrice))*deltaKC)) %>%
    mutate(XC = ifelse(StrikePrice == min(StrikePrice),xc*optPrice*deltaKC,0.5*(xc*optPrice + lag(xc)*lag(optPrice))*deltaKC))
  KP <- testdtaM %>%
    filter(Type == "P") %>%
    select(StrikePrice,optPrice)%>%
    arrange(desc(StrikePrice)) %>%
    mutate(deltaKP = ifelse(StrikePrice==max(StrikePrice),S - StrikePrice,lag(StrikePrice)-StrikePrice)) %>%
    group_by(StrikePrice) %>%
    mutate(vp = .VP(StrikePrice,S)) %>%
    mutate(wp = .WP(StrikePrice,S)) %>%
    mutate(xp = .XP(StrikePrice,S)) %>%
    mutate(VP = ifelse(StrikePrice == max(StrikePrice),vp*optPrice*deltaKP,0.5*(vp*optPrice + lag(vp)*lag(optPrice))*deltaKP))%>%
    mutate(WP = ifelse(StrikePrice == max(StrikePrice),wp*optPrice*deltaKP,0.5*(wp*optPrice + lag(wp)*lag(optPrice))*deltaKP)) %>%
    mutate(XP = ifelse(StrikePrice == max(StrikePrice),xp*optPrice*deltaKP,0.5*(xp*optPrice + lag(xp)*lag(optPrice))*deltaKP))
  V = sum(KC$VC,KP$VP)
  W = sum(KC$WC)- sum(KP$WP)
  X = sum(KC$XC,KP$XP)
  mu = er - 1 - er*V/2 - er*W/6  - er*X/24
  Var = er*V - mu^2
  Skew = (er*W - 3*mu*er*V + 2*mu^3)/(er*V - mu^2)^(3/2)
  Kurt = (er*X - 4*mu*W + 6*er*mu^2*V - mu^4)/(er*V - mu^2)^2
  ans <- ifelse(out == 1,Var,ifelse(out == 2,Skew,Kurt))
  return(ans)
}

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
  return((12*log(K/S)^2 + 4*log(K/S)^3)/(K^2))
}
.XP = function(K,S){
  return((12*log(S/K)^2 + 4*log(S/K)^3)/(K^2))
}
