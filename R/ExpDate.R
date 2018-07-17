#ExpDate function is the function  to identify whether a given date is the third Friday of the Month or the third Saturday of Month or not
ExpDate <- function(date){
  FirstMonthDate <- paste(year(date),"-",month(date),"-",'01',sep = "")
  if(wday(date)!=6 & wday(date) != 7){
    ans = as.logical(0)
  } else if(wday(date)==6){
    if(day(date) < 15 | day(date) > 21){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(21,28)),as.logical(1),as.logical(0))
    }
  } else{
    if(day(date) < 16 | day(date) > 22){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(22,29)),as.logical(1),as.logical(0))
    }
  }
  return(ans)
}
# ExpDateNIK function is the function  to identify whether a given date is the second Friday of the Month
# This is because for the NIK European Option, the last trading day is the business day preceding the second Friday of expiry Month
# In the dataset, they are quoted as the second Friday of the Month
# HOWEVER, in the dataset, there may be a holiday in the second Friday of the month, hence, the last trading day was actually
# the second Thusday of the Month. Hence, the ExpDateNIK function actually filter all the expdate that is the second Friday
# of the month OR the second  Thursday of the month. The replicating problem will be sold outside of the function later on
ExpDateNIK <- function(date){
  FirstMonthDate <- paste(year(date),"-",month(date),"-",'01',sep = "")
  if(wday(date)!=6 & wday(date) != 5){
    ans = as.logical(0)
  } else if(wday(date)==6){
    if(day(date) < 8 | day(date) > 14){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(21,14)),as.logical(1),as.logical(0))
    }
  } else{
    if(day(date) < 8 | day(date) > 14){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(20,13)),as.logical(1),as.logical(0))
    }
  }
  return(ans)
}
# ExpDateKOS function is the function  to identify whether a given date is the second Thurdays of the Month
# This is because for the KOS European Option, the last trading day is the business day preceding the second Thursday of expiry Month
# In the dataset, they are quoted as the second Thursday of the Month
ExpDateKOS <- function(date){
  FirstMonthDate <- paste(year(date),"-",month(date),"-",'01',sep = "")
  if(wday(date)!= 5 & wday(date) != 6){
    ans = as.logical(0)
  } else if(date == "2010-10-08"){
    ans = as.logical(1)
  } else if(wday(date)==5){
    if(day(date) < 8 | day(date) > 14){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(20,13)),as.logical(1),as.logical(0))
    }
  } else{
    if(day(date) < 9 | day(date) > 15){
      ans = as.logical(0)
    }else{
      ans <- ifelse(is.element(wday(FirstMonthDate) + day(date),c(21,14)),as.logical(1),as.logical(0))
    }
  }
  return(ans)
}
# ExpDateHSI function is the function  to identify whether a given date is the day after the last trading day of the month
# The last trading day of the month should be the end day of the month but not Weekend Day
ExpDateHSI <- function(date){
  if(wday(date)== 1 | wday(date) == 7){
    ans = as.logical(0)
  } else if(date == "2011-08-29" | date == "2010-05-27") {
    ans = as.logical(0)
  } else{
      ans = as.logical(1)
    }
  return(ans)
}
#-------
# FindFirstTrade is the function to find the date at which we have the data and at the same time, closet to the should-be first
# trading day of the option for next month expiration. 
#--------
FindFirstTrade <- function(DateAvail,FirstTradeDate){
  Filter <- DateAvail %>% #filter(month(TradeDate) ==month(FirstTradeDate) & year(TradeDate)==year(FirstTradeDate))%>%
    mutate(tochose = TradeDate - FirstTradeDate) %>% filter(tochose >=0) %>% filter(tochose == min(tochose)) %>% select(TradeDate)
  return(as.numeric(Filter))
}
#Given the opton dataset, we will have a series of trading day and a series of expiredate.
# Step 1: Decide the epxDate for monthly expriration options, which is the third Firday of the month. Note that in the dataset, sometimes
# they will report the expday as Saturday, not Friday.
# Step 2: From the expiration date list, we will decide the First trading day after the expiration date. It should be the Monday
# or Tuesday nextweek if MOnday was holiday.
# Step 3: Now from the list of trading date and expiration date, we should filter the data so that we only use the option data
# from the first trading day and maturity by next month.
####
# If index is NIK or KOS, then, additional action should be taken to delete the possible replicated expdate to take into
# account possibility of double date selected in the month
# NOTE: KOS To add the day "2010-10-08" because there was most close date to the last trading day of that month
# NOTE: HSI Delete "2011-08-29" and "2010-05-27" as was not the last trading day.
prefilter <- function(optdta,index){
if(index=="nik"){
  expdate <- data_frame(expDate = unique(optdta$expDate)) %>% filter(!is.na(expDate))%>% rowwise() %>%
    mutate(toinclude = ExpDateNIK(expDate)) %>% filter(as.logical(toinclude)) %>%
    mutate(month = month(expDate)) %>% mutate(year = year(expDate)) %>% filter(year < 2016) %>%
    ungroup() %>% arrange(year,month)
  MonthRep <- expdate %>% count(year,month) %>% filter(n == 2)
  expdate <- left_join(expdate,MonthRep,by = c("month","year")) %>%
    mutate(toDel = ifelse(!is.na(n),ifelse(wday(expDate)==5,0,1),2)) %>%
    filter(toDel > 0) %>% select(expDate)%>%
    mutate(TradeDate = as.Date.numeric(ifelse(wday(expDate)==6,expDate +3 , expDate + 4),origin = "1970-01-01"))
  tradedate <- data_frame(TradeDate = unique(optdta$date))
  FirstTradeDate <- vector()
  FirstTradeDate[1] <- as.numeric(tradedate$TradeDate[1])
  for(i in 1:(nrow(expdate)-1)){
    FirstTradeDate[i+1] = FindFirstTrade(tradedate,expdate$TradeDate[i])
  }
  FirstTradeDate <- as.Date(FirstTradeDate,"1970-01-01")
  NonOverlap <- expdate %>% mutate(FirstTradeDate = FirstTradeDate) %>% select(FirstTradeDate,expDate)
  NonOverlapOpt <- optdta %>% filter(is.element(date,NonOverlap$FirstTradeDate) & is.element(expDate,NonOverlap$expDate)) %>% filter(maturity < 40)
} else if(index == "kos"){
  expdate <- data_frame(expDate = unique(optdta$expDate)) %>% filter(!is.na(expDate))%>% rowwise() %>%
    mutate(toinclude = ExpDateKOS(expDate)) %>% filter(as.logical(toinclude)) %>%
    mutate(month = month(expDate)) %>% mutate(year = year(expDate)) %>% filter(year < 2016) %>%
    ungroup() %>% select(expDate)%>%
    mutate(TradeDate = as.Date.numeric(ifelse(wday(expDate)==6,expDate +3 , expDate + 1),origin = "1970-01-01"))
  tradedate <- data_frame(TradeDate = unique(optdta$date))
  FirstTradeDate <- vector()
  FirstTradeDate[1] <- as.numeric(tradedate$TradeDate[1])
  for(i in 1:(nrow(expdate)-1)){
    FirstTradeDate[i+1] = FindFirstTrade(tradedate,expdate$TradeDate[i])
  }
  FirstTradeDate <- as.Date(FirstTradeDate,"1970-01-01")
  NonOverlap <- expdate %>% mutate(FirstTradeDate = FirstTradeDate) %>% select(FirstTradeDate,expDate)
  NonOverlapOpt <- optdta %>% filter(is.element(date,NonOverlap$FirstTradeDate) & is.element(expDate,NonOverlap$expDate)) %>% filter(maturity < 40)
} else if(index == "hsi"){
  expdate <- data_frame(expDate = unique(optdta$expDate)) %>% filter(!is.na(expDate))%>% rowwise() %>%
    mutate(toinclude = ExpDateHSI(expDate)) %>% filter(as.logical(toinclude)) %>%
    mutate(month = month(expDate)) %>% mutate(year = year(expDate)) %>% filter(year < 2016) %>%
    ungroup() %>% arrange(year,month) %>% select(expDate)%>%
    mutate(TradeDate = as.Date.numeric(ifelse(wday(expDate)==6,expDate +3 , expDate + 1),origin = "1970-01-01"))
  tradedate <- data_frame(TradeDate = unique(optdta$date))
  FirstTradeDate <- vector()
  FirstTradeDate[1] <- as.numeric(tradedate$TradeDate[1])
  for(i in 1:(nrow(expdate)-1)){
    FirstTradeDate[i+1] = FindFirstTrade(tradedate,expdate$TradeDate[i])
  }
  FirstTradeDate <- as.Date(FirstTradeDate,"1970-01-01")
  NonOverlap <- expdate %>% mutate(FirstTradeDate = FirstTradeDate) %>% select(FirstTradeDate,expDate)
  NonOverlapOpt <- optdta %>% filter(is.element(date,NonOverlap$FirstTradeDate) & is.element(expDate,NonOverlap$expDate)) %>% filter(maturity < 40)
} else {
  expdate <- data_frame(expDate = unique(optdta$expDate)) %>% filter(!is.na(expDate))%>% rowwise() %>%
    mutate(toinclude = ExpDate(expDate)) %>% filter(as.logical(toinclude)) %>%
    mutate(month = month(expDate)) %>% mutate(year = year(expDate)) %>% filter(year < 2016) %>%
    ungroup() %>% arrange(year,month) %>% select(expDate)%>%
    mutate(TradeDate = as.Date.numeric(ifelse(wday(expDate)==6,expDate +3 , expDate + 2),origin = "1970-01-01"))
  tradedate <- data_frame(TradeDate = unique(optdta$date))
  FirstTradeDate <- vector()
  FirstTradeDate[1] <- as.numeric(tradedate$TradeDate[1])
  for(i in 1:(nrow(expdate)-1)){
    FirstTradeDate[i+1] = FindFirstTrade(tradedate,expdate$TradeDate[i])
  }
  FirstTradeDate <- as.Date(FirstTradeDate,"1970-01-01")
  NonOverlap <- expdate %>% mutate(FirstTradeDate = FirstTradeDate) %>% select(FirstTradeDate,expDate)
  NonOverlapOpt <- optdta %>% filter(is.element(date,NonOverlap$FirstTradeDate) & is.element(expDate,NonOverlap$expDate)) %>% filter(maturity < 40)
}
  return(NonOverlapOpt)
}
