library(tidyverse)
library(readxl)
library(Rblpapi) 
library(zoo)

blpConnect()

#return daily nav between date01 and date02
NAV_Compute <- function(date01, date02) {
  
  #read file
  IMCM_trade <- read_excel(
    "IMCM.xlsx", col_names = TRUE, 
    col_types = c("text", "text", "date", "text", "text", "text", "text"))
  IMCM_trade <- IMCM_trade %>%
    mutate(date = as.Date(date)) %>%
    mutate_at(vars(Quantity, Price, Amount), funs(as.numeric))
  
  #create Buy and Sell
  Buy <- IMCM_trade %>%
    filter(Type == "Buy") %>%
    group_by(ID, date) %>%
    summarise(share_buy = sum(Quantity), 
              value_buy = sum(Amount), 
              px_buy = value_buy/share_buy)
  
  Sell <- IMCM_trade %>%
    filter(Type == "Sell") %>%
    group_by(ID, date) %>%
    summarise(share_sell = sum(Quantity), 
              value_sell = sum(Amount), 
              px_sell = value_sell/share_sell)
  
  #get stock price
  PRICE <- getPrice(unique(IMCM_trade$ID), as.Date(date01), as.Date(date02))
  
  PRICE <- PRICE %>%
    left_join(Buy) %>%
    left_join(Sell)
  PRICE[is.na(PRICE)] <- 0
  
  #compute NAV
  NAV <- PRICE %>% 
    group_by(ID) %>%
    mutate(share_end = cumsum(share_buy) - cumsum(share_sell), 
           share_begin = share_end - share_buy + share_sell) %>% 
    mutate(px0 = c(0, px[1: (length(px)-1)])) %>%
    mutate(value_begin = share_begin*px0 + value_buy, 
           value_end = share_end*px +value_sell) %>%
    mutate(value_begin1 = ifelse(grepl("US Equity$", ID), 7.8 ,1) * value_begin, 
           value_end1 = ifelse(grepl("US Equity$", ID), 7.8 ,1) * value_end) %>%
    ungroup() %>%
    group_by(date) %>%
    summarise(return = sum(value_end1) / sum(value_begin1)) %>%
    mutate(nav = cumprod(return)) %>%
    select(date, nav)
  
  return(NAV)
  
}

#get stock price from bloomberg
getPrice <- function(ticker, date01, date02) {
  
  #read price from blp
  price <- bdh(ticker, "PX_LAST", date01, date02, include.non.trading.days = TRUE)
  
  #transform list into dataframe
  price1 <- mapply(function(x, y) {y$ID <- x; y},
                   names(price), price, 
                   USE.NAMES = FALSE, SIMPLIFY = FALSE)
  PRICE <- do.call("bind_rows", price1)
  PRICE <- na.locf(PRICE)
  PRICE <- PRICE %>% 
    mutate(date = as.Date(date), px = as.double(PX_LAST)) %>% 
    select(date, ID, px)
  
  return(PRICE)
}

# ggplot(NAV, aes(date, nav)) + geom_line() + theme_bw() +
#   theme(panel.grid = element_blank()) +
#   ggtitle("组")
# 
# library(dygraphs)
# library(xts)
# test <- as.xts(NAV %>% select(-date), order.by = NAV$date)
# 
# dygraph(test) %>%
#   dySeries("nav", label = "NAV") %>%
#   dyOptions(stackedGraph = TRUE)
# 
