snapshot <- function(date1, date2, option){
  
  #read file
  #read file
  IMCM_trade <- read_excel(
    "IMCM.xlsx", col_names = TRUE, 
    col_types = c("text", "text", "date", "text", "text", "text", "text"))
  IMCM_trade <- IMCM_trade %>% 
                mutate(date = as.Date(date)) %>%
                mutate_at(vars(Quantity, Price, Amount), funs(as.numeric))
  
  #create two transaction record
  if (option == 0) { date1 <- "2017/12/29"}
  IMCM_trade1 <- filter(IMCM_trade, date <= as.Date(date1))
  IMCM_trade2 <- filter(IMCM_trade, date <= as.Date(date2))
  
  #get sotck price for both query date
  px_1 <- select(getPrice(unique(IMCM_trade$ID), 
                          as.Date(date1), as.Date(date1)), ID, px)
  px_2 <- select(getPrice(unique(IMCM_trade$ID), 
                          as.Date(date2), as.Date(date2)), ID, px)
  
  #generate position status for both query date and combined to one
  IMCM1<- IMCM_trade1 %>% position() %>% rename(Quantity1 = Quantity_sold)
  IMCM2<- IMCM_trade2 %>% position() %>% rename(Quantity2 = Quantity_sold)
  IMCM <- left_join(IMCM2, IMCM1)
  IMCM <- left_join(IMCM, px_1)
  IMCM[is.na(IMCM)] <- 0
  
  #calculate current position
  IMCM <- IMCM %>% group_by(ID) %>%
          mutate(Quantity_sold = Quantity2 - Quantity1) %>%
          mutate(Quantity_remain = Quantity - Quantity2) %>%
          mutate(cost = ifelse(Type == "sell", Price, 
                        ifelse(date >= date1, Price, px))) %>%
          mutate(value_sold = Quantity_sold*cost, 
                 value_remain = Quantity_remain*cost) %>%
          ungroup() %>% group_by(ID, Type) %>%
          summarise(Quantity_sold = sum(Quantity_sold), 
                    Quantity_remain = sum(Quantity_remain),
                    value_sold = sum(value_sold), 
                    value_remain = sum(value_remain)) %>% ungroup()
  
  #report snapshot
  IMCM3 <- IMCM %>% filter(Type == "Buy") %>% select(-Type)
  IMCM4 <- IMCM %>% filter(Type == "Sell") %>% 
                    rename(amount_sold = value_sold) %>% select(ID, amount_sold)
  IMCM <- left_join(IMCM3, IMCM4)
  IMCM <- left_join(IMCM, px_2)
  IMCM[is.na(IMCM)] <- 0
  IMCM <- IMCM %>%
          mutate(cost_remain = round(value_remain / Quantity_remain, digits = 2),  
                 marketvalue = Quantity_remain * px,
                 cost_sold = round(amount_sold / Quantity_sold, digits = 2),  
                 unreal_gain = marketvalue - value_remain,
                 unreal_gain_rate = unreal_gain / value_remain, 
                 real_gain = amount_sold - value_sold,
                 real_gain_rate = real_gain / amount_sold)
  IMCM[is.na(IMCM)] <- 0
  holdings <- IMCM %>% select(ID, Quantity = Quantity_remain,
                              Unit_cost = cost_remain, Price = px,
                              Cost = value_remain, Market_value = marketvalue,
                              Gain = unreal_gain, Return = unreal_gain_rate) %>%
                              filter(Quantity > 0) %>% 
                              arrange(grepl("HK Equity", ID), desc(Market_value))
  sprintf("%1.2f%%", 100 * unreal_gain / value_remain)
}

position <- function(IMCM_trade){

IMCM <- IMCM_trade %>% group_by(ID) %>%
        mutate(Quantity0 = ifelse(Type == "Sell", Quantity, 0)) %>%
        mutate(Quantity1 = ifelse(Type == "Sell", 0, cumsum(Quantity))) %>%
        mutate(Quantity2 = ifelse(Type == "Sell", 0, sum(Quantity0) - Quantity1)) %>%
        mutate(Quantity3 = ifelse(Type == "Sell", 0, 
                                  pmax(sum(Quantity0) - Quantity1 + Quantity,0))) %>%
        mutate(Quantity_sold = ifelse( Quantity2 >= 0, Quantity, Quantity3)) %>%
        select(ID, date, Type, Price, Quantity, Quantity_sold) %>% ungroup()
       
return(IMCM)

}

