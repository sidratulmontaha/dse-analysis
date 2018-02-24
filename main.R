library(xlsx)

options(scipen=999)

#source('read_data.R')
#source('read_fundamental_sb.R')
#source('read_fundamental_dse.R')

analysis <- data.frame(data_matrix[,'code'], stringsAsFactors = FALSE)
analysis[, 'lastprice'] <-  data_matrix[, 'lastprice']
analysis[, 'category'] <-  data_matrix[, 'category']
analysis[, 'pe'] <-  data_matrix[, 'pe']
analysis[, 'sector'] <-  data_matrix[, 'sector']
analysis[, 'public_shares'] <- data_matrix[, 'outstanding_securities'] * data_matrix[, 'public_holding_mid'] / 100 
analysis[, 'public_cap'] <- data_matrix[, 'outstanding_securities'] * data_matrix[, 'lastprice'] * data_matrix[, 'public_holding_mid'] / 100 

rs_start_date <- c(as.Date("2018-01-25"), as.Date("2018-01-03"), as.Date("2017-11-26"))
rs_end_date <- c(as.Date("2018-02-05"), as.Date("2018-01-15"), as.Date("2017-12-24"))
index_symbol <- '00DSEX'

percent_change <- function(x,lag = 1)
{
   n = length(x)
   pchange = c((x[(1+lag):n] - x[1:(n-lag)])/x[1:(n-lag)],NA)
   return(pchange)
}

for(n in 1:length(rs_start_date)){
  print(n)
  index_return <- (share_prices[share_prices$date == rs_end_date[n] & share_prices$symbol == index_symbol, 6] - share_prices[share_prices$date == rs_start_date[n] & share_prices$symbol == index_symbol, 6])/share_prices[share_prices$date == rs_start_date[n] & share_prices$symbol == index_symbol, 6]
  
  for(s in data_matrix[,'code']){
    analysis[analysis[1] == s, paste('relative_strength_', n, sep = "")] <- ((share_prices[share_prices$date == rs_end_date[n] & share_prices$symbol == s, 6][1] - share_prices[share_prices$date == rs_start_date[n] & share_prices$symbol == s, 6][1])/share_prices[share_prices$date == rs_start_date[n] & share_prices$symbol == s, 6][1])
    #analysis[analysis[1] == s, "yoy_profit_growth"] <- mean(percent_change(financial_performance_list[[s]]['profit_cont_ops'][,1]), na.rm = TRUE)
  }
  
  analysis[!is.na(analysis[,paste('relative_strength_', n, sep = "")]) & analysis[,paste('relative_strength_', n, sep = "")] < (index_return/2), paste('relative_strength_', n, sep = "")] <- NA
}

eps_lookback <- 1

for(s in data_matrix[,'code']){
  tryCatch({
    #s <- "BIFC"
    q1 <- sb_eps_data[sb_eps_data['symbol'] == s & sb_eps_data['quarter'] == 'Q1',]
    q1 <- mean(percent_change(tail(q1 ,1 + eps_lookback)[,'eps']), na.rm = TRUE)
    
    q2 <- sb_eps_data[sb_eps_data['symbol'] == s & sb_eps_data['quarter'] == 'Q2',]
    q2 <- mean(percent_change(tail(q2 ,1 + eps_lookback)[,'eps']), na.rm = TRUE)
    
    q3 <- sb_eps_data[sb_eps_data['symbol'] == s & sb_eps_data['quarter'] == 'Q3',]
    q3 <- mean(percent_change(tail(q3 ,1 + eps_lookback)[,'eps']), na.rm = TRUE)
    
    q4 <- sb_eps_data[sb_eps_data['symbol'] == s & sb_eps_data['quarter'] == 'Q4',]
    q4 <- mean(percent_change(tail(q4 ,1 + eps_lookback)[,'eps']), na.rm = TRUE)
    
    analysis[analysis[1] == s, "q1_eps_growth"] <- q1
    analysis[analysis[1] == s, "q2_eps_growth"] <- q2
    analysis[analysis[1] == s, "q3_eps_growth"] <- q3
    analysis[analysis[1] == s, "q4_eps_growth"] <- q4
    
    analysis[analysis[1] == s, "overall_eps_growth"] <- mean(c(q1,q2,q3,q4)[c(q1,q2,q3,q4)!= 0], na.rm = TRUE)
  }, error=function(e){cat(s, " ERROR :",conditionMessage(e), "\n")})
}

for(s in data_matrix[,'code']){
  sorted_eps <- eps_data[eps_data['symbol'] == s,][rev(order(as.Date(eps_data$from_date))),]
  sorted_eps$eps_change <- (sorted_eps$eps-sorted_eps$eps_against)/sorted_eps$eps_against
  analysis[analysis[1] == s, "dse_qoq_eps_growth"] <- mean(sorted_eps$eps_change, na.rm = TRUE)
}

rm(n, s,q1,q2,q3,q4,lines,index_return,index_symbol,eps_response,eps_lookback,instrument_id,lines)

write.xlsx(analysis, "analysis-1.xlsx")
