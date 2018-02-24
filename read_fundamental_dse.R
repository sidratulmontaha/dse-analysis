library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(jsonlite)
library(stringr)

exclusion_list <- c("00DSEX", "00DSEGEN", '00DS30', '00DSES', 'DSES', 'DSEX', 
                    'DS30', "01.Bank", "03.Ceramics_Sector","05.Financial_Institutions",
                    "07.Fuel_&_Power", "09.IT_Sector", "11.Miscellaneous", "13.Paper_&_Printing", 
                    "15.Services_&_Real_Estate", "17.Telecommunication", "19.Travel_&_Leisure", 
                    "02.Cement", "04.Engineering", "06.Food_&_Allied", "08.Insurance", 
                    "10.Jute", "12.Mutual_Funds", "14.Pharmaceuticals_&_Chemicals",
                    "16.Tannery_Industries", "18.Textile", "20.Bond")

financial_performance_list <- list()

for(s in data_matrix[,'code']){
  tryCatch({
    #s <- "KOHINOOR"
    print(paste("Symbol: ", s))
    doc <- htmlParse(getURL(paste("http://dsebd.org/displayCompany_print.php?name=", s, sep = "")),asText=TRUE)
    
    market_info_table <- getNodeSet(doc, '//table[@id="company"]')
    
    data_matrix[data_matrix$code == s, 'market_cap'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[5]], stringsAsFactors=FALSE, header = FALSE)[7,2]))
    
    data_matrix[data_matrix$code == s, 'authorized_cap'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[7]], stringsAsFactors=FALSE, header = FALSE)[1,2]))
    data_matrix[data_matrix$code == s, 'paidup_cap'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[7]], stringsAsFactors=FALSE, header = FALSE)[2,2]))
    data_matrix[data_matrix$code == s, 'outstanding_securities'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[7]], stringsAsFactors=FALSE, header = FALSE)[4,2]))
    
    data_matrix[data_matrix$code == s, 'cash_dividend'] <- readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[1,2]
    data_matrix[data_matrix$code == s, 'stock_dividend'] <- readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[2,2]
    data_matrix[data_matrix$code == s, 'right_issue'] <- readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[3,2]
    data_matrix[data_matrix$code == s, 'year_end'] <- readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[4,2]
    data_matrix[data_matrix$code == s, 'reserve_surplus_without_oci'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[5,2]))
    data_matrix[data_matrix$code == s, 'oci'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[11]], stringsAsFactors=FALSE, header = FALSE)[6,2]))
    
    data_matrix[data_matrix$code == s, 'q1_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,2]))
    data_matrix[data_matrix$code == s, 'q1_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,2]))
    data_matrix[data_matrix$code == s, 'q1_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,2]))
    data_matrix[data_matrix$code == s, 'q1_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,2]))
    data_matrix[data_matrix$code == s, 'q1_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,2]))
    data_matrix[data_matrix$code == s, 'q1_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,2]))
    data_matrix[data_matrix$code == s, 'q1_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,2]))
    data_matrix[data_matrix$code == s, 'q1_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,2]))
    
    
    data_matrix[data_matrix$code == s, 'q2_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,3]))
    data_matrix[data_matrix$code == s, 'q2_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,3]))
    data_matrix[data_matrix$code == s, 'q2_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,3]))
    data_matrix[data_matrix$code == s, 'q2_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,3]))
    data_matrix[data_matrix$code == s, 'q2_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,3]))
    data_matrix[data_matrix$code == s, 'q2_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,3]))
    data_matrix[data_matrix$code == s, 'q2_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,3]))
    data_matrix[data_matrix$code == s, 'q2_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,3]))
    
    data_matrix[data_matrix$code == s, 'hy_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,4]))
    data_matrix[data_matrix$code == s, 'hy_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,4]))
    data_matrix[data_matrix$code == s, 'hy_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,4]))
    data_matrix[data_matrix$code == s, 'hy_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,4]))
    data_matrix[data_matrix$code == s, 'hy_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,4]))
    data_matrix[data_matrix$code == s, 'hy_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,4]))
    data_matrix[data_matrix$code == s, 'hy_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,4]))
    data_matrix[data_matrix$code == s, 'hy_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,4]))
    
    data_matrix[data_matrix$code == s, 'q3_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,5]))
    data_matrix[data_matrix$code == s, 'q3_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,5]))
    data_matrix[data_matrix$code == s, 'q3_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,5]))
    data_matrix[data_matrix$code == s, 'q3_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,5]))
    data_matrix[data_matrix$code == s, 'q3_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,5]))
    data_matrix[data_matrix$code == s, 'q3_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,5]))
    data_matrix[data_matrix$code == s, 'q3_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,5]))
    data_matrix[data_matrix$code == s, 'q3_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,5]))
    
    data_matrix[data_matrix$code == s, 'm9_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,6]))
    data_matrix[data_matrix$code == s, 'm9_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,6]))
    data_matrix[data_matrix$code == s, 'm9_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,6]))
    data_matrix[data_matrix$code == s, 'm9_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,6]))
    data_matrix[data_matrix$code == s, 'm9_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,6]))
    data_matrix[data_matrix$code == s, 'm9_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,6]))
    data_matrix[data_matrix$code == s, 'm9_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,6]))
    data_matrix[data_matrix$code == s, 'm9_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,6]))
    
    data_matrix[data_matrix$code == s, 'annual_turnover_revenue'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[5,7]))
    data_matrix[data_matrix$code == s, 'annual_profit_continuing_ops'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[6,7]))
    data_matrix[data_matrix$code == s, 'annual_profit_period'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[7,7]))
    data_matrix[data_matrix$code == s, 'annual_total_comprehensive_income'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[8,7]))
    data_matrix[data_matrix$code == s, 'annual_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[10,7]))
    data_matrix[data_matrix$code == s, 'annual_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[11,7]))
    data_matrix[data_matrix$code == s, 'annual_cont_ops_basic_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[13,7]))
    data_matrix[data_matrix$code == s, 'annual_cont_ops_diluted_eps'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[13]], stringsAsFactors=FALSE, header = FALSE)[14,7]))
    
    financial_performance<-cbind(readHTMLTable(market_info_table[[19]], stringsAsFactors=FALSE, header = FALSE,skip.rows=c(1:3), colClasses = c(rep("numeric", 13))), readHTMLTable(market_info_table[[21]], stringsAsFactors=FALSE, header = FALSE, skip.rows = c(1:4))[,8])
    colnames(financial_performance) <- c("year", "eps_basic_original", "eps_basic_restated", "eps_diluted", "eps_cont_ops_basic_original", "eps_cont_ops_basic_restated", "eps_cont_ops_diluted", "nav_original", "nav_restated", "nav_diluted", "profit_cont_ops", "profit_for_year", "total_comprehensive_income_year", "dividend")
    
    financial_performance_list[[s]] <- financial_performance
    #data_matrix[data_matrix$code == s, 'financial_performance'] <- list(financial_performance)
    
    data_matrix[data_matrix$code == s, 'listing_year'] <- as.numeric(gsub(",", "", readHTMLTable(market_info_table[[24]], stringsAsFactors=FALSE, header = FALSE)[1,2]))
    sponsor_table <- readHTMLTable(market_info_table[[24]], stringsAsFactors=FALSE, header = FALSE)
    sponsor_table <- sponsor_table[startsWith(sponsor_table[,1], "Sponsor"),]
    
    data_matrix[data_matrix$code == s, 'director_holding_earliest'] <- as.numeric(str_extract_all(sponsor_table[1,1], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'director_holding_mid'] <- as.numeric(str_extract_all(sponsor_table[2,1], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'director_holding_latest'] <- as.numeric(str_extract_all(sponsor_table[3,1], "[\\d.]+$")[[1]])
    
    data_matrix[data_matrix$code == s, 'govt_holding_earliest'] <- as.numeric(str_extract_all(sponsor_table[1,2], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'govt_holding_mid'] <- as.numeric(str_extract_all(sponsor_table[2,2], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'govt_holding_latest'] <- as.numeric(str_extract_all(sponsor_table[3,2], "[\\d.]+$")[[1]])
    
    data_matrix[data_matrix$code == s, 'institute_holding_earliest'] <- as.numeric(str_extract_all(sponsor_table[1,3], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'institute_holding_mid'] <- as.numeric(str_extract_all(sponsor_table[2,3], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'institute_holding_latest'] <- as.numeric(str_extract_all(sponsor_table[3,3], "[\\d.]+$")[[1]])
    
    data_matrix[data_matrix$code == s, 'foreign_holding_earliest'] <- as.numeric(str_extract_all(sponsor_table[1,4], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'foreign_holding_mid'] <- as.numeric(str_extract_all(sponsor_table[2,4], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'foreign_holding_latest'] <- as.numeric(str_extract_all(sponsor_table[3,4], "[\\d.]+$")[[1]])
    
    data_matrix[data_matrix$code == s, 'public_holding_earliest'] <- as.numeric(str_extract_all(sponsor_table[1,5], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'public_holding_mid'] <- as.numeric(str_extract_all(sponsor_table[2,5], "[\\d.]+$")[[1]])
    data_matrix[data_matrix$code == s, 'public_holding_latest'] <- as.numeric(str_extract_all(sponsor_table[3,5], "[\\d.]+$")[[1]])
    
    data_matrix[data_matrix$code == s, 'present_operational_status'] <- readHTMLTable(market_info_table[[26]], stringsAsFactors=FALSE, header = FALSE)[1,2]

  }, error=function(e){cat(s, " ERROR :",conditionMessage(e), "\n")})
}

rm(financial_performance, doc, exclusion_list, market_info_table, s, sponsor_table)
