library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(jsonlite)
library(httr)
library(stringr)

sb_eps_data <- data.frame(symbol=character(), quarter=character(), year=character(),eps=numeric(), stringsAsFactors = FALSE )

for(s in data_matrix[,'code']){
  tryCatch({
    #s <- "ALLTEX"
    print(paste("Symbol: ", s))
    
    instrument_id <- data_matrix[data_matrix['code'] == s,'id']
    eps_response <- url(paste0("https://stockbangladesh.com/ajax/load_block/block_name=block.eps_history_chart_quarter_to_quarter:instrument_id=", instrument_id))
    
    lines <- readLines(eps_response)
    close(eps_response)
    
    category_line <- trimws(lines[17])
    q1_line <- trimws(lines[41])
    q2_line <- trimws(lines[44])
    q3_line <- trimws(lines[47])
    q4_line <- trimws(lines[50])
    
    match_result <- gsub('"', '', str_match(category_line, "categories: \\[(.*?)\\],")[2])
    categories <- unlist(strsplit(match_result, fixed = TRUE, split = ","))
    
    match_result <- str_match(q1_line, "data: \\[(.*?)\\]")[2]
    q1_eps <- as.numeric(unlist(strsplit(match_result, fixed = TRUE, split = ",")))
    match_result <- str_match(q2_line, "data: \\[(.*?)\\]")[2]
    q2_eps <- as.numeric(unlist(strsplit(match_result, fixed = TRUE, split = ",")))
    match_result <- str_match(q3_line, "data: \\[(.*?)\\]")[2]
    q3_eps <- as.numeric(unlist(strsplit(match_result, fixed = TRUE, split = ",")))
    match_result <- str_match(q4_line, "data: \\[(.*?)\\]")[2]
    q4_eps <- as.numeric(unlist(strsplit(match_result, fixed = TRUE, split = ",")))
    
    for(n in 1:length(categories)){
      sb_eps_data <- rbind(sb_eps_data, data.frame(symbol=s, quarter="Q1", year=categories[n],eps=q1_eps[n], stringsAsFactors = FALSE ))
      sb_eps_data <- rbind(sb_eps_data, data.frame(symbol=s, quarter="Q2", year=categories[n],eps=q2_eps[n], stringsAsFactors = FALSE ))
      sb_eps_data <- rbind(sb_eps_data, data.frame(symbol=s, quarter="Q3", year=categories[n],eps=q3_eps[n], stringsAsFactors = FALSE ))
      sb_eps_data <- rbind(sb_eps_data, data.frame(symbol=s, quarter="Q4", year=categories[n],eps=q4_eps[n], stringsAsFactors = FALSE ))
    }
    
    sb_eps_data <- sb_eps_data[sb_eps_data['eps'] != 0,]
  }, error=function(e){cat(s, " ERROR :",conditionMessage(e), "\n")})
}

rm(q1_eps, q2_eps,q3_eps,q4_eps,match_result,q1_line,q2_line,q3_line,q4_line,categories,category_line)
