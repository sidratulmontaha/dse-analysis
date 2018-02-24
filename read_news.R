library(httr)

convert.brackets <- function(x){
  if(grepl("\\(.*\\)", x)){
    gsub("[^0-9\\.\\-]", "", trimws(paste0("-", gsub("\\(|\\)", "", x))))
  } else {
    gsub("[^0-9\\.\\-]", "", trimws(x))
  }
}

eps_data <- data.frame(symbol=character(), quarter=character(), from_date=as.Date(character()), to_date=as.Date(character()),eps=numeric(),eps_against=numeric(), stringsAsFactors = FALSE )

for(s in data_matrix[,'code']){
  tryCatch({
    #s <- "UNIQUEHRL"
    
    print(paste("Symbol: ", s))
    
    form <- list(cboSymbol = s)
    
    doc <- htmlParse(POST("http://dsebd.org/old_news.php", body = form, encode = "form"))
    
    news_table <- getNodeSet(doc, "//td[@width='70%']")
    
    #news_table <- news_table[which(sapply(news_table, function(x) nchar(xmlValue(x))) > 100)]
    
    eps_regex <- "\\((.+?) Un-audited\\):.*? (?:EPS|EPU) was Tk\\. (.+?) for (.+?)-(.+?)[,]? (.+?) as against Tk\\. (.+?) for (.+?)-(.+?)[,]? (.+?);"
    
    for(n in 1:length(news_table)){
      match_result <- str_match(xmlValue(news_table[[n]]), eps_regex)
      if(is.na(match_result[1])){
        next()
      }
      
      eps_data <- rbind(eps_data, data.frame(symbol=s, quarter=match_result[2], from_date=as.Date(paste0("01 ", match_result[4], " ", match_result[6]), format = "%d %B %Y"), to_date=as.Date(paste0("01 ", match_result[5], " ", match_result[6]), format = "%d %B %Y"),eps=as.numeric(convert.brackets(match_result[3])),eps_against=as.numeric(convert.brackets(match_result[7]))))
    }
  }, error=function(e){cat(s, " ERROR :",conditionMessage(e), "\n")})
}

rm(eps_regex,doc,n)


