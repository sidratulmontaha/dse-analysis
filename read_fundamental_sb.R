library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(jsonlite)
library(httr)

sb_data_matrix_json <- content(GET("https://stockbangladesh.com/ajax/data_matrix?_dc=1517076729878&page=1&start=0&limit=1000"), as='text')

sb_data_matrix <- fromJSON(sb_data_matrix_json, flatten = TRUE)[['maingrid']]

data_matrix <- sb_data_matrix[, c('code', 'sector', 'category', 'lastprice', 'pe', 'id')]

rm(sb_data_matrix, sb_data_matrix_json)
