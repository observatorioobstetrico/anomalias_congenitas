library(dplyr)
library(httr)
library(ggplot2)
library(getPass)
library(repr)
library(httr)

token = getPass()

url_base = "https://bigdata-api.fiocruz.br"

convertRequestToDF <- function(request, column_names = c()){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  if (!length(column_names)){
    column_names <- unname(variables)
  }
  values = content(request,)$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) {
    row <- r
    row[sapply(row, is.null)] <- NA
    rbind(unlist(row))
  } )))
  names(df) <- column_names
  return(df)
}

endpoint = paste0(url_base,"/","show_tables")
request <- POST(url = endpoint, body = list("token" = token), encode = "json")
as_tibble(content(request))

##Anomalias
#estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
dataframe <- data.frame()

endpoint <- paste0(url_base,"/","sql_query")


params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT * FROM \\"datasus-sinasc\\" WHERE (ano_nasc BETWEEN 2014 AND 2020) AND (IDANOMAL = 1 OR CODANOMAL IS NOT NULL) ","fetch_size": 10000}
          }
          
        }')

request <- POST(url = endpoint, body = params, encode = "form")
df_premat <- convertRequestToDF(request)
dataframe <- rbind(dataframe, df_premat)

repeat {
  
  cursor <- content(request)$cursor
  
  params = paste0('{
        "token": {
          "token": "',token,'"
        },
        "sql": {
          "sql": {"cursor": "',cursor,'"}
        }
        }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  
  if (length(content(request)$rows) == 0)
    break
  
  df_premat <- convertRequestToDF(request,colnames(dataframe))
  dataframe <- rbind(dataframe, df_premat)
}

nrow(dataframe)

write.csv(dataframe,"anomalias_microdados_01-20.csv")