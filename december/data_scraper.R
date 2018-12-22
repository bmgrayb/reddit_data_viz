library(rvest)
library(tidyverse)
library(lubridate)
library(stringi)
library(stringr)

#URL for lake data
lake_url <- "http://www.aos.wisc.edu/~sco/lakes/Mendota-ice.html"
lake_html <- "data/raw_lake_data.html"

#download file if needed
if(!file.exists(lake_html) | FALSE){
  download.file(lake_url,lake_html,method="auto",mode="w")
}

lake_data <- read_html(lake_html)

#convert to table from html
lake_table <- lake_data %>%
  html_nodes("table") %>%
  html_table()

#split out the 4 different tables
tab1 <- lake_table[[1]]
tab2 <- lake_table[[2]]
tab3 <- lake_table[[3]]
tab4 <- lake_table[[4]]

#funciton to help us fix data
#pass in a regex to extract, and whether to include --- at the end
fix_data <- function(s, p, addDashes){

  contains_quotes <- FALSE
  ind <- -1
  if(str_detect(s,"\"")){
    contains_quotes <- TRUE
    ind <- regexpr("\"",s)
  }
    
  column <- str_extract_all(string=s,pattern = p)
  
  if(addDashes){
    column <- c(column,"---")
  }
  column <- paste(unlist(column), collapse='\n')
  
  if(contains_quotes){
    stri_sub(column, ind, ind-1) <- "0000-00\n"
  }
  
  return(column)
}

#function to split the columns...this is becuase the HTML table is weird.
parse_col <- function(x, splitter="\n"){
  return((strsplit(x, split=splitter)))
}

#split each table into their respective tables....again due to weird html code
#then parse each "table" and append dataframes
fill_df <- function(col_names=c("WINTER","CLOSED","OPENED","DAYS"),splitter="\n",tab){
  tab1 <- tab[,1:4]
  tab2 <- tab[,6:ncol(tab)]
  
  df1 <- load_df(col_names, tab1,splitter)
  df2 <- load_df(col_names, tab2,splitter)
  
  return(rbind(df1,df2))
}

#meant to parse each section of the data
load_df <- function(columns,tab,splitter="\n"){
  
  cols <- c()
  
  for(i in 1:ncol(tab)){
    curr <- parse_col(tab[,i],splitter)
    cols <- c(cols,curr)
  }
  
  df<-data.frame(cols)
  colnames(df) <- columns
  
  return(df)
}

tab4[,1] <- fix_data(tab4[,1],"\\d{0,4}-\\d{2}",FALSE)
tab4[,2] <- fix_data(tab4[,2],"\\d{0,2}\\s[a-zA-z]{3}",FALSE)
tab4[,3] <- fix_data(tab4[,3],"\\d{0,2}\\s[a-zA-z]{3}",FALSE)
tab4[,4] <- fix_data(tab4[,4],"\\d{2,3}",TRUE)
tab4[,6] <- fix_data(tab4[,6],"\\d{0,4}-\\d{2}",FALSE)
tab4[,7] <- fix_data(tab4[,7],"\\d{0,2}\\s[a-zA-z]{3}",FALSE)
tab4[,8] <- fix_data(tab4[,8],"\\d{0,2}\\s[a-zA-z]{3}",TRUE)
tab4[,9] <- fix_data(tab4[,9],"\\d{2,3}",TRUE)


#convert each html table into a dataframe
df1 = fill_df(tab=tab1)
df2 = fill_df(tab=tab2)
df3 = fill_df(tab=tab3)
df4 = fill_df(tab=tab4)

final_df <- rbind(df1,df2,df3,df4)

#after inspecting the final dataframe, we remove the first 4 rows
#there is a lot of missing data, so we can just ignore it.
final_df <- final_df[4:nrow(final_df),]

write.csv(final_df,file="data/raw_lake_mendota_freeze.csv", row.names = FALSE)
