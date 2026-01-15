

cores <- rcartocolor::carto_pal(12, "Bold")[c(1, 2, 3, 7, 5, 12, 6, 11)]

# scales::show_col(cores)

change_date <- function(x){
  
  if(is.na(x)) return(NA_Date_)
  
  if(str_count(x, ".") == 6){
    return(as.Date(paste0(x,"01"), "%Y%m%d"))
  }else{
    if(str_count(x, ".") == 8){
      return(as.Date(x, "%Y%m%d"))
    }
  }
}


read_any <- function(db, table){
  
  
  tab <- odbc::dbGetQuery(
    db, paste0("SELECT * FROM public.", table,";")
  )
  
  return(tab)
}


drop_table <- function(db, table){
  odbc::dbSendQuery(db, paste("drop table", table))
}

conecta_base <- function(){
  # create a file named senha.txt with your Postgres user in the first line and password in the second line
  senha <-read.table("senha.txt")
  
  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "DATASUS_PA",
                       user = senha$V1[1],
                       password = senha$V1[2],
                       host = "localhost",
                       port = 5433
                       )
  return(db)
  
}


get_types_sql <- function(){
  
}






tema <- theme_bw()+
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
  )

virgula <- scales::comma_format(big.mark = ".",
                                decimal.mark = ",")


paleta <- rcartocolor::carto_pal(12, "Bold")[c(1, 2, 3, 7, 12, 8, 9)]

