##### Functions to treat POD orari 


ConvertDate <- function(df)
{
  df <- as.data.frame(df)
  dfc <- as.Date(as.POSIXct(unlist(df['Giorno']), origin = "1970-01-01"))
  df$Giorno <- dfc
  return(df)
}
#######################################################
HourAggregator <- function(df)
{
  M <- matrix(0, nrow = nrow(df), ncol = 24)
  df2 <- data_frame()
  s <- seq(1, 96, by = 4)
  dfloc <- df[,4:99]
  for(i in 1:nrow(df))
  {
    df2 <- bind_rows(df2, df[i,1:2])
    for(j in 1:(length(s)-1))
    {
      M[i, j] <- sum(dfloc[i, s[j]:(s[j+1]-1)])
    }
    M[i, 24] <- sum(dfloc[i, 93:96])
  }
  df2 <- bind_cols(df2, data.frame(M))
  colnames(df2) <- c('Pod', 'Giorno', as.character(1:24))
  return(df2)
}