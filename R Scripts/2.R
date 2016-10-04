outlierKD2 <- function(df5, var) {
  dataplus <- eval(substitute(var),eval(df5))
  na1 <- sum(is.na(dataplus))
  m1 <- mean(dataplus, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(dataplus, main="With outliers")
  hist(dataplus, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(dataplus)$out
  mo <- mean(outlier)
  dataplus <- ifelse(dataplus %in% outlier, NA, dataplus)
  boxplot(dataplus, main="Without outliers")
  hist(dataplus, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(dataplus))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(dataplus))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(dataplus, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df5[as.character(substitute(var))] <- invisible(dataplus)
    assign(as.character(as.list(match.call())$df5), df5, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df5))
  } else{
    cat("Nothing changed", "n")
    return(invisible(dataplus))
  }
}



