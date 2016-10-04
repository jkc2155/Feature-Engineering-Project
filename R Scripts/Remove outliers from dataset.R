outlierKD1 <- function(df5, var) {
  TMA <- eval(substitute(var),eval(df5))
  na1 <- sum(is.na(TMA))
  m1 <- mean(TMA, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(TMA, main="With outliers")
  hist(TMA, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(TMA)$out
  mo <- mean(outlier)
  TMA <- ifelse(TMA %in% outlier, NA, TMA)
  boxplot(TMA, main="Without outliers")
  hist(TMA, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(TMA))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(TMA))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(TMA, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df5[as.character(substitute(var))] <- invisible(TMA)
    assign(as.character(as.list(match.call())$df5), df5, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df5))
  } else{
    cat("Nothing changed", "n")
    return(invisible(TMA))
  }
}



