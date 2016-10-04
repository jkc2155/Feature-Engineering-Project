outlierKD <- function(df3, var) {
  **VARIABLE** <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(**VARIABLE**))
  m1 <- mean(**VARIABLE**, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(**VARIABLE**, main="With outliers")
  hist(**VARIABLE**, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(**VARIABLE**)$out
  mo <- mean(outlier)
  **VARIABLE** <- ifelse(**VARIABLE** %in% outlier, NA, **VARIABLE**)
  boxplot(**VARIABLE**, main="Without outliers")
  hist(**VARIABLE**, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(**VARIABLE**))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(**VARIABLE**))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(**VARIABLE**, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(**VARIABLE**)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(**VARIABLE**))
  }
}