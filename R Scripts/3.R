outlierKD3 <- function(df5, var) {
  dualpane <- eval(substitute(var),eval(df5))
  na1 <- sum(is.na(dualpane))
  m1 <- mean(dualpane, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(dualpane, main="With outliers")
  hist(dualpane, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(dualpane)$out
  mo <- mean(outlier)
  dualpane <- ifelse(dualpane %in% outlier, NA, dualpane)
  boxplot(dualpane, main="Without outliers")
  hist(dualpane, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(dualpane))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(dualpane))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(dualpane, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df5[as.character(substitute(var))] <- invisible(dualpane)
    assign(as.character(as.list(match.call())$df5), df5, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df5))
  } else{
    cat("Nothing changed", "n")
    return(invisible(dualpane))
  }
}



