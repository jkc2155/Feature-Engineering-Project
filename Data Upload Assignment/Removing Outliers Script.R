outlierKD1 <- function(df3, var) {
  TMA <- eval(substitute(var),eval(df3))
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
    df3[as.character(substitute(var))] <- invisible(TMA)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(TMA))
  }
}

outlierKD2 <- function(df3, var) {
  dataplus <- eval(substitute(var),eval(df3))
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
    df3[as.character(substitute(var))] <- invisible(dataplus)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(dataplus))
  }
}

outlierKD3 <- function(df3, var) {
  dualpane <- eval(substitute(var),eval(df3))
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
    df3[as.character(substitute(var))] <- invisible(dualpane)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(dualpane))
  }
}

outlierKD4 <- function(df3, var) {
  forumng <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(forumng))
  m1 <- mean(forumng, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(forumng, main="With outliers")
  hist(forumng, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(forumng)$out
  mo <- mean(outlier)
  forumng <- ifelse(forumng %in% outlier, NA, forumng)
  boxplot(forumng, main="Without outliers")
  hist(forumng, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(forumng))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(forumng))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(forumng, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(forumng)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(forumng))
  }
}
outlierKD5 <- function(df3, var) {
  glossary <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(glossary))
  m1 <- mean(glossary, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(glossary, main="With outliers")
  hist(glossary, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(glossary)$out
  mo <- mean(outlier)
  glossary <- ifelse(glossary %in% outlier, NA, glossary)
  boxplot(glossary, main="Without outliers")
  hist(glossary, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(glossary))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(glossary))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(glossary, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(glossary)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(glossary))
  }
}

outlierKD6 <- function(df3, var) {
  homepage <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(homepage))
  m1 <- mean(homepage, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(homepage, main="With outliers")
  hist(homepage, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(homepage)$out
  mo <- mean(outlier)
  homepage <- ifelse(homepage %in% outlier, NA, homepage)
  boxplot(homepage, main="Without outliers")
  hist(homepage, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(homepage))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(homepage))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(homepage, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(homepage)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(homepage))
  }
}

outlierKD7 <- function(df3, var) {
  htmlactivity <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(htmlactivity))
  m1 <- mean(htmlactivity, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(htmlactivity, main="With outliers")
  hist(htmlactivity, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(htmlactivity)$out
  mo <- mean(outlier)
  htmlactivity <- ifelse(htmlactivity %in% outlier, NA, htmlactivity)
  boxplot(htmlactivity, main="Without outliers")
  hist(htmlactivity, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(htmlactivity))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(htmlactivity))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(htmlactivity, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(htmlactivity)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(htmlactivity))
  }
}

outlierKD8 <- function(df3, var) {
  oucollaborate <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(oucollaborate))
  m1 <- mean(oucollaborate, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(oucollaborate, main="With outliers")
  hist(oucollaborate, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(oucollaborate)$out
  mo <- mean(outlier)
  oucollaborate <- ifelse(oucollaborate %in% outlier, NA, oucollaborate)
  boxplot(oucollaborate, main="Without outliers")
  hist(oucollaborate, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(oucollaborate))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(oucollaborate))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(oucollaborate, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(oucollaborate)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(oucollaborate))
  }
}
outlierKD9 <- function(df3, var) {
  oucontent <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(oucontent))
  m1 <- mean(oucontent, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(oucontent, main="With outliers")
  hist(oucontent, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(oucontent)$out
  mo <- mean(outlier)
  oucontent <- ifelse(oucontent %in% outlier, NA, oucontent)
  boxplot(oucontent, main="Without outliers")
  hist(oucontent, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(oucontent))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(oucontent))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(oucontent, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(oucontent)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(oucontent))
  }
}

outlierKD10 <- function(df3, var) {
  ouwiki <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(ouwiki))
  m1 <- mean(ouwiki, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(ouwiki, main="With outliers")
  hist(ouwiki, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(ouwiki)$out
  mo <- mean(outlier)
  ouwiki <- ifelse(ouwiki %in% outlier, NA, ouwiki)
  boxplot(ouwiki, main="Without outliers")
  hist(ouwiki, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(ouwiki))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(ouwiki))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(ouwiki, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(ouwiki)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(ouwiki))
  }
}
outlierKD11 <- function(df3, var) {
  page <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(page))
  m1 <- mean(page, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(page, main="With outliers")
  hist(page, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(page)$out
  mo <- mean(outlier)
  page <- ifelse(page %in% outlier, NA, page)
  boxplot(page, main="Without outliers")
  hist(page, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(page))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(page))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(page, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(page)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(page))
  }
}

outlierKD12 <- function(df3, var) {
  questionnaire <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(questionnaire))
  m1 <- mean(questionnaire, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(questionnaire, main="With outliers")
  hist(questionnaire, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(questionnaire)$out
  mo <- mean(outlier)
  questionnaire <- ifelse(questionnaire %in% outlier, NA, questionnaire)
  boxplot(questionnaire, main="Without outliers")
  hist(questionnaire, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(questionnaire))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(questionnaire))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(questionnaire, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(questionnaire)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(questionnaire))
  }
}

outlierKD13 <- function(df3, var) {
  quiz <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(quiz))
  m1 <- mean(quiz, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(quiz, main="With outliers")
  hist(quiz, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(quiz)$out
  mo <- mean(outlier)
  quiz <- ifelse(quiz %in% outlier, NA, quiz)
  boxplot(quiz, main="Without outliers")
  hist(quiz, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(quiz))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(quiz))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(quiz, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(quiz)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(quiz))
  }
}

outlierKD14 <- function(df3, var) {
  repeatactivity <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(repeatactivity))
  m1 <- mean(repeatactivity, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(repeatactivity, main="With outliers")
  hist(repeatactivity, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(repeatactivity)$out
  mo <- mean(outlier)
  repeatactivity <- ifelse(repeatactivity %in% outlier, NA, repeatactivity)
  boxplot(repeatactivity, main="Without outliers")
  hist(repeatactivity, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(repeatactivity))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(repeatactivity))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(repeatactivity, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(repeatactivity)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(repeatactivity))
  }
}
outlierKD15 <- function(df3, var) {
  resource <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(resource))
  m1 <- mean(resource, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(resource, main="With outliers")
  hist(resource, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(resource)$out
  mo <- mean(outlier)
  resource <- ifelse(resource %in% outlier, NA, resource)
  boxplot(resource, main="Without outliers")
  hist(resource, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(resource))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(resource))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(resource, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(resource)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(resource))
  }
}

outlierKD16 <- function(df3, var) {
  subpage <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(subpage))
  m1 <- mean(subpage, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(subpage, main="With outliers")
  hist(subpage, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(subpage)$out
  mo <- mean(outlier)
  subpage <- ifelse(subpage %in% outlier, NA, subpage)
  boxplot(subpage, main="Without outliers")
  hist(subpage, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(subpage))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(subpage))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(subpage, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(subpage)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(subpage))
  }
}
outlierKD17 <- function(df3, var) {
  url <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(url))
  m1 <- mean(url, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(url, main="With outliers")
  hist(url, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(url)$out
  mo <- mean(outlier)
  url <- ifelse(url %in% outlier, NA, url)
  boxplot(url, main="Without outliers")
  hist(url, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(url))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(url))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(url, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(url)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(url))
  }
}
outlierKD18 <- function(df3, var) {
  CMA <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(CMA))
  m1 <- mean(CMA, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(CMA, main="With outliers")
  hist(CMA, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(CMA)$out
  mo <- mean(outlier)
  CMA <- ifelse(CMA %in% outlier, NA, CMA)
  boxplot(CMA, main="Without outliers")
  hist(CMA, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(CMA))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(CMA))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(CMA, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(CMA)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(CMA))
  }
}
outlierKD19 <- function(df3, var) {
  TMA <- eval(substitute(var),eval(df3))
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
    df3[as.character(substitute(var))] <- invisible(TMA)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(TMA))
  }
}
outlierKD20 <- function(df3, var) {
  num_of_prev_attempts <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(num_of_prev_attempts))
  m1 <- mean(num_of_prev_attempts, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(num_of_prev_attempts, main="With outliers")
  hist(num_of_prev_attempts, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(num_of_prev_attempts)$out
  mo <- mean(outlier)
  num_of_prev_attempts <- ifelse(num_of_prev_attempts %in% outlier, NA, num_of_prev_attempts)
  boxplot(num_of_prev_attempts, main="Without outliers")
  hist(num_of_prev_attempts, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(num_of_prev_attempts))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(num_of_prev_attempts))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(num_of_prev_attempts, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(num_of_prev_attempts)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(num_of_prev_attempts))
  }
}
outlierKD21 <- function(df3, var) {
  studied_credits <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(studied_credits))
  m1 <- mean(studied_credits, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(studied_credits, main="With outliers")
  hist(studied_credits, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(studied_credits)$out
  mo <- mean(outlier)
  studied_credits <- ifelse(studied_credits %in% outlier, NA, studied_credits)
  boxplot(studied_credits, main="Without outliers")
  hist(studied_credits, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(studied_credits))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(studied_credits))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(studied_credits, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(studied_credits)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(studied_credits))
  }
}

outlierKD_final_result <- function(df3, var) {
  final_result <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(final_result))
  m1 <- mean(final_result, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(final_result, main="With outliers")
  hist(final_result, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(final_result)$out
  mo <- mean(outlier)
  final_result <- ifelse(final_result %in% outlier, NA, final_result)
  boxplot(final_result, main="Without outliers")
  hist(final_result, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(final_result))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(final_result))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(final_result, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(final_result)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(final_result))
  }
}
