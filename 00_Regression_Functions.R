#-----------------Missing Value Treatment------------------#
#For numeric and integer variables, the missing values are replaced by the 
#value derived from the sumFun function provided (Default it is median).
#For charecter variables, the missing values are simply replaced by the string 'Missing'

missing_value_trt <- function(df=NULL, summFun = median){
  df <- df
  for(i in 1:ncol(df)){
    if (typeof(df[,i]) %in% c("integer", "numeric")){
      df[is.na(df[,i]), i] <- summFun(core_train_data[,i], na.rm = TRUE)
    }
    else{
      df[is.na(df[,i]), i] <- 'Missing'
    }
  }
  return(df)
}


#-------------Outlier Treatment----------------------------------#
#The variables containing outliers, i.e. the values which are more than the sum of 75th percentile and 
#1.5 times IQR or less than the difference of 25th percentile and 1.5 times IQR of the corresponding variable
#are replced by the value of 99th and 1st percentiles of that variable respectively.

outlierTrtmt <- function(df = NULL, qtr_probs = c(0.01, 0.99)){
  df <- df
  par(mfrow = c(1, 2))
  for (i in 1:length(df)){
    if(is.integer(df[,i]) || is.numeric(df[,i])){
      x <- df[,i]
      qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
      caps <- quantile(x, probs=c(.01, .99), na.rm = T)
      H <- 1.5 * IQR(x, na.rm = T)
      x[x < (qnt[1] - H)] <- caps[1]
      x[x > (qnt[2] + H)] <- caps[2]
      df[, i] <- x
      boxplot(x)
    }
  }
  return(df)
}


#-------------------------Bivariate Plotting----------------------------------------#
#For integer and numeric variables, each variable is divided into bins (default 11) and plotted against the 
#the corresponding values derived from the sumFun (default median) of the dependent variable for each bin.
#For charecter variables or factors, each level of  the variable is plotted agaisnt the value derived from
#the sumFun(default median) of the dependent variable for each level.
#All the plots are saved as png in the png_dir directory.

bivariate_plots <- function(df = NULL, depvar = "None", intBinCount = 11, summFun = "median"
                            , png_dir = ""){
  cols <- colnames(df)
  cols <-  cols[!cols %in% c(depvar)]
  for(c in cols){
    if(typeof(df[, which(colnames(df) %in% c(c))]) == "integer" || typeof(df[, which(colnames(df) %in% c(c))]) == "numeric"){
      temp <- df
      temp$quantiles <- with(df[, which(colnames(df) %in% c(depvar, c))]
                             , cut(df[, which(colnames(df) %in% c(c))]
                                   , breaks = unique(quantile(df[, which(colnames(df) %in% c(c))]
                                                              , probs = seq(0, 1, length = intBinCount), na.rm = T)))
      )
      
      agg_mat <- aggregate(x=temp[, which(colnames(df) %in% c(depvar))]
                           , by = list(Variable = temp$quantiles)
                           , FUN = summFun)
      
    }else{
      agg_mat <- aggregate(x=df[, which(colnames(df) %in% c(depvar))]
                           , by = list(Variable = df[, which(colnames(df) %in% c(c))])
                           , FUN = summFun)
      agg_mat$Variable <- as.factor(agg_mat$Variable)
    }
    png(paste(png_dir, c, ".png"))
    plot(agg_mat, xlab = c, ylab = paste(summFun, depvar), type = 'p')
    dev.off()
  }
}



#----------------Creating Dummies of Character Variables---------------#
#For charecter variables with n levels, n-1 dummy variables are created and added to the dataframe named as 
#variable name added with corresponding level name separeted by a underscore.

createDummy <- function(df = NULL, dfname = ""){
  df <- df
  for(c in colnames(df)){
    if(is.character(df[, which(colnames(df) %in% c(c))])){
      dummyVarMat <- data.frame(dummy(df[, which(colnames(df) %in% c(c))], sep = "_"))
      cols <- colnames(dummyVarMat)
      cols <- gsub(" ", "", gsub(dfname, c, cols))
      colnames(dummyVarMat) <- cols
      df <- cbind(df, dummyVarMat)
    }
  }
  return (df)
}
