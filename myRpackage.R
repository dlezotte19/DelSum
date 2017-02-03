myFun <- function(x) {
  if (class(x) == "numeric") {
  output <- data.frame("stats"=c("n:","n w/o NA's:",
                                 "NA's:","Sum:", "Mean:",
                                 "Median:","Mode:","Min:",
                                 "1st Quartile:","3rd Quartile:",
                                 "Max:","IQR:","Standard Deviation:",
                                 "Variation:", "Standard Error:"))
  x2 <- x[!is.na(x)]
  output[1,2] <- length(x)
  output[2,2] <- length(x2)
  output[3,2] <- length(x[is.na(x)])
  output[4,2] <- sum(x2)
  output[5,2] <- round(mean(x2),4)
  output[6,2] <- median(x2)
  output[7,2] <- as.integer((names(sort(-table(x2)))[1]))
  output[8,2] <- min(x,na.rm=T)
  output[9,2] <- summary(x2)[2]
  output[10,2] <- summary(x2)[5]
  output[11,2] <- max(x2)
  output[12,2] <- summary(x2)[5]-summary(x2)[2]
  output[13,2] <- sd(x2)
  output[14,2] <- var(x2)
  
  #Confidence Intervals
  s <- sd(x,na.rm=T)
  n <- length(x[!is.na(x)])
  m <- mean(x,na.rm=T)
  CI90L <- m-1.645*(s/sqrt(n))
  CI90U <- m+1.645*(s/sqrt(n))
  CI95L <- m-1.96*(s/sqrt(n))
  CI95U <- m+1.96*(s/sqrt(n))
  CI99L <- m-2.576*(s/sqrt(n))
  CI99U <- m+2.576*(s/sqrt(n))
  Confidence.Intervals <- data.frame("Lower" = c(CI90L,CI95L,CI99L),
                                     "Upper" = c(CI90U,CI95U,CI99U), 
                                     row.names = c("90%","95%","99%"))
  output[15,2] <- round((s/sqrt(n)),4)
  #Outliers
  mino <- output[9,2]-1.5*output[12,2]
  maxo <-output[10,2]+1.5*output[12,2]
  outliers <- data.frame("Outliers" = sort(c(x2[x2 < mino], x2[x2 > maxo])))
  no <- x2[x > mino & x < maxo]
  output2 <- data.frame("stats"=c("n:","n w/o NA's:",
                                 "NA's:","Sum:", "Mean:",
                                 "Median:","Mode:","Min:",
                                 "1st Quartile:","3rd Quartile:",
                                 "Max:","IQR:","Standard Deviation:",
                                 "Variation:", "Standard Error:"))
  output2[1,2] <- length(no)+output[3,2]
  output2[2,2] <- length(no)
  output2[3,2] <- length(x[is.na(x)])
  output2[4,2] <- sum(no)
  output2[5,2] <- round(mean(no),4)
  output2[6,2] <- median(no)
  output2[7,2] <- as.integer((names(sort(-table(no)))[1]))
  output2[8,2] <- min(no,na.rm=T)
  output2[9,2] <- summary(no)[2]
  output2[10,2] <- summary(no)[5]
  output2[11,2] <- max(no)
  output2[12,2] <- summary(no)[5]-summary(no)[2]
  output2[13,2] <- sd(no)
  output2[14,2] <- var(no)
  
  s2 <- sd(no,na.rm=T)
  n2 <- length(no[!is.na(no)])
  m2 <- mean(no,na.rm=T)
  CI90L2 <- m2-1.645*(s2/sqrt(n2))
  CI90U2 <- m2+1.645*(s2/sqrt(n2))
  CI95L2 <- m2-1.96*(s2/sqrt(n2))
  CI95U2 <- m2+1.96*(s2/sqrt(n2))
  CI99L2 <- m2-2.576*(s2/sqrt(n2))
  CI99U2 <- m2+2.576*(s2/sqrt(n2))
  Confidence.Intervals2 <- data.frame("Lower" = c(CI90L2,CI95L2,CI99L2),
                                     "Upper" = c(CI90U2,CI95U2,CI99U2), 
                                     row.names = c("90%","95%","99%"))
  output2[15,2] <- round((s2/sqrt(n2)),4)
  final <- list("Summary" = output,
                "Confidence_Intervals" = Confidence.Intervals, 
                "Outliers" = outliers, "Summary_Outliers_Removed" = output2,
                "Confidence_Outliers_Removed" = Confidence.Intervals2)
  final
  }
  else {
    print("x input must be numeric")
  }
}

 data <- rnorm(1000,50,18)

