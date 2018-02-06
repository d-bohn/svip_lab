vis_angle <- function(size, distance){
  rad = 2*atan(size/(2*distance))
  ang = rad*(180/pi)
  return(ang)
}

desired_size <- function(visAngle, distance){
  rad = visAngle/(180/pi)
  size = 2*distance*tan(rad/2)
  return(size)
}

rm_eta <- function(aov, n_terms){
  SSTotal <- sum(aov$ANOVA$SSn[-1])
  etas <- list()
  for ( i in 1:n_terms){
    j <- i+1
    etas[i] <- (aov$ANOVA$SSn[j])/SSTotal
  }
  return(etas)
  if (sum(unlist(etas)) != 1) message("Sum of etas does not equal 1. Please make sure you have the correct number of terms (minus intercept).")
}

vip_sum <- function(.,...){
  as.character(match.call(expand.dots = TRUE))[-c(1:2)]
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$variable))))
}

vip_plot <- function(summary_data){
  library(ggplot2)
}
  