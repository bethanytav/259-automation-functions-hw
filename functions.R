plus_minus_SD <- function(input, num_of_SD = 1) {
  mean_input <- mean(input, na.rm=TRUE)
  sd_input <- sd(input, na.rm=TRUE)
  lower <- mean_input - (num_of_SD*sd_input)
  upper <- mean_input + (num_of_SD*sd_input)
  return(c("value of input sd around the mean:", lower, upper))
}

limit_replace <- function(input) {
  stopifnot(is.numeric(input))
  stopifnot(length(input) > 0)  
  input <- if_else(input < 2 | input > 8, NA, input)
  return(input)
}