med_screen <- function(base_rate, sensitivity, specificity) {
  prior <- (base_rate * sensitivity)
  likelihood <- ((1 - base_rate) * (1 - specificity))
  posterior <- prior / (prior + likelihood)
  print(paste("The likelihood that the condition exists is", round(posterior*100, 2),"%"))
}

med_screen(base_rate = 0.004, sensitivity = 0.99, specificity = 0.85)
