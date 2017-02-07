library(ggplot2)

roll_dice <- function(rolls, n_sims) {
  output <- c()
  for (i in seq_along(1:n_sims)) {
    result <- as.integer(runif(rolls, 1, 7))
    output <- append(output, sum(result))
  }

  print(qplot(output, geom = 'bar'))
  print(table(output))
  return(output)
}

x <- roll_dice(5, 100000)
