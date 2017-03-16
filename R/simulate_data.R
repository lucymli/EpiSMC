get_SIR_data <- function (simulation, reporting=0.1, sampling="prop", filename=NULL) {
  # simulation is a data frame with $Time, $S, $I, $R, $Incidence columns
  # if sampling is prop(ortional), then every infected individual is equally likely to be sampled
  # if inv-prop, then an infected individual is less likely to be sampled when there are many other people also infected
  if (sampling=="prop") {
    data.set <- sapply(simulation$Incidence, function (x) rbinom(1, x, reporting))
  } else if (sampling=="inv-prop") {
    total.infected <- rbinom(1, sum(simulation$Incidence), reporting)
    indices <- unlist(mapply(rep, 1:nrow(simulation), simulation$Incidence, SIMPLIFY=FALSE))
    prob <- 1/simulation$Incidence
    prob[!is.finite(prob)] <- 0
    counts <- sample(indices, total.infected, 1/simulation$Incidence, prob)
    data.set <- sapply(1:nrow(simulation), function (x) sum(x==counts))
  }
  simulation$Data <- data.set
  if (!is.null(filename)) write.csv(simulation[, c("Time", "Data")], filename, row.names=FALSE, quote=FALSE)
  return (simulation)
}