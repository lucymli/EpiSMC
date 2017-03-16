visualize.SIR <- function (simulation, filename=NULL, fig.width=6, fig.height=5) {
  require(ggplot2)
  require(grid)
  require(gridExtra)
  prev.plot <- 
    ggplot(reshape2::melt(simulation, id.vars=c("Time", "Incidence", "Data"))) + theme_bw() +
    geom_line(aes(x=Time, y=value, colour=variable)) + 
    scale_colour_discrete("Variable") + 
    xlab("Time") + ylab("Number of individuals") +
    theme(legend.position="top")
  inc.plot <- 
    ggplot(reshape2::melt(simulation, id.vars=c("Time", "S", "I", "R"))) + theme_bw() +
    geom_bar(aes(x=Time, y=value, fill=variable), stat="identity", position="dodge") +
    scale_fill_discrete("", labels=c("Reported", "Total")) +
    xlab("Time") + ylab("Incidence") +
    theme(legend.position="top")
  output.plot <- arrangeGrob(prev.plot, inc.plot, ncol=1)
  if (!is.null(filename)) ggsave(filename, output.plot, width=fig.width, height=fig.height)
  return (output.plot)
}