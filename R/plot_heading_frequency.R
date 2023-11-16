#' Plot Heading Frequency
#'
#' @param heading_freq a numeric vector representing the frequency of headings.
#' @param to_file a logical indicating whether the plot should be saved to a file. Defaults to TRUE.
#' @param n an integer indicating the number of headings to be plotted. Defaults to 25.
#' @param path A string indicating the path and file name for saving the plot. Default is "output/heading_frequency.pdf"
#'
#' @return Plots the heading frequency, if to_file is TRUE, saves the plot as a PDF.
#'
#' @export
plot_heading_frequency = function (heading_freq,
                                   to_file = TRUE,
                                   n = 25,
                                   path = "output/heading_frequency.pdf") {
  
  if (to_file) {
    pdf(file = path, width = 4, height = 6)
  }
  
  par(mar = c(3, 8, 2, 1),
      mgp = c(1.5, 0.25, 0))
  
  plot(x = 1,
       xlim = c(0, max(heading_freq)),
       ylim = c(1, (n*1.2)-0.8),
       type = "n",
       xlab = "Proportion of headings",
       ylab = "",
       axes = FALSE)
  
  n_headings = sum(heading_freq)
  max_pct = heading_freq[1] / n_headings
  pct_seq = seq(from = 0, to = max_pct, by = .025)
  
  abline(v = pct_seq*n_headings, lwd = 1/4, col = "gray")
  
  barplot(height = heading_freq[n:1],
          horiz = TRUE,
          las = 2,
          axes = FALSE,
          add = TRUE,
          cex.names = 3/4)
  
  axis(side = 1, at = pct_seq*n_headings, labels = pct_seq, tck = -0.02, cex.axis = 2/3)
  axis(side = 3, tck = -0.02, cex.axis = 2/3)
  box()
  
  if (to_file) {
    dev.off()
  }
  
}