data_value_plot <- function(data_frame, line_color, point_color, title, xLab) {
  ggplot(data_frame, aes(x = Period, y = Value)) +
    geom_line(color=line_color, size = 1) +
    geom_point(shape = 21, color = point_color, fill = point_color, size = 4) +
    scale_color_manual(values = line_color) +
    xlab("Period") +
    ylab(xLab) +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    )
}
