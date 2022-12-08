library(ggrepel)

data_value_plot <- function(data_frame, line_color, point_color, title, xLab) {
  ggplot(data_frame, aes(x = Period, y = Value, label=Value)) +
    geom_line(color=line_color, size = 1) +
    geom_point(shape = 21, color = point_color, fill = point_color, size = 4) +
    geom_text(aes(label = scales::comma(Value)), size = 3, vjust = -1.5) +
    scale_color_manual(values = line_color) +
    xlab("Year") +
    ylab(xLab) +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title=element_text(size = 30, hjust=0.5, vjust=0.5, face='bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size=20,face="bold"),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size=15,
        face=3
      ),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20),
    )
}
