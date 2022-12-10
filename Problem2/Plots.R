library(ggrepel)
library(ggpubr)

data_value_plot <- function(data_frame, line_color, point_color, title, xLab) {
  ggplot(data_frame, aes(x = Period, y = Value, label=Value)) +
    geom_line(color=line_color, size = 1) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

cor_visualization <- function(first_df, second_df, xlab, y_lab) {
  data = data.frame(Period = first_df$Period, x = first_df$Value, y = second_df$Value)

  ggscatter(
    data,
    x = "x",
    y = "y",
    add = "reg.line",
    conf.int = TRUE,
    cor.coef = TRUE,
    cor.method = "pearson",
    xlab = xlab,
    ylab = y_lab) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

donat_plot <- function(data) {
  data$fraction = data$Count / sum(data$Count)

  data$ymax = cumsum(data$fraction)

  data$ymin = c(0, head(data$ymax, n=-1))
  data$labelPosition <- (data$ymax + data$ymin) / 2
  data$label <- paste0(data$Name, ": ", data$Count, '%')

  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Name)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
}
