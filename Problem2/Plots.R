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


# draw_several_data <- function(data_frame, title) {
#   ggplot(data_frame, aes(x = Date, y = Inflation, fill = Country)) +
#     geom_area() +
#     scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
#     xlab("Date") +
#     ylab("Inflation") +
#     ggtitle(title) +
#     theme(
#       plot.title = element_text(size = 18)
#     )
# }
#
# draw_stacking_data <- function(data_frame, title) {
#   ggplot(data_frame, aes(x = Date, y = Inflation, group = Country, color = Country)) +
#     scale_color_viridis(discrete = TRUE) +
#     geom_line(aes(color = Country), size = 1.5) +
#     geom_point(shape = 21, color = "black", fill = "black", size = 1.5) +
#     scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
#     guides(fill = guide_legend(title = NULL)) +
#     xlab("Date") +
#     ylab("Inflation") +
#     ggtitle(title) +
#     theme(
#       plot.title = element_text(size = 18)
#     )
# }
#
# draw_box_plot <- function(data_frame, title) {
#   ggplot(data_frame, aes(x = Country, y = Inflation, fill = Country)) +
#     geom_boxplot() +
#     scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
#     geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
#     xlab("Country") +
#     ylab("Inflation") +
#     ggtitle(title) +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(size = 18)
#     )
# }