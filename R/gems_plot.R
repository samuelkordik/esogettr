#' Generates a "Gems" style plot
#'
#' Creates plot with data points on left side and a main plot on right.
#' Data points coming in are parsed as a single row with measure names
#' as the variable/column names, and measure values as the data.
#'
#' @param data Data coming in
#' @param main_plot Main plot to add gems to
#' @param title Title of plot
#'
#' @return plot format
#' @export
#'
#'
gems_plot <- function(data, main_plot, title = "") {
  message(class(main_plot))
  stopifnot("ggplot" %in% class(main_plot))

  data %>% head(1) %>% pivot_longer(everything(), names_to="Measure", values_to = "Value") -> measure_values

  measure_values %>%
    ggplot(aes(x=1, y=seq(from=nrow(measure_values), to=1))) +
    geom_text(aes(label=Measure), size=4) +
    geom_text(aes(label=Value), nudge_y = .3, size=12, fontface="bold") +
    theme_void() + ggtitle(title) -> gems
  gems
  #main_plot + gems + plot_layout(ncol = 2, widths = c(4,1))
  # ggplot(mpg, aes(displ, hwy)) +
  #   geom_point(aes(colour = class)) +
  #   geom_smooth(se = FALSE) +
  #   theme(legend.position = "bottom") +
  #   guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) -> mpg_plot
  #
  # mpg %>% group_by(class) %>%
  #   summarize(mean_hwy = mean(hwy)) %>%
  #   mutate(class=str_to_title(class)) %>%
  #   pivot_wider(everything(), names_from = "class", values_from = "mean_hwy") %>%
  #   gems_plot(mpg_plot, title="Mean Highway Mileage")
  #
  #
}

