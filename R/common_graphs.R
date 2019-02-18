#' Select a base graphics font family
#'
#' The Benton Sans Regular font is preferred with a fallback of Arial Narrow.
#'   If neither font is available, use a default `sans` family font.
#'
#' @importFrom extrafont choose_font
#' @return String of the preferred base font.
#' @export
#' @examples
#' get_base_fontfamily()
get_base_fontfamily <- function() {
  dat <- extrafont::choose_font(c("BentonSansRE", "Arial Narrow"))
  if (dat == "") {
    "sans"
  } else {
    dat
  }
}

#' Default ggplot theme used by all Evaluator-supplied graphics
#'
#' Returns a standardized ggplot theme used by all built-in Evaluator plots.
#'
#' @importFrom ggplot2 theme_minimal theme %+replace% element_text element_blank
#' @param base_family Font family.
#' @return A ggplot theme object.
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars) + geom_point(aes(wt, mpg, color = factor(gear))) + facet_wrap(~am)
#' font_family <- get_base_fontfamily()
#' p + theme_evaluator(font_family)
theme_evaluator <- function(base_family = "BentonSansRE") {
  theme_minimal(base_family = base_family) %+replace%
    theme(panel.border = element_blank(), legend.position = "bottom",
          plot.caption = element_text(size = 9, hjust = 1))
}

#' Display a heatmap of impact by domain
#'
#' Given a domain_summary and a list of all domains, generate a heatmap colored
#'   by the 95% VaR. This plot displays the domains in which aggregreate risk is
#'   greater than others.
#'
#' @importFrom dplyr arrange mutate
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom rlang .data
#' @param domain_summary Simulations summarized at a domain level via \code{summarize_domains}.
#' @return A ggplot object.
#' @export
#' @examples
#' data(domain_summary)
#' generate_heatmap(domain_summary)
generate_heatmap <- function(domain_summary) {
  dat <- domain_summary %>% dplyr::arrange(.data$domain_id) %>%
    unnest(.data$summary) %>%
    dplyr::mutate(full_label = paste0(.data$domain_id, "\n",
                                      "$", round(.data$ale_var/10^6), "M"),
                   aux = seq(1, 2 * nrow(.), by = 2))
  gg <- ggplot(dat, aes_(x = quote(aux), y = 1))
  gg <- gg + geom_tile(stat = "identity", color = "white",
                       aes_(fill = quote(ale_var)), width = 1)
  gg <- gg + geom_text(aes_(x = quote(aux), y = 1, label = quote(full_label)),
                       color = "white", size = 3)
  gg <- gg + coord_equal()
  gg <- gg + viridis::scale_fill_viridis(guide = FALSE)
  gg <- gg + labs(x = NULL, y = NULL, title = "Value at Risk by Domain",
                  caption = "Source: Evaluator toolkit")
  gg <- gg + theme_evaluator(base_family = get_base_fontfamily())
  gg <- gg + theme(axis.text = element_blank())
  gg <- gg + theme(panel.grid = element_blank())
  gg
}

#' Display a scatterplot for a particular scenario ID
#'
#' Given a detailed results dataframe and a specific scenario identifier,
#'   create a scatterplot of the number of loss events versus the total amount of
#'   expected annual loss expected for each simulation. This provides a
#'   detailed view on the results for a particular scenario.
#'
#' @import ggplot2
#' @importFrom scales comma
#' @param simulation_results Simulation results from \code{run_simulations}.
#' @param scenario_id ID of the scenario to display.
#' @return A ggplot object.
#' @export
#' @examples
#' data(simulation_results)
#' generate_scatterplot(simulation_results, scenario_id = "50")
generate_scatterplot <- function(simulation_results, scenario_id){
  all_results <- unnest(simulation_results, .data$results)
  dat <- all_results[all_results$scenario_id == scenario_id, ]

  gg <- ggplot(dat, aes(x = .data$loss_events, y = .data$ale))
  gg <- gg + geom_point(alpha = 1/4)
  gg <- gg + scale_y_continuous(labels = dollar_millions,
                                limits = c(NA, max(all_results$ale,
                                                   na.rm = TRUE)))
  gg <- gg + scale_x_continuous(labels = scales::comma,
                                limits = c(NA, max(all_results$loss_events,
                                                   na.rm = TRUE)))
  gg <- gg + labs(x = "Loss Frequency (Annualized)", y = "Total Annual Loss Size")
  gg <- gg + theme_evaluator(base_family = get_base_fontfamily())
  gg
}

#' Display the distribution of threat events contained vs. realized across
#'   all domains
#'
#' Creates a barbell plot showing the number and percentage of events
#'   contained (not resulting in loss) vs the number and percentage of
#'   loss events (threat events resulting in losses).
#'
#' @importFrom dplyr arrange mutate desc
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom viridis viridis
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @param domain_summary Domain-level summary from \code{domain_summary}.
#' @param domain_id Variable to group plot by.
#' @return ggplot object.
#' @export
#' @examples
#' data(domain_summary)
#' generate_event_outcomes_plot(domain_summary)
generate_event_outcomes_plot <- function(domain_summary, domain_id = domain_id) {
  domain_id <- rlang::ensym(domain_id)
  all_domains <- unnest(domain_summary, summary)
  dat <- all_domains %>%
    dplyr::arrange(dplyr::desc(.data$mean_loss_events),
                   dplyr::desc(.data$mean_threat_events)) %>%
    dplyr::mutate(domain_id = factor(!!domain_id,
                                     levels = rev(unique(!!domain_id)),
                                     ordered = TRUE)) %>%
    dplyr::mutate(contained_events = .data$mean_threat_events - .data$mean_loss_events)

  # nudge labels 5% off from the end of the segment
  label_nudge <- c(max(dat$mean_loss_events) / 20 * -1, max(dat$contained_events) / 20)

  # set breakpoints to half of the range
  break_locations <-  c(max(dat$mean_loss_events) / 2 * -1.5, max(dat$contained_events) / 2 * 1.5)

  # convert data into tidy-er format
  dat <- tidyr::gather(dat, "type", "events", c("mean_loss_events", "contained_events")) %>%
    dplyr::mutate(actual_events = .data$events,
                   events = .data$events + 25000,
                   events = ifelse(.data$type == "mean_loss_events", -1 * .data$events, .data$events)) %>%
    dplyr::mutate(nudge = ifelse(.data$type == "mean_loss_events", label_nudge[1], label_nudge[2])) %>%
    dplyr::mutate(hjust = ifelse(.data$type == "mean_loss_events", "right", "left")) %>%
    dplyr::mutate(full_lab = ifelse(.data$type == "mean_loss_events",
                                         sprintf("%s (%s)", scales::comma(.data$actual_events), .data$mean_tc_exceedance),
                                         sprintf("%s (%s)", scales::comma(.data$actual_events), .data$mean_diff_exceedance)))

  # auto-calculate the limits of the plot
  event_range <- range(dat$events) * c(1.4, 1.5)

  # graph
  gg <- ggplot(dat, aes(x = .data$events, xend = 0, y = !!domain_id)) -> gg
  gg <- gg + geom_segment(aes(yend = !!domain_id), color = viridis::viridis(1))
  gg <- gg + geom_point(color = viridis::viridis(1), size = 2)
  gg <- gg + geom_label(aes(label = .data$full_lab, x = .data$events + .data$nudge,
                             y = !!domain_id, hjust = .data$hjust),
                        size = 3, label.size = NA)
  gg <- gg + geom_label(aes(x = 0, y = !!domain_id, label = !!domain_id),
                        size = 3, label.size = NA)
  gg <- gg + scale_x_continuous(breaks = c(break_locations[1], 0, break_locations[2]),
                                labels = c("Loss Events", "Domain", "Contained Events"),
                                position = "top",
                                limits = event_range)
  gg <- gg + labs(x = NULL, y = NULL,
                  title = "Simulation Outcomes by Domain",
                  subtitle = "Losses/Contained with Control Gap/Surplus",
                  caption = "Source: Evaluator toolkit")
  gg <- gg + theme_evaluator(base_family = get_base_fontfamily())
  gg <- gg + theme(panel.grid = element_blank())
  gg <- gg + theme(axis.text.x = element_text(hjust = .5))
  gg <- gg + theme(axis.text.y = element_blank())
  gg <- gg + theme(axis.ticks.x = element_blank())
  gg <- gg + theme(axis.ticks.y = element_blank())
  gg
}
