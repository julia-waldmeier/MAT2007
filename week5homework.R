cat("Julia Waldmeier\n")

# First type source("week5homework.R") in the terminal - this will run through both exercises in order.
# After that, you can also call any exercise individually in the R terminal, e.g. exercise1(), exercise2().

# Both exercises read "solar_cycle_data.txt", the pseudodata written by the
# Week 4 homework (10 solar cycles of 11 years, one sunspot value per month,
# f(t) = A*sin(2*pi/P * (t - t0)) + C, where each cycle has its own mean).
# The file must be in the working directory.

data_file_path   <- "solar_cycle_data.txt"
num_cycles       <- 10
months_per_cycle <- 11 * 12
phase_shift      <- 0        # t0 used in the Week 4 simulation, in months

# Shared functions

load_sunspot_data <- function(input_file_path, num_cycles, months_per_cycle) {
  if (!is.character(input_file_path) || length(input_file_path) != 1) {
    stop("input_file_path must be a single string")
  }
  if (!file.exists(input_file_path)) {
    stop("cannot find ", input_file_path, " in ", getwd(),
         " - run the Week 4 homework first, or set the working directory")
  }
  if (!is.numeric(num_cycles) || anyNA(num_cycles) || num_cycles <= 0 ||
        num_cycles != round(num_cycles)) {
    stop("num_cycles must be a positive whole number")
  }
  if (!is.numeric(months_per_cycle) || anyNA(months_per_cycle) ||
        months_per_cycle <= 0 || months_per_cycle != round(months_per_cycle)) {
    stop("months_per_cycle must be a positive whole number")
  }

  sunspot_counts <- scan(input_file_path, quiet = TRUE)

  if (length(sunspot_counts) == 0) {
    stop("input_file_path contains no data")
  }
  if (anyNA(sunspot_counts)) {
    stop("input_file_path contains NA values")
  }
  if (length(sunspot_counts) != num_cycles * months_per_cycle) {
    stop("the number of data points does not match num_cycles * months_per_cycle")
  }

  return(sunspot_counts)
}

summarise_cycles <- function(sunspot_counts, num_cycles, months_per_cycle,
                             phase_shift) {
  if (!is.numeric(sunspot_counts) ||
        length(sunspot_counts) != num_cycles * months_per_cycle) {
    stop("sunspot_counts must be numeric with num_cycles * months_per_cycle values")
  }
  if (months_per_cycle < 2) {
    stop("need at least 2 months per cycle to calculate an uncertainty")
  }
  if (!is.numeric(phase_shift) || anyNA(phase_shift)) {
    stop("phase_shift must be a number")
  }

  cycle_means <- numeric(num_cycles)
  statistical_uncertainties <- numeric(num_cycles)

  for (cycle_index in 1:num_cycles) {
    start_position <- (cycle_index - 1) * months_per_cycle + 1
    end_position <- cycle_index * months_per_cycle
    cycle_data <- sunspot_counts[start_position:end_position]

    # The mean is calculated from each cycle's own data. The statistical
    # uncertainty is the standard deviation of the monthly values around that
    # cycle's own mean.
    cycle_means[cycle_index] <- mean(cycle_data)
    statistical_uncertainties[cycle_index] <- sd(cycle_data)
  }

  # The cycle top is where the sine is at its maximum, a quarter of a period
  # after the start of each cycle (shifted by t0). Converted from months to years.
  cycle_start_months <- (0:(num_cycles - 1)) * months_per_cycle
  cycle_top_years <- (cycle_start_months + months_per_cycle / 4 + phase_shift) / 12

  data.frame(
    cycle = 1:num_cycles,
    cycle_top_year = cycle_top_years,
    mean = cycle_means,
    statistical_uncertainty = statistical_uncertainties
  )
}

# Opens a PNG file, runs the drawing function, and always closes the file
# again, even if the drawing fails halfway.
save_plot <- function(output_file_path, width_inches, height_inches, draw_plot) {
  if (!is.character(output_file_path) || length(output_file_path) != 1) {
    stop("output_file_path must be a single string")
  }
  if (!is.function(draw_plot)) {
    stop("draw_plot must be a function")
  }

  png(output_file_path, width = width_inches, height = height_inches,
      units = "in", res = 150)
  on.exit(dev.off())
  par(bg = plot_colours$background, col.lab = plot_colours$secondary_text,
      las = 1, mar = c(4.5, 4.5, 4.5, 1.5))
  draw_plot()
  cat("Plot saved to", normalizePath(output_file_path, mustWork = FALSE), "\n")
}

# One place for all the colours, so both plots look the same.
plot_colours <- list(
  background     = "#fcfcfb",
  title_text     = "#0b0b0b",
  secondary_text = "#52514e",
  axis_text      = "#898781",
  gridline       = "#e1e0d9",
  axis_line      = "#c3c2b7",
  monthly_data   = "#2a78d6",                               # blue
  cycle_average  = "#eb6834",                               # orange
  total_line     = "#2a78d6",                               # blue
  total_box      = adjustcolor("#2a78d6", alpha.f = 0.22)   # same blue, see-through
)

# Draws the empty plot area in the shared style: light horizontal gridlines,
# no box around the plot, and a left-aligned title with a subtitle underneath.
start_styled_plot <- function(x_limits, y_limits, x_label, y_label, title, subtitle) {
  plot(NA, xlim = x_limits, ylim = y_limits, axes = FALSE,
       xlab = x_label, ylab = y_label)
  abline(h = axTicks(2), col = plot_colours$gridline)
  axis(1, col = plot_colours$axis_line, col.axis = plot_colours$axis_text)
  axis(2, col = NA, col.axis = plot_colours$axis_text)   # tick labels only, no line
  title(main = title, adj = 0, line = 2.4, col.main = plot_colours$title_text)
  mtext(subtitle, side = 3, adj = 0, line = 0.9,
        col = plot_colours$secondary_text, cex = 0.9 * par("cex"))
}

plot_solar_cycle <- function(sunspot_counts) {
  # Month index is 0-based, as in the Week 4 simulation, converted to years.
  time_in_years <- (seq_along(sunspot_counts) - 1) / 12

  start_styled_plot(range(time_in_years), range(sunspot_counts),
                    "Time (years)", "Number of sunspots",
                    "Solar cycle",
                    "Monthly number of sunspots over 10 cycles of 11 years")
  lines(time_in_years, sunspot_counts, col = plot_colours$monthly_data)
}

# Draws the average per cycle with statistical error bars. If
# systematic_fraction is given (e.g. 0.10 for 10%), the total uncertainty
# (statistical and systematic combined) is drawn as a blue box behind each
# point, so both the statistical part and the total can be seen.
plot_cycle_averages <- function(cycle_summary, systematic_fraction = NULL) {
  required_columns <- c("cycle_top_year", "mean", "statistical_uncertainty")
  if (!is.data.frame(cycle_summary) ||
        !all(required_columns %in% names(cycle_summary))) {
    stop("cycle_summary must be a data frame made by summarise_cycles()")
  }
  if (!is.null(systematic_fraction) &&
        (!is.numeric(systematic_fraction) || length(systematic_fraction) != 1 ||
           is.na(systematic_fraction) || systematic_fraction < 0)) {
    stop("systematic_fraction must be a single non-negative number or NULL")
  }

  top_years <- cycle_summary$cycle_top_year
  means <- cycle_summary$mean
  statistical <- cycle_summary$statistical_uncertainty
  has_systematic <- !is.null(systematic_fraction)

  if (has_systematic) {
    systematic <- systematic_fraction * means
    # Statistical and systematic uncertainties are independent, so they are
    # added in quadrature: total = sqrt(stat^2 + syst^2).
    total <- sqrt(statistical^2 + systematic^2)
  } else {
    total <- statistical
  }

  # Leave room on the y-axis for the largest uncertainty drawn.
  largest_uncertainty <- pmax(statistical, total)
  y_limits <- range(means - largest_uncertainty, means + largest_uncertainty)
  y_limits <- y_limits + c(-0.05, 0.25) * diff(y_limits)   # extra room for the legend

  start_styled_plot(
    range(top_years), y_limits,
    "Year of the cycle top (years since start)", "Average number of sunspots",
    "Average number of sunspots per cycle",
    if (has_systematic) {
      paste0("Error bars: statistical uncertainty. Boxes: total uncertainty, sqrt(stat^2 + syst^2),",
             " with syst. = ", 100 * systematic_fraction, "% of the average.")
    } else {
      "Error bars: statistical uncertainty (standard deviation of the monthly values)"
    }
  )

  if (has_systematic) {
    box_half_width <- 1.8   # years, only sets how wide the boxes look
    rect(top_years - box_half_width, means - total,
         top_years + box_half_width, means + total,
         col = plot_colours$total_box, border = NA)
    # solid lines at the top and bottom of each box mark the total's limits
    segments(top_years - box_half_width, means - total,
             top_years + box_half_width, means - total,
             col = plot_colours$total_line, lwd = 2)
    segments(top_years - box_half_width, means + total,
             top_years + box_half_width, means + total,
             col = plot_colours$total_line, lwd = 2)
  }

  # Statistical uncertainty as error bars (arrows with flat heads).
  arrows(top_years, means - statistical, top_years, means + statistical,
         angle = 90, code = 3, length = 0.04, lwd = 2,
         col = plot_colours$cycle_average)
  # Filled dots with a thin background-coloured ring so they stand out
  # from the error bars.
  points(top_years, means, pch = 21, cex = 1.6, lwd = 2,
         bg = plot_colours$cycle_average, col = plot_colours$background)

  # "\u00b1" is the plus-minus sign.
  average_label <- "Cycle average \u00b1 statistical uncertainty"
  if (has_systematic) {
    legend("topright", box.col = NA, bg = plot_colours$background,
           text.col = plot_colours$secondary_text,
           legend = c(average_label,
                      paste0("Total uncertainty (stat. + ", 100 * systematic_fraction,
                             "% syst.)")),
           pch = c(19, 15), pt.cex = c(1.3, 2.2),
           col = c(plot_colours$cycle_average, plot_colours$total_box))
  } else {
    legend("topright", box.col = NA, bg = plot_colours$background,
           text.col = plot_colours$secondary_text,
           legend = average_label, pch = 19, pt.cex = 1.3,
           col = plot_colours$cycle_average)
  }
}

# Exercise 1: Plotting the Solar Cycle
# Plot the solar cycle as a function of time using the pseudodata generated in
# the previous homework. Plot the average number of sunspots and its
# statistical uncertainty as a function of the year of the cycle top.

exercise1 <- function() {
  cat("Exercise 1: Plotting the Solar Cycle\n")

  sunspot_counts <- load_sunspot_data(data_file_path, num_cycles, months_per_cycle)
  cycle_summary <- summarise_cycles(sunspot_counts, num_cycles, months_per_cycle,
                                    phase_shift)

  cat("Average number of sunspots per cycle, with statistical uncertainty:\n")
  print(cycle_summary)

  save_plot("week5_homework1_solar_cycle.png", 10, 10, function() {
    par(mfrow = c(2, 1))
    plot_solar_cycle(sunspot_counts)
    plot_cycle_averages(cycle_summary)
  })
}

# Exercise 2: Plotting with Systematic Uncertainty
# Plot the same data as in Homework 1, but this time assign a systematic
# uncertainty which is 10% of the average number of sunspots of each cycle.

exercise2 <- function() {
  cat("Exercise 2: Plotting with Systematic Uncertainty\n")

  systematic_fraction <- 0.10
  sunspot_counts <- load_sunspot_data(data_file_path, num_cycles, months_per_cycle)
  cycle_summary <- summarise_cycles(sunspot_counts, num_cycles, months_per_cycle,
                                    phase_shift)
  cycle_summary$systematic_uncertainty <- systematic_fraction * cycle_summary$mean
  cycle_summary$total_uncertainty <- sqrt(cycle_summary$statistical_uncertainty^2 +
                                            cycle_summary$systematic_uncertainty^2)

  cat("Average number of sunspots per cycle, with statistical and systematic uncertainty:\n")
  print(cycle_summary)

  save_plot("week5_homework2_systematic_uncertainty.png", 10, 6, function() {
    plot_cycle_averages(cycle_summary, systematic_fraction)
  })
}

# Function calls to run through both exercises when sourced.
# You can still call any exercise individually afterwards, e.g. exercise1().
exercise1()
exercise2()
