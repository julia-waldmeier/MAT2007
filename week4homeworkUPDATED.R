cat("Julia Waldmeier\n")

# First type source("week4homework.R") in the terminal - this will run through both exercises in order.
# After that, you can also call any exercise individually in the R terminal, e.g. exercise1(), exercise2().

# Functions are defined outside exercise1()/exercise2() so they can be tested
# on their own too. Note: exercise1() must run first, since it creates the
# "solar_cycle_data.txt" file that exercise2() reads from.

# Exercise 1: Simulating the Solar Cycle
# Simulate 10 solar cycles, sampling the average number of sunspots, C, for each
# month from a Gaussian distribution with a mean of 100 and a standard deviation
# of 10, following f(t) = A*sin(2*pi/P * (t - t0)) + C with A = 20 and t0 = 0.
# Store the pseudodata in an ASCII file.
#
# The mean is not fixed at 100 for every cycle: each cycle first gets its own
# mean, sampled around 100, and the monthly values of C are then sampled around
# the mean of their own cycle. This way the mean fluctuates from cycle to cycle.

generate_solar_cycle_data <- function(num_cycles, months_per_cycle, amplitude,
                                       phase_shift, mean_sunspots,
                                       sd_between_cycles, sd_sunspots,
                                       random_seed, output_file_path) {
  # Guard against characters/other types where numbers are expected, and vice versa.
  if (!is.numeric(num_cycles) || anyNA(num_cycles) || num_cycles <= 0 ||
        num_cycles != round(num_cycles)) {
    stop("num_cycles must be a positive whole number")
  }
  if (!is.numeric(months_per_cycle) || anyNA(months_per_cycle) ||
        months_per_cycle <= 0 || months_per_cycle != round(months_per_cycle)) {
    stop("months_per_cycle must be a positive whole number")
  }
  if (!is.numeric(amplitude) || anyNA(amplitude)) {
    stop("amplitude must be a number")
  }
  if (!is.numeric(phase_shift) || anyNA(phase_shift)) {
    stop("phase_shift must be a number")
  }
  if (!is.numeric(mean_sunspots) || anyNA(mean_sunspots)) {
    stop("mean_sunspots must be a number")
  }
  if (!is.numeric(sd_between_cycles) || anyNA(sd_between_cycles) ||
        sd_between_cycles < 0) {
    stop("sd_between_cycles must be a non-negative number")
  }
  if (!is.numeric(sd_sunspots) || anyNA(sd_sunspots) || sd_sunspots < 0) {
    stop("sd_sunspots must be a non-negative number")
  }
  if (!is.numeric(random_seed) || anyNA(random_seed)) {
    stop("random_seed must be a number")
  }
  if (!is.character(output_file_path)) {
    stop("output_file_path must be a string")
  }

  # Fixed seed, so every run writes the same pseudodata.
  set.seed(random_seed)

  # Month index (0-based) across all cycles combined, used as t in the formula.
  total_months <- num_cycles * months_per_cycle
  time_in_months <- 0:(total_months - 1)

  # Every cycle gets its own mean, so the mean fluctuates between cycles.
  cycle_true_means <- rnorm(num_cycles, mean = mean_sunspots, sd = sd_between_cycles)

  # Each month gets its own randomly sampled C, around the mean of its cycle.
  # rep(..., each = ...) repeats every cycle mean once for each of its months.
  monthly_baseline <- rnorm(total_months,
                            mean = rep(cycle_true_means, each = months_per_cycle),
                            sd = sd_sunspots)

  # f(t) = A*sin(2*pi/P * (t - t0)) + C, applied to every month at once.
  sunspot_counts <- amplitude * sin(2 * pi / months_per_cycle * (time_in_months - phase_shift)) +
    monthly_baseline

  # Plain ASCII file, one value per line.
  write(sunspot_counts, file = output_file_path, ncolumns = 1)

  return(invisible(sunspot_counts))
}

exercise1 <- function() {
  cat("Exercise 1: Simulating the Solar Cycle\n")
  sunspot_counts <- generate_solar_cycle_data(
    num_cycles = 10, months_per_cycle = 11 * 12, amplitude = 20,
    phase_shift = 0, mean_sunspots = 100, sd_between_cycles = 30,
    sd_sunspots = 10, random_seed = 2007,
    output_file_path = "solar_cycle_data.txt"
  )
  cat("Generated", length(sunspot_counts),
      "months of pseudodata across 10 solar cycles and saved them to solar_cycle_data.txt\n")
}

# Exercise 2: Analysis of Solar Cycle Data
# Calculate the average number of sunspots per cycle along with its statistical
# uncertainty and present it in an array. Also, calculate the average number of
# sunspots over all cycles with its statistical uncertainty using the
# sub-sampling technique.

split_into_cycles <- function(input_file_path, num_cycles, months_per_cycle) {
  if (!is.character(input_file_path)) {
    stop("input_file_path must be a string")
  }
  if (!file.exists(input_file_path)) {
    stop("input_file_path does not exist - run exercise1() first to create it")
  }
  if (!is.numeric(num_cycles) || anyNA(num_cycles) || num_cycles <= 0 ||
        num_cycles != round(num_cycles)) {
    stop("num_cycles must be a positive whole number")
  }
  if (!is.numeric(months_per_cycle) || anyNA(months_per_cycle) ||
        months_per_cycle <= 0 || months_per_cycle != round(months_per_cycle)) {
    stop("months_per_cycle must be a positive whole number")
  }

  # Read the pseudodata back in from the file exercise1() wrote.
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

  cycles <- vector("list", num_cycles)

  # Slice out the block of consecutive months belonging to each cycle.
  for (cycle_index in 1:num_cycles) {
    start_position <- (cycle_index - 1) * months_per_cycle + 1
    end_position <- cycle_index * months_per_cycle
    cycles[[cycle_index]] <- sunspot_counts[start_position:end_position]
  }

  return(cycles)
}

calculate_cycle_mean_and_uncertainty <- function(cycle_data) {
  if (!is.numeric(cycle_data) || length(cycle_data) < 2) {
    stop("cycle_data must be a numeric vector with at least 2 values")
  }
  if (anyNA(cycle_data)) {
    stop("cycle_data must not contain NA values")
  }

  # Uncertainty on a mean = standard error of the mean.
  list(
    mean = mean(cycle_data),
    uncertainty = sd(cycle_data) / sqrt(length(cycle_data))
  )
}

calculate_all_cycles_mean_and_uncertainty <- function(cycles) {
  if (!is.list(cycles) || length(cycles) == 0) {
    stop("cycles must be a non-empty list of numeric vectors")
  }

  num_cycles <- length(cycles)
  cycle_means <- numeric(num_cycles)
  cycle_uncertainties <- numeric(num_cycles)

  # Mean + uncertainty computed separately for each cycle.
  for (cycle_index in 1:num_cycles) {
    cycle_result <- calculate_cycle_mean_and_uncertainty(cycles[[cycle_index]])
    cycle_means[cycle_index] <- cycle_result$mean
    cycle_uncertainties[cycle_index] <- cycle_result$uncertainty
  }

  # An array (matrix) of mean + uncertainty per cycle, as the exercise asks for.
  cbind(mean = cycle_means, uncertainty = cycle_uncertainties)
}

calculate_overall_mean_and_uncertainty <- function(cycle_means) {
  if (!is.numeric(cycle_means) || length(cycle_means) < 2) {
    stop("cycle_means must be a numeric vector with at least 2 values")
  }
  if (anyNA(cycle_means)) {
    stop("cycle_means must not contain NA values")
  }

  # Sub-sampling technique: average of the cycle means, sd() of those means as
  # the uncertainty (same method as the Week 4 tutorial exercises).
  list(
    mean = mean(cycle_means),
    uncertainty = sd(cycle_means)
  )
}

save_summary_csv <- function(cycle_results, overall_result, output_file_path) {
  if (!is.matrix(cycle_results) ||
        !all(c("mean", "uncertainty") %in% colnames(cycle_results))) {
    stop("cycle_results must be a matrix with 'mean' and 'uncertainty' columns")
  }
  if (!is.list(overall_result) || !is.numeric(overall_result$mean) ||
        !is.numeric(overall_result$uncertainty)) {
    stop("overall_result must be a list with a numeric mean and uncertainty")
  }
  if (!is.character(output_file_path)) {
    stop("output_file_path must be a string")
  }

  num_cycles <- nrow(cycle_results)

  # Same numbers already printed to the console, plus one extra row for the
  # overall sub-sampling result, saved as a reusable CSV file.
  summary_table <- data.frame(
    cycle = c(as.character(1:num_cycles), "overall (sub-sampling)"),
    mean = c(cycle_results[, "mean"], overall_result$mean),
    uncertainty = c(cycle_results[, "uncertainty"], overall_result$uncertainty)
  )

  write.csv(summary_table, file = output_file_path, row.names = FALSE)
}

exercise2 <- function() {
  cat("Exercise 2: Analysis of Solar Cycle Data\n")

  cycles <- split_into_cycles("solar_cycle_data.txt", 10, 11 * 12)
  cycle_results <- calculate_all_cycles_mean_and_uncertainty(cycles)

  cat("Average number of sunspots per cycle, with statistical uncertainty:\n")
  print(cycle_results)

  overall_result <- calculate_overall_mean_and_uncertainty(cycle_results[, "mean"])
  cat("Average number of sunspots over all cycles (sub-sampling technique):",
      overall_result$mean, "+/-", overall_result$uncertainty, "\n")

  summary_file_path <- "solar_cycle_summary.csv"
  save_summary_csv(cycle_results, overall_result, summary_file_path)
  cat("Summary saved to", summary_file_path, "\n")
}

# Function calls to run through both exercises when sourced.
# You can still call any exercise individually afterwards, e.g. exercise1().
exercise1()
exercise2()
