# Easy Exercise: Temperature Conversion
x <- 25 # x = Temperature in Celsius

# Formula to convert Celsius to Fahrenheit: multiply by 9/5 then add 32
fahrenheit <- x * 9/5 + 32

print(paste("Temperature in Fahrenheit:", fahrenheit))

# Difficult Exercise: Simple Calculator
# To use the calculator, open the R Terminal and run: source("week1homework.R"), then calculate()
calculate <- function() {

  # readline() shows a prompt and waits for the user to type something
  # as.numeric() converts that typed text into a number so we can do math with it
  num1 <- as.numeric(readline(prompt = "Enter first number: "))

  # op stays as text (a symbol like + - * /) so we do NOT use as.numeric() here
  op   <- readline(prompt = "Enter operator (+, -, *, /): ")

  num2 <- as.numeric(readline(prompt = "Enter second number: "))

  # switch() checks the value of op and runs only the matching calculation
  result <- switch(
    op,
    "+" = num1 + num2,
    "-" = num1 - num2,
    "*" = num1 * num2,
    "/" = if (num2 != 0) num1 / num2 else NA, # if dividing by zero, store NA instead of crashing
    { cat("Invalid operator\n"); NA }          # if op is not + - * /, print an error and store NA
  )

  # Check for division by zero first; otherwise print the result
  if (op == "/" && num2 == 0) {
    cat("Error: division by zero\n")
  } else if (!is.na(result)) {
    # sprintf() builds a formatted string by filling in placeholders:
    # %.4g = a number shown with up to 4 significant digits (e.g. 3.333 not 3.333333333)
    # %s   = text printed exactly as-is (used here for the operator symbol like + or *)
    # The four values num1, op, num2, result match the four placeholders in order
    cat(sprintf("%.4g %s %.4g = %.4g\n", num1, op, num2, result))
  }
} 
