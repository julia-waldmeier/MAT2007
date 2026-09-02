# Easy Exercise: Temperature Conversion
x <- 25 # x = Temperature in Celsius

# Formula to convert Celsius to Fahrenheit
fahrenheit <- x * 9/5 + 32  

print(paste("Temperature in Fahrenheit:", fahrenheit))

# Difficult Exercise: Simple Calculator
# To use the calculator, open the R terminal and run the function `calculate()`. It will prompt you to enter two numbers and an operator (+, -, *, /) to perform the calculation. The result will be displayed in the console.
calculate <- function() {
  # readline() prompts the user for input; as.numeric() converts the text to a number
  num1 <- as.numeric(readline(prompt = "Enter first number: "))
  op   <- readline(prompt = "Enter operator (+, -, *, /): ")
  num2 <- as.numeric(readline(prompt = "Enter second number: "))
  
  # switch() matches the operator to the correct calculation
  result <- switch(
    op,
    "+" = num1 + num2,
    "-" = num1 - num2,
    "*" = num1 * num2,
    "/" = if (num2 != 0) num1 / num2 else NA, # Prevent division by zero
    { cat("Invalid operator\n"); NA } # Handle unrecognised operators
  )

# Print the result, or an error message if something went wrong
  if (op == "/" && num2 == 0) {
    if (op == "/" && num2 == 0) {
    cat("Error: division by zero\n")
  } else if (!is.na(result)) {
    # sprintf() formats the output neatly into one line
    # %.4g means: print the number using up to 4 significant digits
    # %s means: print text as-is (used for the operator symbol)
    # the four % placeholders are filled in order: num1, op, num2, result
    cat(sprintf("%.4g %s %.4g = %.4g\n", num1, op, num2, result))
  }
}
} 
