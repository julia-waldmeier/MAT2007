# To view all of the answers to the exercises at once, press run.
# To view the answers to each exercise individually, comment the corresponding function call in the R terminal; ex exercise1() 

# Exercise 1: Print Your Name
exercise1 <- function() {
  cat("Julia Waldmeier\n")
}

# Exercise 2: Simple Math Operations
exercise2 <- function() {
  a <- 15
  b <- 7

  print(paste("Addition:", a + b))
  print(paste("Subtraction:", a - b))
  print(paste("Multiplication:", a * b))
  print(paste("Division:", round(a / b, 2))) # round() limits decimal places; 2 means 2 decimal places
}

# Exercise 3: List Operations
exercise3 <- function() {
  my_list <- c(1, 2, 3, 4, 5)
  print(my_list)

  # Append a new element
  my_list <- c(my_list, 6)
  print(my_list)

  # Remove the last element
  my_list <- my_list[-length(my_list)] # Negative index removes that position; -length removes the last element
  print(my_list) 
}

# Exercise 4: Simple Conditions
exercise4 <- function() {
  num <- 7
if (num %% 2 == 0) { # %% gives the remainder after division (e.g., 7 %% 2 = 1, so 7 is odd)
  print("The number is even.")
} else {
  print("The number is odd.")
} 
} 

exercise1()
exercise2()
exercise3()
exercise4()
