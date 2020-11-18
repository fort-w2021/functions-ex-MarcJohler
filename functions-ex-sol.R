# a)
# set a seed
set.seed(20141012)

# generate a sample of 20 values between 1 - 10 (allow NAs)
obs_var1 <- sample(c(1:10, NA), size = 20, replace = TRUE)
obs_var2 <- runif(min = 0, max = 1, 20)
cor(obs_var1,
    obs_var2,
    method = "kendall", 
    use = "pairwise.complete.obs")

# b) 
f1 <- function(x = {
  y <- 1
  2
}, y = 0) {
  c(y, x, y)
}
# y is zero, as long as the function assigned to x is not called
# if x is called, y is set to 1 and 2 is returned
# This illustrates the concept of dynamic lookup

# c)
z <- 50
f2 <- function(x = z) {
  z <- 100
  return(c(x,z))
}
# --> ?

# d)
# define XOR - function
'%xor%' <- function(A,B) {
  # check if input variables are logical
  checkmate::check_logical(A, any.missing = FALSE, min.len = 1)
  checkmate::check_logical(B, any.missing = FALSE, min.len = 1)
  !(A == B)
}

library(testthat)

context("Infix: %xor% ")

test_that("%xor% works as expected for scalar input", {
  expect_true(TRUE %xor% FALSE)
  expect_false(TRUE %xor% TRUE)
  expect_true(FALSE %xor% TRUE)
  expect_false(FALSE %xor% FALSE)
})

test_that("%xor% works (element-wise) for vector input", {
  expect_identical(
    c(TRUE, TRUE, FALSE, FALSE) %xor% c(FALSE, TRUE, TRUE, FALSE),
    c(TRUE, FALSE, TRUE, FALSE))
})

