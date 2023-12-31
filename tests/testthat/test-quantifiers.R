# Sample operators for testing
greater_than_operator <- function(e1, e2) e1 > e2
less_than_operator <- function(e1, e2) e1 < e2
less_than_equal_operator <- function(e1, e2) e1 <= e2
greater_than_equal_operator <- function(e1, e2) e1 >= e2
equal_to_operator <- function(e1, e2) e1 == e2

test_that("Aggregator returns proportion correctly", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec, type = "p")
  expect_equal(result, 0.6)
})

test_that("Aggregator returns count correctly", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec, type = "n")
  expect_equal(result, 3)
})

test_that("Aggregator handles NA values correctly (proportions)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec_with_na, type = "p", na.rm = TRUE)
  expect_equal(result, 0.50)
})

test_that("Aggregator handles NA values correctly (counts)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec_with_na, type = "n", na.rm = TRUE)
  expect_equal(result, 2)
})

test_that("Aggregator throws error for invalid type", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  expect_error(aggregator(logical_vec, type = "invalid_type"))
})

test_that("Aggregator throws error for invalid type length", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  expect_error(aggregator(logical_vec, type = c("p", "n")))
})

test_that("Quantifier with greater_than_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  result <- quant_func(logical_vec, p = 0.5)
  expect_true(result)
})

test_that("Quantifier with greater_than_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  result <- quant_func(logical_vec, n = 2)
  expect_true(result)
})

test_that("Quantifier with less_than_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_operator)
  result <- quant_func(logical_vec, p = 0.7)
  expect_true(result)
})

test_that("Quantifier with less_than_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_false(result)
})

test_that("Quantifier throws error for missing p and n", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  expect_error(quant_func(logical_vec))
})

test_that("Quantifier throws error for both p and n", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  expect_error(quant_func(logical_vec, p = 0.6, n = 3))
})

test_that("Quantifier with less_than_equal_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_equal_operator)
  result <- quant_func(logical_vec, p = 0.7)
  expect_true(result)
})

test_that("Quantifier with less_than_equal_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_equal_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_true(result)
})

test_that("Quantifier with greater_than_equal_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_equal_operator)
  result <- quant_func(logical_vec, p = 0.4)
  expect_true(result)
})

test_that("Quantifier with greater_than_equal_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_equal_operator)
  result <- quant_func(logical_vec, n = 2)
  expect_true(result)
})

test_that("Quantifier with equal_to_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(equal_to_operator)
  result <- quant_func(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("Quantifier with equal_to_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(equal_to_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_true(result)
})

test_that("Proportion calculation works correctly (without NA removal)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- prop(logical_vec)
  expect_equal(result, 0.6)
})

test_that("Proportion calculation works correctly (with NA removal)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- prop(logical_vec_with_na, na.rm = TRUE)
  expect_equal(result, 0.50)
})

test_that("Proportion calculation handles all TRUE values", {
  all_true_logical_vec <- c(TRUE, TRUE, TRUE)
  result <- prop(all_true_logical_vec)
  expect_equal(result, 1)
})

test_that("Proportion calculation handles all FALSE values", {
  all_false_logical_vec <- c(FALSE, FALSE, FALSE)
  result <- prop(all_false_logical_vec)
  expect_equal(result, 0)
})

test_that("Proportion calculation handles NA values", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- prop(logical_vec_with_na, na.rm = T)
  expect_equal(result, 0.5)
})

test_that("is_p_or_n returns 'p' for valid proportion", {
  result <- is_p_or_n(0.5)
  expect_equal(result, "p")
})

test_that("is_p_or_n returns 'n' for valid count", {
  result <- is_p_or_n(10)
  expect_equal(result, "n")
})

test_that("is_p_or_n throws error for value of 1 (error_on_1 = TRUE)", {
  expect_error(is_p_or_n(1))
})

test_that("is_p_or_n returns 'p' for value of 1 (error_on_1 = FALSE)", {
  result <- is_p_or_n(1, error_on_1 = FALSE)
  expect_equal(result, "p")
})

test_that("is_p_or_n throws error for invalid value", {
  expect_error(is_p_or_n("invalid_value"))
})

test_that("is_p_or_n returns FALSE for invalid value (error_on_nothing = FALSE)", {
  result <- is_p_or_n("invalid_value", error_on_nothing = FALSE)
  expect_false(result)
})

test_that("is_p_or_n throws error for non-numeric value (error_on_nothing = TRUE)", {
  expect_error(is_p_or_n("non_numeric_value"))
})

test_that("is_logical_vec returns TRUE for valid logical vector", {
  result <- is_logical_vec(c(TRUE, FALSE, TRUE))
  expect_true(result)
})

test_that("is_logical_vec returns FALSE for empty vector", {
  result <- is_logical_vec(c())
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for non-logical vector", {
  result <- is_logical_vec(c(1, 2, 3))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for all NA vector", {
  result <- is_logical_vec(c(NA, NA, NA))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for mixed vector with NA", {
  result <- is_logical_vec(c(TRUE, 1, "text", NA))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for non-vector input", {
  expect_true(is_logical_vec(T))
  expect_false(is_logical_vec("a"))
})

test_that("at_least works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_least(logical_vec, p = 0.4)
  expect_true(result)
})

test_that("at_least works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_least(logical_vec, n = 2)
  expect_true(result)
})

test_that("more_than works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- more_than(logical_vec, p = 0.6)
  expect_false(result)
})

test_that("more_than works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- more_than(logical_vec, n = 4)
  expect_false(result)
})

test_that("at_most works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_most(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("at_most works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_most(logical_vec, n = 3)
  expect_true(result)
})

test_that("less_than works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- less_than(logical_vec, p = 0.4)
  expect_false(result)
})

test_that("less_than works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- less_than(logical_vec, n = 2)
  expect_false(result)
})

test_that("exactly_equal works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- exactly_equal(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("exactly_equal works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- exactly_equal(logical_vec, n = 3)
  expect_true(result)
})

test_that("some_of throws error for empty ... argument", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of throws an error when ... is empty
  expect_error(some_of(logical_vec))
})

test_that("some_of throws error for unnamed arguments", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of throws an error when arguments are not named
  expect_error(some_of(logical_vec, TRUE, FALSE, at_least = 2))
})

test_that("some_of handles single quantifier argument correctly", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of correctly handles a single quantifier argument
  result <- some_of(logical_vec, at_least = 2)
  expect_true(result)
})

test_that("some_of handles multiple quantifier arguments correctly", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of correctly handles multiple quantifier arguments
  result <- some_of(logical_vec, at_least = 2, less_than = 3)
  expect_true(result)
})

test_that("some_of handles complex conditions correctly", {
  logical_vec <- c(TRUE, FALSE, TRUE, TRUE)

  # Test that some_of correctly handles complex conditions
  result <- some_of(logical_vec, at_least = 2, at_most = 3)
  expect_true(result)
})

test_that("some_of throws error for non-logical input", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of throws an error for non-logical input
  expect_error(some_of(logical_vec, 2 > 1))
})

test_that("some_of fails when at_least = 1", {
  logical_vec <- c(TRUE, FALSE, TRUE)

  # Test that some_of fails when at_least = 1
  expect_error(some_of(logical_vec, at_least = 1))
})

test_that("some_of handles empty input vector", {
  logical_vec <- logical(0)
  expect_error(some_of(logical_vec, at_least = 2))
})

test_that("some_of handles all FALSE values", {
  logical_vec <- c(FALSE, FALSE, FALSE)
  result <- some_of(logical_vec, at_least = 2)
  expect_false(result)
})

test_that("some_of handles zero quantifiers", {
  logical_vec <- c(TRUE, FALSE, TRUE)
  expect_error(some_of(logical_vec))
})

test_that("some_of handles mixed quantifiers", {
  logical_vec <- c(TRUE, FALSE, TRUE, TRUE)
  result <- some_of(logical_vec, at_least = 2, at_most = 3, less_than = 4)
  expect_true(result)
})

test_that("some_of handles non-numeric quantifiers", {
  logical_vec <- c(TRUE, FALSE, TRUE, TRUE)
  expect_error(some_of(logical_vec, at_least = "a", at_most = "b"))
})

test_that("some_of handles a large input vector", {
  logical_vec <- rep(TRUE, 1000)
  result <- some_of(logical_vec, at_least = 1000)
  expect_true(result)
})

test_that("some_of handles proportions for at_least correctly", {
  logical_vec <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

  # Test that some_of correctly handles at_least as a proportion
  result <- some_of(logical_vec, at_least = 0.4)
  expect_true(result)

  # Test that some_of correctly handles at_least as a proportion
  result <- some_of(logical_vec, at_least = 0.6)
  expect_true(result)

  # Test that some_of correctly handles at_least as a proportion with rounding
  result <- some_of(logical_vec, at_least = 0.9)
  expect_false(result)
})

test_that("some_of throws error for non-integer / non-prop values", {
  logical_vec <- c(TRUE, FALSE, TRUE)
  expect_error(some_of(logical_vec, at_least = 1.2))
})

test_that("some_of throws error for negative values", {
  logical_vec <- c(TRUE, FALSE, TRUE)
  expect_error(some_of(logical_vec, at_least = -3))
})

test_that("whenever correctly evaluates when is_observed and then_expect are both TRUE", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

  result <- whenever(is_observed, then_expect)
  expect_true(result)
})

test_that("whenever correctly evaluates when is_observed is TRUE and then_expect is FALSE", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, FALSE, TRUE, FALSE)

  result <- whenever(is_observed, then_expect)
  expect_false(result)
})

test_that("whenever correctly evaluates when is_observed is FALSE and then_expect is TRUE", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(FALSE, FALSE, TRUE, TRUE, TRUE)

  # Test that whenever correctly evaluates when is_observed is FALSE and then_expect is TRUE
  result <- whenever(is_observed, then_expect)
  expect_false(result)
})

test_that("whenever correctly evaluates when at_least condition is met", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  # Test that whenever correctly evaluates when at_least condition is met
  result <- whenever(is_observed, then_expect, at_least = 3)
  expect_true(result)

  result <- whenever(is_observed, then_expect, at_least = 4)
  expect_false(result)
})

test_that("whenever correctly evaluates when at_most condition is met", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  result <- whenever(is_observed, then_expect, at_most = 4)
  expect_true(result)

  result <- whenever(is_observed, then_expect, at_most = 3)
  expect_false(result)
})

test_that("whenever correctly evaluates when both at_least and at_most conditions are met", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  result <- whenever(is_observed, then_expect, at_least = 2, at_most = 4)
  expect_true(result)

  result <- whenever(is_observed, then_expect, at_least = 2, at_most = 2)
  expect_false(result)
})

test_that("whenever throws error for non-logical input in is_observed", {
  is_observed <- c(TRUE, FALSE, TRUE, "string", FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  expect_error(whenever(is_observed, then_expect))
})

test_that("whenever throws error for non-logical input in then_expect", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, "string", TRUE)

  expect_error(whenever(is_observed, then_expect))
})

test_that("whenever correctly evaluates at_least as proportion", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Test that whenever correctly evaluates at_least as a proportion
  result <- whenever(is_observed, then_expect, at_least = 0.6)
  expect_true(result)

  # Test that whenever correctly evaluates at_least as a proportion with rounding
  result <- whenever(is_observed, then_expect, at_least = 0.7)
  expect_false(result)
})

test_that("whenever correctly evaluates at_most as proportion", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  # Test that whenever correctly evaluates at_most as a proportion
  result <- whenever(is_observed, then_expect, at_least = 0.5)
  expect_true(result)

  # Test that whenever correctly evaluates at_most as a proportion with rounding
  result <- whenever(is_observed, then_expect, at_most = 0.3)
  expect_false(result)
})

test_that("whenever throws error for at_least = 1", {
  is_observed <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  then_expect <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

  # Test that whenever throws an error for at_least = 1
  expect_error(whenever(is_observed, then_expect, at_least = 1))
})

test_that("for_case correctly resolves to TRUE when all logical vectors have TRUE at case", {
  case <- 2
  logical_vec1 <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  logical_vec2 <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  result <- for_case(case, logical_vec1, logical_vec2)
  expect_true(result)
})

test_that("for_case correctly receives valid logical vector (for case arg) with only 1 TRUE value", {
  case <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  logical_vec1 <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  logical_vec2 <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  result <- for_case(case, logical_vec1, logical_vec2)
  expect_true(result)
})

test_that("for_case correctly throws error for  invalid logical vector (for case arg) with > 1 TRUE value", {
  case <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  logical_vec1 <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  logical_vec2 <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  expect_error(for_case(case, logical_vec1, logical_vec2))
})

test_that("for_case correctly resolves to FALSE when at least one logical vector has FALSE at case", {
  case <- 3
  logical_vec1 <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
  logical_vec2 <- c(TRUE, TRUE, FALSE, TRUE, TRUE)

  result <- for_case(case, logical_vec1, logical_vec2)
  expect_false(result)
})
