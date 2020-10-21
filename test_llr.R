##test_llr

library(testthat)

test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})

test_that("make_weight_matrix works on simple cases", {
  Wz <- make_weight_matrix(z[1],x,1)
  expect_true(all(Wz[upper.tri(Wz)] == 0, Wz[lower.tri(Wz)] == 0))
  r <- abs(x-z[1])/1
  expect_equal(diag(Wz), sapply(r,W))
})

test_that("make_predictor_matrix works on simple cases", {
  n <-length(x)
  X <- make_predictor_matrix(x)
  expect_equal(dim(X),c(n,2))
  expect_equal(X[,1],rep(1,n))
})

