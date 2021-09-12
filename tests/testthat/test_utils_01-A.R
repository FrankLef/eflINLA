
test_that("prec2sd", {
  # r <- system.file("testdata", "i04M07ctr.rds", package = "eflINLA", mustWork = TRUE)
  r <- here::here("tests", "testdata", "i04M07ctr.rds")
  r <- readRDS(r)
  pos <- grep(pattern = "^Log precision", x = names(r$internal.marginals.hyperpar))
  imargh <- r$internal.marginals.hyperpar[[pos]]
  expect_identical(dim(imargh), c(75L, 2L))
})
