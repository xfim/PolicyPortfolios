require(testthat)
data(PolicyPortfolio)
context("Measures of policy portfolios")

test_that("pp_measures converts tidy dataset to matrix", {
            real.dim <- as.integer(c(363, 6))
            expect_identical(dim(pp_measures(P.education)), real.dim)
})

test_that("pp_measures returns a only certain cases when 'id' is used", {
            real.dim <- as.integer(c(22, 6))
            expect_identical(dim(pp_measures(P.education, 
                                             id = list(Country = c("San Theodoros", "Syldavia"), Year = 2022))), 
                             real.dim)
})

test_that("pp_measures calculates correctly", {
            real.size <- 0.05333
            p.size <- round(subset(pp_measures(P.education, id = list(Country = "Syldavia", Year = 2022)), Measure == "Size")$value, 5)
            expect_identical(p.size, real.size)
})

