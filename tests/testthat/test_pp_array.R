require(testthat)
data(PolicyPortfolio)
context("Convert to array")

test_that("pp_array onverts tidy dataset to array", {
            real.dim <- as.integer(c(3, 1, 11, 10, 15))
            expect_identical(dim(pp_array(P.education)), real.dim)
})

test_that("pp_array converts tidy dataset to two-dimensional matrix", {
            real.dim <- as.integer(c(15, 25))
            P1 <- P.energy %>%
              filter(Country == "Syldavia" & Year == 2020)
            expect_identical(dim(pp_array(P1, return_matrix = TRUE)), real.dim)
})
