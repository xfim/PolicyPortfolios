library(testthat)
data(consensus)
context("Check that diversity (AID) calculation matches APSR")

calc.aid <- consensus %>%
  filter(Sector == "Environmental") %>%
  filter(Country %in% c("France", "United States")) %>%
  filter(Year %in% c(1976, 2005)) %>%
  pp_array() %>%
  apply(., c(1, 2, 3), diversity_aid) %>%
  as.vector() %>%
  round(digits = 3)

test_that("diversity matches reported in APSR", {
            apsr.aid <- c(0.464, 0.839, 0.855, 0.850) # 1976 (FR, US), 2005 (FR, US)
            expect_identical(calc.aid, apsr.aid)
})

