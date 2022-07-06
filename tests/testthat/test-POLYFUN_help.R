test_that("POLYFUN_help works", {
    
    out <- POLYFUN_help()
    testthat::expect_true(grepl("PolyFun",out[[2]]))
    testthat::expect_gte(length(out), 50)
})
