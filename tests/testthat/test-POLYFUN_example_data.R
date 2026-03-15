test_that("POLYFUN_example_data returns path for sumstats type", {

    ## This test only runs if the polyfun example_data ships with the package
    ex_dir <- system.file("tools", "polyfun", "example_data",
                          package = "echofinemap")
    testthat::skip_if(ex_dir == "" || !dir.exists(ex_dir),
                      "PolyFun example_data not shipped with this install")

    res <- echofinemap:::POLYFUN_example_data(type = "sumstats")
    testthat::expect_true(is.character(res))
    testthat::expect_true(file.exists(res))
})

test_that("POLYFUN_example_data returns path for annotations type", {

    ex_dir <- system.file("tools", "polyfun", "example_data",
                          package = "echofinemap")
    testthat::skip_if(ex_dir == "" || !dir.exists(ex_dir),
                      "PolyFun example_data not shipped with this install")

    res <- echofinemap:::POLYFUN_example_data(type = "annotations")
    testthat::expect_true(is.character(res))
    testthat::expect_true(grepl("annotations", res))
})

test_that("POLYFUN_example_data returns path for weights type", {

    ex_dir <- system.file("tools", "polyfun", "example_data",
                          package = "echofinemap")
    testthat::skip_if(ex_dir == "" || !dir.exists(ex_dir),
                      "PolyFun example_data not shipped with this install")

    res <- echofinemap:::POLYFUN_example_data(type = "weights")
    testthat::expect_true(is.character(res))
    testthat::expect_true(grepl("weights", res))
})
