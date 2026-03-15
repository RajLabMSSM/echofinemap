test_that("required_cols returns a data.table", {

    d <- echofinemap::required_cols(verbose = FALSE)
    testthat::expect_s3_class(d, "data.table")
    testthat::expect_true("method" %in% colnames(d))
    testthat::expect_true("required" %in% colnames(d))
    testthat::expect_true("suggested" %in% colnames(d))
})

test_that("required_cols contains expected methods", {

    d <- echofinemap::required_cols(verbose = FALSE)
    expected_methods <- c("ABF", "FINEMAP", "SUSIE",
                          "POLYFUN_SUSIE", "POLYFUN_FINEMAP",
                          "PAINTOR",
                          "COJO_stepwise", "COJO_conditional", "COJO_joint")
    testthat::expect_true(all(expected_methods %in% d$method))
})

test_that("required_cols includes proportion_cases for case_control", {

    d_cc <- echofinemap::required_cols(case_control = TRUE, verbose = FALSE)
    abf_req <- d_cc[d_cc$method == "ABF", ]$required[[1]]
    testthat::expect_true("proportion_cases" %in% abf_req)
})

test_that("required_cols excludes proportion_cases when not case_control", {

    d_quant <- echofinemap::required_cols(case_control = FALSE,
                                           verbose = FALSE)
    abf_req <- d_quant[d_quant$method == "ABF", ]$required[[1]]
    testthat::expect_false("proportion_cases" %in% abf_req)
})

test_that("required_cols adds sources when requested", {

    d <- echofinemap::required_cols(add_sources = TRUE, verbose = FALSE)
    testthat::expect_true("source" %in% colnames(d))
    ## Each source should be a URL-like string
    testthat::expect_true(all(grepl("http", d$source)))
})

test_that("required_cols adds citations when requested", {

    d <- echofinemap::required_cols(add_citations = TRUE, verbose = FALSE)
    testthat::expect_true("citation" %in% colnames(d))
    testthat::expect_true(all(grepl("doi\\.org|http", unlist(d$citation))))
})

test_that("required_cols embed_links wraps in markdown", {

    d <- echofinemap::required_cols(add_sources = TRUE,
                                     add_citations = TRUE,
                                     embed_links = TRUE,
                                     verbose = FALSE)
    ## Sources should be markdown links
    testthat::expect_true(all(grepl("^\\[source\\]\\(", unlist(d$source))))
    testthat::expect_true(all(grepl("^\\[cite\\]\\(", unlist(d$citation))))
})

test_that("required_cols is keyed by method", {

    d <- echofinemap::required_cols(verbose = FALSE)
    testthat::expect_equal(data.table::key(d), "method")
})
