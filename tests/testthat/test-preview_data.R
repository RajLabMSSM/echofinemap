test_that("preview_data prints data as message", {

    dat <- data.frame(a = 1:3, b = letters[1:3])
    out <- testthat::expect_message(
        echofinemap:::preview_data(dat),
        regexp = "a b"
    )
})
