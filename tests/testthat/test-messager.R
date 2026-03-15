test_that("messager outputs message when v=TRUE", {

    testthat::expect_message(
        echofinemap:::messager("hello world", v = TRUE),
        "hello world"
    )
})

test_that("messager is silent when v=FALSE", {

    testthat::expect_silent(
        echofinemap:::messager("secret message", v = FALSE)
    )
})

test_that("messager concatenates multiple arguments", {

    testthat::expect_message(
        echofinemap:::messager("part1", "part2", "part3", v = TRUE),
        "part1 part2 part3"
    )
})
