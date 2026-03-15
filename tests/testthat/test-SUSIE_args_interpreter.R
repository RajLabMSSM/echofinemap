test_that("SUSIE_args_intrepreter returns list for valid args", {

    args_list <- list(
        max_iter = 500,
        verbose = FALSE
    )

    res <- echofinemap:::SUSIE_args_intrepreter(
        args_list = args_list,
        arg_name = "max_iter"
    )
    ## Should return the full args_list if any match SUSIE formals
    testthat::expect_type(res, "list")
})

test_that("SUSIE_args_intrepreter returns NULL for no valid args", {

    args_list <- list(
        fake_arg_1 = 100,
        fake_arg_2 = TRUE
    )

    res <- echofinemap:::SUSIE_args_intrepreter(
        args_list = args_list,
        arg_name = "fake_arg_1"
    )
    testthat::expect_null(res)
})
