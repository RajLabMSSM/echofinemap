test_that("PAINTOR_check_ld returns NULL when LD_matrix is NULL", {

    res <- echofinemap:::PAINTOR_check_ld(
        LD_matrix = NULL,
        dat_ls = list(d1 = data.frame()),
        locus_dir = "/tmp/test_locus"
    )
    testthat::expect_null(res)
})

test_that("PAINTOR_check_ld wraps single matrix", {

    ld <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
    dat_ls <- list(d1 = data.frame(SNP = c("rs1", "rs2")))

    res <- echofinemap:::PAINTOR_check_ld(
        LD_matrix = ld,
        dat_ls = dat_ls,
        locus_dir = "/tmp/my_locus",
        verbose = FALSE
    )
    testthat::expect_type(res, "list")
    testthat::expect_length(res, 1)
    ## Name follows PAINTOR convention: <locus_name>.ld<N>
    testthat::expect_true(grepl("my_locus\\.ld1", names(res)[1]))
})

test_that("PAINTOR_check_ld errors when LD list length mismatches dat", {

    ld_list <- list(
        matrix(1, 2, 2),
        matrix(1, 2, 2)
    )
    dat_ls <- list(
        d1 = data.frame(SNP = "rs1"),
        d2 = data.frame(SNP = "rs2"),
        d3 = data.frame(SNP = "rs3")
    )

    testthat::expect_error(
        echofinemap:::PAINTOR_check_ld(
            LD_matrix = ld_list,
            dat_ls = dat_ls,
            locus_dir = "/tmp/test"
        ),
        "must match the number of datasets"
    )
})
