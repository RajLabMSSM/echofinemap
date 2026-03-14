test_that("multifinemap works with BST1", {

    testthat::skip_if_not_installed("echoLD")
    ## FINEMAP binary must be runnable
    FINEMAP_path <- tryCatch(
        echofinemap:::FINEMAP_find_executable(verbose = FALSE),
        error = function(e) NULL
    )
    testthat::skip_if(is.null(FINEMAP_path),
                      "FINEMAP binary not available")
    finemap_ok <- tryCatch(
        echofinemap:::FINEMAP_check_runnable(FINEMAP_path, verbose = FALSE),
        error = function(e) FALSE
    )
    testthat::skip_if_not(isTRUE(finemap_ok),
                          "FINEMAP binary cannot run on this system")

    dat <- echofinemap::drop_finemap_cols(echodata::BST1)
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    fullSS_path <- echodata::example_fullSS()
    finemap_methods <- c("ABF", "SUSIE", "FINEMAP")

    run_tests <- function(dat, dat2, finemap_methods,
                          cs_sizes = NULL, pp_sizes = NULL){
        dat <- data.table::copy(dat)
        testthat::expect_gte(nrow(dat), nrow(dat2))
        testthat::expect_true(all(c(
            paste(finemap_methods, "CS", sep = "."),
            paste(finemap_methods, "PP", sep = "."),
            c("Support", "Consensus_SNP")
        ) %in% colnames(dat2)))
        if(!is.null(cs_sizes)){
            cs_dict <- stats::setNames(cs_sizes, finemap_methods)
            for(m in finemap_methods){
                cs <- dat2[, (paste0(m, ".CS")), with = FALSE]
                n_cs <- sum(cs > 0, na.rm = TRUE)
                testthat::expect_equal(n_cs, cs_dict[[m]])
            }
        }
        if(!is.null(pp_sizes)){
            pp_dict <- stats::setNames(pp_sizes, finemap_methods)
            for(m in finemap_methods){
                pp <- dat2[, (paste0(m, ".PP")), with = FALSE]
                n_pp <- sum(pp > 0.95, na.rm = TRUE)
                testthat::expect_equal(n_pp, pp_dict[[m]])
            }
        }
    }

    #### n_causal = 1 ####
    dat0 <- echofinemap::multifinemap(dat = dat,
                                      locus_dir = locus_dir,
                                      LD_matrix = LD_matrix,
                                      fullSS_path = fullSS_path,
                                      force_new_finemap = TRUE,
                                      n_causal = 1,
                                      finemap_methods = finemap_methods)
    run_tests(dat = dat, dat2 = dat0,
              finemap_methods = finemap_methods,
              cs_sizes = c(1, 1, 1),
              pp_sizes = c(1, 1, 1))
    top_snp <- subset(dat0, Support == max(Support))
    testthat::expect_equal(top_snp$Support, 3)
    testthat::expect_equal(top_snp$SNP, "rs4698412")

    #### n_causal = 5 (default) ####
    dat2 <- echofinemap::multifinemap(dat = dat,
                                      locus_dir = locus_dir,
                                      LD_matrix = LD_matrix,
                                      fullSS_path = fullSS_path,
                                      force_new_finemap = TRUE,
                                      finemap_methods = finemap_methods)
    run_tests(dat = dat, dat2 = dat2,
              finemap_methods = finemap_methods,
              cs_sizes = c(1, 5, 5),
              pp_sizes = c(1, 5, 5))

    #### Re-run with n_causal = 10, SUSIE only ####
    dat3 <- echofinemap::multifinemap(dat = dat2,
                                      locus_dir = locus_dir,
                                      LD_matrix = LD_matrix,
                                      fullSS_path = fullSS_path,
                                      n_causal = 10,
                                      finemap_methods = "SUSIE")
    run_tests(dat = dat, dat2 = dat3,
              finemap_methods = finemap_methods,
              cs_sizes = c(1, 5, 5),
              pp_sizes = c(1, 5, 5))
})

test_that("multifinemap works with LRRK2 (requires network)", {

    testthat::skip_if_not_installed("echoLD")
    testthat::skip_if_offline()

    FINEMAP_path <- echofinemap:::FINEMAP_find_executable(verbose = FALSE)
    testthat::skip_if_not(
        isTRUE(echofinemap:::FINEMAP_check_runnable(FINEMAP_path,
                                                     verbose = FALSE)),
        "FINEMAP binary cannot run on this system"
    )

    dat <- echofinemap::drop_finemap_cols(echodata::LRRK2)[seq_len(100), ]
    locus_dir <- file.path(tempdir(), "LRRK2")
    fullSS_path <- echodata::example_fullSS()
    finemap_methods <- c("ABF", "SUSIE", "FINEMAP")

    ## Get LD from 1KG
    LD_list <- echoLD::get_LD(query_dat = dat,
                              locus_dir = locus_dir,
                              LD_reference = "1KGphase3")

    testthat::expect_equal(
        sum(grepl(paste(finemap_methods, collapse = "|"), colnames(dat))), 0
    )
    dat4 <- echofinemap::multifinemap(dat = dat,
                                      locus_dir = locus_dir,
                                      LD_matrix = LD_list$LD,
                                      fullSS_path = fullSS_path,
                                      force_new_finemap = TRUE,
                                      finemap_methods = finemap_methods)
    testthat::expect_true(all(c("Support", "Consensus_SNP") %in%
                              colnames(dat4)))
    testthat::expect_gt(sum(dat4$Support > 0, na.rm = TRUE), 0)

    #### Using an LD Panel from the wrong locus ####
    testthat::expect_error(
        echofinemap::multifinemap(dat = dat,
                                  locus_dir = locus_dir,
                                  LD_matrix = echodata::BST1_LD_matrix,
                                  fullSS_path = fullSS_path,
                                  force_new_finemap = TRUE,
                                  finemap_methods = finemap_methods)
    )
})

test_that("multifinemap works with POLYFUN_SUSIE", {

    testthat::skip_if_not_installed("echoLD")
    testthat::skip_if_not_installed("echoconda")
    testthat::skip_if_not_installed("Ckmeans.1d.dp")
    testthat::skip_if_offline()
    conda_available <- tryCatch(
        echoconda::env_exists(conda_env = "echoR_mini"),
        error = function(e) FALSE
    )
    testthat::skip_if_not(conda_available,
                          message = "echoR_mini conda env not available")
    testthat::skip_if_not(
        dir.exists(system.file("tools", "polyfun", package = "echofinemap")),
        message = "PolyFun submodule not installed"
    )

    dat <- echofinemap::drop_finemap_cols(echodata::LRRK2)[seq_len(100), ]
    locus_dir <- file.path(tempdir(), "LRRK2")
    fullSS_path <- echodata::example_fullSS()

    LD_list <- echoLD::get_LD(query_dat = dat,
                              locus_dir = locus_dir,
                              LD_reference = "1KGphase3")

    dat5 <- echofinemap::multifinemap(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_list$LD,
        fullSS_path = fullSS_path,
        force_new_finemap = TRUE,
        finemap_methods = c("SUSIE", "POLYFUN_SUSIE"))
    testthat::expect_true(all(
        c("SUSIE.CS", "SUSIE.PP",
          "POLYFUN_SUSIE.CS", "POLYFUN_SUSIE.PP") %in% colnames(dat5)
    ))
})
