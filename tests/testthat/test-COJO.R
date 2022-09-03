test_that("COJO works", {

    vcf <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
        package = "echodata")
    dat <- echodata::BST1
    conditioned_snps <- dat[(leadSNP==TRUE),]$SNP
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    fullSS_path <- echodata::example_fullSS()
    bfile <- echoLD::vcf_to_plink(vcf = vcf)$prefix
    
    step_cols <- c("bJ","bJ_se","pJ","LD_r","CS")
    cond_cols <- c("bC","bC_se","pC","CS_cond")
    testthat::expect_false(
        all(step_cols %in% names(dat))
    )
    testthat::expect_false(
        all(cond_cols %in% names(dat))
    )
    
    #### All 3 modes at once: BEFORE running other functions ####
    ## The flag
    testthat::expect_warning(
        cojo_DT <- COJO(dat = dat,
                        locus_dir = locus_dir,
                        fullSS_path = fullSS_path,
                        bfile = bfile, 
                        run_stepwise = TRUE, 
                        run_conditional = TRUE,
                        run_joint = TRUE, 
                        conditioned_snps = conditioned_snps)
    )
    testthat::expect_false(
        all(step_cols %in% names(cojo_DT))
    )
    #### mode: stepwise: locus-specific ####
    cojo_DT <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    bfile = bfile, 
                    run_stepwise = TRUE, 
                    run_conditional = FALSE,
                    run_joint = FALSE) 
    testthat::expect_true(
       all(step_cols %in% names(cojo_DT))
    )
    #### mode: stepwise: genome-wide ####
    cojo_DT <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    bfile = bfile, 
                    run_stepwise = TRUE, 
                    run_conditional = FALSE,
                    run_joint = FALSE,
                    full_genome = TRUE)
    testthat::expect_true(
        all(step_cols %in% names(cojo_DT))
    )
    #### mode: conditional ####
    cojo_DT <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    bfile = bfile, 
                    run_stepwise = FALSE, 
                    run_conditional = TRUE,
                    run_joint = FALSE, 
                    conditioned_snps = conditioned_snps) 
    testthat::expect_true(
        all(cond_cols %in% names(cojo_DT))
    ) 
})
