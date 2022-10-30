test_that("POLYFUN_compute_priors works", {
  
    fullSS_path <- echodata::example_fullSS()
    ldsc_files <- echofinemap::POLYFUN_compute_priors(fullSS_path=fullSS_path)
})
