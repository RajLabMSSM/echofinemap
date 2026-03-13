#' Setup FINEMAP dynamic libraries
#'
#' On Apple Silicon Macs, the pre-compiled x86_64 FINEMAP binary requires
#' x86_64 versions of several dynamic libraries (zstd, gcc runtime, libomp).
#' This function downloads and compiles the required libraries, then uses
#' \code{install_name_tool} to rewrite the binary's library paths to point
#' to the local copies.
#'
#' @param FINEMAP_path Path to the FINEMAP executable.
#' @param verbose Print messages.
#' @returns The (potentially modified) FINEMAP path, invisibly.
#' @keywords internal
#' @importFrom echodata get_os
FINEMAP_setup_dylibs <- function(FINEMAP_path,
                                 verbose = TRUE){

    OS <- echodata::get_os()
    arch <- Sys.info()[["machine"]]

    ## Only needed on Apple Silicon Macs
    if(!(OS == "osx" && arch == "arm64")) return(invisible(FINEMAP_path))

    finemap_dir <- dirname(FINEMAP_path)
    libs_dir <- file.path(finemap_dir, "libs")

    ## Check if already set up
    if(file.exists(file.path(libs_dir, ".setup_complete"))){
        return(invisible(FINEMAP_path))
    }

    messager("Setting up x86_64 dynamic libraries for FINEMAP on",
             "Apple Silicon...", v = verbose)
    dir.create(libs_dir, showWarnings = FALSE, recursive = TRUE)

    ## Get required library paths from the binary
    otool_out <- system(paste("otool -L", shQuote(FINEMAP_path)),
                        intern = TRUE)

    ## 1. Build x86_64 libzstd from source
    if(!file.exists(file.path(libs_dir, "libzstd.1.dylib"))){
        messager("  Building x86_64 libzstd...", v = verbose)
        zstd_ok <- tryCatch({
            tmp <- tempfile()
            dir.create(tmp)
            ## Find zstd version from FINEMAP's dependency
            zstd_ver <- "1.5.5"  # Compatible with most versions
            zstd_url <- paste0(
                "https://github.com/facebook/zstd/releases/download/v",
                zstd_ver, "/zstd-", zstd_ver, ".tar.gz"
            )
            utils::download.file(zstd_url,
                                destfile = file.path(tmp, "zstd.tar.gz"),
                                quiet = !verbose)
            utils::untar(file.path(tmp, "zstd.tar.gz"), exdir = tmp)
            zstd_src <- list.files(tmp, pattern = "^zstd-",
                                  full.names = TRUE)[1]
            ret <- system(paste(
                "cd", shQuote(zstd_src),
                "&& arch -x86_64 make lib -j4",
                "CFLAGS='-arch x86_64' 2>&1"
            ), intern = TRUE)
            src_lib <- file.path(zstd_src, "lib", "libzstd.1.dylib")
            if(file.exists(src_lib)){
                file.copy(src_lib, file.path(libs_dir, "libzstd.1.dylib"),
                         overwrite = TRUE)
                TRUE
            } else FALSE
        }, error = function(e){
            messager("  Warning: Could not build libzstd:", e$message,
                     v = verbose)
            FALSE
        })
        if(!zstd_ok){
            messager("  Failed to build x86_64 libzstd.", v = verbose)
        }
    }

    ## 2. Build x86_64 libomp from source
    if(!file.exists(file.path(libs_dir, "libomp.dylib"))){
        cmake_path <- Sys.which("cmake")
        if(nchar(cmake_path) > 0){
            messager("  Building x86_64 libomp...", v = verbose)
            tryCatch({
                tmp <- tempfile()
                dir.create(tmp)
                omp_url <- paste0(
                    "https://github.com/llvm/llvm-project/releases/",
                    "download/llvmorg-12.0.1/openmp-12.0.1.src.tar.xz"
                )
                utils::download.file(omp_url,
                                    destfile = file.path(tmp, "omp.tar.xz"),
                                    quiet = !verbose)
                utils::untar(file.path(tmp, "omp.tar.xz"), exdir = tmp)
                omp_src <- file.path(tmp, "openmp-12.0.1.src")
                omp_build <- file.path(omp_src, "build")
                dir.create(omp_build)
                system(paste(
                    "cd", shQuote(omp_build),
                    "&&", shQuote(cmake_path),
                    "-DCMAKE_OSX_ARCHITECTURES=x86_64",
                    "-DCMAKE_BUILD_TYPE=Release .. 2>&1 &&",
                    "make -j4 2>&1"
                ), intern = TRUE)
                src_lib <- file.path(omp_build, "runtime", "src",
                                    "libomp.dylib")
                if(file.exists(src_lib)){
                    file.copy(src_lib, file.path(libs_dir, "libomp.dylib"),
                             overwrite = TRUE)
                }
            }, error = function(e){
                messager("  Warning: Could not build libomp:", e$message,
                         v = verbose)
            })
        } else {
            messager("  cmake not found, skipping libomp build.", v = verbose)
        }
    }

    ## 3. Get x86_64 gcc runtime libs from Homebrew bottle
    gcc_libs_needed <- c("libgcc_s.1.dylib", "libgcc_s.1.1.dylib",
                         "libgfortran.5.dylib", "libquadmath.0.dylib",
                         "libstdc++.6.dylib")
    missing_gcc <- gcc_libs_needed[
        !file.exists(file.path(libs_dir, gcc_libs_needed))
    ]
    if(length(missing_gcc) > 0){
        messager("  Downloading x86_64 gcc runtime libraries...", v = verbose)
        tryCatch({
            ## Fetch gcc@10 bottle for Intel Mac
            bottle_out <- system(
                "brew fetch --bottle-tag=big_sur gcc@10 2>&1",
                intern = TRUE
            )
            ## Find the cached bottle
            bottle_path <- list.files(
                file.path(Sys.getenv("HOME"),
                         "Library/Caches/Homebrew"),
                pattern = "gcc@10--",
                full.names = TRUE
            )
            if(length(bottle_path) > 0){
                bottle_path <- bottle_path[1]
                tmp <- tempfile()
                dir.create(tmp)
                utils::untar(bottle_path, exdir = tmp)
                gcc_lib_dir <- list.files(tmp, pattern = "^gcc",
                                         full.names = TRUE,
                                         recursive = FALSE)
                if(length(gcc_lib_dir) > 0){
                    gcc_dylibs <- list.files(
                        gcc_lib_dir, pattern = "\\.dylib$",
                        recursive = TRUE, full.names = TRUE
                    )
                    for(lib_name in gcc_libs_needed){
                        src <- grep(paste0("/", lib_name, "$"),
                                   gcc_dylibs, value = TRUE)
                        if(length(src) > 0){
                            file.copy(src[1],
                                     file.path(libs_dir, lib_name),
                                     overwrite = TRUE)
                        }
                    }
                }
            }
        }, error = function(e){
            messager("  Warning: Could not download gcc libs:", e$message,
                     v = verbose)
        })
    }

    ## 4. Fix permissions on all libs
    all_libs <- list.files(libs_dir, pattern = "\\.dylib$",
                          full.names = TRUE)
    for(lib in all_libs){
        Sys.chmod(lib, "0755")
    }

    ## 5. Fix internal Homebrew placeholder paths in gcc libs
    for(lib in all_libs){
        ## Fix install name ID
        system(paste("install_name_tool -id",
                     shQuote(lib), shQuote(lib)),
               ignore.stderr = TRUE)
        ## Fix references to other libs
        deps <- system(paste("otool -L", shQuote(lib)),
                      intern = TRUE)
        brew_deps <- grep("@@HOMEBREW", deps, value = TRUE)
        for(dep_line in brew_deps){
            dep_path <- trimws(strsplit(dep_line, "\\(")[[1]][1])
            dep_name <- basename(dep_path)
            local_dep <- file.path(libs_dir, dep_name)
            if(file.exists(local_dep)){
                system(paste("install_name_tool -change",
                            shQuote(dep_path),
                            shQuote(local_dep),
                            shQuote(lib)),
                      ignore.stderr = TRUE)
            }
        }
    }

    ## 6. Rewrite FINEMAP binary to point to local libs
    messager("  Rewriting FINEMAP binary library paths...", v = verbose)
    Sys.chmod(FINEMAP_path, "0755")
    finemap_deps <- system(paste("otool -L", shQuote(FINEMAP_path)),
                          intern = TRUE)

    rewrite_map <- list(
        "/usr/local/lib/libzstd.1.dylib" =
            file.path(libs_dir, "libzstd.1.dylib"),
        "/usr/local/opt/gcc/lib/gcc/10/libgfortran.5.dylib" =
            file.path(libs_dir, "libgfortran.5.dylib"),
        "/usr/local/opt/gcc/lib/gcc/10/libquadmath.0.dylib" =
            file.path(libs_dir, "libquadmath.0.dylib"),
        "/usr/local/opt/gcc/lib/gcc/10/libstdc++.6.dylib" =
            file.path(libs_dir, "libstdc++.6.dylib"),
        "/usr/local/opt/libomp/lib/libomp.dylib" =
            file.path(libs_dir, "libomp.dylib"),
        "/usr/local/lib/gcc/10/libgcc_s.1.dylib" =
            file.path(libs_dir, "libgcc_s.1.dylib")
    )

    for(old_path in names(rewrite_map)){
        new_path <- rewrite_map[[old_path]]
        if(any(grepl(old_path, finemap_deps, fixed = TRUE)) &&
           file.exists(new_path)){
            system(paste("install_name_tool -change",
                        shQuote(old_path),
                        shQuote(new_path),
                        shQuote(FINEMAP_path)),
                  ignore.stderr = TRUE)
        }
    }

    ## 7. Mark setup as complete
    writeLines("setup complete", file.path(libs_dir, ".setup_complete"))
    messager("  FINEMAP dylib setup complete.", v = verbose)
    return(invisible(FINEMAP_path))
}
