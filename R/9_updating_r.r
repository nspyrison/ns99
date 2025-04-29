if(F){
  dev_msg("When in doubt, try to upgrade your software packages. --- Yihui Xie, 2017")
  browseURL("https://yihui.name/en/2017/05/when-in-doubt-upgrade/")
  update.packages(ask = FALSE, checkBuilt = TRUE)
}

#' Update R to the Latest Version
#'
#' This function checks for updates to R and installs the latest version if available.
#' It uses the `installr` package to perform the update.
#'
#' @return Updates R to the latest version and prints a message.
#' @examples
#' ns1_update_r()
ns1_update_r <- function(){
  if(!require("installr")) install.packages("installr")
  if(!installr::check.for.updates.R()){
    dev_msg("No new version of R found.")
  } else {
    quo <- quote({
      dev_msg("Installing new version of R.")
      if(require("tictoc")) tictoc::tic("installr::updateR()")
      installr::updateR(GUI = TRUE, fast = TRUE, install_R = TRUE,
                        browse_news = TRUE, quit_R = TRUE, silent = FALSE,
                        update_packages = TRUE, copy_packages = TRUE,
                        keep_old_packages = FALSE, keep_install_file = FALSE)
      if(require("tictoc")) tictoc::toc()
      dev_msg("New version of R installed. Restart session to take effect.")
    })
    
    if(installr::is.Rgui() == TRUE){
      eval(quo)
    } else {
      clipr::write_clip(deparse(quo))
      dev_msg("installr::updateR() is better ran in the R Gui. ",
              "The relavent code has been copied to your clipboard. ",
              "Please close this session, open R Gui, ",
              "paste and run the code in the console.")
    }
  }
}



#' Install Utility Packages
#'
#' This function installs a set of utility packages using the `pak` package.
#'
#' @return Installs the specified utility packages and prints a message.
#' @examples
#' ns2_install_util_pkgs()
ns2_install_util_pkgs <- function(){
  if(!require("pak"))
    install.packages("pak")
  .util_pkgs <- c(
    "rlang", "later", "scales", "lattice", "curl", "haven", "stringi",   ## LOAD FIRST
    "installr", "pkgbuild", "BiocInstaller", "remotes", "devtools",      ## Coding quality of life
    "clipr", "fcuk", "fortunes", "statquotes", "tictoc", "beepr", "pak", ## Coding quality of life 2
    "webshot", "bindr", "feather", "processx", "remotes")                ## Other
  pak::pak(.util_pkgs)
  dev_msg("R Utilities updated!")
}


#' Update RStudio to the Latest Version
#'
#' This function updates RStudio to the latest version using the `installr` package.
#'
#' @return Updates RStudio to the latest version and prints a message.
#' @examples
#' ns3_update_rstudio()
ns3_update_rstudio <- function(){
  require("installr")
  installr::install.RStudio()
  dev_msg("RStudio updated!")
}

#' Install/update R tools for Windows to compile binary packages
#'
#' This function installs or updates Rtools on Windows OS for compiling packages from binary files.
#'
#' @return Installs or updates Rtools and prints a message.
#' @examples
#' ns4_install_rtools()
ns4_install_rtools <- function(){
  if(installr::is.windows() == FALSE){
    dev_msg("Not Windows OS, skipping installation/update of Rtools.")
  } else { ## Continue if Windows OS:
    q <- "OS is windows. Attempt to install/update Rtools for compiling packages from binary files?"
    if(installr::ask.user.yn.question(q)){
      tryCatch(
        installr::install.Rtools(),
        error = function(c)
          dev_warn(
            "!! installr::install.Rtools() errored, ",
            "Please download manually from: ",
            "https://cran.r-project.org/bin/windows/Rtools/ for more info.") ## for WIN OS (only?)
      )
    }
    dev_msg("rtools installed")
  }
}


#' Update Existing CRAN Packages
#'
#' This function updates all existing CRAN packages using the `pak` package.
#'
#' @return Updates the CRAN packages and prints a message.
#' @examples
#' ns5_update_existing_cran_pkgs()
ns5_update_existing_cran_pkgs <- function(){
  if(F)
    browseURL("https://community.rstudio.com/t/issues-with-packages-after-updating-to-r-3-5-0/7639/19")
  if(!require("pak"))
    install.packages("pak")
  ns_tic("Crude update CRAN package")
  pkgs <- installed.packages()
  pkgs <- pkgs[is.na(pkgs[, "Priority"]), 1]
  sapply(pkgs, pak::pak)
  ns_toc()
  ns_beep()
}


#' Install Extended Technology Packages
#'
#' This function installs various extended technology packages and tools for R.
#'
#' @return Installs the specified packages and tools and prints a message.
#' @examples
#' ns6_install_extended_tech()
ns6_install_extended_tech <- function(){
  ns_tic("ns")
  
  if(installr::ask.user.yn.question("Do want to install tinytex for pdf LaTeX?"))
    tinytex::install_tinytex()
  
  if(installr::ask.user.yn.question("Do want toinstall Hugo themes for blogdown?")){
    blogdown::install_hugo() ## Hugo themes for blogdown
    dev_msg("after install: hugo version is: ", blogdown::hugo_version())
  }
  
  # if(installr::ask.user.yn.question("Do want to try to install phantom_js for making gifs?")){
  #   if(!require("webshot"))
  #     pak::pak("webshot")
  #   webshot::install_phantomjs() ## ??For saving .gif's?
  # }
  
  if(installr::ask.user.yn.question("Do want to install ImageMagick for .gif creation and saving?")){
    dev_warn("!!@install.ImageMagick MAKE SURE TO TICK THE CHECKBOX: 'legacy utilites (eg convert)'")
    installr::install.ImageMagick() ## WIN OS: for .gif creation
  }
  
  if(installr::is.windows()){
    if(installr::ask.user.yn.question("Do want to install git for version control and remote storage?"))
      installr::install.git()
    
    if(installr::ask.user.yn.question("Do want to install the GitHub Desktop application?"))
      installr::install.github()
    
    if(installr::ask.user.yn.question("Do want to install the GitKraken (x64) application, a GUI for Git?"))
      installr::install.URL("https://release.gitkraken.com/win64/GitKrakenSetup.exe")
    
    if(installr::ask.user.yn.question("Do want to install the Notepad++ application?"))
      installr::install.notepadpp()
  }
  
  ns_toc()
  dev_msg("Finished installing extended tech!")
}



#' Update or Install Specified Packages
#'
#' This function installs or updates specified CRAN and GitHub packages using the `pak` and `remotes` packages.
#'
#' @param dependencies Logical, whether to install package dependencies. Default is TRUE.
#' @param ... Additional arguments passed to `pak::pak` and `remotes::install_github`.
#' @return Installs or updates the specified packages and prints a message.
#' @examples
#' ns_install_these_pkgs()
ns_install_these_pkgs <- function(dependencies = TRUE, ...){
  if(!require("pak"))
    install.packages("pak")
  ns_tic("Install/update CRAN packages:")
  lib_ls <- list(
    .util_pkgs <- c(
      "rlang", "later", "scales", "lattice", "curl", "haven", "stringi", ## LOAD FIRST
      "installr", "pkgbuild", "BiocInstaller", "remotes", "devtools",    ## Coding quality of life
      "clipr", "fcuk", "fortunes", "statquotes", "tictoc", "beepr",      ## Coding quality of life 2
      "webshot", "bindr", "feather", "processx"                          ## Other
    ),
    ### tier1 ------
    tier1 <- c("devtools", "roxygen2", "usethis", "testthat", #"assertthat",  ## Dev tools
               "tidyverse", "tidylog", #"tsibble",                            ## Tidyverse includes: "ggplot2", dplyr", "magrittr", "purrr", "tibble", AND many more dependancies
               "glue", "lubridate", "broom", "forcats", "lazyeval", "reprex", ## Should be installed with tidyverse, but just to be safe.
               "plotly", "patchwork", "cowplot", "kableExtra",                ## Ggplot2 aes & arrangement
               "ggthemes", "ggrepel", "ggfortify", "ggpmisc", "ggExtra",      ## GG Allies
               "GGally", "visdat", "corrplot",                                ## Ggplot2 extenders
               "RColorBrewer", "viridis", "dichromat", "colorspace",          ## RColorBrewer and friends
               "knitr", "tinytex", "rticles", "bookdown", "here",             ## R mark down (rmd) and LaTeX
               "blogdown", "xaringan",                                        ## Extending rmd to websites and html slides
               "shiny", "flexdashboard", "DT",                                ## Shiny and friends
               "shinythemes", #"shinytest", "shinyjs", "shinyBS",             ## Shiny and friends
               "MASS", "tourr", "spinifex", "geozoo", "mvtnorm", "Rtsne",     ## For tabular multivariate data
               "rgl", "rgdal", "magick", "plot3D",                            ## For 3D vis with WebGL backend.
               "Rdimtools", "DALEX", "randomForest", "cheem",                 ## Tours and multivariate data
               "ggmap", "maps", "leaflet",                                    ## For world maps
               "ggraph", "igraph", "tidygraph", "geomnet",                    ## Network data
               "microbenchmark", "rbenchmark", "lintr", "styler", "git2r",    ## Quality of life
               "readr", "dbplyr", "readxl", "Rcpp"                            ## Extended access
               ## Benchmarking:  browseURL("https://www.alexejgossmann.com/benchmarking_r/"
               #### Formalness and scaling: tictoc < rbenchmark < microbenchmark
               #### Remember to profile your code to find the culprit with {lineprof} first, downloaded below.
    )
  )
  lapply(lib_ls, pak::pak, dependencies = dependencies, ...)
  if(!require("remotes"))
    pak::pak("remotes")
  gh_pkgs <- c("hadley/lineprof")
  sapply(gh_pkgs, remotes::install_github, dependencies = dependencies, ...)
  ns_toc()
  ns_beep(2)
}

