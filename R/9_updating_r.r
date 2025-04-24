if(F){
  message("When in doubt, try to upgrade your software packages. --- Yihui Xie, 2017")
  browseURL("https://yihui.name/en/2017/05/when-in-doubt-upgrade/")
  update.packages(ask = FALSE, checkBuilt = TRUE)
}

nsUpR1 <- nsUpdateR <- function(){
  if(!require("installr")) install.packages("installr")
  if(!installr::check.for.updates.R()){
    message("No new version of R found.")
  } else {
    quo <- quote({
      message("Installing new version of R.")
      if(require("tictoc")) tictoc::tic("installr::updateR()")
      installr::updateR(GUI = TRUE, fast = TRUE, install_R = TRUE,
                        browse_news = TRUE, quit_R = TRUE, silent = FALSE,
                        update_packages = TRUE, copy_packages = TRUE,
                        keep_old_packages = FALSE, keep_install_file = FALSE)
      if(require("tictoc")) tictoc::toc()
      if(require("beepr")) beepr::beep()
      message("New version of R installed. Restart session to take effect.")
    })
    
    if(installr::is.Rgui() == TRUE){
      eval(quo)
    } else {
      warning("installr::updateR() is better ran in the R Gui")
      clipr::write_clip(deparse(quo))
      message("The relavent code has been copied to your clipboard.
              Please close this session, open R Gui,
              paste and run the code in the console.")
    }
  }
}


nsUpR2 <- nsInstallUtilPkgs <- function(){
  if(require("installr") & require("pkgbuild") & require("pkgbuild") == F)
    install.packages(c("installr", "pkgbuild", "pak"))
  .util_pkgs <- c(
    "rlang", "later", "scales", "lattice", "curl", "haven", "stringi",  ## LOAD FIRST
    "installr", "pkgbuild", "BiocInstaller", "remotes", "devtools",     ## Coding quality of life
    "clipr", "fcuk", "fortunes", "statquotes", "tictoc", "beepr", "pak" ## Coding quality of life 2
    "webshot", "bindr", "feather", "processx"                           ## Other
  )
  pak::pak(.util_pkgs)
  message("R Utilities updated!")
}

nsUpR3 <- nsInstallRStudio <- function(){
  require("installr"); require("tictoc"); require("beepr")
  tictoc::tic("installr::install.RStudio()")
  installr::install.RStudio()
  tictoc::toc()
  beepr::beep()
  message("RStudio updated!")
}

nsUpR4 <- nsInstallRtools <- function(){
  require("installr"); require("tictoc"); require("beepr"); require("pkgbuild");
  if(installr::is.windows() == FALSE){
    message("Not Windows OS, skipping installation/update of Rtools.")
  } else { ## Continue if Windows OS:
    tictoc::tic("installr::install.rtools()")
    q <- "Attempt to install/update Rtools for compiling packages from binary files?"
    if(installr::ask.user.yn.question(q)){
      tryCatch(
        installr::install.Rtools(),
        error = function(c)
          message(
            "!! If installr::install.Rtools() doesn't work,",
            "Please download manually from: ",
            "https://cran.r-project.org/bin/windows/Rtools/ for more info.") ## for WIN OS (only?)
      )
    }
    tictoc::tic()
    beepr::beep()
    message("nsInstallRtools ended.")
  }
}

nsInstallTinytex <- function(){
  require("installr", "tictoc", "beepr")
  tictoc::tic("tinytex::install_tinytex()")
  q <- "Attempt to install/update TinyTeX (light LaTeX compiler) for Rmarkdown?"
  if(installr::ask.user.yn.question(q)){
    require("tinytex");
    tinytex::install_tinytex()
  }
}


### Windows OS, Special Considerations: ----
nsInstallCranPkg <- function(pkgs, ...){
  .nm <- substitute(pkgs)
  if(require("tictoc"))
    tictoc::tic(.nm)
  install.packages(pkgs, dependencies = TRUE, force = FALSE, ...)
  if(require("tictoc"))
    tictoc::toc()
  message(paste0("Finished installing/updating ", .nm, " CRAN package!"))
}

if(F){
  if(require("tictoc"))
    tictoc::tic("Windows special considerations")
  if(installr::ask.user.yn.question("Do want to try to install Hugo themes for blogdown?")){
    require("blogdown")
    blogdown::install_hugo() ## Hugo themes for blogdown
    message(paste0("after install: hugo version is: ", blogdown::hugo_version()))
  }
  webshot::install_phantomjs() ## ??For saving .gif's?
  if(installr::is.windows()){
    if(installr::ask.user.yn.question("Do want to try to install ImageMagick for .gif creation and saving?")){
      warning("!!!@install.ImageMagick MAKE SURE TO TICK THE CHECKBOX: 'legacy utilites (eg convert)'")
      installr::install.ImageMagick() ## WIN OS: for .gif creation
    }
    if(installr::ask.user.yn.question("Do want to try to install git for version control and remote storage?")){
      installr::install.git() ## WIN OS
    }
    if(installr::ask.user.yn.question("Do want to try to install the local GitHub Desktop application?")){
      installr::install.github() ## WIN OS
    }
    if(installr::ask.user.yn.question("Do want to try to install the GitKraken (x64) application, a GUI for Git?")){
      install.URL("https://release.gitkraken.com/win64/GitKrakenSetup.exe")
    }
    if(installr::ask.user.yn.question("Do want to try to install the Notepad++ application?")){
      installr::install.notepadpp() ## WIN OS
    }
  }
  nsInstallCranPkg("magick")
  if(require("tictoc"))
    tictoc::toc()
  message("Finished installing the special cases!")
}



### Setting up R for Jypter Labs:
if(F){
  JypterLabs <- c("repr", "IRdisplay", "evaluate", "crayon",
                  "pbdZMQ","devtools", "uuid", "digest")
  install.packages(JypterLabs, dependencies = c("Depends", "Imports"))
  
  # NOW RESTART R SESSION. (Cntl + Shift + F10)
  devtools::install_github("IRkernel/IRkernel")
  IRkernel::installspec()
}


# Updating packages ----
### Update all existing packages
if(F){
  nsUpdateAllExistingCranPkgs <- function(){
    if(F)
      browseURL("https://community.rstudio.com/t/issues-with-packages-after-updating-to-r-3-5-0/7639/19")
    if(!require("pak"))
      install.packages("pak")
    if(require("tictoc"))
      tictoc::tic("Crude update CRAN package")
    pkgs <- installed.packages()
    .is_na_priority <- is.na(pkgs[, "Priority"])
    pkgs <- pkgs[.is_na_priority, 1L]
    sapply(pkgs, pak::pak)
    if(require("tictoc"))
      tictoc::toc()
    if(require("beepr"))
      beeper::beep(2L)
  }
  nsUpdateAllExistingCranPkgs()
}

### Update/install specified packages
if(F){
  nsInstallTheseCranPkgs <- function(dependencies = TRUE, ...){
    dev_msg()
    if(!require("pak"))
      install.packages("pak")
    if(require("tictoc"))
      tictoc::tic("Install/update CRAN packages:")
    lib_ls <- list(
      .util_pkgs <- c(
        "rlang", "later", "scales", "lattice", "curl", "haven", "stringi", ## LOAD FIRST
        "installr", "pkgbuild", "BiocInstaller", "remotes", "devtools",    ## Coding quality of life
        "clipr","fcuk", "fortunes", "statquotes", "tictoc", "beepr",       ## Coding quality of life 2
        "webshot", "bindr", "feather", "processx"                          ## Other
      ),
      ### tier1 ------
      tier1 <- c("devtools", "roxygen2", "usethis", "testthat", #"assertthat",    ## Dev tools
                 "tidyverse", #"tsibble", "tidylog",                              ## Tidyverse includes: "ggplot2", dplyr", "magrittr", "purrr", "tibble", AND many more dependancies
                 "glue", "lubridate", "broom", "forcats", "lazyeval", "reprex",  ## Should be installed with tidyverse, but just to be safe.
                 "plotly", "patchwork", "cowplot", "ggthemes", "ggrepel",        ## Ggplot2 aes & arrangement
                 "GGally", "visdat", "corrplot", "ggfortify",                    ## Ggplot2 extenders
                 "ggExtra", "kableExtra", "ggrepel", "ggpmisc",
                 "RColorBrewer", "viridis", "dichromat", "colorspace",           ## RColorBrewer and friends
                 "knitr", "tinytex", "rticles", "kableExtra", "bookdown",        ## R mark down (rmd) and LaTeX
                 #"blogdown", "xaringan",                                         ## Extending rmd to websites and html slides
                 "shiny", "flexdashboard", "DT",                                 ## Shiny and friends
                 "shinythemes", #"shinytest", "shinyjs", "shinyBS",               ## Shiny and friends
                 "MASS", "tourr", "spinifex", "geozoo", "mvtnorm", "Rtsne",      ## For tabular multivariate data
                 #"rgl", "rgdal", "magick", "plot3D",                    ## For 3D vis with WebGL backend.
                 "Rdimtools", "DALEX", "randomForest", "cheem",                  ## Tours and multivariavte data
                 "ggmap", "maps", "leaflet",                                     ## For world maps
                 #"ggraph", "igraph", "tidygraph", "geomnet",                     ## Network data
                 "microbenchmark", "rbenchmark", "lintr", "styler", "git2r", "here",## Quality of life
                 "readr", "dbplyr", "readxl", "Rcpp"
                 ## Benchmarking:  browseURL("https://www.alexejgossmann.com/benchmarking_r/"
                 #### Formalness and scaling: tictoc < rbenchmark < microbenchmark
                 #### Remember to profile your code to find the culprit with {lineprof} first, downloaded below.
      )
    )
    lapply(lib_ls, pak::pak, dependencies = dependencies, ...)
    if(require("tictoc"))
      tictoc::toc()
    if(require("beepr"))
      beepr::beep(2)
  }
  nsInstallTheseCranPkgs()
}

### Packages from github and bioconductor: -----
if(F){
  req <- c("remotes", "BiocManager")
  lapply(req, require, character.only = TRUE)
  if(require("tictoc"))
    tictoc::tic("Install/update github/bioconductor packages:")
  gh_pkgs <- c("hadley/lineprof" ## Profiling!
               # "nspyrison/spinifex",
               # "ggobi/tourr",
               # "sayani07/gravitas",
               # "sa-lee/liminal",
               # "thomasp85/gganimate",
               # "hadley/ggplot2" ## Incase plotly whines so much without it.
  )
  remotes::install_github(gh_pkgs)
  #BiocManager::install("plyranges")
  if(require("tictoc"))
    tictoc::toc()
}

### NEEDS VALIDATION/DEV ----
# ## TODO: Validate!!!
# ## gold bar: pipeable arround any call, making it safe, quiet, beep, and tictoc based on outcome.
# nsCall <- function(call, beeps = T, tictocs = T){
#   if(class(call) != "call" & require("purrr"))
#     call <- substitute(call)
#   purrr::possibly(call)
#   if(class(call) != "call")
#     stop("call should be of class 'call', try wrapping 'call' in quote() or substitue()?")
#   if(beeps == T)   require("beepr")
#   if(tictocs == T) require("tictoc")
#   call_nm <- paste0(deparse(call), collapse = "")
#   len <- nchar(call_nm)
#   if(len > 25){
#     call_nm <- paste0(substr(call_nm, 1, 10), "...", substr(call_nm, len - 10, len))
#   }
#   if(tictocs == T) tictoc::tic(call_nm)
#   ret <- eval(call)
#   if(tictocs == T) tictoc::toc()
#   if(beeps == T)   beepr::beep(4)
#   return(ret)
# }

# ## TODO: Validate on shiny.
# ## Attempt to call a shiny reactive, if it causes errors or warnings show notifications instead.
# nsShinyQuietly <- function(.f, ...){
#   qs_fun <- .f %>% purrr::quietly() %>% purrr::safely()
#   function(...){
#     ret <- qs_fun(...)
#
#     if(!is.null(ret$error)){ ## safely output
#       showNotification(ret$error$message, duration = 10, type="error")
#       return(ret$result)
#     }
#     ret <- ret$result ## quietly output
#     if(!is.null(ret$warnings) && length(ret$warnings) > 0){
#       lapply(unique(ret$warnings), function(w){
#         showNotification(w, duration = 10, type = "warning")
#       })
#     }
#     return(ret$result)
#   }
# }