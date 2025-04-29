## General helpers -----
get_function_name <- function() {
  if (sys.nframe() > 1) {
    ret <- deparse(sys.call(-4)[[1]])
    if(ret != "get_function_name")
      return(paste0(ret, ": "))
  }
  return("")
}
get_function_name2 <- function() {
  if (sys.nframe() > 1) {
    ret <- deparse(sys.call(-sys.nframe())[[1]])
    if(ret != "get_function_name2")
      return(paste0(ret, ": "))
  }
  return("")
}


dev_msg <- function(...) {
  if (length(list(...)) > 0)
    message(paste0(get_function_name(), ...))
}
dev_warn <- function(...) {
  if (length(list(...)) > 0)
    warning(paste0(get_function_name(), ...))
}
dev_stop <- function(...) {
  if (length(list(...)) > 0)
    stop(paste0(get_function_name(), ...))
}
dev_time <- function(){
  message("Started ", substr(Sys.time(), 12, 19))
}
ns_tic <- function(...){
  tictoc::tic(paste0(get_function_name(), ...))
}
ns_toc <- function(...){
  tictoc::toc()
}
ns_beep <- function(sound = 1, ...){
  dev_msg(...)
  beepr::beep(sound)
}

#' @examples
#' my_function <- function() {
#'   dev_msg("a msg");return("")
#' }
#' my_function()
#' 
#' my_warn <- function() {
#'   dev_warn("a warning");return("")
#' }
#' my_warn()
#' 
#' my_stop <- function() {
#'   dev_stop("an error");return("")
#' }
#' my_stop()



#' Open .Rprofile File
#'
#' This function opens the .Rprofile file for editing.
#' Be cautious as this file is run on start-up.
#'
#' @return Opens the .Rprofile file in the default editor.
#' @examples
#' ns_open_rprofile()
ns_open_rprofile <- function(){
  dev_msg("Be cautious as this file is run on start-up.")
  file.edit(file.path("~", ".Rprofile"))
}


#' Open .Renviron File
#'
#' This function opens the .Renviron file for editing.
#' Useful for setting API keys and environmental variables.
#'
#' @return Opens the .Renviron file in the default editor.
#' @examples
#' ns_open_renviron()
ns_open_renviron <- function(){
  dev_msg("Store API keys and envirnmental variables here.")
  file.edit(file.path("~", ".Renviron"))
}



#' Enhanced Head Function
#'
#' This function prints the first few rows of an object with extended print options.
#' Temporarily extends the print max and width for better visibility.
#'
#' @param obj The object to print.
#' @param n The number of rows to print. Default is 6.
#' @param width The width of the print output. Default is Inf.
#' @param as_tibble Logical, whether to convert the object to a tibble. Default is TRUE.
#' @return Prints the object with specified options.
#' @examples
#' mt_wide <- dplyr::bind_cols(mtcars, mtcars)
#' ns_head(mt_wide, n = 3)
ns_head <- function(obj, n = 6, width = Inf, as_tibble = TRUE){
  if(as_tibble) obj <- tibble::as_tibble(obj)
  if(class(obj)[1] == "tbl_df"){
    print(obj, width = width, n = n)
  } else {
    prev_max_print <- getOption("max.print")
    options(max.print = n)
    print(obj)
    options(max.print = prev_max_print)
  }
}


#' Load Specified Packages
#'
#' This function loads specified packages safely without changing default options.
#' Note: Do NOT change the defaultPackages option or call library() in your .Rprofile.
#' Instead, use this function to load libraries; it's much safer.
#'
#' @param libs A character vector of package names to load. Default includes common packages.
#' @return Loads the specified packages and prints a message.
#' @examples
#' ns_libs(c("ggplot2", "dplyr"))
#' ns_libs()
ns_libs <- function(libs = c("tictoc", "beepr", "fcuk", 
                              "ggplot2", "dplyr", "tidyr")) {
  suppressPackageStartupMessages({
    lapply(libs, require, character.only = TRUE)
  })
  dev_msg(length(libs)," packages loaded! (",
          paste(libs, collapse = ", "), ")")
}



##### Package dev helpers -----

#' Check Package on R-hub
#'
#' This function checks the package on specified R-hub platforms.
#'
#' @param platforms_to_check A character vector of platforms to check. Default includes major platforms.
#' @param ... Additional arguments passed to rhub::rhub_check.
#' @return Runs the package check on R-hub.
#' @examples
#' ns_rhub_check(c("linux", "windows"))
ns_rhub_check <- function(
    platforms_to_check = c("linux", "macos-arm64", 
                           "windows", "ubuntu-release"), ...
) {
  dev_time()
  rhub::rhub_check(platform = platforms_to_check, ...)
}
ns_rhub_check()


#' Rebuild and Check Package on R-hub
#'
#' This function cleans, documents, installs, and checks the package on specified R-hub platforms.
#'
#' @param platforms_to_check A character vector of platforms to check. Default includes major platforms.
#' @param ... Additional arguments passed to rhub::rhub_check.
#' @return Runs the rebuild and check process on R-hub.
#' @examples
#' ns_rebuild_rhub_check(c("linux", "windows"))
ns_rebuild_rhub_check <- function(
    platforms_to_check = c("linux", "macos-arm64", 
                           "windows", "ubuntu-release"), ...){
  dev_msg("This may take some time; clean_dll(), clean_vignettes(), ",
          "document(), install(), and rhub_check()")
  devtools::clean_dll()
  devtools::clean_vignettes()
  devtools::document()
  devtools::install()
  ns_rhub_check(platforms_to_check)
}


##### Bookdown helper functions -----

#' Convert HTML Slides to PDF Slides
#'
#' This function converts HTML slides to PDF slides using the webshot package.
#'
#' @param dir A character string representing the directory containing HTML files or the HTML file directly. Default is the current directory.
#' @return Converts the HTML file to a PDF file and prints a message.
#' @examples
#' ns_html_to_pdf("path/to/html/file.html")
ns_html_to_pdf <- function(dir = "."){ ## Folder containing or .html file directly
  ext <- tolower(substr(dir, length(dir) - 4, length(dir)))
  if(ext != ".html") dir <- dir(dir)[endsWith(dir(dir), ".html")]
  stopifnot(tolower(substr(dir, length(dir) - 4, length(dir))) == "html")
  pdf_file <- paste0(substr(dir, 1, length(dir) - 4), "pdf")
  webshot::webshot(dir, pdf_file)
  message(paste0("html file converted to'", pdf,"'."))
}


#' Count Lines of Code in Repository
#'
#' This function counts the lines of code in .R and .Rmd files in the specified repository.
#'
#' @param repo_path A character string representing the path to the repository. Default is the current directory.
#' @return A named vector with the number of files and the total number of lines of code.
#' @examples
#' ns_count_repo_lines("path/to/repo")
ns_count_repo_lines <- function(repo_path = "."){
  ## Get list of file names
  files <- list.files(path = repo_path, recursive = TRUE, full.names = TRUE)
  tgt_files <- append(
    stringr::str_subset(files, ".[Rr]$"), ## .r/.R
    stringr::str_subset(files, "\\.[Rr][Mm][Dd]$$") ## .rmd/.Rmd
  )
  ## Number of lines by file, inc. comments and blank lines.
  file_lines <- sapply(
    tgt_files, function(x) x %>% readLines() %>% length()
  )
  ## Summary message
  dev_msg(length(file_lines), " files contained ",
          sum(file_lines), " lines combined.")
  ## Return
  return(c(n_files = length(file_lines), n_lines = sum(file_lines)))
}

####### Debugging and trouble shooting -----

#' Set Error Option to Browser
#'
#' This function sets the error option to `browser`, which allows debugging in an interactive environment.
#'
#' @return Sets the error option to `browser`.
#' @examples
#' ns_err_opts_browser()
ns_err_opts_browser <- function(){options(error = browser)}


#' Set Error Option to Recover
#'
#' This function sets the error option to `recover`, which provides a menu of frames for debugging.
#'
#' @return Sets the error option to `recover`.
#' @examples
#' ns_err_opts_recover()
ns_err_opts_recover <- function(){options(error = recover)}


#' Set Error Option to Default
#'
#' This function sets the error option to `NULL`, which is the default behavior.
#'
#' @return Sets the error option to `NULL`.
#' @examples
#' ns_err_opts_default()
ns_err_opts_default <- function(){options(error = NULL)}


#' Set Error Option for Next Error
#'
#' This function sets the error option to `browser`, `recover`, or `NULL` for the next error only.
#'
#' @param func_on_err A function to call on error. Options are `browser`, `recover`, or `NULL`.
#' @return Sets the error option for the next error and then reverts to the previous option.
#' @examples
#' ns_on_err_once(browser)
ns_on_err_once <- function(func_on_err = c(browser, recover, NULL)){
  .errOpt <- function(){
    old <- getOption("error")
    function(){
      options(error = old)
      func_on_err[[1]]
    }
  }
  options(error = .errOpt)
}


#' Undebug All Functions in Attached Namespaces
#'
#' This function removes debugging from all functions in the attached namespaces.
#'
#' @param where A character vector of environments to search. Default is the search path.
#' @return Invisibly returns the list of functions that were undebugged.
#' @examples
#' library(dplyr)
#' debug(filter)
#' if(F) ## Goes to Browse[1] debug
#'   filter(mtcars, cyl > 4)
#' ns_undebug_namespaces()
#' ## Doesn't go to Browse[1] debug
#' filter(mtcars, cyl > 4)
ns_undebug_namespaces <- function(where = search()){
  isdebugged_safe <- function(x, ns=NULL){
    g <- if(is.null(ns)) get(x) else getFromNamespace(x,ns)
    is.function(g) && isdebugged(g)
  }
  which_debugged <- function(objnames, ns = NULL){
    if(!length(objnames)) return(character(0))
    objnames[sapply(objnames, isdebugged_safe, ns = ns)]
  }
  all_debugged <- function(where = search(), show_empty = FALSE){
    ## Name of attached packages
    ap <- setNames(lapply(where,function(x){
      which_debugged(ls(x, all.names = TRUE))
    }), gsub("package:","",where))
    ## Namespaces of attached packages
    ns <- unlist(sapply(gsub(
      "package:","",where),
      function(x){
        if(inherits({n <- try(getNamespace(x), silent = TRUE)},"try-error"))
          NULL else x
      }))
    ap_ns <- setNames(lapply(ns,function(x){
      objects <- ls(getNamespace(x), all.names = TRUE)
      which_debugged(objects, ns = x)
    }), ns)
    if(!show_empty){
      ap <- ap[sapply(ap,length) > 0]
      ap_ns <- ap_ns[sapply(ap_ns, length) > 0]
    }
    ## Drop overlaps
    for(i in names(ap))
      ap_ns[[i]] <- setdiff(ap_ns[[i]], ap[[i]])
    list(env = ap, ns = ap_ns)
  }
  aa <- all_debugged(where)
  lapply(aa$env, undebug)
  ## Debug namespaces
  invisible(mapply(function(ns, fun){
    undebug(getFromNamespace(fun, ns))
  }, names(aa$ns), aa$ns))
}

