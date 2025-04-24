## General helpers -----
get_function_name <- function() {
  if (sys.nframe() == 1) {
    return("")
  } else {
    return(deparse(sys.call(-4)[[1]]))
  }
}

dev_msg <- function(...) {
  message(paste0(get_function_name(), ": ", paste0(...)))
}
dev_time <- function(){
  message("started ", Sys.time() %>% substr(., 12, 19))
}
dev_warn <- function(...) {
  warning(paste0(get_function_name(), ": ", paste0(...)))
}
dev_stop <- function(...) {
  stop(paste0(get_function_name(), ": ", paste0(...)))
}

#' @examples
#' my_function <- function() {
#'   dev_msg("Hi");return("")
#' }
#' my_function()
#' 
#' my_warn <- function() {
#'   dev_warn("Hi");return("")
#' }
#' my_warn()
#' 
#' my_stop <- function() {
#'   dev_stop("Hi");return("")
#' }
#' my_stop()


ns_open_rprofile <- function() file.edit(file.path("~", ".Rprofile")) ## Ran on start-up, be careful, this is playing with fire.
ns_open_renviron <- function() file.edit(file.path("~", ".Renviron")) ## For API keys and envirnmental variables

### ns_head(), for when head and print, just doesn't cut it. Temp extend print max.
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


### Load specified packages.
## Note: do NOT change defaultPackages option or call library() in your .Rprofile!!
## Instead use a function to load libraries; much more safe.
ns_libs <- function(libs <- c("tictoc", "beepr", "fcuk", 
                              "ggplot2", "dplyr", "tidyr")) {
  suppressPackageStartupMessages({
    lapply(libs, require, character.only = TRUE)
  })
  message(paste0("ns_libs: ", length(libs)," packages loaded! (",
                 paste(libs, collapse = ", "), ")"))
}
ns_libs()

##### Package dev helpers -----
#also see 
ns_rhub_check <- function(
    platforms_to_check = c("linux", "macos-arm64", 
                           "windows", "ubuntu-release"), ...
) {
  ## see rhub::platforms() for possible platforms
  dev_time()
  #dev_msg("started ", Sys.time() %>% substr(., 12, 16))
  rhub::rhub_check(platform = platforms_to_check, ...)
}
ns_rhub_check()

## Erroring b/c not waiting?
## Run clean dll,& vignette, document(), install(), and rhub::check(), package dev check, on 4 most important platforms.
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

# ## Copy 'n Paste a (bib) file to reduce compilation contention.
# nsCnP_bib <- function(
#     from = dir()[endsWith(dir(), ".bib")],
#     to = paste0(normalizePath("../../../Downloads/"),
#                 dir()[endsWith(dir(), ".bib")]),
#     open = TRUE){
#   if(!file.exists(from) | length(from) == 0L)
#     stop(paste0("nsCnP_bib: File '", from, "' doesn't exist."))
#   if(file.exists(to)){file.remove(to)}
#   file.copy(from = from, to = to)
#   if(open) file.edit(to)
#   message(paste0("BibTex file copied to '", to, "'."))
# }

## Convert HTML slides to PDF slides
ns_html_to_pdf <- function(dir = "."){ ## Folder containing or .html file directly
  ext <- tolower(substr(dir, length(dir) - 4, length(dir)))
  if(ext != ".html") dir <- dir(dir)[endsWith(dir(dir), ".html")]
  stopifnot(tolower(substr(dir, length(dir) - 4, length(dir))) == "html")
  pdf_file <- paste0(substr(dir, 1, length(dir) - 4), "pdf")
  webshot::webshot(dir, pdf_file)
  message(paste0("html file converted to'", pdf,"'."))
}

## Counts the lines of code in .R and .Rmd files in the repo
ns_count_repo_lines <- function(repo_path = "."){
  ## Get list of filenames
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
ns_err_opts_browser <- function(){options(error = browser)}
ns_err_opts_recover <- function(){options(error = recover)}
ns_err_opts_default <- function(){options(error = NULL)}

## On next error go to browser(), default, or recover()
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

## DON'T FORGET; to use debugcall() when you only want to debug once.
## !!NOTE: This only searches attached namespaces
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

