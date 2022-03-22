
#' @name 01_library_installer
#' @title library installer
#'
#' @description Installs multiple libraries as once
#'
#' @param librarys A vector of strings of libraries that are required to install.
#'

library_installer <- function(librarys) {
  for (i in 1:length(librarys)) {
    if (librarys[i] %in% rownames(installed.packages()) == FALSE) {
      install.packages(librarys[i], dependencies = TRUE)
    }
  }
}
