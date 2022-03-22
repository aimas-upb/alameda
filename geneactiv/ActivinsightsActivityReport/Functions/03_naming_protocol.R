
#' @name naming_protocol
#' @title Naming Protocol from a header file
#' @description Used to give outputs a file name
#'
#' @param binfile File being processed
#' @param prefix Text used a prefix
#' @param suffix Text used a suffix
#'
#' @details Sleep prefix is the default

naming_protocol <- function(binfile,
                            prefix = "",
                            suffix = "_All_Data.rds") {
  name <- paste0(
    prefix,
    strsplit(
      unlist(strsplit(binfile, "/"))[length(unlist(strsplit(binfile, "/")))],
      ".bin"
    ),
    suffix
  )
  return(name)
}
