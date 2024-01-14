hasValue <- function(x) {
  if (missing(x)) return(FALSE)
  return(length(x) > 0)
}

removeFileNameEnding <- function(x) {
  return(gsub("\\.[^.]*$", "", x))
}


longestCommonPrefix <- function(x) {
  if (length(x) == 0) return(character(0))
  x <- sort(x)
  n <- min(nchar(x))
  charsFirst <- strsplit(first(x), "")[[1]][1:n]
  charsLast <- strsplit(last(x), "")[[1]][1:n]
  fristNonMatch <- which(charsFirst != charsLast)[1]
  if (is.na(fristNonMatch)) {
    return(paste0(charsFirst, collapse=""))
  } else if (fristNonMatch == 1) {
    return(character(0))
  } else {
    return(paste0(charsFirst[1:(fristNonMatch-1)], collapse=""))
  }
}

longestCommonSuffix <- function(x) {
  if (length(x) == 0) return(character(0))
  stringi::stri_reverse(x) |> longestCommonPrefix() |> stringi::stri_reverse()
}

uniqueMiddle <- function(x) {
  if (length(x) == 0) return(character(0))
  prefix <- longestCommonPrefix(x)
  suffix <- longestCommonSuffix(x)
  return(gsub(paste0("^", prefix, "|", suffix, "$"), "", x))
}


printPackagesInfo <- function() {
  sessioninfo::session_info() |> print()
}
