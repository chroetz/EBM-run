#' @export
saveBoundingBoxes <- function(boundingBoxes, outFilePath, regionColumnName = "GID_1") {
  tbl <- as_tibble(t(boundingBoxes), rownames = regionColumnName)
  readr::write_csv(tbl, outFilePath)
}

#' @export
getBoundingBoxesFromMask <- function(path) {
  nc <- open.nc(path)
  on.exit(close.nc(nc))
  fileInfo <- file.inq.nc(nc)
  stopifnot(fileInfo$ndims == 2)
  dimNames <- c(dim.inq.nc(nc, 0)$name, dim.inq.nc(nc, 1)$name)
  boundingBoxes <- sapply(
    seq_len(fileInfo$nvars-2),
    \(i) {
      mask <- var.get.nc(nc, 1 + i)
      getBoundingBox(mask)
    })
  rownames(boundingBoxes) <- c(
    paste0("min_", dimNames[1]),
    paste0("max_", dimNames[1]),
    paste0("min_", dimNames[2]),
    paste0("max_", dimNames[2]))
  colnames(boundingBoxes) <- sapply(
    seq_len(fileInfo$nvars-2),
    \(i) var.inq.nc(nc, 1 + i)$name)
  return(boundingBoxes)
}

getBoundingBox <- function(x) {
  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x))
  rowSumsX <- x |> abs() |> rowSums()
  rowBoundingIdxs <- range(which(rowSumsX > 0))
  colSumsX <- x |> abs() |> colSums()
  colBoundingIdxs <- range(which(colSumsX > 0))
  return(c(
    rowMin = rowBoundingIdxs[1],
    rowMax = rowBoundingIdxs[2],
    colMin = colBoundingIdxs[1],
    colMax = colBoundingIdxs[2]))
}
