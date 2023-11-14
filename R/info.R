#' @export
getNcInfo <- function(filePath) {
  nc <- RNetCDF::open.nc(filePath)
  ncInfo <- RNetCDF::file.inq.nc(nc)
  dimInfo <- lapply(seq_len(ncInfo$ndims)-1, RNetCDF::dim.inq.nc, ncfile = nc)
  varInfo <- lapply(seq_len(ncInfo$nvars)-1, RNetCDF::var.inq.nc, ncfile = nc)
  attInfo <- lapply(
    varInfo,
    \(vi) {
      lapply(
        seq_len(vi$natts)-1,
        RNetCDF::att.inq.nc,
        ncfile = nc,
        variable = vi$id)
    })
  attValue <- lapply(
    varInfo,
    \(vi) {
      lapply(
        seq_len(vi$natts)-1,
        RNetCDF::att.get.nc,
        ncfile = nc,
        variable = vi$id)
    })
  RNetCDF::close.nc(nc)
  return(lst(ncInfo, dimInfo, varInfo, attInfo, attValue))
}
