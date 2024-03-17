generatePredictorList <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, "PredictorListGenerator")

  nameAndType <- str_split_fixed(names(opts$elements), "_", 2)
  settings <- tibble(
    name = nameAndType[,1],
    type = nameAndType[,2],
    value = opts$elements)

  createInstance("main", settings)
}

createInstance <- function(name, settings) {
  idx <- getIdx(name, settings)
  if (length(idx) == 0) return(list(name))
  type <- settings$type[idx]
  value <- settings$value[[idx]]
  instances <- switch(
    type,
    and = {
      instanceList <- lapply(value, \(v) createInstance(v, settings))
      grid <- do.call(expand.grid, rev(instanceList))
      instances <- lapply(seq_len(nrow(grid)), \(i) unlist(rev(grid[i, ]), recursive = TRUE))
    },
    or = lapply(value, \(v) createInstance(v, settings)) |> unlist(recursive = FALSE),
    lag = lapply(value, \(v) if (v == 0) name else c(name, paste0(name, "_lag", seq_len(v)))),
    stop("Unknown type: ", type))
  if (name == "main") instances <- c(list(""), instances)
  instances <- lapply(instances, \(inst) inst[nchar(inst) > 0])
  instances <- unique(instances)
  return(instances)
}

getIdx <- function(name, settings) {
  if (length(name) != 1) return(integer(0))
  idx <- which(settings$name == name)
  if (length(idx) > 1) stop("Found multiple ", name, " in settings!")
  return(idx)
}

