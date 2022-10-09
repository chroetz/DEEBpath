#' @export
getFileMeta <- function(path, pattern, ...) {
  fileNames <-
    path |>
    dir() |>
    str_subset(pattern)
  if (length(fileNames) == 0) return(NULL)
  parts <- str_match(fileNames, pattern)[,-1,drop=FALSE]
  partsLst <- apply(parts, 2, asNumericConditional, simplify=FALSE)
  names(partsLst) <- c(...)
  meta <- dplyr::bind_cols(
    path = normalizePath(file.path(path, fileNames)),
    tibble::as_tibble(partsLst))
  return(meta)
}


asNumericConditional <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  if (any(is.na(z))) return(x)
  zi <- as.integer(z)
  if (all(abs(zi - z) <= 2*.Machine$double.eps)) return(zi)
  return(z)
}

getMetaGenericOne <- function(path, tagsFilter) {

  fileNames <-
    path |>
    dir() |>
    str_subset("^([^\\d\\._]+[\\d_]+)+([^\\d\\._]+)?\\.(csv|json)$")

  fileNamesWoEnding <- str_remove(fileNames, "\\.(csv|json)$")
  tags <- str_extract_all(fileNamesWoEnding, "[^\\d\\._]+")

  sel <- sapply(tags, \(tg) all(tg %in% tagsFilter))
  fileNames <- fileNames[sel]
  fileNamesWoEnding <- fileNamesWoEnding[sel]
  tags <- tags[sel]

  nums <- str_extract_all(fileNamesWoEnding, "[\\d_]+")

  tagsConcat <- sapply(tags, paste, collapse="0")
  ids <- unique(tagsConcat)

  metaList <- lapply(ids, \(id) {
    sel <- tagsConcat == id
    fullPaths <- normalizePath(file.path(path, fileNames[sel]))
    tg <- tags[sel][[1]]
    nm <- unlist(nums[sel])
    nm[nm == "_"] <- NA
    nm <- nm |> as.integer() |> matrix(ncol = sum(sel)) |> t() |> as.data.frame()
    meta <- cbind(fullPaths, nm)
    colnames(meta) <- c(paste0(tg[length(tg)], "Path"), paste0(tg[1:ncol(nm)], "Nr"))
    meta <- tibble::as_tibble(meta)
    meta <- dplyr::select(meta, where(~!all(is.na(.)))) # TODO: where() will be exported from tidyselect in the future
    return(meta)
  })

  return(metaList)
}


#' @export
getMetaGeneric <- function(
    paths,
    tagsFilter = c("task", "esti", "truth", "obs"),
    nrFilters = NULL
) {
  paths <- normalizePath(paths, mustWork=TRUE)
  paths <- unique(paths)
  metaList <- lapply(paths, getMetaGenericOne, tagsFilter=tagsFilter) |> unlist(recursive=FALSE)
  colCount <- sapply(metaList, ncol)
  meta <- Reduce(fullJoinMeta, metaList[order(colCount, decreasing=TRUE)])
  if (!tibble::is_tibble(meta) || nrow(meta) == 0) {
    stop(
      "Could not find any meta files in paths\n -",
      paste(paths, collapse="\n -"),
      "\nwith tags ",
      paste(tagsFilter, collapse=", "))
  }
  for (i in seq_along(nrFilters)) {
    nm <- names(nrFilters)[i]
    if (!nm %in% colnames(meta)) next
    meta <- meta[meta[[nm]] %in% nrFilters[[i]], ]
  }
  meta <- dplyr::relocate(meta, dplyr::ends_with("Nr"), dplyr::ends_with("Path"))
  return(meta)
}

fullJoinMeta <- function(x, y) {
  common <- intersect(names(x), names(y))
  if (length(common) == 0) {
    dplyr::bind_cols(
      x[rep(seq_len(nrow(x)), each = nrow(y)), ],
      y[rep(seq_len(nrow(y)), times = nrow(x)), ])
  } else {
    dplyr::full_join(x, y, by = common)
  }
}
