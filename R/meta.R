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
  meta <- bind_cols(
    path = normalizePath(file.path(path, fileNames)),
    as_tibble(partsLst))
  return(meta)
}


asNumericConditional <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  if (any(is.na(z))) return(x)
  zi <- as.integer(z)
  if (all(abs(zi - z) <= 2*.Machine$double.eps)) return(zi)
  return(z)
}

getMetaGenericOne <- function(path, tagsFilter, tagFileFilter) {

  fileNames <-
    path |>
    dir() |>
    str_subset("^([^\\d\\._]+[\\d_]+)+([^\\d\\._]+)?\\.(csv|json|rds)$")

  fileNamesWoEnding <- str_remove(fileNames, "\\.(csv|json|rds)$")
  tags <- str_extract_all(fileNamesWoEnding, "[^\\d\\._]+")

  if (!is.null(tagsFilter)) {
    sel1 <- vapply(tags, \(tg) all(tg %in% tagsFilter), FUN.VALUE=logical(1))
  } else {
    sel1 <- TRUE
  }
  if (!is.null(tagFileFilter)) {
    sel2 <- vapply(
      tags,
      \(tg) {
        any(
          vapply(
            tagFileFilter,
            \(filt) length(tg) == length(filt) && all(tg == filt),
            FUN.VALUE=logical(1))
        )
      },
      FUN.VALUE=logical(1))
  } else {
    sel2 <- TRUE
  }
  sel <- sel1 & sel2

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
    meta <- as_tibble(meta)
    meta <- select(meta, tidyselect::where(~!all(is.na(.))))
    return(meta)
  })

  return(metaList)
}


#' @export
getMetaGeneric <- function(
    paths,
    tagsFilter = c("task", "esti", "truth", "obs"),
    tagFileFilter = NULL,
    nrFilters = NULL,
    removeNa = FALSE
) {
  paths <- normalizePath(paths, mustWork=TRUE)
  paths <- unique(paths)
  metaList <-
    lapply(
      paths,
      getMetaGenericOne,
      tagsFilter = tagsFilter,
      tagFileFilter = tagFileFilter
    ) |>
      unlist(recursive=FALSE)
  colCount <- sapply(metaList, ncol)
  meta <- Reduce(fullJoinMeta, metaList[order(colCount, decreasing=TRUE)])
  if (!tibble::is_tibble(meta) || nrow(meta) == 0) {
    warning(
      "Could not find any meta files in paths\n -",
      paste(paths, collapse="\n -"),
      "\nwith tags ",
      paste(tagsFilter, collapse=", "))
    return(NULL)
  }
  for (i in seq_along(nrFilters)) {
    nm <- names(nrFilters)[i]
    if (!nm %in% colnames(meta)) next
    if (is.null(nrFilters[[nm]])) next
    meta <- meta[meta[[nm]] %in% nrFilters[[i]], ]
  }
  if (removeNa) {
    meta <- drop_na(meta)
  }
  meta <- dplyr::relocate(meta, dplyr::ends_with("Nr"), dplyr::ends_with("Path"))
  return(meta)
}

fullJoinMeta <- function(x, y) {
  common <- intersect(names(x), names(y))
  if (length(common) == 0) {
    bind_cols(
      x[rep(seq_len(nrow(x)), each = nrow(y)), ],
      y[rep(seq_len(nrow(y)), times = nrow(x)), ])
  } else {
    full_join(x, y, by = common)
  }
}
