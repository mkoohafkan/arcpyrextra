#' Extensions to arcpyr
#'
#' Extends the functionality of \code{arcpyr}. Provides some additional
#' interfaces to working with arcpy from R, including support for
#' raster calculations and reading attribute tables.
#'
#' @name arcpyrextra-package
#' @aliases arcpyrextra
#' @docType package
NULL

#'Raster Calculations with arcpy.sa
#'
#' Perform Raster Calculations with arcpy.sa.
#'
#' @param expressions A named list of raster calculation expressions to 
#'   be executed in sequence. The names must be valid python object 
#'   names.
#' @param inrasters A named list of file paths to Rasters used in the 
#'   calculation expressions, to be converted to ArcPy raster objects. 
#'   The names must be valid python object names.
#' @param outrasters A named list of file paths to save Rasters 
#'   resulting the calculation expressions. The names must be valid 
#'   python object names.
#' @return No return value; raster files listed in \code{outrasters} 
#'   will be saved.
#'
#' @details Note that the functions exposes the \code{arcpy.sa} 
#'   namespace, rather than importing all function from 
#'   \code{arcpy.sa} into the global environment. Therefore, 
#'   calls to \code{arcpy.sa} functions in the \code{expressions} 
#'   argument must include the namespace, e.g. \code{arcpy.sa.Log10} 
#'   rather than simply \code{Log10}.
#'
#' @importFrom utils capture.output
#' @export
sa_calc = function(expressions, inrasters = list(), outrasters = list()) {
  if (PythonInR::pyGet('arcpy.CheckOutExtension("Spatial")') != "CheckedOut")
    stop("extension 'Spatial' is not available.", call. = FALSE)
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.sa'), silent = TRUE))) > 0)
    stop("Could not import arcpy.sa.")
  on.exit(
    PythonInR::pyExec(sprintf("del %s",
      paste(unique(names(c(inrasters, expressions, outrasters))),
        collapse = ", ")))
  )
  load_exprs = sprintf('%s = arcpy.sa.Raster("%s")', names(inrasters), inrasters)
  calc_exprs = sprintf('%s = %s', names(expressions), expressions)
  save_exprs = sprintf('%s.save("%s")', names(outrasters), outrasters)
  # load, execute, save
  for (e in c(load_exprs, calc_exprs, save_exprs))
    PythonInR::pyExec(e)
  invisible(NULL)
}

#' Read Table with arcpy.da
#'
#' Read a table (e.g. attribute table of a layer) with the arcpy.da module.
#'
#' @param file The file path to the table.
#' @param fields A vector of field names to retreive.
#' @return a dataframe with columns corresponding to \code{fields}.
#' 
#' @details This implementation is generally slower than accessing the
#'   \code{@data} slot of an object created from \code{rgdal::readOGR}.
#'   The advantage of \code{da_read} is that it can read 
#'   raster attribute tables and stand-alone tables stored in file
#'   file geodatabases, which \code{rgdal::readOGR} cannot.
#'
#' @importFrom stats setNames
#' @importFrom utils capture.output
#' @export
da_read = function(file, fields) {
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da.")
  on.exit( 
    PythonInR::pyExec("del rows, val")
  )
  if(missing(fields))
    fields = PythonInR::pyGet(
      sprintf('[f.name for f in arcpy.ListFields("%s")]', file)
    )
  # convert attribute table to list of rows
  PythonInR::pyExec(sprintf(
    'rows = [row for row in arcpy.da.SearchCursor("%s", [%s])]',
    file, paste(sprintf('"%s"', fields), collapse = ", "))
  )
  # read from Python into R
  alist = setNames(vector("list", length(fields)), fields)
  for (i in seq_along(fields)) {
    PythonInR::pyExec(sprintf("val = [row[%d] for row in rows]", i - 1))
    alist[[i]] = PythonInR::pyGet("val")
  }
  # output as data.frame
  as.data.frame(alist, stringsAsFactors = FALSE)
}


# Field class formatter for da_* functions
field_fmt = function(x) {
  if (class(x) == "numeric")
    "%f"
  else if (class(x) == "integer")
    "%d"
  else
    "%s"
  }

# write data frame to Python list for da_* functions
df2ltxt = function(d, fmt){
  paste(lapply(seq(nrow(d)), function(x) 
      do.call(sprintf, c(
        fmt = sprintf("[%s]", paste(fmt, collapse = ", ")), 
        as.list(d[x,])))), 
    collapse = ",\n  ")
}

#' Update Table with arcpy.da
#'
#' Update a table (e.g. attribute table of a layer) with the 
#' arcpy.da module.
#'
#' @param d The data to write to the table, with the same number of rows
#'   as the table to be written to. Column names must match field names 
#'   of the table to be written to.
#' @param file The file path to the table.
#' @param fmt Vector of formats for the columns in \code{d}. If missing,
#'   the format will be automatically detected as numeric (\code{\%f}), 
#'   integer (\code{\%d}), or string (\code{\%s}).
#' @return The path to \code{file}.
#' 
#' @details When \code{update = TRUE}, \code{arcpy.da.updateCursor} is 
#'   used instead of \code{arcpy.da.InsertCursor}. Note that the number
#'   of rows of \code{d} is not checked prior to updating.
#'
#' @importFrom utils capture.output
#' @export
da_update = function(d, file, fmt){
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da.")
  if (missing(fmt))
    fmt = lapply(names(d), function(x) field_fmt(d[[x]]))
  fmt = gsub("\\%s", "'\\%s'", fmt)
  fields = sprintf("[%s]", 
    paste(sprintf("'%s'", names(d)), collapse = ", "))
  # write d as python list  
  pytxt = c(
    sprintf("rtable = [%s]", df2ltxt(d, fmt)),
    "i = 0",
    sprintf("with arcpy.da.UpdateCursor('%s', %s) as cursor:", 
      file, fields),
    "  for row in cursor:",
    "    cursor.updateRow(rtable[i])",
    "    i = i + 1"
  )
  on.exit = PythonInR::pyExec("del rtable, i")
  PythonInR::pyExec(paste(pytxt, collapse = "\n"))  
  file
}


#' Insert Table with arcpy.da
#'
#' Insert rows into a table (e.g. attribute table of a layer) with the 
#' arcpy.da module.
#'
#' @param d The data to write to the table. Column names must match 
#'  field names of the table to be written to.
#' @param file The file path to the table.
#' @param fmt Vector of formats for the columns in \code{d}. If missing,
#'   the format will be automatically detected as numeric (\code{\%f}), 
#'   integer (\code{\%d}), or string (\code{\%s}).
#' @return The path to \code{file}.
#' 
#' @details When \code{update = TRUE}, \code{arcpy.da.updateCursor} is 
#'   used instead of \code{arcpy.da.InsertCursor}. Note that the number
#'   of rows of \code{d} is not checked prior to updating.
#'
#' @importFrom utils capture.output
#' @export
da_insert = function(d, file, fmt) { 
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da.")
  if (missing(fmt))
    fmt = lapply(names(d), function(x) field_fmt(d[[x]]))
  fmt = gsub("\\%s", "'\\%s'", fmt)
  fields = sprintf("[%s]", 
    paste(sprintf("'%s'", names(d)), collapse = ", "))
  pytxt = c(
    sprintf("rtable = [%s]", df2ltxt(d, fmt)),
    sprintf("with arcpy.da.InsertCursor('%s', %s) as cursor:", 
      file, fields),
    "  for row in rtable:",
    "    cursor.insertRow(row)"
  )
  on.exit = PythonInR::pyExec("del rtable")
  PythonInR::pyExec(paste(pytxt, collapse = "\n"))  
  file
}

