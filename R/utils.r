#' Extensions to arcpyr
#'
#' Extends the functionality of \code{arcpyr}. Provides some additional
#' interfaces to working with arcpy from R, including support for
#' raster calculations and working with attribute tables.
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
#' @param inrasters A named list of file paths to rasters to be loaded
#'   prior to performing the calculations listed in \code{expressions}.
#'   The names must be valid python object names. Raster objects will 
#'   be created via \code{arcpy.sa.Raster()}.
#' @param outrasters A named list of file paths to save rasters 
#'   resulting from the calculations in \code{expressions}. The names 
#'   must be valid python object names. Rasters will be saved via
#'   \code{<raster object>.save()}
#' @return No return value; raster files listed in \code{outrasters} 
#'   will be saved.
#'
#' @details Note that this function exposes the \code{arcpy.sa} 
#'   namespace, rather than importing all functions from 
#'   \code{arcpy.sa} into the global environment. Therefore, 
#'   calls to \code{arcpy.sa} functions in the \code{expressions} 
#'   argument must include the namespace, e.g. \code{arcpy.sa.Log10} 
#'   rather than simply \code{Log10}.
#'
#' @examples
#' \dontrun{ 
#' input.rasters = list(inras = "path/to/input/raster")
#' temp.rasters = list(
#'   firstras = "inras > 5",
#'   secondras = "arcpy.sa.Power(firstras, 2)",
#'   thirdras = "arcpy.sa.Log10(secondras)"
#' )
#' output.rasters = list(thirdras = "path/to/output/raster")
#' sa_calc(temp.rasters, input.rasters, output.rasters) 
#' }
#'
#' @importFrom utils capture.output
#' @export
sa_calc = function(expressions, inrasters = list(), outrasters = list()) {
  if (PythonInR::pyGet('arcpy.CheckOutExtension("Spatial")') != "CheckedOut")
    stop("extension 'Spatial' is not available", call. = FALSE)
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.sa'), silent = TRUE))) > 0)
    stop("Could not import arcpy.sa")
  on.exit({
    it = !(names(expressions) %in% c(names(inrasters), names(outrasters)))
    lapply(sprintf("arcpy.Delete_management(%s)", names(expressions)[it]),
      PythonInR::pyExec)
    PythonInR::pyExec(sprintf("del %s",
      paste(unique(names(c(inrasters, expressions, outrasters))),
        collapse = ", ")))
  })
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
#' @param table.path The file path to the table.
#' @param fields A vector of field names or column indices to retrieve.
#' @return a dataframe with columns corresponding to \code{fields}.
#' 
#' @details This implementation may be faster than accessing the
#'   \code{@data} slot of an object created from \code{rgdal::readOGR}
#'   in cases where there are a very large number of features. An 
#'   additional advantage of \code{da_read} is that it can read 
#'   raster attribute tables and stand-alone tables stored in file
#'   geodatabases, which is not supported by \code{rgdal::readOGR}.
#'
#' @examples
#' \dontrun{
#' layer = "path/to/table"
#' fields = c("VALUE", "NOTE")
#' da_read(layer, fields)
#' }
#'
#' @importFrom stats setNames
#' @importFrom utils capture.output
#' @export
da_read = function(table.path, fields) {
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da")
  # initialize to avoid error on exit
  PythonInR::pyExec("rows = None; val = None")
  on.exit(
    PythonInR::pyExec("del rows, val")
  )
  if (missing(fields))
    fields = da_fields(table.path)
  if (is.numeric(fields))
    fields = da_fields(table.path)[fields]
  fields_exist(table.path, fields)
  # convert attribute table to list of rows
  PythonInR::pyExec(sprintf(
    'rows = [row for row in arcpy.da.SearchCursor("%s", [%s])]',
    table.path, paste(sprintf('"%s"', fields), collapse = ", "))
  )
  # return NULL if table is empty
  if (PythonInR::pyGet("len(rows)") < 1) {
    warning("Table is empty. Returning NULL.")
    return(NULL)
  }
  # read from Python into R
  alist = setNames(vector("list", length(fields)), fields)
  for (i in seq_along(fields)) {
    PythonInR::pyExec(sprintf("val = [row[%d] for row in rows]", i - 1))
    alist[[i]] = PythonInR::pyGet("val")
  }
  dropcols = which(sapply(alist, function(x) class(x) == "list"))
  if (length(dropcols) > 0) {
    warning("The following fields could not be imported: ",
      paste(sprintf("'%s'", names(dropcols)), collapse = ", "))
    alist = alist[-dropcols]
  }
  # output as data.frame
  as.data.frame(alist, stringsAsFactors = FALSE)
}


#- Field class formatter for da_* functions
#-
#- Format R classes values for use with \code{sprintf}.
#-
#- @param x A vector of values.
#- @return The string format code.
#-
field_fmt = function(x) {
  if (class(x) == "numeric")
    "%f"
  else if (class(x) == "integer")
    "%d"
  else
    "%s"
  }

#- Data Frame To Text
#-
#- Write data frame to Python list for da_* functions.
#-
#- @param d The data frame.
#- @param fmt A vector specifying the format codes for each column.
#- @return The data frame as text, to be pasted into Python to generate
#-   a list structure.
#-
df2ltxt = function(d, fmt) {
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
#' @param table.path The file path to the table.
#' @param d The data to write to \code{table.path}, with the same number 
#'   of rows as the table. Column names must match field names 
#'   of the table.
#' @param fmt Vector of formats for the columns in \code{d}. If missing,
#'   the format will be automatically detected as numeric (\code{\%f}), 
#'   integer (\code{\%d}), or string (\code{\%s}). See \code{sprintf}
#'   for more information on format strings.
#' @return The path to the table, i.e. \code{table.path}.
#'
#' @examples
#' \dontrun{
#' layer = "path/to/table"
#' fields = c("VALUE", "NOTE")
#' field.formats = c("%f", "%s")
#' d = da_read(layer, fields)
#' d["VALUE"] = d$VALUE + 5
#' d["NOTE"] = "modified"
#' da_update(layer, d, field.formats)
#' }
#'
#' @seealso \code{\link{sprintf}}
#'
#' @importFrom utils capture.output
#' @export
da_update = function(table.path, d, fmt){
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da")
  fields_exist(table.path, names(d))
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
      table.path, fields),
    "  for row in cursor:",
    "    cursor.updateRow(rtable[i])",
    "    i = i + 1"
  )
  on.exit(
    PythonInR::pyExec("del rtable, i")
  )
  PythonInR::pyExec(paste(pytxt, collapse = "\n"))  
  table.path
}

#' Insert Table with arcpy.da
#'
#' Insert rows into a table (e.g. attribute table of a layer) with the 
#' arcpy.da module.
#'
#' @param table.path The file path to the table.
#' @param d The data to write to \code{table.path}. Column names must 
#'   match field names of the table.
#' @param fmt Vector of formats for the columns in \code{d}. If missing,
#'   the format will be automatically detected as numeric (\code{\%f}), 
#'   integer (\code{\%d}), or string (\code{\%s}). See \code{sprintf}
#'   for more information on format strings.
#' @return The path to the table, i.e. \code{table.path}.
#'
#' @examples
#' \dontrun{
#' # create an empty table
#' library(arcpyr)
#' connect_ArcPython()
#' arcpy = arcpy_env(NULL, NULL)
#' attach_toolbox(arcpy, "management")
#' folder = "path/to/folder"
#' name = "table name"
#' arcpy$management$CreateTable(folder, name)
#' # create fields in new table
#' arcpy$management$AddField(file.path(folder, name), "ID", "SHORT")
#' arcpy$management$AddField(file.path(folder, name), "VALUE", "FLOAT")
#' arcpy$CreateTable_management(outpath, tablename)
#' # generate table contents in R
#' d = data.frame(ID = seq(10), VALUE = rnorm(10))
#' # write data to table
#' da_insert(file.path(folder, name), d)
#' }
#'
#' @seealso \code{\link{sprintf}}
#'
#' @importFrom utils capture.output
#' @export
da_insert = function(table.path, d, fmt) { 
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da")
  fields_exist(table.path, names(d))
  if (missing(fmt))
    fmt = lapply(names(d), function(x) field_fmt(d[[x]]))
  fmt = gsub("\\%s", "'\\%s'", fmt)
  fields = sprintf("[%s]", 
    paste(sprintf("'%s'", names(d)), collapse = ", "))
  pytxt = c(
    sprintf("rtable = [%s]", df2ltxt(d, fmt)),
    sprintf("with arcpy.da.InsertCursor('%s', %s) as cursor:", 
      table.path, fields),
    "  for row in rtable:",
    "    cursor.insertRow(row)"
  )
  on.exit(
    PythonInR::pyExec("del rtable")
  )
  PythonInR::pyExec(paste(pytxt, collapse = "\n"))  
  table.path
}


#' List Attribute Table Fields
#'
#' Read attribute table field names with arcpy.da module.
#'
#' @param table.path The file path to the table.
#' @return A vector of field names.
#'
#' @importFrom utils capture.output
#' @export
da_fields = function(table.path) {
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da")
  fields = PythonInR::pyGet(
    sprintf('[f.name for f in arcpy.ListFields("%s")]', table.path)
  )
  fields
}

#- Fields Exist
#-
#- Check if specified fields are present in a table.
#-
#- @param table.path The file path to the table.
#- @param fields The field names.
#- @return No return value; will produce an error if any of the 
#-   specified fields are not present in the table.
#-
fields_exist = function(table.path, fields) {
  actual = da_fields(table.path)
  res = fields %in% actual
  if (!all(res))
    stop("Specified fields do not exist in table: ", 
      paste(fields[!res], sep = ", "), call. = FALSE)
  invisible(NULL)
}