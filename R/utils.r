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
#' @param expressions A list of raster calculation expressions to be 
#'   executed in sequence.
#' @param inrasters A named list of file paths to Rasters used in the 
#'   calculation expressions, to be converted to ArcPy raster objects. 
#' @param outrasters A named list of file paths to save Rasters 
#'   resulting the calculation expressions.
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
RasterCalculator = function(expressions, inrasters = list(), outrasters = list()) {
  if (PythonInR::pyGet('arcpy.CheckOutExtension("Spatial")') != "CheckedOut")
    stop("extension 'Spatial' is not available.", call. = FALSE)
  if(length(capture.output(try(PythonInR::pyExecp('import arcpy.sa'), silent = TRUE))) > 0)
    stop("Could not import arcpy.sa.")
  load_exprs = sprintf('%s = arcpy.sa.Raster("%s")', names(inrasters), inrasters)
  save_exprs = sprintf('%s.save("%s")', names(outrasters), outrasters)
  # load, execute, save
  for (e in c(load_exprs, expressions, save_exprs))
    PythonInR::pyExec(e)
  invisible(NULL)
}

#'Get Attribute Table with arcpy.da
#'
#' Get the attribute table of a layer or raster with the arcpy.da module.
#'
#' @param layer The layer to access.
#' @param path The folder or geodatabase containing \code{layer}.
#' @param fields A vector of field names to retreive.
#' @return a dataframe with columns corresponding to \code{fields}.
#' 
#' @details This implementation is generally slower than accessing the
#'   \code{@data} slot of an object created from \code{rgdal::readOGR}.
#'   The advantage of \code{get_attrtable} is that it can read 
#'   raster attribute tables and stand-alone tables stored in file
#'   file geodatabases, which \code{rgdal::readOGR} cannot.
#'
#' @importFrom stats setNames
#' @importFrom utils capture.output
#' @export
get_attrtable = function(layer, path, fields) {
  if (length(capture.output(try(PythonInR::pyExecp('import arcpy.da'), silent = TRUE))) > 0)
    stop("Could not import arcpy.da.")
  layerpath = file.path(path, layer)

  PythonInR::pyExec(sprintf(
    'rows = [row for row in arcpy.da.SearchCursor("%s", [%s])]',
    layerpath, paste(sprintf('"%s"', fields), collapse = ", "))
  )
  alist = setNames(vector("list", length(fields)), fields)

  for (i in seq_along(fields)) {
    PythonInR::pyExec(sprintf("val = [row[%d] for row in rows]", i - 1))
    alist[[i]] = PythonInR::pyGet("val")
  }
  as.data.frame(alist, stringsAsFactors = FALSE)
}
