# DEPRECATION NOTICE

This package, and the base package 
[arcpyr](https://github.com/mkoohafkan/arcpyr) that
it extends, is now
deprecated. Development has transitioned to a new package 
[arcpy](https://github.com/mkoohafkan/arcpy), 
which uses 
[reticulate](https://cran.r-project.org/package=reticulate)
for the Python-R
bridge. `arcpy` supports all the functionality of `arcpyr`
without the need for the additional wrapper functions that
made up the bulk of the `arcpyr` code base, instead
producing an `arcpy` object that provides a seamless interface
arcpy functions and classes. The helper functions `da_read`,
`da_update` and `da_insert` have also been ported over from 
`arcpyrextra`, and S3 methods were added to `arcpy` to replace
the raster math tools from `arcpyrextra`.

# arcpyrextra

Extends the functionality of [arcpyr](https://github.com/mkoohafkan/arcpyr).
Provides some additional interfaces to working with arcpy from R, including 
support for raster calculations and working with attribute tables.
