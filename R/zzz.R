.First.lib <- function(libname, pkgname, where)
 {
 require(methods)
 where <- match(paste("package:", pkgname, sep=""), search())
 .initClasses(where)
 cacheMetaData(as.environment(where))
 }

