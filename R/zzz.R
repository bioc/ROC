.First.lib <- function(libname, pkgname, where) {
    require(methods)
    require(Biobase)
    if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- pos.to.env(where)
    }
   .initClasses(where)
    cacheMetaData(as.environment(where))
}
#.First.lib <- function(libname, pkgname, where) {
#    require(methods)
#    where <- match(paste("package:", pkgname, sep=""), search())
#    .initROCclasses(where)
#    cacheMetaData(as.environment(where))
#}
#
#.First.lib <- function(libname, pkgname, where)
# { require(methods)
#  if(missing(where)) {
#        where <- match(paste("package:", pkgname, sep=""), search())
#        if(is.na(where)) {
#            warning(paste("Not a package name: ",pkgname))
#            return()
#        }
#        where <- pos.to.env(where)
#    }
#   .initClasses(where)
#   require(Biobase)
#}

