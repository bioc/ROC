.First.lib <- function(libname, pkgname, where)
 {
 require(methods)
 where <- match(paste("package:", pkgname, sep=""), search())
 cacheMetaData(as.environment(where))
 if(.Platform$OS.type == "windows" && require("Biobase") && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("ROC")
 }

 }

