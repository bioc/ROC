.First.lib <- function(libname, pkgname, where)
 {
 ##require(methods)
 where <- match(paste("package:", pkgname, sep=""), search())
 cacheMetaData(as.environment(where))

 if((.Platform$OS.type == "windows") && ("Biobase" %in% installed.packages()[,"Package"])
    && (interactive()) && (.Platform$GUI ==  "Rgui")){
     if (require("Biobase"))
         addVigs2WinMenu("ROC")
 }

}

