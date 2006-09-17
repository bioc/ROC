.onLoad <- function(libname, pkgname) {
    require("methods")
}

.onAttach <- function(libname, pkgname) {
    suppressWarnings(require("Biobase")) && addVigs2WinMenu("ROC")
}


