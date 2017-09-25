## unit tests will not be done if RUnit is not available
if(require("testthat", quietly=TRUE)) {
	#pkg <- "PKG" # <-- Change to package name!
	pkg <- "myPackageId" # <-- Change to package name!
	#library(pkg, character.only = TRUE)
	#test_package(pkg)
	test_check(pkg)
} else {
	warning("cannot run unit tests -- package testthat is not available")
}