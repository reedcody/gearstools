# For jgrn/krypton:
pathtopackage <- "/Users/jgrn307/git/GEARStools/GEARStools/pkg/GEARStools"

# Kandor:
pathtopackage <- "C://Users//jgreenberg//git//gearstools//GEARStools//pkg//GEARStools"



setwd(pathtopackage)

# This builds the man pages and updates the NAMESPACE.
# When in doubt, you can delete all your local man files
# and this will re-create them.
require("roxygen2")
roxygenize(package.dir=pathtopackage)
