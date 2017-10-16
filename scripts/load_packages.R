### Load Required Packages

# Below is some useful code that will:



# Make a vector of the packages used by this tutorial
required.pkg <- c("maps", "ggplot2", "ggmap", "RColorBrewer", "classInt")

# Identify any uninstalled packages
pkgs.not.installed <- required.pkg[!sapply(required.pkg, function(p) require(p, character.only=T))]

# Install any needed packages that are not currentlty installed
if(length(pkgs.not.installed) > 0) {
  install.packages(pkgs.not.installed, dependencies=TRUE)
} else {
  print("All required packages installed.")
}

# Load all libraries them all at once.
lapply(required.pkg, library, character.only = TRUE)         
