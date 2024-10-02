# Georgia_CEQ_package
package version of Georgia CEQ  for easy use.

# Set up

The easiest way to run a local version of the Georgia CEQ app, is by running these couple of lines of code.
The only thing you will need to edit are the `path_to_presim` and the  `path_to_baseline`. These should be file paths (or relative file paths if you are using a Rproject). These two file paths refer to the baseline CEQ data in `.rds` format and the pre-simulation data in `.rds` format.


_Please note, that in order to run the shiny app it is required to have this confidential data downloaded and saved on your PC. As no confidential data is being shared on the GitHub, users must request this data (and it will be shared during the training). The files you need are most likely named `presim.rds` and `baseline.rds`, but you migh decide to rename them_

```r
# 1) download the remotes pacakage if you do not all ready have this installed.
#    this package is needed to download a package from Github
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 2) Check if the GeoappPackage is installed, if it is not it is installed from GitHub
if (!requireNamespace("GeoappPackage", quietly = TRUE)) remotes::install_github("wbEPL/Georgia_CEQ_package", force = TRUE, upgrade = FALSE)

# 3) Load the package
library("GeoappPackage")

# 4) Start the tool
#    make sure to edit the path_to_presim and path_to_baseline
run_georgia_ceq_app(path_to_presim = "C:/Users/PATH_TO_PRESIM_FILE/presim.rds",
                    path_to_baseline = "C:/Users/PATH_TO_BASELINE_FILE/baseline.rds")


````
__This package is still under active development, so any comments/suggestions are welcome.__
