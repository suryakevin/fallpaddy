# Written by Kevin Surya

# This log file records the steps in building the `fallpaddy` package. I
#   consulted Hadley Wickham and Jenny Bryan's book at
#   https://r-pkgs.org/index.html, an excellent resource for building `R`
#   packages.

# Load the `R` package `devtools` ----
library(devtools)  # make sure that `devtools` is up-to-date

# Check if the name `fallpaddy` is available ----
library(available)
available(name = "fallpaddy")
#> -- fallpaddy --------------------------------------------------------------------------------
#> Name valid: ✔
#> Available on CRAN: ✔
#> Available on Bioconductor: ✔
#> Available on GitHub:  ✔
#> Abbreviations: http://www.abbreviations.com/fallpaddy
#> Wikipedia: https://en.wikipedia.org/wiki/fallpaddy
#> Wiktionary: https://en.wiktionary.org/wiki/fallpaddy
#> Urban Dictionary:
#>   Not found.
#> Sentiment:???

# Create the `R` package `fallpaddy` ----
create_package(path = "fallpaddy")  # execute in `RStudio`
#> √ Creating 'fallpaddy/'
#> √ Setting active project to 'C:/Users/sadik/Documents/GitHub/fallpaddy'
#> √ Creating 'R/'
#> √ Writing 'DESCRIPTION'
#> Package: fallpaddy
#> Title: What the Package Does (One Line, Title Case)
#> Version: 0.0.0.9000
#> Authors@R (parsed):
#>     * First Last <first.last@example.com> [aut, cre] (YOUR-ORCID-ID)
#> Description: What the package does (one paragraph).
#> License: `use_mit_license()`, `use_gpl3_license()` or friends to
#>     pick a license
#> Encoding: UTF-8
#> Roxygen: list(markdown = TRUE)
#> RoxygenNote: 7.1.2
#> √ Writing 'NAMESPACE'
#> √ Writing 'fallpaddy.Rproj'
#> √ Adding '^fallpaddy\\.Rproj$' to '.Rbuildignore'
#> √ Adding '.Rproj.user' to '.gitignore'
#> √ Adding '^\\.Rproj\\.user$' to '.Rbuildignore'
#> √ Opening 'C:/Users/sadik/Documents/GitHub/fallpaddy/' in new RStudio session
#> √ Setting active project to '<no active project>'

# Reload `devtools` ---
library(devtools)

# Write .r function files ----

# Create documentation and generate NAMESPACE ----
document()
#> i Updating fallpaddy documentation
#> i Loading fallpaddy
#> Writing NAMESPACE
#> Writing NAMESPACE
#> Writing calc_partialr2.Rd
#> Writing calc_r2.Rd
#> Writing calc_vif.Rd
#> Writing compare_models.Rd
#> Writing create_dmat.Rd
#> Writing decomp_vcv.Rd
#> Writing est_node_density.Rd
#> Writing print_node_density.Rd
#> Writing est_punc_contrib.Rd
#> Writing export_data.Rd
#> Writing export_data_bt.Rd
#> Writing export_plot_html.Rd
#> Writing export_plot_pdf.Rd
#> Writing export_plot_svg.Rd
#> Writing export_tree.Rd
#> Writing fallpaddy.Rd
#> Writing fit_punc_model.Rd
#> Writing print_punc_model.Rd
#> Writing get_node_count.Rd
#> Writing get_path_length.Rd
#> Writing import_data.Rd
#> Writing import_data_bt.Rd
#> Writing import_data_csv.Rd
#> Writing import_tree_nex.Rd
#> Writing import_tree_nwk.Rd
#> Writing plot_data_2d.Rd
#> Writing plot_data_3d.Rd
#> Writing plot_diagnostics.Rd
#> Writing plot_node_density.Rd
#> Writing plot_punc_model.Rd
#> Writing plot_tree.Rd
#> Writing plot_tree_color.Rd
#> Writing reorder_data.Rd
#> Writing sim_punc.Rd

# Pick a license ----
use_mit_license()
#> √ Setting active project to 'C:/Users/sadik/Documents/GitHub/fallpaddy'
#> √ Setting License field in DESCRIPTION to 'MIT + file LICENSE'
#> √ Writing 'LICENSE'
#> √ Writing 'LICENSE.md'
#> √ Adding '^LICENSE\\.md$' to '.Rbuildignore'

# Edit DESCRIPTION ----

# Write README.md ----

# [...RESUME...].

# clean and store external data ----

# create vignette ----

# render `fallpaddy` as a git repo ----
use_git()
#> √ Initialising Git repo
#> √ Adding '.Rhistory', '.Rdata', '.httr-oauth', '.DS_Store' to '.gitignore'
#> There are 12 uncommitted files:
#> * '.gitignore'
#> * '.Rbuildignore'
#> * 'DESCRIPTION'
#> * 'fallpaddy.Rproj'
#> * 'LICENSE'
#> * 'LICENSE.md'
#> * 'log.r'
#> * 'man/'
#> * 'NAMESPACE'
#> * 'R/'
#> * ...
#> Is it ok to commit them?
#>
#> 1: No
#> 2: Definitely
#> 3: Nope
#>
#> Selection: 2
#> √ Adding files
#> √ Making a commit with message 'Initial commit'
#> * A restart of RStudio is required to activate the Git pane
#> Restart now?
#>
#> 1: Absolutely not
#> 2: Negative
#> 3: Yes
#>
#> Selection: 3

# create a new GitHub repo with the same name as the package ----

# synchronize the local git repo with the GitHub repo ----
#> git remote add origin https://github.com/suryakevin/fallpaddy.git
#> git push -u origin master

# Check `fallpaddy` ----
check()
#> i Updating fallpaddy documentation
#> i Loading fallpaddy
#> Writing NAMESPACE
#> Writing NAMESPACE
#> -- Building ------------------------------------------------------------------ fallpaddy --
#> Setting env vars:
#> * CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
#> * CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
#> * CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
#> * CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
#> * CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
#> * CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
#> -------------------------------------------------------------------------------------------
#> √  checking for file 'C:\Users\sadik\Documents\GitHub\fallpaddy/DESCRIPTION' ...
#> -  preparing 'fallpaddy':
#> √  checking DESCRIPTION meta-information ...
#> -  checking for LF line-endings in source and make files and shell scripts
#> -  checking for empty or unneeded directories
#> -  building 'fallpaddy_1.0.0.tar.gz'
#>
#> -- Checking ------------------------------------------------------------------ fallpaddy --
#> Setting env vars:
#> * _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
#> * _R_CHECK_CRAN_INCOMING_       : FALSE
#> * _R_CHECK_FORCE_SUGGESTS_      : FALSE
#> * NOT_CRAN                      : true
#> -- R CMD check ----------------------------------------------------------------------------
#> -  using log directory 'C:/Users/sadik/AppData/Local/Temp/RtmpAdM8P9/fallpaddy.Rcheck'
#> -  using R version 4.1.2 (2021-11-01)
#> -  using platform: x86_64-w64-mingw32 (64-bit)
#> -  using session charset: ISO8859-1
#> -  using options '--no-manual --as-cran'
#> √  checking for file 'fallpaddy/DESCRIPTION'
#> -  this is package 'fallpaddy' version '1.0.0'
#> -  package encoding: UTF-8
#> √  checking package namespace information
#> √  checking package dependencies (2.1s)
#> √  checking if this is a source package ...
#> √  checking if there is a namespace
#> √  checking for executable files (573ms)
#> √  checking for hidden files and directories ...
#> √  checking for portable file names ...
#> √  checking serialization versions
#> √  checking whether package 'fallpaddy' can be installed (8.9s)
#> √  checking installed package size ...
#> √  checking package directory (351ms)
#> √  checking for future file timestamps ...
#> √  checking DESCRIPTION meta-information (341ms)
#> √  checking top-level files
#> √  checking for left-over files ...
#> √  checking index information
#> √  checking package subdirectories ...
#> √  checking R files for non-ASCII characters ...
#> √  checking R files for syntax errors ...
#> √  checking whether the package can be loaded (2.2s)
#> √  checking whether the package can be loaded with stated dependencies (2s)
#> √  checking whether the package can be unloaded cleanly (2.2s)
#> √  checking whether the namespace can be loaded with stated dependencies (2.1s)
#> √  checking whether the namespace can be unloaded cleanly (2.3s)
#> √  checking loading without being on the library search path (2.3s)
#> √  checking dependencies in R code (2s)
#> √  checking S3 generic/method consistency (3.1s)
#> √  checking replacement functions (2.1s)
#> √  checking foreign function calls (2.1s)
#> √  checking R code for possible problems (10.5s)
#> √  checking Rd files ...
#> √  checking Rd metadata ...
#> √  checking Rd line widths ...
#> √  checking Rd cross-references ...
#> √  checking for missing documentation entries (2.1s)
#> √  checking for code/documentation mismatches (6.3s)
#> √  checking Rd \usage sections (3.4s)
#> √  checking Rd contents ...
#> √  checking for unstated dependencies in examples ...
#> -  checking examples ... NONE
#> √  checking for non-standard things in the check directory
#> √  checking for detritus in the temp directory
#>
#>
#> -- R CMD check results ----------------------------------------------- fallpaddy 1.0.0 ----
#> Duration: 1m 0.6s
#>
#> 0 errors √ | 0 warnings √ | 0 notes √
