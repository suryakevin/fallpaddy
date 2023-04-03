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
#> Writing est_branching_rate.Rd
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
#> Writing get_sampling_time.Rd
#> Writing import_data.Rd
#> Writing import_data_bt.Rd
#> Writing import_data_csv.Rd
#> Writing import_data_excel.Rd
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

# Render `fallpaddy` as a git repo ----
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

# Create a new GitHub repo with the same name as the package ----

# Synchronize the local git repo with the GitHub repo ----
#> git remote add origin https://github.com/suryakevin/fallpaddy.git
#> git push -u origin master

# Clean and store external data ----
load(file = "data.rda")
library(ape)
tree_dinosaur_morpho <- as.phylo(x = tree_dinosaur_morpho)
tree_dinosaur_time <- as.phylo(x = tree_dinosaur_time)
tree_lepidosaur_mol <- as.phylo(x = tree_lepidosaur_mol)
tree_mammal_morpho <- as.phylo(x = tree_mammal_morpho)
tree_zika_mol <- as.phylo(x = tree_zika_mol)
tree_zika_time <- as.phylo(x = tree_zika_time)
use_data(
  tree_lepidosaur_mol,
  tree_mammal_morpho,
  data_mammal,
  tree_zika_mol,
  tree_zika_time,
  tree_dinosaur_time,
  tree_dinosaur_morpho,
  data_dinosaur
)
#> √ Setting active project to 'C:/Users/sadik/Documents/GitHub/fallpaddy'
#> √ Adding 'R' to Depends field in DESCRIPTION
#> √ Creating 'data/'
#> √ Setting LazyData to 'true' in 'DESCRIPTION'
#> √ Saving 'tree_lepidosaur_mol', 'tree_mammal_morpho', 'data_mammal', 'tree_zika_mol', 'tree_zika_time', 'tree_dinosaur_time', 'tree_dinosaur_morpho', 'data_dinosaur' to 'data/tree_lepidosaur_mol.rda', 'data/tree_mammal_morpho.rda', 'data/data_mammal.rda', 'data/tree_zika_mol.rda', 'data/tree_zika_time.rda', 'data/tree_dinosaur_time.rda', 'data/tree_dinosaur_morpho.rda', 'data/data_dinosaur.rda'
#> * Document your data (see 'https://r-pkgs.org/data.html')

# Document data ----
document()
#> Writing data_dinosaur.Rd
#> Writing data_mammal.Rd
#> Writing tree_dinosaur_morpho.Rd
#> Writing tree_dinosaur_time.Rd
#> Writing tree_lepidosaur_mol.Rd
#> Writing tree_mammal_morpho.Rd
#> Writing tree_zika_mol.Rd
#> Writing tree_zika_time.Rd

# Create vignette ----
use_vignette(name = "fallpaddy")
#> ✓ Setting active project to 'C:/Users/sadik/Documents/GitHub/fallpaddy'
#> ✓ Adding 'knitr' to Suggests field in DESCRIPTION
#> ✓ Setting VignetteBuilder field in DESCRIPTION to 'knitr'
#> ✓ Adding 'inst/doc' to '.gitignore'
#> ✓ Creating 'vignettes/'
#> ✓ Adding '*.html', '*.R' to 'vignettes/.gitignore'
#> ✓ Adding 'rmarkdown' to Suggests field in DESCRIPTION
#> ✓ Writing 'vignettes/fallpaddy.Rmd'
#> • Modify 'vignettes/fallpaddy.Rmd'
build_rmd()

# Create the package citation ----
use_citation()
#> ✔ Setting active project to 'C:/Users/sadik/Documents/GitHub/fallpaddy'
#> ✔ Creating 'inst/'
#> ✔ Writing 'inst/CITATION'
#> • Modify 'inst/CITATION'

# Check `fallpaddy` ----
check()
