#' Dinosaur phenotypic trait dataset
#'
#' @description This dataset contains the body masses and clades of 402
#'   Mesozoic dinosaur taxa.
#'
#' @usage data_dinosaur
#'
#' @format A data frame with 402 rows and 2 variables:
#' \describe{
#'   \item{`log10_mass`}{Log10-transformed body mass estimate (kg);
#'     species average}
#'   \item{`clade`}{Ornithischia, Sauropodomorpha, or Theropoda}
#' }
#'
#' @note Benson et al. (2014) and Benson et al. (2017) predicted dinosaur adult
#'   body masses using estimated relationships between body mass and limb
#'   skeletal measurements across living tetrapods. See their manuscripts for
#'   more details.
#'
#' The original dataset contains 993 rows. I removed taxa without body mass
#'   estimates and those not present in the timetree.
#'
#' @source \href{https://doi.org/10.1371/journal.pbio.1001853}{Benson et al. (2014)};
#'   \href{https://doi.org/10.1111/pala.12329}{Benson et al. (2017)}
"data_dinosaur"

#' Mammal phenotypic trait dataset
#'
#' @description This dataset contains the body masses and foot postures of 880
#'   terrrestrial mammal species.
#'
#' @usage data_mammal
#'
#' @format A data frame with 880 rows and 2 variables:
#' \describe{
#'   \item{`log10_mass`}{Log10-transformed body mass (kg); species average}
#'   \item{`posture`}{Plantigrady (`P`: walking on the whole foot), digitigrady
#'     (`D`: walking on toes), or unguligrady (`U`: walking on hooved tiptoes)}
#' }
#'
#' @source \href{https://doi.org/10.1073/pnas.1814329116}{Kubo et al. (2019)}
"data_mammal"

#' Dinosaur phenotypic-scaled tree
#'
#' @description This phylogenetic tree, scaled to the amount of body mass
#'   evolution, comprises 402 Mesozoic dinosaur taxa.
#'
#' @usage tree_dinosaur_morpho
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Dinosauria (Mesozoic dinosaurs)
#'   \item __Number of tips:__ 402 dinosaur taxa
#'   \item __Data:__ Log10-transformed body mass estimate (kg); species average
#'   \item __Branch length unit:__ Original branch length unit (million years)
#'     \eqn{\times} unitless rate scalar \eqn{=} relative amount of body mass
#'     evolution
#'   \item __Inference method:__ Variable rates model (`BayesTraits`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @note Regarding the dinosaur timetree, Benson et al. (2014) constructed a
#'   composite cladogram. O'Donovan et al. (2018) acquired this cladogram,
#'   first and last appearance date estimates, and scaled the branch lengths
#'   using the 'mbl' method, enforcing a minimum branch length of one million
#'   years. The tree file is unavailable online, but is available upon request
#'   from the authors.
#'
#' The timetree from O'Donovan et al. (2018) contains 624 tips. I removed taxa
#'   not present in the body mass dataset. Also, I arbitrarily resolved each
#'   polytomy (or multifurcation) into a series of bifurcations with zero
#'   branch lengths. The resulting bifurcations follow the order in which the
#'   tips appear in the tree.
#'
#' To scale the dinosaur timetree so that the branch lengths reflect body mass
#'   evolution, I used the variable rates model
#'   \href{https://doi.org/10.1038/nature10516}{(Venditti et al. 2011)} in the
#'   program
#'   \href{http://www.evolution.reading.ac.uk/SoftwareMain.html}{`BayesTraits`}.
#'   This model allows the rates of body mass evolution to vary by stretching
#'   and compressing branches during a reversible-jump Markov chain Monte Carlo
#'   (RJMCMC) run. The final tree is a consensus, where each branch length is
#'   the average across the 5,000 MCMC posterior trees.
#'
#' @source \href{https://doi.org/10.1371/journal.pbio.1001853}{Benson et al. (2014)};
#'   \href{https://doi.org/10.1111/pala.12329}{Benson et al. (2017)};
#'   \href{https://doi.org/10.1038/s41559-017-0454-6}{O'Donovan et al. (2018)}
"tree_dinosaur_morpho"

#' Dinosaur timetree
#'
#' @description This time-calibrated phylogenetic tree comprises 402 Mesozoic
#'   dinosaur taxa.
#'
#' @usage tree_dinosaur_time
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Dinosauria (Mesozoic dinosaurs)
#'   \item __Number of tips:__ 402 dinosaur taxa
#'   \item __Data:__ Cladograms and first and last appearance date estimates
#'   \item __Branch length unit:__ Million years
#'   \item __Inference method:__ Minimum branch length (mbl) (`paleotree`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @note Regarding the dinosaur timetree, Benson et al. (2014) constructed a
#'   composite cladogram. O'Donovan et al. (2018) acquired this cladogram,
#'   first and last appearance date estimates, and scaled the branch lengths
#'   using the 'mbl' method, enforcing a minimum branch length of one million
#'   years. The tree file is unavailable online, but is available upon request
#'   from the authors.
#'
#' The timetree from O'Donovan et al. (2018) contains 624 tips. I removed taxa
#'   not present in the body mass dataset. Also, I arbitrarily resolved each
#'   polytomy (or multifurcation) into a series of bifurcations with zero
#'   branch lengths. The resulting bifurcations follow the order in which the
#'   tips appear in the tree.
#'
#' @source \href{https://doi.org/10.1371/journal.pbio.1001853}{Benson et al. (2014)};
#'   \href{https://doi.org/10.1111/pala.12329}{Benson et al. (2017)};
#'   \href{https://doi.org/10.1038/s41559-017-0454-6}{O'Donovan et al. (2018)}
"tree_dinosaur_time"

#' Lepidosaur molecular tree
#'
#' @description This molecular phylogenetic tree comprises 4,162 lepidosaur
#'   species.
#'
#' @usage tree_lepidosaur_mol
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Lepidosauria (mostly snakes and lizards)
#'   \item __Number of tips:__ 4,162 (4,161 squamate and 1 tuatara species)
#'   \item __Data:__ 12 genes (5 mtDNA and 7 nuclear); 12,896 bp
#'   \item __Branch length unit:__ Substitutions per site
#'   \item __Inference method:__ Maximum likelihood (`RAxML`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @note Pyron et al. (2013) focus on broad taxonomic sampling, resulting in
#'   a tree with a more extensive record of net speciation events than many
#'   other Lepidosauria trees. They found that the terminal branch lengths do
#'   not correlate with the completeness of the molecular data.
#'
#' @source \href{https://doi.org/10.1186/1471-2148-13-93}{Pyron et al. (2013)}
"tree_lepidosaur_mol"

#' Mammal phenotypic-scaled tree
#'
#' @description This phylogenetic tree, scaled to the amount of body mass
#'   evolution, comprises 880 terrestrial mammal species.
#'
#' @usage tree_mammal_morpho
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Mammalia
#'   \item __Number of tips:__ 880 terrestrial mammal species
#'   \item __Data:__ Log10-transformed body mass (kg); species average
#'   \item __Branch length unit:__ Original branch length unit (million years)
#'     \eqn{\times} unitless rate scalar \eqn{=} relative amount of body mass
#'     evolution
#'   \item __Inference method:__ Variable rates model (`BayesTraits`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @note The original mammal timetree contains polytomies (or multifurcations).
#'   I arbitrarily resolved each polytomy into a series of bifurcations with
#'   zero branch lengths. The resulting bifurcations follow the order in which
#'   the tips appear in the tree.
#'
#' To scale the mammal timetree so that the branch lengths reflect body mass
#'   evolution, I used the variable rates model
#'   \href{https://doi.org/10.1038/nature10516}{(Venditti et al. 2011)} in the
#'   program
#'   \href{http://www.evolution.reading.ac.uk/SoftwareMain.html}{`BayesTraits`}.
#'   This model allows the rates of body mass evolution to vary by stretching
#'   and compressing branches during a reversible-jump Markov chain Monte Carlo
#'   (RJMCMC) run. The final tree is a consensus, where each branch length is
#'   the average across the 10,000 MCMC posterior trees.
#'
#' @source \href{https://doi.org/10.1073/pnas.1814329116}{Kubo et al. (2019)}
"tree_mammal_morpho"

#' Zika virus molecular tree
#'
#' @description This molecular phylogenetic tree comprises 104 Zika virus
#'   genomes.
#'
#' @usage tree_zika_mol
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Zika virus (ZIKV)
#'   \item __Number of tips:__ 104 genomes (2013-2016; the Pacific, Brazil,
#'     other South and Central Americas, the Caribbean, and the United States)
#'   \item __Data:__ Complete genomes; 10,269 bases
#'   \item __Branch length unit:__ Substitutions per site
#'   \item __Inference method:__ Maximum likelihood (`PhyML`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @source \href{https://doi.org/10.1038/nature22400}{Grubaugh et al. (2017)}
"tree_zika_mol"

#' Zika virus timetree
#'
#' @description This time-calibrated phylogenetic tree comprises 104 Zika virus
#'   genomes.
#'
#' @usage tree_zika_time
#'
#' @format An object of class `phylo` of length 4
#'
#' @details
#' \itemize{
#'   \item __Clade:__ Zika virus (ZIKV)
#'   \item __Number of tips:__ 104 genomes (2013-2016; the Pacific, Brazil,
#'     other South and Central Americas, the Caribbean, and the United States)
#'   \item __Data:__ Complete genomes; 10,269 bases
#'   \item __Branch length unit:__ Years
#'   \item __Inference method:__ Relaxed molecular clock + Bayesian skyline
#'     (`BEAST`)
#'   \item __Rooted?__ Yes
#'   \item __Binary or fully-bifurcating?__ Yes
#' }
#'
#' @note The timetree is a maximum clade credibility tree.
#'
#' @source \href{https://doi.org/10.1038/nature22400}{Grubaugh et al. (2017)}
"tree_zika_time"
