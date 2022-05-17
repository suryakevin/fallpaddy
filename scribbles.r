# Written by Kevin Surya

# This scribble file contains some of my unsuccessful attempts and tuning
#   efforts at making the `sim_punc` function.

# Trial #1 ====================================================================

# I tried simulating node count by drawing from a Poisson distribution, but
#   this approach does not work. First, the node count may not come from a
#   univariate Poisson distribution. The correct distribution may be
#   multivariate, appropriate for modeling non-independence. Second, the
#   distribution must have some conditions that specify, for example, that
#   sister taxa in the phylogenetic tree must have the same node counts. So, my
#   solution is to simulate a random tree and extract the node count from that
#   tree.

# Load packages and functions ----
library(ape)
library(ggplot2)
library(ggthemes)
source("R/get_node_count.r")

# Estimate a function for the expected node count given the size of a
#   fully-bifurcating tree ----
n <- c(10, 100, 1000, 10000)  # number of tips
E_node <- NULL  # expected node count
set.seed(0)
for (i in 1:length(n)) {  # for each number of tips
  trees <- rmtree(N = 100, n = n[i])  # 100 reps/trees
  node <- NULL
  for (j in 1:100) {  # for each random tree
    node[j] <- get_node_count(tree = trees[[j]])
  }
  E_node[i] <- mean(node)
}
data <- data.frame(E_node = E_node, n = n)
data
#>   E_node     n
#> 1   1.83    10
#> 2   4.00   100
#> 3   6.51  1000
#> 4   8.33 10000
plot(data$E_node ~ data$n)  # check the relationship
model <- lm(E_node ~ log(n), data = data)
model
#>
#> Call:
#> lm(formula = E_node ~ log(n), data = data)
#>
#> Coefficients:
#> (Intercept)       log(n)
#>     -0.3350       0.9559
#>

# Visualize the estimated function ----
ggplot(data = data, aes(x = n, y = E_node)) +
  geom_point() +
  stat_function(
    fun = function(n) {
      model$coefficients[[1]] + (model$coefficients[[2]] * log(n))
    }
  ) +
  theme_minimal() +
  labs(x = "Number of tips", y = "Expected node count")

# Simulate node count using a Poisson distribution ----
n <- 100
lambda <- model$coefficients[[1]] + (model$coefficients[[2]] * log(n))
set.seed(1)
node <- rpois(n = n, lambda = lambda)
mean(node)
#> [1] 4.13
hist(node)
node
#>  [1]  3  3  4  7  2  7  7  5  5  1  2  2  5  3  5  4  5 10  3  6  7  2  5  2  3
#> [26]  3  0  3  6  3  4  4  4  2  6  5  6  2  5  3  6  5  6  4  4  6  1  4  5  5
#> [51]  4  6  4  3  1  2  3  4  5  3  7  3  4  3  5  3  4  5  1  6  3  6  3  3  4
#> [76]  7  6  3  6  8  4  5  3  3  5  2  5  2  3  2  3  1  5  6  6  6  4  3  6  4

# It is impossible to have only one taxon with a node count of ten.

# Trial #2 ====================================================================

# I tried to simulate the node count from a random tree and path length from a
#   multivariate normal distribution. I created a matrix enlisting the number
#   of shared nodes between taxa with the intent to use it as the foundation of
#   the variance-covariance matrix when drawing from the multivariate
#   distribution. But, this matrix is not positive definite.

# Load packages ----
library(ape)
library(Matrix)
source("R/get_node_count.r")
source("R/get_path_length.r")

# Specify inputs ----
n <- 5
beta1 <- 1.25
error <- 0.01

# Generate a random tree to simulate node count ----
set.seed(0)
tree <- rtree(n = n)
#>
#> Phylogenetic tree with 5 tips and 4 internal nodes.
#>
#> Tip labels:
#>   t4, t3, t1, t2, t5
#>
#> Rooted; includes branch lengths.
plot(x = tree)
nodelabels()
node <- get_node_count(tree = tree)
#> t4 t3 t1 t2 t5
#>  1  1  1  2  2

# Creates a similarity matrix based on the number of shared nodes ----
node_mat <- diag(node)
node_mat
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    0    0    0    0
#> [2,]    0    1    0    0    0
#> [3,]    0    0    1    0    0
#> [4,]    0    0    0    2    0
#> [5,]    0    0    0    0    2
node_path <- nodepath(phy = tree)
node_path
#> [[1]]
#> [1] 6 7 1
#>
#> [[2]]
#> [1] 6 7 2
#>
#> [[3]]
#> [1] 6 8 3
#>
#> [[4]]
#> [1] 6 8 9 4
#>
#> [[5]]
#> [1] 6 8 9 5
#>
node_path <- lapply(node_path, function(x) {x[-1]})  # removes the root node
node_path
#> [[1]]
#> [1] 7 1
#>
#> [[2]]
#> [1] 7 2
#>
#> [[3]]
#> [1] 8 3
#>
#> [[4]]
#> [1] 8 9 4
#>
#> [[5]]
#> [1] 8 9 5
#>
for (row_i in 2:dim(node_mat)[1]) {
  max_j <- row_i - 1
  for (col_j in 1:max_j) {
    node_mat[row_i, col_j] <- length(
      intersect(node_path[[row_i]], node_path[[col_j]])
    )
  }
}
node_mat <- as.matrix(forceSymmetric(x = node_mat, uplo = "L"))
rownames(node_mat) <- colnames(node_mat) <- names(node)
node_mat
#>    t4 t3 t1 t2 t5
#> t4  1  1  0  0  0
#> t3  1  1  0  0  0
#> t1  0  0  1  1  1
#> t2  0  0  1  2  2
#> t5  0  0  1  2  2

# Simulate path length ----
vcv <- node_mat * beta1
vcv
#>      t4   t3   t1   t2   t5
#> t4 1.25 1.25 0.00 0.00 0.00
#> t3 1.25 1.25 0.00 0.00 0.00
#> t1 0.00 0.00 1.25 1.25 1.25
#> t2 0.00 0.00 1.25 2.50 2.50
#> t5 0.00 0.00 1.25 2.50 2.50
vcv_n <- dim(vcv)[1] * dim(vcv)[2]  # 5 x 5
vcv_n
#> [1] 25
set.seed(1)
vcv <- vcv + abs(rnorm(n = vcv_n, mean = 0, sd = error))
vcv <- as.matrix(forceSymmetric(x = vcv, uplo = "L"))
round(vcv, 3)
#>       t4    t3    t1    t2    t5
#> t4 1.263 1.254 0.017 0.032 0.007
#> t3 1.254 1.260 0.015 0.012 0.006
#> t1 0.017 0.015 1.262 1.294 1.272
#> t2 0.032 0.012 1.294 2.516 2.512
#> t5 0.007 0.006 1.272 2.512 2.512

# Generate tree ----
tree <- vcv2phylo(mat = vcv)
#> Error in vcv2phylo(mat = vcv) : Matrix is not positive-definite

# Trial #3 ====================================================================

# This trial stems from the need to translate a given scenario of punctuated
#   evolution to the kappa value used to transform the simulated tree.

# Load packages and functions ----
library(ape)
library(betareg)
library(Cairo)
library(GGally)
library(ggplot2)
library(ggthemes)
library(motmot)
library(nlme)
library(phytools)
source("R/create_dmat.r")
source("R/decomp_vcv.r")
source("R/est_punc_contrib.r")
source("R/export_plot.r")
source("R/get_node_count.r")
source("R/get_path_length.r")
source("R/fit_punc_model.r")

# Estimate the node count coefficient for a given kappa value ----
kappa <- seq(from = 0.001, to = 2.001, by = 0.1)
kappa  # avoids kappa = 0.000
#>  [1] 0.001 0.101 0.201 0.301 0.401 0.501 0.601 0.701 0.801 0.901 1.001 1.101
#> [13] 1.201 1.301 1.401 1.501 1.601 1.701 1.801 1.901 2.001
kappa <- c(kappa, 0.021, 0.041, 0.061, 0.081, 0.921, 0.941, 0.961, 0.981, 0.999)
  # adds more values near zero and one
kappa <- kappa[order(kappa)]
kappa
#>  [1] 0.001 0.021 0.041 0.061 0.081 0.101 0.201 0.301 0.401 0.501 0.601 0.701
#> [13] 0.801 0.901 0.921 0.941 0.961 0.981 0.999 1.001 1.101 1.201 1.301 1.401
#> [25] 1.501 1.601 1.701 1.801 1.901 2.001
beta_node_extant <- beta_node_fossil <- beta_time_fossil <-
  punc_contrib_extant <- punc_contrib_fossil <- length_tree_fossil <-
  vector(mode = "list", length = length(kappa))
set.seed(0)
for (i in 1:length(kappa)) {  # for each kappa value
  for (j in 1:100) {  # for each random timetree
    birth <- runif(n = 1, min = 0, max = 5)
    death <- runif(n = 1, min = 0, max = birth)
    tree_time_extant <- rphylo(  # ultrametric timetree
      n = 100,
      birth = birth,
      death = death,
      fossils = FALSE
    )
    birth <- runif(n = 1, min = 1, max = 5)
    death <- birth - 0.15
    tree_time_fossil <- rphylo(  # non-ultrametric timetree
      n = 100,
      birth = birth,
      death = death,
      fossils = TRUE
    )
    fossil_path <- diag(vcv(phy = tree_time_fossil))
    extant_keep <- names(fossil_path[fossil_path < max(fossil_path)])
    tree_time_fossil <- keep.tip(phy = tree_time_fossil, tip = extant_keep)
      # removes extant taxa
    if (length(tree_time_fossil$tip.label) > 100) {
      taxon_keep <- sample(x = tree_time_fossil$tip.label, size = 100)
      tree_time_fossil <- keep.tip(phy = tree_time_fossil, tip = taxon_keep)
    }
    # Codes below adapted from http://blog.phytools.org/2012/02/quicker-way-to-rescale-total-length-of.html
    tree_time_extant$edge.length <-
      tree_time_extant$edge.length /
        max(nodeHeights(tree = tree_time_extant)[, 2])
          # scales tree so that its total depth equals one
    tree_time_fossil$edge.length <-
      tree_time_fossil$edge.length /
        max(nodeHeights(tree = tree_time_fossil)[, 2])
    tree_evol_extant <- transformPhylo(  # kappa transform
      phy = tree_time_extant,  # molecular/morphological tree
      model = "kappa",
      kappa = kappa[i]
    )
    tree_evol_fossil <- transformPhylo(
      phy = tree_time_fossil,  # molecular/morphological tree
      model = "kappa",
      kappa = kappa[i]
    )
    tree_evol_extant$edge.length <-
      tree_evol_extant$edge.length /
        max(nodeHeights(tree = tree_evol_extant)[, 2])
    tree_evol_fossil$edge.length <-
      tree_evol_fossil$edge.length /
        max(nodeHeights(tree = tree_evol_fossil)[, 2])
    path_extant <- get_path_length(tree = tree_evol_extant)
    path_fossil <- get_path_length(tree = tree_evol_fossil)
    node_extant <- get_node_count(tree = tree_evol_extant)
    node_fossil <- get_node_count(tree = tree_evol_fossil)
    time_extant <- get_path_length(tree = tree_time_extant)
    time_fossil <- get_path_length(tree = tree_time_fossil)
    data_extant <- data.frame(
      path = path_extant,
      node = node_extant,
      time = time_extant
    )
    data_fossil <- data.frame(
      path = path_fossil,
      node = node_fossil,
      time = time_fossil
    )
    vcv_parts_extant <- decomp_vcv(tree = tree_evol_extant)
    vcv_parts_fossil <- decomp_vcv(tree = tree_evol_fossil)
    D_extant <- create_dmat(tree = tree_evol_extant)
    D_fossil <- create_dmat(tree = tree_evol_fossil)
    model_extant <- fit_punc_model(  # path ~ node
      data = data_extant,
      vcv_parts = vcv_parts_extant,
      D = D_extant,
      model = "pn"
    )
    model_fossil <- fit_punc_model(  # path ~ time + node
      data = data_fossil,
      vcv_parts = vcv_parts_fossil,
      D = D_fossil,
      model = "ptn"
    )
    beta_node_extant[[i]][j] <- model_extant$model$coefficients[[2]]
    beta_node_fossil[[i]][j] <- model_fossil$model$coefficients[[3]]
    beta_time_fossil[[i]][j] <- model_fossil$model$coefficients[[2]]
    punc_contrib_extant[[i]][j] <- est_punc_contrib(
      tree = tree_evol_extant,
      output_reg = model_extant
    )
    punc_contrib_fossil[[i]][j] <- est_punc_contrib(
      tree = tree_evol_fossil,
      output_reg = model_fossil
    )
    length_tree_fossil[[i]][j] <- length(tree_evol_fossil$tip.label)
  }
  print(paste0("finished round #", i, "..."))
}

# Check the number of taxa in the simulated fossil trees ----
length_tree_fossil <- unlist(length_tree_fossil)
unique(length_tree_fossil)
#> [1] 100

# Prepare datasets ----
kappa_vec <- rep(kappa, 100)
kappa_vec <- kappa_vec[order(kappa_vec)]
beta_node_extant <- unlist(beta_node_extant)
beta_node_fossil <- unlist(beta_node_fossil)
beta_time_fossil <- unlist(beta_time_fossil)
punc_contrib_extant <- unlist(punc_contrib_extant)
punc_contrib_fossil <- unlist(punc_contrib_fossil)
data_sim <- data.frame(
  kappa = kappa_vec,
  beta_node_extant = beta_node_extant,
  beta_node_fossil = beta_node_fossil,
  beta_time_fossil = beta_time_fossil,
  punc_contrib_extant = punc_contrib_extant,
  punc_contrib_fossil = punc_contrib_fossil
)
head(data_sim)
#>   kappa beta_node_extant beta_node_fossil beta_time_fossil punc_contrib_extant
#> 1 0.001       0.08324025       0.02699976     0.0009486853           0.9988679
#> 2 0.001       0.07687051       0.03701109     0.0008752444           0.9991192
#> 3 0.001       0.06245772       0.03568766     0.0008395260           0.9991156
#> 4 0.001       0.08326116       0.04541629     0.0007640035           0.9989770
#> 5 0.001       0.07137887       0.02775784     0.0007066797           0.9991111
#> 6 0.001       0.06663403       0.03224071     0.0007426631           0.9989300
#>   punc_contrib_fossil
#> 1           0.9991592
#> 2           0.9990498
#> 3           0.9990113
#> 4           0.9990785
#> 5           0.9991561
#> 6           0.9988365

# Visualize the relationships between parameters ----
ggpairs(
  data = data_sim,
  columnLabels = c(
    "Kappa",
    "Slope - Node\n(Extant)",
    "Slope - Node\n(Fossil)",
    "Slope - Time\n(Fossil)",
    "Punc. Contrib.\n(Extant)",
    "Punc. Contrib.\n(Fossil)"
  ),
  upper = list(continuous = wrap("points", alpha = 0.15)),
  lower = list(continuous = wrap("points", alpha = 0.15))
) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      linetype = "dashed",
      colour = "gray",
      fill = NA
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank()
  )
plot1 <- ggplot(data = data_sim, aes(x = kappa, y = beta_node_extant)) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Kappa",
    y = "Node count coefficient",
    title = "Extant-only dataset"
  )

# When all the taxa in the dataset are extant, the estimated node count
#   coefficient in the path length regression (i.e., slope) decreases with
#   increasing kappa (0: punctuational; 1: gradual). This relationship is as
#   expected given the interpretation of kappa. However, as kappa increases
#   beyond one, the relationship between kappa and the node count coefficient
#   becomes a flat line.

plot2 <- ggplot(data = data_sim, aes(x = kappa, y = beta_node_fossil)) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Kappa",
    y = "Node count coefficient",
    title = "Fossil-only dataset"
  )

# When all the taxa in the dataset are fossils, the estimated node count
#   coefficient in the path length regression (i.e., slope) decreases with
#   increasing kappa (0: punctuational; 1: gradual). This relationship is as
#   expected given the interpretation of kappa. However, as kappa increases
#   beyond one, the relationship between kappa and the node count coefficient
#   becomes a flat line.

plot3 <- ggplot(data = data_sim, aes(x = kappa, y = beta_time_fossil)) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Kappa",
    y = "Sampling time coefficient",
    title = "Fossil-only dataset"
  )

# When all the taxa in the dataset are fossils, the estimated sampling time
#   coefficient (or tip date coefficient) in the path length regression (i.e.,
#   slope) increases with increasing kappa (0: punctuational; 1: gradual). This
#   relationship is as expected given the interpretation of kappa. However, as
#   kappa goes beyond one, the "effect" of sampling time degrades and varies
#   more. When kappa is much higher than one, the tree is transformed to the
#   point where neither node count nor sampling time explains the path length.

plot4 <- ggplot(
  data = data_sim,
  aes(x = beta_node_fossil, y = beta_time_fossil)
) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Node count coefficient",
    y = "Sampling time coefficient",
    title = "Fossil-only dataset"
  )

# Based on the last two plots, it makes sense that the node count and sampling
#   time coefficients are inversely related. Again, the exception is when the
#   node count coefficient is negative.

plot5 <- ggplot(data = data_sim, aes(x = kappa, y = punc_contrib_extant)) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Kappa",
    y = "Punctuational contribution to total tree length",
    title = "Extant-only dataset"
  )

# When all the taxa in the dataset are extant, the estimated proportion of the
#   total tree length attributable to punctuational effects decreases with
#   increasing kappa. This relationship is as expected given the interpretation
#   of kappa. The variance in the punctuational contribution decreases as kappa
#   approaches zero (punctuational) or one (gradual). This variance
#   heteroskedasticity is different from those in the first two plots.

plot6 <- ggplot(data = data_sim, aes(x = kappa, y = punc_contrib_fossil)) +
  geom_point(alpha = 0.15) +
  theme_minimal() +
  labs(
    x = "Kappa",
    y = "Punctuational contribution to total tree length",
    title = "Fossil-only dataset"
  )

export_plot_pdf(plot = plot1, file = "plot1.pdf")
export_plot_pdf(plot = plot2, file = "plot2.pdf")
export_plot_pdf(plot = plot3, file = "plot3.pdf")
export_plot_pdf(plot = plot4, file = "plot4.pdf")
export_plot_pdf(plot = plot5, file = "plot5.pdf", width = 9.5, height = 5.87)
export_plot_pdf(plot = plot6, file = "plot6.pdf", width = 9.5, height = 5.87)

# Remove rows where kappa is higher than one ----
data_sim <- data_sim[!data_sim$kappa > 1, ]

# Model the relationships between parameters ----
# See https://doi.org/10.1080/0266476042000214501 and
#   https://cran.r-project.org/web/packages/betareg/vignettes/betareg.pdf
#   regarding the beta regression
model_betareg_extant <- betareg(
  formula = kappa ~ beta_node_extant,
  data = data_sim,
  link = "logit"
)
summary(model_betareg_extant)
#>
#> Call:
#> betareg(formula = kappa ~ beta_node_extant, data = data_sim, link = "logit")
#>
#> Standardized weighted residuals 2:
#>     Min      1Q  Median      3Q     Max
#> -8.6299 -0.4912  0.0278  0.5960  2.5986
#>
#> Coefficients (mean model with logit link):
#>                  Estimate Std. Error z value Pr(>|z|)
#> (Intercept)        2.7365     0.0296   92.43   <2e-16 ***
#> beta_node_extant -74.9439     0.6922 -108.27   <2e-16 ***
#>
#> Phi coefficients (precision model with identity link):
#>       Estimate Std. Error z value Pr(>|z|)
#> (phi)  15.1519     0.5108   29.66   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Type of estimator: ML (maximum likelihood)
#> Log-likelihood:  2854 on 3 Df
#> Pseudo R-squared: 0.8203
#> Number of iterations: 14 (BFGS) + 3 (Fisher scoring)
model_betareg_fossil <- betareg(
  formula = kappa ~ beta_node_fossil,
  data = data_sim,
  link = "logit"
)
summary(model_betareg_fossil)
#>
#> Call:
#> betareg(formula = kappa ~ beta_node_fossil, data = data_sim, link = "logit")
#>
#> Standardized weighted residuals 2:
#>     Min      1Q  Median      3Q     Max
#> -8.5963 -0.3887  0.0711  0.6552  2.3358
#>
#> Coefficients (mean model with logit link):
#>                    Estimate Std. Error z value Pr(>|z|)
#> (Intercept)         2.52209    0.03226   78.19   <2e-16 ***
#> beta_node_fossil -121.20618    1.29826  -93.36   <2e-16 ***
#>
#> Phi coefficients (precision model with identity link):
#>       Estimate Std. Error z value Pr(>|z|)
#> (phi)  10.4346     0.3582   29.13   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Type of estimator: ML (maximum likelihood)
#> Log-likelihood:  2440 on 3 Df
#> Pseudo R-squared: 0.7757
#> Number of iterations: 15 (BFGS) + 6 (Fisher scoring)
model_betareg_extant2 <- betareg(
  formula = kappa ~ punc_contrib_extant,
  data = data_sim,
  link = "logit"
)
summary(model_betareg_extant2)
#>
#> Call:
#> betareg(formula = kappa ~ punc_contrib_extant, data = data_sim, link = "logit")
#>
#> Standardized weighted residuals 2:
#>     Min      1Q  Median      3Q     Max
#> -2.1529 -0.5417  0.1000  0.6289  3.3800
#>
#> Coefficients (mean model with logit link):
#>                     Estimate Std. Error z value Pr(>|z|)
#> (Intercept)          2.75623    0.02550   108.1   <2e-16 ***
#> punc_contrib_extant -5.91520    0.04711  -125.6   <2e-16 ***
#>
#> Phi coefficients (precision model with identity link):
#>       Estimate Std. Error z value Pr(>|z|)
#> (phi)  21.1546     0.7044   30.03   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Type of estimator: ML (maximum likelihood)
#> Log-likelihood:  3196 on 3 Df
#> Pseudo R-squared: 0.8449
#> Number of iterations: 14 (BFGS) + 3 (Fisher scoring)
model_betareg_fossil2 <- betareg(
  formula = kappa ~ punc_contrib_fossil,
  data = data_sim,
  link = "logit"
)
summary(model_betareg_fossil2)
#>
#> Call:
#> betareg(formula = kappa ~ punc_contrib_fossil, data = data_sim, link = "logit")
#>
#> Standardized weighted residuals 2:
#>     Min      1Q  Median      3Q     Max
#> -2.2145 -0.5662  0.1504  0.6466  3.4039
#>
#> Coefficients (mean model with logit link):
#>                     Estimate Std. Error z value Pr(>|z|)
#> (Intercept)          2.83029    0.02492   113.6   <2e-16 ***
#> punc_contrib_fossil -6.04651    0.04613  -131.1   <2e-16 ***
#>
#> Phi coefficients (precision model with identity link):
#>       Estimate Std. Error z value Pr(>|z|)
#> (phi)  23.4199     0.7781    30.1   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Type of estimator: ML (maximum likelihood)
#> Log-likelihood:  3290 on 3 Df
#> Pseudo R-squared: 0.8517
#> Number of iterations: 13 (BFGS) + 1 (Fisher scoring)

# Visualize the relationships again ----
ggpairs(
  data = data_sim,
  columnLabels = c(
    "Kappa",
    "Slope - Node\n(Extant)",
    "Slope - Node\n(Fossil)",
    "Slope - Time\n(Fossil)",
    "Punc. Contrib.\n(Extant)",
    "Punc. Contrib.\n(Fossil)"
  ),
  upper = list(continuous = GGally::wrap(funcVal = ggally_cor, stars = FALSE)),
  lower = list(continuous = wrap("points", alpha = 0.15))
) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      linetype = "dashed",
      colour = "gray",
      fill = NA
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank()
  )
plot1a <- ggplot(data = data_sim, aes(x = beta_node_extant, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(beta_node_extant) {
      plogis(
        model_betareg_extant$coefficients$mean[[1]] +
        model_betareg_extant$coefficients$mean[[2]] * beta_node_extant
      )
    }
  ) +
  theme_minimal() +
  labs(
    x = "Node count coefficient",
    y = "Kappa",
    title = "Extant-only dataset"
  )

# Note that the scope of the model inference goes from kappa = 0.01 to
#   kappa = 0.99. So, the actual model is probably a mixture of the model like
#   the one above, kappa = 1 when the coefficient = 0, and kappa = 0 for the
#   maximum value of the node count coefficient.

plot2a <- ggplot(data = data_sim, aes(x = beta_node_fossil, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(beta_node_fossil) {
      plogis(
        model_betareg_fossil$coefficients$mean[[1]] +
        model_betareg_fossil$coefficients$mean[[2]] * beta_node_fossil
      )
    }
  ) +
  theme_minimal() +
  labs(
    x = "Node count coefficient",
    y = "Kappa",
    title = "Fossil-only dataset"
  )
plot5a <- ggplot(data = data_sim, aes(x = punc_contrib_extant, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(punc_contrib_extant) {
      plogis(
        model_betareg_extant2$coefficients$mean[[1]] +
        model_betareg_extant2$coefficients$mean[[2]] * punc_contrib_extant
      )
    }
  ) +
  theme_minimal() +
  labs(
    x = "Punctuational contribution to total tree length",
    y = "Kappa",
    title = "Extant-only dataset"
  )
plot6a <- ggplot(data = data_sim, aes(x = punc_contrib_fossil, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(punc_contrib_fossil) {
      plogis(
        model_betareg_fossil2$coefficients$mean[[1]] +
        model_betareg_fossil2$coefficients$mean[[2]] * punc_contrib_fossil
      )
    }
  ) +
  theme_minimal() +
  labs(
    x = "Punctuational contribution to total tree length",
    y = "Kappa",
    title = "Fossil-only dataset"
  )

# The fitted lines seem off. Perhaps, a beta regression is not a good choice.

# Model the relationships again ----
data_sim$punc_contrib_extant2 <- data_sim$punc_contrib_extant^2
data_sim$punc_contrib_extant3 <- data_sim$punc_contrib_extant^3
model_curve_extant <- lm(
  formula = kappa ~ punc_contrib_extant + punc_contrib_extant2 +
                    punc_contrib_extant3,
  data = data_sim
)
summary(model_curve_extant)
#>
#> Call:
#> lm(formula = kappa ~ punc_contrib_extant + punc_contrib_extant2 +
#>     punc_contrib_extant3, data = data_sim)
#>
#> Residuals:
#>       Min        1Q    Median        3Q       Max
#> -0.102935 -0.006732  0.001781  0.008218  0.071448
#>
#> Coefficients:
#>                        Estimate Std. Error t value Pr(>|t|)
#> (Intercept)           0.9917222  0.0009736 1018.58   <2e-16 ***
#> punc_contrib_extant  -1.5558826  0.0131016 -118.75   <2e-16 ***
#> punc_contrib_extant2  1.1931311  0.0340746   35.02   <2e-16 ***
#> punc_contrib_extant3 -0.6306910  0.0228263  -27.63   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Residual standard error: 0.01958 on 1896 degrees of freedom
#> Multiple R-squared:  0.9973,    Adjusted R-squared:  0.9973
#> F-statistic: 2.361e+05 on 3 and 1896 DF,  p-value: < 2.2e-16
#>
data_sim$punc_contrib_fossil2 <- data_sim$punc_contrib_fossil^2
data_sim$punc_contrib_fossil3 <- data_sim$punc_contrib_fossil^3
model_curve_fossil <- lm(
  formula = kappa ~ punc_contrib_fossil + punc_contrib_fossil2 +
                    punc_contrib_fossil3,
  data = data_sim
)
summary(model_curve_fossil)
#>
#> Call:
#> lm(formula = kappa ~ punc_contrib_fossil + punc_contrib_fossil2 +
#>     punc_contrib_fossil3, data = data_sim)
#>
#> Residuals:
#>       Min        1Q    Median        3Q       Max
#> -0.064581 -0.005222  0.000622  0.004836  0.076226
#>
#> Coefficients:
#>                        Estimate Std. Error t value Pr(>|t|)
#> (Intercept)           0.9978070  0.0007231 1379.89   <2e-16 ***
#> punc_contrib_fossil  -1.4586438  0.0094315 -154.66   <2e-16 ***
#> punc_contrib_fossil2  0.8925897  0.0243903   36.60   <2e-16 ***
#> punc_contrib_fossil3 -0.4323218  0.0163264  -26.48   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Residual standard error: 0.01395 on 1896 degrees of freedom
#> Multiple R-squared:  0.9986,    Adjusted R-squared:  0.9986
#> F-statistic: 4.653e+05 on 3 and 1896 DF,  p-value: < 2.2e-16
#>

# Visualize the relationships again ----
plot5b <- ggplot(data = data_sim, aes(x = punc_contrib_extant, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(punc_contrib_extant) {
      model_curve_extant$coefficients[[1]] +
      model_curve_extant$coefficients[[2]] * punc_contrib_extant +
      model_curve_extant$coefficients[[3]] * punc_contrib_extant^2 +
      model_curve_extant$coefficients[[4]] * punc_contrib_extant^3
    }
  ) +
  theme_minimal() +
  labs(
    x = "Punctuational contribution to total tree length",
    y = "Kappa",
    title = "Extant-only dataset"
  )
plot6b <- ggplot(data = data_sim, aes(x = punc_contrib_fossil, y = kappa)) +
  geom_point(alpha = 0.15) +
  stat_function(
    color = "red",
    size = 0.5,
    fun = function(punc_contrib_fossil) {
      model_curve_fossil$coefficients[[1]] +
      model_curve_fossil$coefficients[[2]] * punc_contrib_fossil +
      model_curve_fossil$coefficients[[3]] * punc_contrib_fossil^2 +
      model_curve_fossil$coefficients[[4]] * punc_contrib_fossil^3
    }
  ) +
  theme_minimal() +
  labs(
    x = "Punctuational contribution to total tree length",
    y = "Kappa",
    title = "Fossil-only dataset"
  )

# Much better!

# Check how the punctuational contributions estimated from the unscaled and
#   scaled trees connect ----
set.seed(1)
tree_time <- rphylo(n = 100, birth = 1, death = 0.85, fossils = TRUE)
path_time <- diag(vcv(phy = tree_time))
extant_keep <- names(path_time[path_time < max(path_time)])
tree_time <- keep.tip(phy = tree_time, tip = extant_keep)
if (length(tree_time$tip.label) > 100) {
  taxon_keep <- sample(x = tree_time$tip.label, size = 100)
  tree_time <- keep.tip(phy = tree_time, tip = taxon_keep)
}
tree_time_scaled <- tree_time
tree_time_scaled$edge.length <-
  tree_time_scaled$edge.length / max(nodeHeights(tree = tree_time_scaled)[, 2])
tree_evol <- transformPhylo(phy = tree_time, model = "kappa", kappa = kappa)
kappa <- 0.25
tree_evol_scaled <- transformPhylo(
  phy = tree_time_scaled,
  model = "kappa",
  kappa = kappa
)
path <- get_path_length(tree = tree_evol)
path_scaled <- get_path_length(tree = tree_evol_scaled)
node <- get_node_count(tree = tree_evol)
node_scaled <- get_node_count(tree = tree_evol_scaled)
time <- get_path_length(tree = tree_time)
time_scaled <- get_path_length(tree = tree_time_scaled)
data <- data.frame(path = path, node = node, time = time)
data_scaled <- data.frame(
  path = path_scaled,
  node = node_scaled,
  time = time_scaled
)
vcv_parts <- decomp_vcv(tree = tree_evol)
vcv_parts_scaled <- decomp_vcv(tree = tree_evol_scaled)
D <- create_dmat(tree = tree_evol)
D_scaled <- create_dmat(tree = tree_evol_scaled)
model <- fit_punc_model(
  data = data,
  vcv_parts = vcv_parts,
  D = D,
  model = "ptn"
)
model_scaled <- fit_punc_model(
  data = data_scaled,
  vcv_parts = vcv_parts_scaled,
  D = D_scaled,
  model = "ptn"
)
beta_node <- model$model$coefficients[[3]]
beta_node_scaled <- model_scaled$model$coefficients[[3]]
punc_contrib <- est_punc_contrib(tree = tree_evol, output_reg = model)
punc_contrib_scaled <- est_punc_contrib(
  tree = tree_evol_scaled,
  output_reg = model_scaled
)
c(punc_contrib, punc_contrib_scaled)
#> [1] 0.7274516 0.7274516

# The estimated proportion of total tree length attributable to punctuational
#   effects does not depend on the tree depth.
