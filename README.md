# Detecting Punctuated Evolution in Dinosaurs and Viruses

This package contains the tools to simulate, detect, and visualize punctuated
evolution in any clades, from co-occurring species (e.g., present-day mammals)
to serially-sampled viruses (e.g., SARS-CoV-2 genomes) and long-extinct taxa
(e.g., Mesozoic dinosaurs).

To install `fallpaddy`:
```r
library(devtools)
install_github("suryakevin/fallpaddy")
```

To use `fallpaddy`, load as usual:
```r
library(fallpaddy)
```

I customized `fallpaddy` to my preference, not yours. So, please don't hold me
accountable if you find yourself cursing out loud :innocent:. If you're curious
about how I string the functions together to analyze real-world data, please
check out the vignette for this package by navigating the directory above or:
```r
browseVignettes("fallpaddy")
```

You can see all of the functions available in this package:
```r
?fallpaddy
```

And to access documentation for a function, say the `sim_punc` function, for
example:
```r
?sim_punc
```

If you have questions, thoughts, or suggestions, you can email me at
[sadikin.kevin@gmail.com](mailto:sadikin.kevin@gmail.com) or open new issues or
pull requests (if you're comfortable with these `git` functionalities).

If you end up using this package (thanks!), please cite the following paper. If
it's still not published yet, you can use the bibliography format typical for
any other `R` packages.

> Surya, K., Gardner, J.D. & Organ, C. Detecting punctuated evolution in
> SARS-CoV-2 over the first year of the pandemic. In review.

You should also cite relevant publications from the Reading Evolutionary
Biology Group since they were the ones who established the foundational
statistical methods used here for detecting punctuated evolution. You know
which papers I mean if you've done your homework :wink:.

P.S. The package name comes from the first poem of the Ogura Hyakunin Isshu
(lit. "One Hundred Poets, One Poem [Each]"). All thanks go to *Chihayafuru* for
educating me on the beauty of Japanese poetry!

> The fall paddy shacks have rough thatching  
> as my sleeves are wet with dew.
