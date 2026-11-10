## How to install `nswtools`
The package can be installed from GitHub using `devtools` or similar packages, which you may need to install first.

```{r installation, eval = FALSE}
# install.packages('devtools')
devtools::install_github("tgoddard-tsy/nswtools")

# Or remotes
remotes::install_github("tgoddard-tsy/nswtools")

# Or pak
pak::pkg_install("tgoddard-tsy/nswtools")
```

After installing the package, you can load it like any other package.

```{r load-packages}
library(tidyverse)
library(nswtheme)
library(nswtools)
```
