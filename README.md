This is the R package version of **Mendak**, available online on a [huma-num shiny server](https://analytics.huma-num.fr/Mathieu.Ferry/mendak/).

To install and use on your local computer, open R and type:

```
install.packages("remotes") 
library(remotes) 
remotes::install_github("MathieuFerry/Mendak-package") 
library(shiny)
library(mendak) 
mendak::run_app()
```
