Cheatsheet related to SY09 lab projects
=================================

<!-- TOC -->

- [Variables analysis](#variables-analysis)
    - [Basics](#basics)
    - [Tests](#tests)
        - [Summary](#summary)
        - [Exemples](#exemples)
    - [Plots](#plots)
- [PCA (Principal Components Analysis)](#pca-principal-components-analysis)
    - [Main command](#main-command)
    - [Other commands](#other-commands)

<!-- /TOC -->


# Variables analysis

## Basics
- `head(data)` : displays the first rows of the dataset
- `summary(data)` : displays a summary (mean, variance, etc.) for each dimension of the dataset

## Tests

### Summary
| Comparaison type             | Method to use                         |
|------------------------------|---------------------------------------|
| Quantitative vs quantitative | Correlation: _Pearson_ or  _Spearman_ |
| Quantitative vs qualitative  | Anova/Acnova Test                     |
| Qualitative vs qualitative   | $χ^2$ Test                            |

### Exemples 

- `cor.test(data$col1, data$col2, method = "pearson")`
- `model = lm(data$col1 ~ 1 + data$col2)` and then `anova(model)`
- `chisq.test(data$col1, data$col2)`

For each of those ↑ a _p-value_ is returned which indicates if where wrong to assume what is tested. 

Also, to get the correlation matrix of the data, use : `cor(data)`
## Plots

- Multivariate, quantitative, bi-variate analysis : `plot(iris[1:4], ch=21, bg=c("red", "green", "blue")[as.numeric(iris$Species)])`
- boxplot : `boxplot(data)`
- Boxplot for comparaison of one dimension against the modularity of another variable : `plot(crabs[,1]~crabs$sex, xlab = "sex", ylab = cols[i])`

# PCA (Principal Components Analysis)

## Main command

```R
PCA = princomp(data, cor = TRUE, scores = TRUE)
```
- `cor` : should `data` be centered before analysis.
- `scores` : should `princomp` return the content of `data` projected onto the components.  


## Other commands

- `summary(PCA)` : display the importance of components (standard deviation, proportion of variance, cumulative proportion of variance)
- `as.data.frame(ACP$loadings[,1:ncols(data)])` : to retreive the eigen vectors associated with the components.
- `biplot(PCA)` : quick plot with initial dimensions vectors and `scores` reprensented in the plane (1st component, 2nd component).
- `plot(PCA)` : histogram of the variances of each component.