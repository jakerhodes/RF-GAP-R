README.rmd
================
Jake S. Rhodes
4/17/2023

# RF-GAP

## Update:

## Random Forest Geometry- and Accuracy-Preserving proximities

This is the official R code repository for the paper “Random Forest-
Geometry- and Accuracy-Preserving Proximities”
(<https://ieeexplore.ieee.org/document/10089875>). In the paper we show
that random forest (RF) predictions can be exactly determined by using
RF-GAP proximities as weights in a weighted-sum regressor or
weighted-majority vote classifier. This repo provides the base code to
generate the various proximity definitions described in the paper.

## Generate RF-GAP proximities:

Random forests are capable of making predictions on both continuous and
categorical response variables for regression and classification,
respectively. Additionally, random forests handle mixed feature
variables, that is, predictor variables may be either numeric or
categorical and since the partitioning decisions are rank-based,
numerical predictor variables do not need to be normalized or
standardized as is typical in other machine learning contexts. Thus, the
construction of random forest-based proximities does not require the
same preprocessing steps as may be needed for other ML processes. This
simplifies the use of random forests and thus, for our purposes, the
generation of random forest proximities.

Let be a dataframe or matrix object with labels . Here must be numeric
(for a regression forest) or a factor type (for a classification task).
If is a character vector it will be coerced to be a factor type. To
generate the proximities, we use the function. The user may use a
pre-trained random forest to construct the proximities, which has the
benefit of a direct comparison of proximity types, or to train when
calling .

``` r
library(rfgap)

# Defining the data and labels
x <- iris[, 1:4]
y <- iris[, 5]

# Generating the proximities
prox <- get_proximities(x, y)
```

This is the simplest way to generate proximities. Here we simply call
$\texttt{get\_proximities}$ using the dataframe $\texttt{x}$ and labels
$\texttt{y}$ as inputs. By default, RF-GAP proximities are constructed.
The argument \$ allows the user to select the type of proximities to be
constructed, the package currently supports $\texttt{"original"}$,
$\texttt{"oob"}$, and $\texttt{"rfgap"}$.

The user may train a random forest prior to calling
$\texttt{get\_proximities}$. In this case, the user must train the
$\texttt{ranger}$ forest with the options $\texttt{keep.inbag}$ and
$\texttt{write.forest}$ set to $\texttt{TRUE}$. Using a pre-trained
forest allows the user to fairly compare different proximity types
without the need of retraining a forest each time.

    library(rfgap)

    # Defining the data and labels
    x <- iris[, -5]
    y <- iris[, 5]

    # Training the random forest
    rf <- ranger(x = x, y = y, keep.inbag = TRUE, write.forest = TRUE, seed = 42)

    # Constructing three sets of proximities
    proximities_rfgap <- get_proximities(x, rf = rf,
                                         type = 'rfgap')
                                         
    proximities_oob   <- get_proximities(x, rf = rf,
                                         type = 'oob')
                                         
    proximities_orig  <- get_proximities(x, rf = rf, 
                                         type = 'original')

$\texttt{get\_proximities}$ has the additional option for the user to
supply a test set. Including the test set will extend the proximities to
the test observations. This is done by using the argument
$\texttt{x\_test}$. The returned proximity matrix will have
$\texttt{n\_train + n\_test}$ rows and columns. The returned proximity
matrix is an S3 object of type $\texttt{rf\_proximities}$. This object
type has additional methods associated with it for making predictions,
producing visualizations, detecting outliers, and imputing missing data.

## Create 2-dimensional MDS embedding using RF-GAP proximities and plot

We apply these random forest distances,
$d(x_i, x_j) = \sqrt{1 - prox(x_i, x_j)}$ to multidimensional scaling
using the function $\texttt{rf\_mds}$. To use this function, the user
may choose to supply a precomputed proximity matrix, a trained
$\texttt{ranger}$ object, or just the dataframe $\texttt{x}$ with labels
$\texttt{y}$ ($\texttt{x}$ is required). If a proximity matrix is not
supplied, the user may choose the proximity type (default is RF-GAP).
Two types of MDS may be run; metric MDS using the $\texttt{cmdscale}$
function from the $\texttt{stats}$ package, and non-metric MDS using the
$\texttt{isoMDS}$ function from the $\texttt{MASS}$ packages. The number
of dimensions can be selected using the $\texttt{n\_dim}$ argument
(default is 2). The generic $\texttt{plot}$ function may be used to
generate a scatterplot of the MDS embeddings based on the
$\texttt{ggplot2}$. If the labels, $\texttt{y}$, are supplied, the
points will be colored and shaped according to class if $\texttt{y}$ is
of factor type, or just colored according to scale if $\texttt{y}$ is
numeric.

``` r
x <- iris[, 1:4]
y <- iris[, 5]
mds <- rf_mds(x, y, type = 'rfgap')
```

    ## initial  value 16.001404 
    ## iter   5 value 8.170306
    ## iter  10 value 7.717371
    ## iter  10 value 7.710932
    ## iter  10 value 7.708673
    ## final  value 7.708673 
    ## converged

``` r
plot(mds, y)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

We also provide the means of imputing missing data and detecting
outliers.

## Impute missing data

The $\texttt{rfgap}$ package provides a simple function to run random
forest imputation. It is assumed that the missing data takes the form
$\texttt{NA}$. The function $\texttt{rf\_impute}$ requires the dataset
with missing values, $\texttt{x}$, vector of associated labels,
$\texttt{y}$, the proximity $\texttt{type}$ (default is
$\texttt{rfgap}$), number of iterations to run the imputation
$(\texttt{n\_iters}$, default 1), and any additional $\texttt{ranger}$
options ($\texttt{...}$). The function returns a dataframe with the
imputed values. An additional argument, $\texttt{x\_true}$, may be used
to supply the true data without missing values. This is used for testing
the quality of the imputation. If the user supplies $\texttt{x\_true}$,
then function returns a list with two elements, the imputed dataframe
and the mean-squared error between the true and imputed values.

``` r
x <- airquality[, -4]
y <- airquality[, 4]
imputed_data <- rf_impute(x, y, type = 'rfgap')
```

## Run Outlier Detection

To compute the outlier scores, we use the function
$\texttt{rf\_outliers}$ which takes the a dataframe or
$\texttt{rf\_proximities}$ object, $\texttt{x}$, labels $\texttt{y}$,
and proximity type as arguments. The proximity type is ignored if an
$\texttt{rf\_proximities}$ object is supplied. Additionally, the user
may provide a pretrained $\texttt{ranger}$ if $\texttt{x}$ is the data
matrix, rather than a proximity matrix. $\texttt{rf\_outliers}$ returns
an object of S3 type $\texttt{rf\_outlier}$ which is an array of the
length of the number of objects in the dataset $\texttt{x}$.

``` r
x <- mtcars[, -c(1, 2)]
y <- as.factor(mtcars[, 2])
outlier_scores <- rf_outliers(x, y, type = 'rfgap')

plot(outlier_scores, x, y)
```

    ## initial  value 16.244852 
    ## iter   5 value 13.174290
    ## iter  10 value 11.956024
    ## iter  15 value 11.783160
    ## final  value 11.680537 
    ## converged

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Cite As:

    @ARTICLE{10089875,
      author={Rhodes, Jake S. and Cutler, Adele and Moon, Kevin R.},
      journal={IEEE Transactions on Pattern Analysis and Machine Intelligence}, 
      title={Geometry- and Accuracy-Preserving Random Forest Proximities}, 
      year={2023},
      volume={},
      number={},
      pages={1-13},
      doi={10.1109/TPAMI.2023.3263774}}
