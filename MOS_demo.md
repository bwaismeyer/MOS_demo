# MOS Demo: Simulating TA Evaluations
Brian Waismeyer  
June 5, 2015  
## What is this project?
This project is a simple demonstration of what a functional Multinomial
Outcome Simulator (MOS) instance looks like. 

## What is the Multinomial Outcome Simulator (MOS) framework?
The MOS framework is a configurable R and Shiny application for exploring 
multinomial models through simulation and visualization.

A detailed description of the MOS - along with the files needed to construct an
MOS instance - are available at the [MOS GitHub repository](https://github.com/bwaismeyer/MOS_framework).

To create an MOS instance, you need three things:
1. a dataset with at least one multinomial outcome
2. a multinomial model formula for that dataset
3. an `MOS_config.R` file tailored to the model, data, and your audience

## What is the point of this document and who is it written for?
This documentat overviews the the process of going from data to a configured MOS 
instance deployed on Amazon Web Services (AWS).

The documentation was written for users who are (a) reasonably comfortable with 
R and (b) reasonably comfortable with multinomial models. These users should
(fingers crossed) be ready to produce their own MOS instances after 
reviewing this project.

More novice users may also have some success with setting up an MOS instance,
though you should expect to do some supplemental learning on your own where
R, Shiny, or multinomial statistics are unfamiliar to you.

## Summary of key steps to constructing the demo MOS instance
* Select a well formatted, publicly available dataset that will permit the
  development of a multinomial model.
* Develop a simple multinomial model for the selected dataset.
* Fork the MOS framework repository.
* Configure the MOS framework for the selected dataset and developed model,
  creating a fully functional MOS instance allowing users to interact with the
  model via simulation.
* Test the MOS instance locally.
* Deploy the MOS instance to a free AWS server.

## Selecting and Preparing a Dataset
The internet is rich with free data. However, for this project we want a
minimally complex dataset that still meets our modeling needs. 

My goal is not to demonstrate how to choose a model or even how to fit a model 
well - my goal is to show the user how to take a given multinomial model and
its dataset and create an MOS instance.

To this end, I chose the [Wine Qaulity Dataset](http://archive.ics.uci.edu/ml/datasets/Wine+Quality)
available for free from the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/about.html).

The dataset is a collection of measurements for red and white wines coupled
with a rating of wine quality for each measurement. I merged the red and white
wine datasets together (giving us a factor predictor) and converted the
outcome variable (a 1-10 measurement of quality) into quality groups.

The following code was used to obtain and process the dataset for this demo.

```r
# the red and white datasets both come as CSVs with semi-colon used as the
# separator
red_URL <- paste0("http://archive.ics.uci.edu/ml/machine-learning-databases/",
                  "wine-quality/winequality-red.csv")
white_URL <- paste0("http://archive.ics.uci.edu/ml/machine-learning-databases/",
                    "wine-quality/winequality-white.csv")

# we read in the data
red <- read.table(red_URL, header = TRUE, sep = ";", 
                  stringsAsFactors = FALSE)
white <- read.table(white_URL, header = TRUE, sep = ";", 
                    stringsAsFactors = FALSE)

# we add a new column to each to indicate the wine type
red$type <- "red"
white$type <- "white"

# we merge into a single dataset
wine <- rbind(red, white)

# we create a multinomial outcome variable
wine$quality_group[wine$quality <= 4] <- "Poor"
wine$quality_group[wine$quality == 5] <- "Acceptable"
wine$quality_group[wine$quality == 6] <- "Good"
wine$quality_group[wine$quality > 6] <- "Very Good"

# we convert the character vectors to proper factors
wine$type <- factor(wine$type, 
                    levels = c("red", "white"),
                    labels = c("Red", "White"))
wine$quality_group <- factor(wine$quality_group,
                             levels = c("Poor", "Acceptable", 
                                        "Good", "Very Good"))

# we double check that this produces the dataset we're looking for
str(wine)
```

```
## 'data.frame':	6497 obs. of  14 variables:
##  $ fixed.acidity       : num  7.4 7.8 7.8 11.2 7.4 7.4 7.9 7.3 7.8 7.5 ...
##  $ volatile.acidity    : num  0.7 0.88 0.76 0.28 0.7 0.66 0.6 0.65 0.58 0.5 ...
##  $ citric.acid         : num  0 0 0.04 0.56 0 0 0.06 0 0.02 0.36 ...
##  $ residual.sugar      : num  1.9 2.6 2.3 1.9 1.9 1.8 1.6 1.2 2 6.1 ...
##  $ chlorides           : num  0.076 0.098 0.092 0.075 0.076 0.075 0.069 0.065 0.073 0.071 ...
##  $ free.sulfur.dioxide : num  11 25 15 17 11 13 15 15 9 17 ...
##  $ total.sulfur.dioxide: num  34 67 54 60 34 40 59 21 18 102 ...
##  $ density             : num  0.998 0.997 0.997 0.998 0.998 ...
##  $ pH                  : num  3.51 3.2 3.26 3.16 3.51 3.51 3.3 3.39 3.36 3.35 ...
##  $ sulphates           : num  0.56 0.68 0.65 0.58 0.56 0.56 0.46 0.47 0.57 0.8 ...
##  $ alcohol             : num  9.4 9.8 9.8 9.8 9.4 9.4 9.4 10 9.5 10.5 ...
##  $ quality             : int  5 5 5 6 5 5 5 7 7 5 ...
##  $ type                : Factor w/ 2 levels "Red","White": 1 1 1 1 1 1 1 1 1 1 ...
##  $ quality_group       : Factor w/ 4 levels "Poor","Acceptable",..: 2 2 2 3 2 2 2 4 4 2 ...
```

```r
# to simplify our data and model, we use the quick and dirty criteria
# of correlation with quality to trim our coefficient candidates
cor(wine[1:11], wine$quality)
```

```
##                             [,1]
## fixed.acidity        -0.07674321
## volatile.acidity     -0.26569948
## citric.acid           0.08553172
## residual.sugar       -0.03698048
## chlorides            -0.20066550
## free.sulfur.dioxide   0.05546306
## total.sulfur.dioxide -0.04138545
## density              -0.30585791
## pH                    0.01950570
## sulphates             0.03848545
## alcohol               0.44431852
```

```r
# we retain only the numerical coefficients with a correlation coefficent
# greater than 0.10
wine <- dplyr::select(wine, volatile.acidity, chlorides, density, alcohol, 
                       type, quality_group)
```

## Defining a Multinomial Model Formula
The actual fitting of the multinomial model will occur when an MOS instance
server is first initialized. (For users familiar with Shiny, the fitting
occurs in the `server.R` file outside of the server loop.)

What this means is that all the MOS framework actually needs is a fully defined 
model formula and the data the formula will be fit to.

How you select this formula is up to you - again, I am not reviewing model
selection or fitting here, simply how to use a model you have already selected
(hopefully with care).

For our demo, we assume our retained coefficients are interesting and that the
best model is linear and additive.

The data and formula will be expanded and then will be fit using the 
`multinom` function from the `nnet` package.

You are **strongly** encouraged to test that expansion/fit succeeds outside of 
the MOS instance. If there are any issues, you may need to adjust either the
formula or how the model is fit in the `server.R` file.

Here is the code used to define the model formula and test the model fitting.

```r
# defining the model formula
wine_quality_formula <- 
    # outcome
    quality_group ~ 
    # additive terms
    volatile.acidity + chlorides + density + alcohol + type
    # interaction terms
    # none for this demo but you can define them with either * or :

# expanding the data and model
exp_data <- model.matrix(wine_quality_formula, wine)
exp_data <- data.frame(wine["quality_group"],  
                        exp_data)
exp_data[, "X.Intercept."] <- NULL

# test the fit (if this succeeds, you're probably good to go - though, again, 
# I'm assuming you've already done the work to select an appropriate model)
test_fit <- nnet::multinom(formula(exp_data), data = exp_data, Hess = T)
```

```
## # weights:  28 (18 variable)
## initial  value 9006.754464 
## iter  10 value 6629.441118
## iter  20 value 6458.391439
## iter  30 value 6440.436410
## iter  40 value 6436.476416
## iter  50 value 6436.402894
## iter  60 value 6434.614674
## iter  70 value 6433.863556
## final  value 6431.695948 
## converged
```

```r
# a common error is the "too many weights" error - if you get this, you can
# adjust MaxNWts (e.g., multinom(..., MaxNWts = 3000)) or - probably better - 
# simplify your formula and/or get a larger dataset
```

## Getting the MOS framework files
The MOS framework files are available in the [MOS GitHub repository](https://github.com/bwaismeyer/MOS_framework).

The suggested approach to getting the files is to [create your own fork of the
repository](https://help.github.com/articles/fork-a-repo/). For instance,
the files for this demo live in a fork of the MOS framework repo.

This approach allows you to monitor the MOS framework repo for updates, to pull
updates if they seem appropriate, and to create pull requests if you have ideas
for how the MOS framework can be improved.

**Note**: If you do pull from the MOS framework repo, make sure you understand
[how pull/fetch works](https://help.github.com/articles/fetching-a-remote/).
You especially want to be cautious about overwriting your `MOS_config.R` file,
as this is what defines your unique MOS instance.

Using GitHub will also make deploying and maintaining your work on Amazon Web
Services *much* easier.

The following directions will assume that you are using Git and GitHub to
manage your MOS instance. If you want to use an alternative approach or 
technology, I will trust that you know what you're up to!

## Configuring the MOS framework files
Once you have your own fork of the MOS framework repo, it's time to adjust
the MOS framework to make it an MOS instance.

In most cases, this entails simply editing the `MOS_config.R` file. This is
where you will specify your data, the model formula, and some key supporting
features (e.g, deciding what variables the user can see).

You will want to **carefully work through `MOS_config.R` from start to 
finish**.

The sections are all clearly documented and example code is provided in the
starting `MOS_config.R` file that you can replace as needed.

If your data loading/processing requires specific files or resources, you
will need to specify those in the appropriate `MOS_config.R` section and
make sure any files that need to be local to the MOS instance are placed
in the instance folder.

If you want to use a custom `bootstrap.css` file to style your MOS instance,
you will need to place the file in the 'www' folder of the instance and
specify the file in the appropriate `MOS_config.R` section. Alternatively, 
you can use the `shinythemes` package to use some premade boostrap variants
by editing the `ui.R` file directly (the MOS intance will use the 
['united'](https://bootswatch.com/united/) theme by default).

Check out the demo [`MOS_config.R`](https://github.com/bwaismeyer/MOS_framework/blob/MOS_demo/MOS_config.R) 
file to see an example of a fully specified config file.

## Installing the Supporting Packages
The MOS requires a family of supporting R packages. These are listed in the 
`MOS_packages_and_custom_functions.R` script.

You can either install the supporting R packages manually or (recommended) use
[Packrat](https://rstudio.github.io/packrat/) to install the R packages from 
the provided binaries.

Using Packrat and the provided binaries is recommended because (1) you will be
using versions of the packages that have been verified to work with the MOS, (2)
Packrat is (usually) makes initial setup of MOS packages pretty painless, and
(3) Packtrat will make managing your MOS instance (and any new packages you 
need for instance) easier.

To use the Packrat approach:

1. Install Packrat manually - `install.packages("packrat")`.
2. Switch your working directory to the MOS instance directory (e.g., 
`setwd("./my_instance/")`).
3. Initialize Packrat - `packrat::init()`.

Packrat should recognize the Packrat structure included with the base MOS
framework and should attempt to install the included binaries.

**Note**: You may have to do some extra legwork to install the supporting
packages on various operating systems. See the AWS setup directions below or
Google the error messages you observe.

## Testing the MOS Instance Locally in R and RStudio
To test the application instance, simply open either the `server.R` or `ui.R`
files in RStudio. 

As long as you have properly installed the Shiny package (`shiny`), RStudio
should recognize either file as associated with a Shiny application and should
give you a "Run Application" button.

If that fails, you can manually start the application via `shiny::runApp()` (
see the help documentation for `runApp()` for details).

## Deploying the MOS Instance to Amazon Web Services (AWS)


## Common Errors
1. Subscript out of bounds, incorrect number of dimensions, etc.: This is
usually caused by a mismatch between your dataset column names and the raw names
you provided in the `MOS_config.R` variable configuration or base formula 
sections. Check these sections and make sure your column names are consistent.

2. `chol.default` errors (e.g., leading minor...not positive definite):
This occurs when the Hessian associated with your model cannot be solved. This
is is a sign that your model is too complex for your data. Simplify the model
or collect more data.

3. Getting unreasonable values when trying to simulate data: This occurs when
your model is overfit and is caused by failures in the likeilhood simulation
code that result from this overfitting.

4. "too many weights" when doing the `multinom` fitting: Your model is too
complex relative to your data. You should probably simplify your model, though
you can also increase the number of allowed weights (e.g., multinom(..., 
MaxNWts = 3000)).
