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
This document overviews the the process of going from data to a configured MOS 
instance deployed on Amazon Web Services (AWS).

The documentation was written for users who are (a) reasonably comfortable with 
R and (b) reasonably comfortable with multinomial models. These users should
(fingers crossed) be ready to produce their own MOS instances after 
reviewing this demo instance and documentation.

More novice users may also have some success with setting up an MOS instance,
though you should expect to do some supplemental learning on your own where
R, Shiny, or multinomial analysis are unfamiliar to you.

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

## Selecting and preparing a Dataset
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

## Defining a multinomial model formula
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

## Installing the supporting packages locally
The MOS requires a family of supporting R packages. These are listed in the 
`MOS_packages_and_custom_functions.R` script.

You can either install the supporting R packages manually or (recommended) use
[`packrat`](https://rstudio.github.io/packrat/) to install the R packages from 
the provided binaries.

Using `packrat` and the provided binaries is recommended because (1) you will be
using versions of the packages that have been verified to work with the MOS, (2)
`packrat` is (usually) makes initial setup of MOS packages pretty painless, and
(3) Packtrat will make managing your MOS instance (and any new packages you 
need for instance) easier.

To use the `packrat` approach:

1. Install `packrat` manually - `install.packages("packrat")`.
2. Switch your working directory to the MOS instance directory (e.g., 
`setwd("./my_instance/")`).
3. Initialize `packrat` - `packrat::init()`.

`packrat` should recognize the `packrat` structure included with the base MOS
framework and should attempt to install the included binaries.

**Note**: You may have to do some extra legwork to install the supporting
packages on various operating systems. See the AWS setup directions below or
Google the error messages you observe.

## Testing the MOS instance locally in R and RStudio
To test the application instance, simply open either the `server.R` or `ui.R`
files in RStudio. 

As long as you have properly installed the Shiny package (`shiny`), RStudio
should recognize either file as associated with a Shiny application and should
give you a "Run Application" button.

If that fails, you can manually start the application via `shiny::runApp()` (
see the help documentation for `runApp()` for details).

## Deploying the MOS instance to Amazon Web Services (AWS) Free Tier
At this point, this document will walk through how to deploy to one of the
servers available on Amazon's free service tier, specifically 
[Amazon's EC2 service](http://aws.amazon.com/ec2/?sc_channel=PS&sc_campaign=acquisition_US&sc_publisher=google&sc_medium=ec2_b&sc_content=ec2_e&sc_detail=amazon.ec2&sc_category=ec2&sc_segment=53611778562&sc_matchtype=e&sc_country=US&s_kwcid=AL!4422!3!53611778562!e!!g!!amazon.ec2&ef_id=VTlq7QAAAQOLjYDQ:20150511210335:s). 

Using the EC2 service, we will deploy to a (free) micro instance of the EC2 
Ubuntu AMI.

### Creating a free AWS account
First step, you need an AWS account. Sign up [here](http://aws.amazon.com/free/?sc_channel=PS&sc_campaign=acquisition_US&sc_publisher=google&sc_medium=cloud_computing_hv_b&sc_content=aws_core_e&sc_detail=amazon%20web%20services&sc_category=cloud_computing&sc_segment=67361869722&sc_matchtype=e&sc_country=US&s_kwcid=AL!4422!3!67361869722!e!!g!!amazon%20web%20services&ef_id=VXdodwAABY2XTHKz:20150609222807:s).

Follow the directions for either creating a new account or linking the free
AWS to your current Amazon account.

**Note**: This process does require a credit card and accessible phone, and the 
free account is limited to a year and has use limits as well. Still, it should
be more than sufficient for testing out MOS instances or deploying low-traffic
instances.

### Sign into the Management Console
You should be able to login [here](http://aws.amazon.com/console/).

### Launching the AWS instance
Once you've logged in, follow Amazon's 
[instance launch instructions](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-instance_linux.html), 
noting the following: 

* The guide assumes that you will use the Ubuntu Amazon Machine Image (AMI). If 
you want to use a different AMI, please be prepared to figure out the correct 
default user name and shell commands.
* Accept the default security settings for the time being. We'll change those 
during the next step.
* Make sure you get the private key!

### Authorize inbound traffic to the AWS instance
We have an instance. Now we need to make sure we (and our users) can talk to it. 
By default, AWS gives very broad access privileges. At least initially, this is 
probably fine.

Verify the server's inbound traffic rules by following 
[these directions](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/authorizing-access-to-an-instance.html).

If needed, update the selected security group to allow access via SSH 
(universally or to your specific IP).

### Setup putty to access the AWS instance
Now that the instance is launched and open to SSH, we need to setup an SSH tool. 
We'll use putty.

Follow the directions for 
[installing and configuring putty](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html).
	
**Note**: You can stop following the directions after you finish the "Starting 
a Putty Session" section.

### Setup a swapfile
Computing environments low in volatile memory (i.e., RAM) run the risk of easily 
running out of working memory during memory intense operations - such as certain 
R package installations and other operations. The micro instances are 
low-memory computing environments.

To counter this, we assign a special file the instance can use - a swapfile - 
during periods when it runs out of RAM. 

Login into the server via putty and then simply run this code (taken from [here](http://serverfault.com/questions/218750/why-dont-ec2-ubuntu-images-have-swap)) 
in the putty console:
```
sudo dd if=/dev/zero of=/var/swapfile bs=1M count=2048 &&
sudo chmod 600 /var/swapfile &&
sudo mkswap /var/swapfile &&
echo /var/swapfile none swap defaults 0 0 | sudo tee -a /etc/fstab &&
sudo swapon -a
```

### Setup the R mirror
When we ask Ubuntu to install applications, it grabs files from certain default 
repositories. The default R repository is usually badly out of date. We want to 
specify one of the official CRAN repositories to insure we get the most up to 
date version of R and packages.

From the putty console, we're going to edit (and if needed create) an extra 
depository-specification file:
```
sudo vi /etc/apt/sources.list.d/sources.list
```

Then we want to add this line to the file and save it.
```
deb http://cran.cs.wwu.edu/bin/linux/ubuntu trusty/
```
**Notes**

* If you have any trouble using the vi editor, here's a 
[good cheet sheet](http://www.lagmonster.org/docs/vi.html).
* The selected CRAN mirror above is a Washington state mirror. If you want to 
use an alternate mirror, here's the 
[official CRAN mirror list](http://cran.r-project.org/mirrors.html).
* The line points to the Ubuntu version Trusty repository. If you used a 
different Amazon AMI than Ubuntu or if the version has changed from Trusty, 
you will need to adjust the line accordingly.

### Install and configure R, Shiny, and Shiny Server
Alright, it's time at last to get our core tools installed and configured. The 
steps below are described in more detail 
[here](https://github.com/chrisrzhou/RShiny-EC2Bootstrap#install-r).

First we make sure all the server files are up to date.
```
sudo apt-get update
sudo apt-get upgrade
```

Then we install base R and the R development tools.
```
sudo apt-get install r-base
sudo apt-get install r-base-dev
sudo apt-get install 
```

**Note**: You might get authentication warnings. Ignore these (say yes where 
needed or ignore them where they're just messages) unless you know how to 
properly setup the authentication process for R repository we're drawing our 
files from.

We give universal read/write permissions to the default R library pathway so 
that installing/managing libraries is simplified
```
sudo chmod 777 /usr/local/lib/R/site-library
```

Do another quick update request before installing the packages to make sure the 
R materials are up to date
```
sudo apt-get update
```

Now we install any R packages we need. Typically, this will just be "shiny" 
(needed before installing Shiny Server) and `packrat` (if your project is using 
`packrat` to manage its dependencies). From the console:
```
R
install.packages("shiny")
install.packages("packrat")
q()
```

And now we install Shiny Server as described [here](http://www.rstudio.com/products/shiny/download-server/):
```
sudo apt-get install gdebi-core
wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
sudo gdebi shiny-server-1.3.0.403-amd64.deb
sudo rm shiny-server-1.3.0.403-amd64.deb
```

We configure Shiny Server to run under the appropriate user account (ubuntu) 
and to look for our shiny apps in the right location (a subdirectory in the 
ubuntu user directory).

* Make the app-hosting folder and the logs folder (using whatever names you 
like).
```
mkdir /home/ubuntu/shiny_apps
mkdir /home/ubuntu/shiny_logs
```
* Adjust the shiny config file to (a) point to the appropriate directories and 
(b) run under ubuntu. More information about the config file and its features is 
available 
[here](http://rstudio.github.io/shiny-server/latest/#default-configuration).
```
sudo vi /etc/shiny-server/shiny-server.conf
```
* Finally, it's time to return to the AWS EC2 security group and open up access
to our Shiny Server port. Edit the instance security group Inbound rules and 
add a custom TCP rule with the appropriate port number (e.g., 3838).

### Install and configure Git
The best way to get our application(s) onto the EC2 server - and to setup an 
efficient development-to-production pathway - is to use git and GitHub. This 
guide assumes your applications are already hosted on GitHUb, so now you just 
need to install git.
```
sudo apt-get install git
```

**Note**: If you've never used SSH with the relevant GitHub account before, you 
might need to do 
[some more setup](https://help.github.com/articles/generating-ssh-keys/) before 
your git clone/pull requests will succeed.

### Cloning the MOS instance
At this point, we have R, Shiny, and Shiny Server installed and configured to 
play nicely with the default ubuntu account. We also have a pathway for pulling 
up to date versions of our Shiny application(s) to our server.

Now we need to clone our app(s) in our chosen app hosting folder (e.g., 
```/home/ubuntu/shiny_apps```).

Example bash commands for cloning an application:
```
cd /home/ubuntu/shiny_apps/
git clone https://github.com/bwaismeyer/MOS_demo
```

### Setting Up MOS instance dependencies
Once you have cloned the repository for you MOS instance, you need to setup
all the packages and other supporting files required for the instance to run.
Setup directions for a particular instance may vary slightly if you have special 
package requirements (e.g., if you modified the core framework files or needed 
extra packages for your data loading).

The MOS framework comes ready to use `packrat` to manage its R dependencies.
All we need to do is clone the project and then start R in the COS application 
directory. `packrat` will install all of the relevant packages from their 
binaries (which `packrat` keeps copies of). 

However, the `Cairo` package is special and attempts to install it will fail 
unless the correct resources are added to our EC2 server.

I THINK the only packages missing from the default EC2 setup are these.
```
sudo apt-get install libcairo2-dev
sudo apt-get install libxt-dev
```

Now we initialize R in the MOS instance project folder - `packrat` should 
automatically try to install all needed packages. Here's some more example
code:
```
cd /home/ubuntu/shiny_apps/MOS_demo
R
```

If `packrat` doesn't initialize:

* First make sure the working directory is correct (```getwd()``` from the R 
console).
* Then manually force the update (```packrat::restore()``` from the R console).

If `packrat` has installation errors, you'll need to problem shoot or install 
those packages manually (e.g., ```install.packages()``` from the R console).

**Exception**: If `packrat` is failing when it tries to download packages, 
check out the workaround [here](https://github.com/rstudio/packrat/issues/209).

### Initalize Shiny Server and get the link to your deployed MOS instance
At this point, it's time to initialize Shiny Server and start looking at your 
Shiny applications. From the putty console, simply use:
```
shiny-server
```

You should see some confirmation dialogue and see that the server is running. 

**Note**: If you get EADDRINUSE errors... try changing the port Shiny is using 
(e.g., to 3939). Don't forget that you need to add this port to the EC2 inbound 
security group AND that this changes the link people need to use to reach the 
app.

The link you're looking for will be a combination of (a) the EC2 server's 
Public DNS, (b) the Shiny Server port number, and (c) the application 
subdirectory. It will look something like this:
```
[Public DNS]:[Port Number]/subdirectory
```

An example:
```
http://ec2-52-26-165-185.us-west-2.compute.amazonaws.com:3939/MOS_demo/
```

If the app of interest is in a sub-directory (most likely), then it may be 
easiest to simply visit the base link (DNS:Port Number) and click through the 
auto-generated index to get the full link to your app.

### Admire your MOS instance
At this point you hopefully have an up-and-running MOS instance. Failing that,
I hope your troubleshooting road is short and merry!

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

5. Can't find your AWS instance when you login to console: The instances are
region specific. While logged into the AWS console, look in the upper right
hand corner and check out what region you're logged into. Switch through the
regions until you the region that is hosting your instance (or until you're
confident it's not there at all).
