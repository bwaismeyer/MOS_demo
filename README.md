# The Multinomial Outcome Simulator (MOS) 
The MOS is an R and Shiny application for exploring multinomial models through simulation and visualization.

Specifically, users interact with an instance of the application by adjusting model inputs and then observing as the application simulates likelihoods for the model outcomes.

An MOS application instance is created by forking the MOS\_framework repo (or otherwise collecting the key files), customizing the MOS\_config.R file, and providing any resources needed by the instance (e.g., a data CSV, additional R scripts that process imported data, etc.).

This goal of this customization is to provide a specific multinomial model formula, the well-formatted data frame this model will be fit against, and some basic user-interface configuration.

Once an MOS application instance has been defined, it can be run and shared like any Shiny application (and there are a lot of options for [sharing a Shiny application](http://shiny.rstudio.com/tutorial/lesson7/)).

A demonstration MOS instance hosted on AWS can be viewed [here]()**(Not Yet)**. The complete code for the instance is available [here]()**(Nope - Not Yet Either)**.

## Application "Modes"
There are different application "modes", all built upon the same application infrastructure but with user interface features and visualizations tailored to different purposes. The application is currently designed so that users select between the modes via simple tab navigation.

As of this writing, the available modes are:
* Explore Mode: appropriate for observing how outcome likelihoods change across a range of values (e.g., How does the likelihood of winning a soccer match change as relative budget increases?)
* Single Case Mode: appropriate for simulating the likelihood of outcomes based on inputs for a specific case (e.g., What is the likelihood that a particular team will win a soccer match given certain team features?)
