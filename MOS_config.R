# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/12/2015
# Date updated: 6/5/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: MOS_config.R is where a Multinomial Outcome Simulator (MOS) instance
#       is defined by an application administrator.
#
#       The MOS is a Shiny application that allows users to explore a given
#       multinomial logit model by interacting with simulations and 
#       visualizations based on that model.
#
#       The model formula and the data the model are fit against are provided to 
#       the application, which then builds the visualizations by:
#       (a) getting a model fit
#       (b) simulating outcomes from that model based on user inputs
#       (c) plotting the outcomes in the context of the user inputs
#
#       The MOS should (theoretically) take any multinomial logit formula
#       and its accompanying, properly formatted R data frame.
#
#       However, certain features of the MOS need to be specified for it
#       to work.
#
#       This config file is where those features are specified. It is sourced
#       by the MOS ui.R file when the application is initialized.
#
#       Non-functional example code has been provided in each configuration
#       section. A working example project with code can be observed here:
#       ##UPDATE NEEDED##
#
# SCRIPT OUTLINE:
# - Name the Application Instance
#   - This is the title that will be displayed in the instance navigation and
#     should be a very concise description.
#
# - Import and Name the Data Object as Needed
#   - The multinomial logit model needs to be fit to a dataset. This dataset
#     needs to be be an R data frame named "base_data" (no quotes).
#   - This section is where the data frame is created (however that needs to 
#     be done) and assigned to "base_data".
#
# - Specify the Multinomial Logit Formula
#   - The multinomial logit formula needs to be provided explicitly and it
#     needs to appropriately reference the "base_data" data frame. The formula
#     needs to be assigned to "base_formula".
#
# - Variable Configuration
#   - We need to specify which variables will the user will be able to interact
#     with (via slider or facet). Key information must be provided for each
#     of these variables. See the section for details.
#
# - Custom Visualization Colors (Optional)
#   - Assign the custom colors (the same number as there are outcomes) to
#     the character string "custom_outcome_colors".
#   - If you don't want to use custom colors, set "custom_outcome_colors" to 
#     NULL.
#
# - Custom bootstrap.css (Optional)
#   - The bootstrap.css file should be placed in a subdirectory of the
#     application titled "www".
#   - Assign the name of the bootstrap.css file to the character string 
#     "custom_css" (just the name, Shiny will know to look in "www").
#   - If you don't want to use a custom bootstrap.css, set "custom_css" to NULL.
#
# - Ribbon Plot Addendum (Optional)
#   - If you want to provide any additional text (e.g., caveats, general
#     context) beneath the ribbon plot text body, you can assign an HTML-
#     formatted string to "ribbon_plot_addendum".
#   - Set this variable to NULL if you don't want to add anything.
#
# - Dot Cloud Plot Addendum (Optional)
#   - Same but for the dot cloud plot (dot_cloud_addendum).
#   - Set this variable to NULL if you don't want to add anything.
#
# - More Info Modal (Optional)
#   - If you want additional information about the plots to be available to
#     the user but you don't want to clutter up the space beneath the plot
#     with a lot of text...
#   - One option is to simply put HTML anchor tags and links into the relevant
#     addendum.
#   - However, if you don't want your user to have to leave the application,
#     you can adjust the features in this section to make a modal link and
#     window available to the user. The link will appear after the plot
#     addendum (if there is one).
#   - Modal details: http://ebailey78.github.io/shinyBS/docs/Modals.html
#   - Simply set "more_info_link_text" to NULL if you don't want to use the
#     modal at all.

###############################################################################
## Name the Application Instance

MOS_instance_name <- "Exploring Cat Huggability"

###############################################################################
## Import and Name the Data Object as Needed

base_data <- read.csv("how_huggable_is_that_cat.csv")

###############################################################################
## Specify the Multinomial Logit Formula

# Note that the formula needs to correctly reference the base_data object
# column names.
base_formula <-
    # outcome column
    huggable_or_not ~ 
    # additive terms
    eye_size + whisker_length + claw_sharpness + color + cuddles_per_min +
    objects_broken + gender +
    # interaction terms
    claw_sharpness : cuddles_per_min

###############################################################################
## Variable Configuration

# The following features must be specified for every model variable that you
# want the user to be able to see and interact with.

# variable_configuration <- list(
#     RAW_NAME = list(
#         pretty_name         = UI_friendly name (REQUIRED),
#         definition          = a concise explanation of how the user should
#                               understand the variable (OPTIONAL),
#         ribbon_plot_summary = a concise summary of the trends observed in the
#                               ribbon plot when this variable is seleted as
#                               the x-axis (OPTIONAL, only useful for slider
#                               variables),
#         custom_x_axis_ticks = NULL or a character vector with as many items
#                               as there are ribbon plot x-axis ticks; the
#                               x-axis tick labels will be replaced with the
#                               character vector items in the order they are
#                               given
#         x_axis_candiate     = TRUE or FALSE, allow the variable to be
#                               selected as the x-axis on the ribbon plot
#                               (REQUIRED),
#         slider_candidate    = TRUE OR FALSE, where appropriate, make a slider
#                               for this variable (REQUIRED)
#         slider_rounding     = NA or a number, refines slider behavior (e.g., 
#                               1 will force the slider for this variable to
#                               snap to whole numbers) (REQUIRED, defaults to
#                               0.1 if NA, only impacts slider_candidates),
#         facet_candidate     = TRUE or FALSE, allow the variable to be 
#                               selected as a facet on the ribbon plot
#                               (REQUIRED, variable will be forced to factor if
#                               TRUE),
#         transform_for_ui    = defaults to "identity" (no transformation) but
#                               can can take other transformations if 
#                               variable needs to be transformed for user 
#                               presentation (REQUIRED),
#         transform_for_model = reverses the user-friendly transformation so
#                               that values are model-friendly again (REQUIRED)
#     ),
#     ...
# )

variable_configuration <- list(   
    whisker_length = list(
        pretty_name         = "Whisker Length (Inches)",
        definition          = paste0("A measurement of the cat's mean whisker ",
                                     "length from skin to whisker tip."),
        ribbon_plot_summary = paste0("As whisker length increases, the ",
                                     "simulated likelihood of the kitty being ",
                                     "huggable also increases."),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),    
    gender = list(
        pretty_name         = paste0("Cat Gender"),
        definition          = paste0("A description of the cat's apparent ",
                                     "biological sex."),
        ribbon_plot_summary = paste0(""),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = FALSE,    
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    )
)

###############################################################################
## Custom Visualization Colors (Optional)

# Colors are applied in the order they are given to outcomes in level order
# (e.g., first outcome level is paired with the first color, etc.).
# If no custom colors are desired, set this to NULL.
custom_outcome_colors <- c("#D9BB32", "#6DB33F")

###############################################################################
## Custom bootstrap.css (Optional)

# Custom bootstrap.css file must be in the www subdirectory of the MOS
# application. Set "custom_css" to NULL if you don't want to use one.
custom_css = "bootstrap.css"

# NOTE: This CSS example theme was provided by the nifty bootswatch.com here:
# https://bootswatch.com/sandstone/
# Check them out for other pre-made boostrap files.

###############################################################################
## Ribbon Plot Addendum (Optional)

# This needs to be an HTML formatted string. It will immediately begin adding
# text after the auto-generated ribbon plot text (variable name, definition,
# and key trends) - you will need to add line breaks where needed. Set to NULL
# if you don't want any added text.
ribbon_addendum <-
    paste0("<br><strong>Please Keep In Mind</strong>",
           
           "<br>This simulation cannot tell if the observed relationships are",
           "causal or correlational.",
           
           "<br><br><strong>What Is This Simulation Based On?</strong>",
           
           "<br>The simulation is modeled on real data: a survey of 32 cats ",
           "conducted by a trained researcher. The survey was conducted in ",
           "a suburb in Seattle, WA.")

###############################################################################
## Dot Cloud Plot Addendum (Optional)

# Like "ribbon_addendum", this also needs to be an HTML formatted string.
# No text is automatically created for the dot cloud plot. A default 
# explanation of the plot is provided below, but you may want to adjust
# the language to be appropriate for the application instance and audience. Set
# to NULL if you simply want the this are to be blank.
dot_cloud_addendum <- 
    paste0("<strong>What Does This Tool Do?</strong>",
           
           "<br>This tool allows you to describe a specific cat ",
           "observe how likely each outcome is for simulations based ",
           "on that cat description.", 
           
           "<br><br>You describe the cat by setting the inputs to the values ",
           "that best fit it.",
           
           "<br><br>Each time the 'SIMULATE' button is clicked, the cat ",
           "you described is run through 1000 versions of our ",
           "cat huggability model. These versions vary based on how much ",
           "uncertainty there is in the model.",
           
           "<br><br>For each model version, we get an estimate of how likely ",
           "the outcomes are. We plot every estimate by its outcome.",
           
           "<br><br>The resulting plot gives us a sense of how likely the ",
           "outcomes tend to be across all the model versions (where do the ",
           "dots tend to cluster for each outcome?) while also suggesting ",
           "how much confidence we should have in our model's ability to ", 
           "accurately simulate outcomes for the described cat ",
           "(how spread out are the dots for each outcome?)")

###############################################################################
## More Info Modal (Optional)

# What should be the text for the user-facing link that can open the more info
# modal? Plain text string - no HTML formatting (will be formatted to look like 
# a section title in ui.R). Set to NULL if you don't want to use the more info
# modal at all.
more_info_link_text <- "What is the simulation based on?"

# Set the title for the modal window. 
more_info_title <- "What is the simulation based on?"

# Build the body for the modal window. This can technically be any Shiny R
# UI objects, but by default should simply be a block of HTML formatted text.
# You will need to reference and adjust the ui.R and server.R scripts if you 
# want more complex Shiny R features here.
more_info_body <- 
    paste0("The simulation is modeled on real data: a survey of 32 cats ",
           "conducted by a trained researcher. The survey was conducted in ",
           "a suburb in Seattle, WA.")


###############################################################################
## END OF SCRIPT
###############################################################################

###############################################################################
## END OF SCRIPT
###############################################################################