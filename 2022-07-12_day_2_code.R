################################################################################
# Name: Intro to Data Wrangling, Exploration, and Analysis with R Day 2
# Author: Nina Brooks 
# Date: July 12, 2022
################################################################################

################################################################################
# Load Packages #
################################################################################
# load packages
library(readxl) # this package allows you to read .xlsx files into R
library(haven) # this package allows you to read & write stata .dta files
library(tabulator) # this is a package that is useful for making cross-tabs
library(janitor) # contains useful commands for data cleaning
library(palmerpenguins) # provides a dataset on penguins we'll use
library(glue) # tidyverse-adjacent package makes working with strings simpler
library(lubridate) # tidyverse package for working with dates
library(broom) # for tidying regression results
library(infer) # a tidyverse package for inference
library(gtsummary) # for summary & regression tables
library(fixest) # for fixed effects regression (among other things)
library(marginaleffects) # for marginal effects after regression
library(modelsummary) # for summary & regression tables
library(hrbrthemes) # additional themes to supplement ggplot2
library(patchwork) # for combining ggplots together
library(ggsci) # additional color palettes for ggplots
library(tidyverse) # this loads the 8 core packages of the tidyverse
library(tidylog) # adds extra explanation of tidyverse commands



################################################################################
# Load the WDI data for demo purposes
################################################################################
wdi <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx")

wdi <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx",
                  n_max = max(which(wdi$`Country Name` == "Zimbabwe"))) %>%
    clean_names() %>%
    select(-c(country_code, series_code)) %>%
    rename(
        country = country_name,
        series = series_name
    ) %>%
    mutate(across(starts_with("x"), as.numeric)) 

wdi_long <- wdi %>% # this time we'll save it as a new object
    pivot_longer(
        -c(country, series), 
        names_to = "year",
        names_pattern = "(\\d+)",
        names_transform = list(year = as.integer),
        values_to = "value") 

wdi_long <- wdi_long %>%
    mutate(
        series_short = case_when( # use case_when() for conditional operations
            str_detect(series, "electricity") ~ "electricity",
            str_detect(series, "(?=.*Education)(?=.*female)") ~ "edu_female",
            str_detect(series, "(?=.*Education)(?=.*male)") ~ "edu_male",
            str_detect(series, "GDP per capita") ~ "gdppc",
            str_detect(series, "malaria") ~ "malaria",
            str_detect(series, "Internet") ~ "internet",
            str_detect(series,  "(?=.*Labor)(?=.*female)") ~ "lfp_female",
            str_detect(series, "(?=.*Labor)(?=.*male)") ~ "lfp_male",
            str_detect(series, "Military") ~ "military",
            str_detect(series, "Poverty") ~ "poverty_gap",
            str_detect(series, "Prevalence") ~ "underourished"
        )
    ) 

wdi_df <- wdi_long %>%
    select(-series) %>%
    pivot_wider(
        names_from = "series_short",
        values_from = "value"
    )

# create a smaller dataset of just south asian countries
sa <- wdi_df %>%
    mutate(
        # create a logical variable to filter with
        south_asia = country %in% c("Afghanistan", "Bangladesh","Bhutan",
                                    "India", "Maldives", "Nepal", 
                                    "Pakistan", "Sri Lanka")
    ) %>%
    filter(south_asia) %>%
    select(country, year, electricity, malaria, gdppc)

################################################################################
# Summary statistics with dplyr
################################################################################

# this code calculates the average of gdppc, electricity & malaria for each country
wdi_df %>%
    group_by(country) %>%
    summarise_at(
        vars(gdppc, electricity, malaria), 
        mean, na.rm = TRUE)


# this code calculates  the mean of all (using the shorthand summarise_all)
# variables overall countries by year
wdi_df %>%
    group_by(year) %>%
    summarise_all(
        mean, na.rm = TRUE)


################################################################################
# Summary tables with gtsummary
################################################################################

# off the shelf table
sa %>%
    tbl_summary(by = country)


#  convert `year` from a numeric variable to a factor, so we get counts instead
sa %>%
    mutate(
        year = factor(year)
    ) %>%
    tbl_summary(by = country)

# create a nicer looking table
sa %>%
    select(-c(year)) %>%
    tbl_summary(
        by = country,
        statistic = all_continuous() ~ "{mean} ({sd})"
    )


# a little more customization
sa %>%
    select(-c(year)) %>%
    tbl_summary(
        by = country,
        statistic = all_continuous() ~ "{mean} ({sd})",
        digits = all_continuous() ~ 2,
        label = list(
            electricity ~ "Electricity access (% of population)",
            malaria ~ "Malaria prevalence (% of population",
            gdppc ~ "GDP per capita"),
        missing_text = "Missing"
    )


# even more customization
sa %>%
    select(-c(year)) %>%
    tbl_summary(
        by = country,
        statistic = all_continuous() ~ "{mean} ({sd})",
        digits = all_continuous() ~ 2,
        label = list(
            electricity ~ "Electricity access (% of population)",
            malaria ~ "Malaria prevalence (% of population",
            gdppc ~ "GDP per capita (constant 2015 US$)"),
        missing = "no"
    ) %>%
    # you can pipe the different commands
    add_overall() %>%
    modify_caption("**Table 1. Development Characteristics for South Asia (2010 - 2021)**") %>%
    bold_labels()


# more gtsummary examples using the built-in data
trial # if the gtsummary package is loaded, you can access the "trial" data just by calling its name
head(trial)

# gtsummary example that compares 2 groups (treatment A and B)
tbl <- trial %>% # notice that this time we store the table in an object called "tbl"
    select(trt, age, stage, marker) %>%
    tbl_summary(
        by = trt,
        missing = "no",
        digits = age ~ 1,
        statistic  = all_continuous() ~ "{mean} ({sd})") %>%
    add_overall() %>% # adds an overall column
    add_n() %>% # adds the overall sample size
    add_difference() %>% # calculates diff b/w Drug A & B; p-value
    # add_difference also adds the CI by default, but i chose to hide that column
    modify_column_hide(ci)

# view the table by calling it by name
tbl

# how to save a table created with gtsummary
tbl %>%
    as_gt() %>% # need to convert to a "gt" object first
    # use extensions .html .tex .ltx .rtf
    gt::gtsave(filename = "./output/trial_characteristics.tex") 


tbl %>%
    as_flex_table() %>% # need to convert to a "flex_table" object first
    flextable::save_as_docx(path = "./output/trial_characteristics.docx")

# need help? these 2 commands are equivalent
?tbl_summary()
help(tbl_summary)


################################################################################
# T-tests
################################################################################

penguins # loads the data from palmerpenguins (since we loaded that package at the top!)

# summarise the data in a few ways first to check out what we have
glimpse(penguins)
summary(penguins)

# t-tests

# testing the hypothesis that mean flipper length == 185
t.test(
    flipper_length_mm ~ 1, 
    mu = 185, 
    alternative = "two.sided", 
    data = penguins # need to specify the dataset to use is penguins
)


# the equivalent t-test but using the tidyverse infer::t_test() command
penguins %>% 
    t_test(
        response = flipper_length_mm, #<<
        mu = 185,  alternative = "two.sided"
    )


# two-group t-test: testing whether mean flipper length differs by penguin sex
# notice how we specify a "formula" that says test the difference of the first 
# variable according to groups of the 2nd variable. 

penguins %>% 
    t_test(
        formula = flipper_length_mm ~ sex, 
        alternative = "two.sided"
    )


################################################################################
# Regression
################################################################################

# simple OLS
lm(body_mass_g ~ flipper_length_mm, data = penguins)

# we can store that regression in a new object, called "reg"
reg <- lm(body_mass_g ~ flipper_length_mm, 
          data = penguins) 

# view the regression results
summary(reg)

# we can also summarize regression results using broom::tidy() (from the tidyverse)
tidy(reg) # returns a dataframe

tidy(reg)$estimate # you can access components just like a dataframe

# can add confidence intervals too
tidy(reg, conf.int = TRUE)

# multivariate regression: separate additional variables with a +
mvreg <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
            data = penguins) 

tidy(mvreg, conf.int = TRUE)


# change the level of the confidence interval (compare to the CI above to confirm!)
tidy(mvreg, conf.int = TRUE, conf.level = .9)

# regression tables with gtsummary::tbl_regression()
tbl_regression(reg)
tbl_regression(mvreg)

# another good package for regression tables is modelsummary 
modelsummary(
    list(
        "Simple OLS" = reg,
        "Multivariate Model" = mvreg
    ),
    gof_map = c("nobs", "r.squared"),
    stars = T
)

# with a few more options you can get an even nicer looking table
modelsummary(
    list(
        "Simple OLS" = reg,
        "Multivariate Model" = mvreg
    ),
    fmt = 2,
    coef_map= c(
        "flipper_length_mm" = "Flipper Length (mm)", 
        "bill_length_mm" = "Bill Length (mm)",
        "bill_depth_mm" = "Bill Depth (mm)",
        "sexmale" = "Male", 
        "(Intercept)" = "Constant"),
    gof_map = c("nobs", "r.squared"),
    stars = FALSE,
    notes = list("This table presents results of univariate and multivarate 
                 regression models."),
    output = "table1.png" # can change to other formats
) 

# regressions with fixest
mvfeols <- feols( body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
                  data = penguins) 

# if you use feols (from fixest) but don't add fixed effects or change your SE
# the output will be the same as the standard base R lm function
# check for yourself
summary(mvreg)
summary(mvfeols)

# but maybe you want to adjust your SE for heteroskedasticity or clustering
regRobust <- feols( 
    body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex, 
    vcov = "hetero", # this specifies robust SE
    data = penguins) 

regCL <- feols( 
    body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex, 
    vcov =  ~species, # this specifies clustering at the species level
    data = penguins)

# now i'm adding year FE - you indicated FE after the vertical bar |
regFE <- feols( 
    body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex | year,
    data = penguins)


# we can put all these stored results in a single table to compare
modelsummary(
    list(
        "Base Model" = mvreg,
        "Robust SE" = regRobust,
        "Clustered SE" = regCL,
        "Fixed Effects" = regFE
    ),
    fmt = 2,
    coef_map= c(
        "flipper_length_mm" = "Flipper Length (mm)", 
        "bill_length_mm" = "Bill Length (mm)",
        "bill_depth_mm" = "Bill Depth (mm)",
        "sexmale" = "Male", 
        "(Intercept)" = "Constant"),
    gof_map = c("r.squared", "vcov.type", "FE: year","nobs")
) 

# non-linear regression models (logits & probits)
# create a binary variable to use as the outcome
penguins <- penguins %>%
    mutate(
        large_penguin = case_when( # using case_when() to create this variable
            body_mass_g > 4050 ~ 1, 
            body_mass_g <= 4050 ~ 0
        )
    )


# base R versions
logit_mod <- glm( # use glm instead of lm (generalized linear model)
    large_penguin ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
    family = "binomial", # specify binomial to get a logistic regression
    data = penguins)

probit_mod <- glm( # also uses glm
    large_penguin ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
    family = binomial(link = "probit"), # specify the link function = probit
    data = penguins)

# fixest versions
felogit_mod <- feglm( # use feglm instead of glm
    large_penguin ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
    family = "binomial",  # this is the same as base R
    data = penguins)

feprobit_mod <- feglm( 
    large_penguin ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
    family = binomial(link = "probit"), 
    data = penguins)

# how to get interpreteable coefficients
# the exponetiate option tells R to exponentiate the coefficients which returns odds ratios
tidy(probit_mod, exponentiate = T) 

# you can add odds ratios to modelsummary tables too
modelsummary(felogit_mod, exponentiate = T)

# for probits, you need to calculate marginal effects
mfx <- marginaleffects(feprobit_mod)
tidy(mfx)

# for comparison, we'll run a linear probability model
lpm_mod <- feols(large_penguin ~ flipper_length_mm + bill_length_mm + bill_depth_mm + sex,
                 data = penguins)

# then we can put all of them in a table to compare
modelsummary(
    list(
        "Linear" = lpm_mod,
        "Logit" = felogit_mod,
        "Probit" = mfx), # can directly use the marginal effects instead of raw coefs
    fmt = 2,
    exponentiate = c(FALSE, TRUE, FALSE),
    coef_map= c(
        "flipper_length_mm" = "Flipper Length (mm)", 
        "bill_length_mm" = "Bill Length (mm)",
        "bill_depth_mm" = "Bill Depth (mm)",
        "sexmale" = "Male", 
        "sex" = "Male"),
    stars = T,
    gof_map = c("nobs"),
    notes = list(
        "95% confidence intervals in brackets.") 
)



################################################################################
# Data visualization: ggplot2 basics
################################################################################

# scatter plots ---
penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g)) 


# look for differences by species
penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g,
            color = species))

# can change the shape of the points too
# notice how the legend automatically adjusts
penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g,
            color = species,
            shape = species))


# we can also change the size of the points according to another variable
penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g,
            color = species,
            size = bill_length_mm))

# if you wanted to change the size of all points (not based on any variable), 
# you need to specify the size OUTSIDE of the aes() argument
penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g,
            color = species),
        size = 2) # like this

penguins %>%
    ggplot() +
    geom_point(
        aes(x = flipper_length_mm,
            y = body_mass_g,
            color = species),
        size = 6) # or this

# Histograms & Density Plots ---

penguins %>%
    ggplot() +
    geom_histogram( 
        aes(x = flipper_length_mm)) 

penguins %>%
    ggplot() +
    geom_density( 
        aes(x = flipper_length_mm)) 


# we can see how the distribution variables by species
penguins %>%
    ggplot() +
    geom_histogram( 
        aes(x = flipper_length_mm,
            color = species)) 

# ooh see how when used color = species (same as for the scatterplot), it only 
# changes the outline of the bars? for histograms and barplots, color corresponds
# to the outline or border, and fill will change the inside

penguins %>%
    ggplot() +
    geom_histogram( 
        aes(x = flipper_length_mm,
            fill = species))


# but for density plots (since they're lines), we can use color
penguins %>%
    ggplot() +
    geom_density( 
        aes(x = flipper_length_mm, 
            color = species)) 

# we can also use fill with density plots
penguins %>%
    ggplot() +
    geom_density( 
        aes(x = flipper_length_mm, 
            fill = species)) 

# with overlapping plots like this, it can be nice to change the opacity
# we can do this with the "alpha" option. alpha can take values from 0-1 and 
# you specify outside of aes() (unless you want to change the opacity by 
# the values of a variable). # closer to 0 is more see-through and closer to 1
# is more opaque 

penguins %>%
    ggplot() +
    geom_density( 
        aes(x = flipper_length_mm, 
            fill = species),
        alpha = 0.6) 

# if you specify color = species too, then you can get outlines that match the 
# color of the shaded areas
penguins %>%
    ggplot() +
    geom_density( 
        aes(x = flipper_length_mm, 
            color = species, 
            fill = species),
        alpha = 0.6) 

# Boxplots & violin plots ---

## a boxplot of the distribution of a single numeric variable
penguins %>%
    ggplot() +
    geom_boxplot( 
        aes(y = flipper_length_mm))


## a boxplot by group (e.g. penguin sex)
penguins %>%
    ggplot() +
    geom_boxplot( 
        aes(x = sex,
            y = flipper_length_mm))


# add the points to a boxplot with an additional layer
penguins %>%
    ggplot(
        aes(x = sex, 
            y = flipper_length_mm, 
            fill = sex)) + 
    geom_boxplot() +
    geom_jitter()

# let's remove those NA values 
penguins %>%
    filter(!is.na(sex)) %>%
    ggplot(
        aes(x = sex, 
            y = flipper_length_mm, 
            fill = sex)) + 
    geom_boxplot() +
    geom_jitter()

# another way to show the distribution thats similar to a boxplot, but shows
# more of the shape of the distribution is a violin plot

penguins %>%
    filter(!is.na(sex)) %>%
    ggplot(
        aes(x = sex, 
            y = flipper_length_mm, 
            fill = sex)) + 
    geom_violin() +
    geom_jitter(alpha = 0.4) # can also reduce the opacity of the dots


# Barplots ---

# the default barplot produces counts by a given variable
penguins %>%
    ggplot(aes(x = species)) +
    geom_bar()

# you can calculate other statistics using dplyr commands and then plot those with bars
# you need the option stat = "identity" if you are plotting the raw values of a
# given variable
penguins %>%
    group_by(species) %>%
    summarise(
        n = n(),
        prop = n/nrow(penguins) #calculates the proportion of the total sample for each species
    ) %>%
    ggplot(aes(x = species, y = prop, fill = species)) +
        geom_bar(stat = "identity")


# Lineplots ---
# basic lineplot
sa %>%
    ggplot(aes(
        x = year, 
        y = gdppc, 
        color = country)) +
    geom_line()


# let's remove the Maldives from the sample
sa %>%
    filter(country != "Maldives") %>%
    ggplot(aes(
        x = year, 
        y = gdppc, 
        color = country)) +
    geom_line()

## interlude on dates & times
# lubridate  is an R package that makes it easier to work with dates and times. 
# The lubridate package is not part of the core tidyverse packages, so it will 
# have to be loaded individually. 

# Date information is often provided as a string.  To generate date objects, you
# can call a function using `y`, `m`, and `d` in the order in which the year (y),
#month (m), and date (d) appear in your data. 

# Let's look at a few examples:

# year-month-date
ymd("1988-09-29")

# month-day-year
mdy("September 29th, 1988")

# day-month-year
dmy("29-Sep-1988")


# You could have a dataset where month, date, year, and/or time information are 
# included in separate columns. For example:

# let's create a data frame with year, monthm & day stored separately
df <- tribble(
    ~year, ~ month, ~day,
   2005, 1, 27,
   2014, 3, 5,
   2019, 8, 16,
   2020, 10, 11
)

df %>%
    mutate(
        date = make_date(year, month, day)
    ) 

# You can also extract information from date/date-time object with the functions: 
# `year()`, `month()`, `mday()`, `wday()`, `hour()`, `minute()` and `second()`.

mydate <- today()
mydate

## extract year information
year(mydate)

## extract day of the month
mday(mydate)

## extract weekday information
wday(mydate)

## label with actual day of the week
wday(mydate, label = TRUE)

# back to line plots!
# let's load the weather_data:
weather_df <- read_csv("data/weather_data.csv")

glimpse(weather_df)


# make a line plot of temperature by day
weather_df %>%
    mutate(
        day = mdy(day),
        temp_F = 9/5*(temp - 273) + 32 # converting rom Kelvin to F
    ) %>%
    ggplot(aes(x = day, y = temp_F)) +
    geom_line()


# format the dates on the x-axis with date_breaks() & date_labels()

# the below plot has ticks every 5 weeks and displays YYYY Mon DD for the labels
weather_df %>%
    mutate(
        day = mdy(day),
        temp_F = 9/5*(temp - 273) + 32 
    ) %>%
    ggplot(aes(x = day, y = temp_F)) +
    geom_line() +
    scale_x_date(
        date_labels = "%Y %b %d",
        date_breaks = "5 weeks")

# this version has ticks for every 2 weeks and shows Mon yy for the labels
# because there are so many more labels to show, I rotated them at a 25 degree
# angle to make it more legible
weather_df %>%
    mutate(
        day = mdy(day),
        temp_F = 9/5*(temp - 273) + 32 
    ) %>%
    ggplot(aes(x = day, y = temp_F)) +
    geom_line() +
    scale_x_date(
        date_labels = "%b %y",
        date_breaks = "2 weeks") +
    theme(
        axis.text.x = element_text(
            angle = 25
        )
    )

# we can also split plots (all ggplots - not just line plots) according to a factor
# (e.g., group) variable.

weather_df %>%
    mutate(
        day = mdy(day),
        temp_F = 9/5*(temp - 273) + 32 
    ) %>%
    ggplot(aes(x = day, y = temp_F)) +
    geom_line() +
    scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "3 weeks") +
    facet_wrap(~season, scales = "free_x") + 
    theme(
        axis.text.x = element_text(angle = 15)
    )


# here's another example with a barplot
sa %>%
    ggplot(aes(
        x = year, 
        y = gdppc)) +
    geom_bar(stat = "identity") +
    facet_wrap(~country)


################################################################################
# Data visualization: ggplot2 advanced customization
################################################################################

## Example 1: Penguin scatter plot
# start with a scatter plot of bill length vs depth - colored by species 
p1 <- penguins %>%
    ggplot(aes(x = bill_length_mm,
               y = bill_depth_mm,
               group = species)) +
    geom_point(aes(color = species, 
                   shape = species),
               size = 3,
               alpha = 0.8)

p1

# add a line of best fit - by species 
p2 <- p1 + geom_smooth(method = "lm", se = FALSE, aes(color = species))
p2

# change the colors
p3 <- p2 + scale_color_manual(values = c("darkorange","purple","cyan4"))
p3 

# properly label the axes
p4 <- p3 + labs(
    title = "Penguin bill dimensions",
    subtitle = "Bill length and depth for Penguins at Palmer Station LTER",
    x = "Bill length (mm)",
    y = "Bill depth (mm)",
    color = "Penguin species",
    shape = "Penguin species") 

p4

# use a built in theme to adjust the background, gridlines, fonts, etc
p5 <- p4 + theme_ipsum() 
p5

# look at the evolution (by combining the plots with patchwork)
p1  + p2 + p3 + p4 + p5 +
    plot_annotation(tag_levels = "1")

## putting it all together in a single call to ggplot
penguins %>%
    ggplot(aes(x = bill_length_mm,
               y = bill_depth_mm,
               group = species)) +
    geom_point(aes(color = species, 
                   shape = species),
               size = 3,
               alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    labs(
        title = "Penguin bill dimensions",
        subtitle = "Bill length and depth for Penguins at Palmer Station LTER",
        x = "Bill length (mm)",
        y = "Bill depth (mm)",
        color = "Penguin species",
        shape = "Penguin species") +
    theme_ipsum() +
    # you can adjust the legend placement in the "standard" theme option
    theme(
        legend.position = "bottom"
    )


## Example 2: Barplot of GDP per capita 
# first identify the 10 countries with highest gdp per capita in 2020

wdi_df %>%
    # filters for only observations from 2020
    filter(year == 2020) %>% 
    
    # removes any countries which are missing gdp data
    filter(!is.na(gdppc)) %>% 
    
    # sorts by gdp (default is to sort ascending, from lowest to highest)
    arrange(gdppc) %>% 
    
    # takes the bottom 10 (which are actually the 10 highest values)
    tail(10) %>% 
    
    # converts country to a factor and orders the factor according to levels of gdp
    mutate(country = factor(country),
           country = fct_reorder(country, gdppc)) %>%
    
    # selects just these variables so it's easy to see
    select(country, year, gdppc)



# ok, now let's plot that!
t1 <- wdi_df %>%
    filter(year == 2020) %>% 
    filter(!is.na(gdppc)) %>% 
    arrange(gdppc) %>% 
    tail(10) %>% 
    mutate(country = factor(country),
           country = fct_reorder(country, gdppc)) %>%
    select(country, year, gdppc) %>%
    ggplot(
        aes(x = country, 
            y = gdppc)) + 
    geom_bar(stat = "identity")

t1

# let's make the scales on the y-axis look nice
t2 <- t1 + scale_y_continuous(labels = scales::label_dollar())
t2

# now add some better labels
t3 <- t2 + labs(
    x = NULL, # this gets rid of the x-axis label - which we don't really need
    y = "GDP per capita (constant 2015 US$)",
    title = "Highest GDP per capita in the world",
    subtitle = "10 Richest Countries in 2020",
    caption = "Source: World Bank World Development Indicators"
    )

t3

# let's flip this so the bars are horizontal
    # we could have done this without coord_flip, by changing which variable we 
    # chose for x & y at the beginning too
t4 <- t3 + coord_flip() 
t4

# finally let's put it all together, choose a better color & alter the theme
top10 <- wdi_df %>%
    filter(year == 2020) %>% 
    filter(!is.na(gdppc)) %>% 
    arrange(gdppc) %>% 
    tail(10) %>% 
    mutate(country = factor(country),
           country = fct_reorder(country, gdppc)) %>%
    select(country, year, gdppc) %>%
    ggplot(
        aes(x = country, 
            y = gdppc)) + 
    geom_bar(stat = "identity",
             fill = "darkorange") +
    scale_y_continuous(labels = scales::label_dollar()) + 
    labs(
        x = NULL, # this gets rid of the x-axis label - which we don't really need
        y = "GDP per capita (constant 2015 US$)",
        title = "Highest GDP per capita in the world",
        subtitle = "10 Richest Countries in 2020",
        caption = "Source: World Bank World Development Indicators"
    ) +
    coord_flip() +
    theme_ipsum() 
  
top10  

# let's make the equivalent plot for the poorest 10 countries
bottom10 <- wdi_df %>%
    filter(year == 2020) %>% 
    filter(!is.na(gdppc)) %>% 
    arrange(gdppc) %>% 
    
    # the only difference is here we take the top 10 (which have the lowest GDP values)
    head(10) %>% 
    
    # then to sort the country by gdp in reverse we add a minus sign to the fct_reorder below
    mutate(country = factor(country),
           country = fct_reorder(country, -gdppc)) %>%
    
    select(country, year, gdppc) %>%
    ggplot(
        aes(x = country, 
            y = gdppc)) + 
    
    # choosing a different color to differentiate from the top 10
    geom_bar(stat = "identity",
             fill = "purple") +
    scale_y_continuous(labels = scales::label_dollar()) + 
    labs(
        x = NULL, # this gets rid of the x-axis label - which we don't really need
        y = "GDP per capita (constant 2015 US$)",
        title = "Lowest GDP per capita in the world",
        subtitle = "10 Poorest Countries in 2020",
        caption = "Source: World Bank World Development Indicators"
    ) +
    coord_flip() +
    theme_ipsum() 

bottom10

# we can combine them with patchwork too
top10 + bottom10

# if you use the / (divide) operator, then the plots will be stacked
top10 / bottom10
