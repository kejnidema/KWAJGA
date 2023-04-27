library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(stevemisc)
library(readxl)
library(countrycode)
library(democracyData)
library(peacesciencer)
library(data.table)

create_data <- function() {
    create_dyads() %>%
        add_joinreg() %>%
        add_democrats() %>%
        add_milcap() %>%
        add_contig() %>%
        add_disputes() %>%
        add_allies() %>%
        add_affinity() %>%
        add_gdp()
}

# use the peacesciencer r library to create the initial diads for the years 1950-1985
# a different range can also be specified as the first argument if needed
# the default is the range specified in the paper (1950-1985)
create_dyads <- function(years = 1950:1985) {
    create_dyadyears(system = "cow", mry = FALSE, directed = TRUE, years)
}

# DEM -> Democracy rating (Polity2)
# AUT -> Autocracy rating (Polity2)
# PCON -> Concentration of Power (Polity2)
# pol_H -> Higher value of the two Polity2 scores
# pol_L -> Lower value of the two Polity2 scores
# selects only the country codes, year and polity2 scores of each country
# in the original paper the REG score is calculated as PCON * (DEM - AUT)
# but PCON is not supported in the newer polity datasets which is used here
# instead the polity2 score is used (DEM - AUT)
# then generates the joinreg score as (pol_H + pol_L) / (pol_H - pol_L + 1)

# TODO ABOUT THE EXPLANATION HERE!!!! ACTUALY CHECK POLITY DOCUMENTATION WHAT THIS POLITY 2 SCORE IS AND WHETHER IT DOES OR DOES NOT INCLUDE PCON IN ANY WAY
# IF IT DOES THE WHOLE EXPLANATION ABOVE IS UNNECESSARY AND THE CORRECT DATA IS INDEED BEING USED
# !!!! also the one below is wrong
add_joinreg <- function(dyads) {
    oldnames <- names(dyads)
    dyads %>%
        add_democracy() %>%
        mutate(joinreg = ifelse(polity21 >= polity22,
            (polity21 + polity22) / (polity21 - polity22 + 1),
            (polity22 + polity21) / (polity22 - polity21 + 1)
        )) %>%
        select(oldnames, joinreg)
}



# TODO actually check experimanetally what the best value for this is
# TODO? analyze the distributions and pick a cutoff point so that
# the ratio of the split is the same althought the split is probably different
#
# In the original paper this is based on the JOINREG variable.
# There it is calculated as PCON * (DEM - AUT) instead of just (DEM - AUT)
# PCON takes values from 1-10, therefore the score can be up to 10
# times lower here
#
# create democrats score (a dychotomous value, 1 if joinreg > 5)
# this criteria is arbitrarily decided experimentaly to acheive the
# closest reproduction of the papers results, because JOINREG is
# calculated differently
add_democrats <- function(dyads) {
    dyads %>% mutate(democrats = ifelse(joinreg > 5, 1, 0))
}


# use COW milirty capabilities index (Singer et al. 1972)
# CAPRATIOij,t is the ratio of the stronger state's capabilities
# to those of the weaker one.
# this data is available from the peacesciencer r library
#
# TODO maybe need to not to this conditional and let it be asymmetrical
add_milcap <- function(dyads) {
    oldnames <- names(dyads)
    dyads %>%
        add_nmc() %>%
        mutate(capratio = ifelse(cinc1 > cinc2,
            cinc1 / cinc2,
            cinc2 / cinc1
        )) %>%
        select(oldnames, capratio)
}

# two types of dyads with potential for conflict
# 1: contiguous dyads
# 2: noncontiguous dyads containing a major power
# Major Powers:
# Name   : ccodes
# __________________
# US     : 2
# UK     : 200
# France : 220
# China  : 710
# USSR   : (cant find, use Russia : 365???)
#
# are all paired with all other states
# CONTIGij,t then equals one for contiguous dyads
# including those indirectly contiguous throuch their colonies
# zero for the non contiguos dyads involving major powers
#
# ^ that's how the paper describes it but the data has contiguity
# on a 1-5 scale, which here are all converted to 1
# I think the data only has contiguos pairs, implicitly showing the
# noncontiguos pairs
# possibly it should be 0 for every ij,t on the contiguity list and 0 otherwise
add_contig <- function(dyads) {
    oldnames <- names(dyads)
    dyads %>%
        add_contiguity() %>%
        mutate(conttype = ifelse(conttype == 0, 0, 1)) %>%
        select(oldnames, conttype)
}


# not quite sure which of the dispute datasets is meant to be used here
# for now it is 1 only for actual confilcts and not provocations etc.
# while in the paper it is 1 for all disputes
add_disputes <- function(dyads) {
    oldNames <- names(dyads)
    dyads %>%
        add_cow_mids() %>%
        mutate(dispute = ifelse(cowmidongoing == 1, 1, 0)) %>%
        select(oldNames, dispute)
}


# according to the authors
# alliesij,t = 1 if i and j are formally allied
# OR if both are allied with the united states   !!!! TODO
# 0 otherwise
# uses the COW alliance dataset. This dataset has 4 dichotomous
# variables. The authors describe this as a single dichotmous variable
# without mentioning how to combine the 4 variables into one
#
# After looking at the codebook each one, seems to represents some level
# of alliance or reluctance towards military conflict.
# Therefore this variable is 1 when at least one of the four variables
# is 1, otherwise 0
add_allies <- function(dyads) {
    oldnames <- names(dyads)
    dyads %>%
        add_cow_alliance() %>%
        mutate(allies = ifelse(cow_defense == 1 |
            cow_neutral == 1 |
            cow_nonagg == 1 |
            cow_entente == 1, 1, 0)) %>%
        select(oldnames, allies)
}

# This data is obtained from the original paper's author
# it only contains the one sided dyads
# to obtain the symetric dyads the data is duplicated,
# the ccode1 and ccode2 columns are swapped, and
# it is then appended to the original data
#
# there are two variables for dyad affinity (s3un4608, s3un4608),
# however there are often missing entries in one or both
# when both are present the mean is taken, otherwise
# the one that is present
#
# the other two variables (s3un4608i, s3un4608i) are the same
# but with interpolated data to fill in the blanks
# For these two the mean is always taken
add_affinity <- function(dyads) {
    read_dta("affinity.dta") %>%
        rename(c(ccode1 = ccodea, ccode2 = ccodeb)) %>%
        select(ccode1, ccode2, year, s3un4608, s2un4608, s3un4608i, s2un4608i) -> affinity

    affinity_inverse <- rename(affinity, c(ccode1 = ccode2, ccode2 = ccode1))

    rbind(affinity, affinity_inverse) %>%
        mutate(sun = ifelse(s3un4608 & s2un4608,
            (s3un4608 + s2un4608) / 2,
            ifelse(s3un4608, s3un4608, s2un4608)
        )) %>%
        mutate(suni = (s2un4608i + s3un4608i) / 2) -> affinity

    dyads <- merge(dyads, affinity, by = intersect(names(dyads), names(affinity)))
    dyads
}


# Uses the RGDPCH indicator from the Penn World Tables
add_gdp <- function(dyads) {
    gdp <- create_gdp()
    oldnames <- names(dyads)
    merge(dyads, gdp, by = intersect(names(dyads), names(gdp)))
}

# The data uses country names in plain english which must be converted to the COW 
# country codes. 
create_gdp <- function() {
    gdp_named <- load_gdp()
    # construct a table containing both country names and COW country codes
    name_to_ccode <- gdp_name_to_ccode_table(gdp_named)

    # join the two tables on country name
    gdp_named_coded <- left_join(gdp_named, name_to_ccode, by = "Country")
    # select only the COW country code, year, and RGDPCH indicator
    ccgdp <- select(gdp_named_coded, Year, RGDPCH, ccode)
    ccgdp
}

# loads the data while simultaneously replacing the names of the contries
# whose names were shortened in order to make them recognizable for the
# countrycode library
load_gdp <- function() {
    read_excel("gdp.xls") %>%
        select(Country, Year, RGDPCH) %>%
        as.data.frame() -> gdp
    gdp$Country[gdp$Country == "CENTRAL AFR.R."] <- "CENTRAL AFRICAN REPUBLIC"
    gdp$Country[gdp$Country == "UNITED ARAB E."] <- "UNITED ARAB EMIRATES"
    gdp$Country[gdp$Country == "PAPUA N.GUINEA"] <- "PAPUA NEW GUINEA"
    gdp
}

# uses the countrycode library to create a table containing country names
# next to their COW country codes
gdp_name_to_ccode_table <- function(gdp) {
    Country <- gdp$Country
    ccode <- countrycode(Country, "country.name", "cown")
    combined <- unique(cbind(Country, ccode), incomparables = FALSE, fromLast = FALSE)
    name_to_ccode <- as.data.frame(combined)
    # the library misses hong kong so it is added manually
    name_to_ccode$ccode[name_to_ccode$Country == "HONG KONG"] <- 997
    name_to_ccode
}


# use IMF data for imports and exports
# todo create depends score based on the above two
# dependsij,t = (importsij,t + exportsij,t) / (gdpij,t)
# these need not be symmetrical

# todo create interdepends with depends similar to joinreg
# interdependsij,t =
# caution: interdepends is not symmetrical
# (dependsij,t + dependsji,t) / (dependsij,t - dependsji,t + 1)

# todo calculate dINTERDEP
# declining trade may be a signal or even cause of detereiorating
# political relations
# to measure this dINTERDEP = INTERDAEPij,t - INTERDEPij,t-4
# when necessary, use shorter time periods (3,2,1 years)

# todo economic growth
# calculate the average annual change in real GDP per capita percent
# (Summers & Heston 1988) of countries i,j of the previous three years
# (or as with dINTERDEP over shorter periods when necessary)
# GROWTHij,t = (GROWTHH + GROWTHL) / (GROWTHH - GROWTHL + 1)


# --- END OF DATA PREPARATION ---------------------------------------


# TODO testing the liberal peace
# dot <- read_stata("dot.dta")

data <- create_data()


model_equation <- function(data, equation) {
    glm(equation, data = data, family = "binomial")
}

# DEMOCRACY is first represented by JOINREG then DEMOCRATS

# Equation 1:
# DISPUTEij,t =
# B0 +
# B1 * DEMOCRACYij,t +
# B2 * GROWTHij,t +
# B3 * ALLIESij,t +
# B4 * CONTIGij,t +
# B5 * CAPRATIONij,t +da
# B6 * INTERDEPij,t-1

# equation1 <- dispute ~ joinreg + growth + allies + conttype + capratio + interdep
# equation1_joinreg <- dispute ~ joinreg + capratio + conttype + allies
# equation1_democrats <- dispute ~ democrats + capratio + conttype + allies
# model_eq1_joinreg <- model_equation(data, equation1_joinreg)
# model_eq1_democrats <- model_equation(data, equation1_democrats)

# Equation 2:
# DISPUTEij,t =
# B0 +
# B1 * DEMOCRACYij,t +
# B2 * GROWTHij,t +
# B3 * ALLIESij,t +
# B4 * CONTIGij,t +
# B5 * CAPRATIONij,t +
# B6 * INTERDEPij,t-1 +
# B7 * dINTERDEPij,t-1

# equation2_joinreg <- dispute ~ joinreg + growth + allies + conttype + capratio + interdep + dinterdep
# equation2_democrats <- dispute ~ democrats + growth + allies + conttype + capratio + interdep + dinterdep
# model_eq2_joinreg <- model_equation(data, equation2_joinreg)
# model_eq2_democrats <- model_equation(data, equation2_democrats)

# Equation 1a:
# DISPUTEij,t =
# B0 +
# B1 * DEMOCRACYij,t +
# B2 * GROWTHij,t +
# B3 * ALLIESij,t +
# B4 * CONTIGij,t +
# B5 * CAPRATIONij,t +da
# B6 * INTERDEPij,t-1
# B7 * AFFINITYij,t

# equation1a_joinreg <- dispute ~ joinreg + growth + allies + conttype + capratio + interdep + affinity
# equation1a_democrats <- dispute ~ democrats + growth + allies + conttype + capratio + interdep + affinity
# model_eq1a_joinreg <- model_equation(data, equation1a_joinreg)
# model_eq1a_democrats <- model_equation(data, equation1a_democrats)

# Equation 2:
# DISPUTEij,t =
# B0 +
# B1 * DEMOCRACYij,t +
# B2 * GROWTHij,t +
# B3 * ALLIESij,t +
# B4 * CONTIGij,t +
# B5 * CAPRATIONij,t +
# B6 * INTERDEPij,t-1 +
# B7 * dINTERDEPij,t-1
# B8 * AFFINITYij,t

# equation2a_joinreg <- dispute ~ joinreg + growth + allies + conttype + capratio + interdep + dinterdep + affinity
# equation2a_democrats <- dispute ~ democrats + growth + allies + conttype + capratio + interdep + dinterdep + affinity
# model_eq2a_joinreg <- model_equation(data, equation2a_joinreg)
# model_eq2a_democrats <- model_equation(data, equation2a_democrats)

# summary(model_eq1_joinreg)
# summary(model_eq1_democrats)
# summary(model_eq2_joinreg)
# summary(model_eq2_democrats)
# summary(model_eq1a_joinreg)
# summary(model_eq1a_democrats)
# summary(model_eq2a_joinreg)


# TODO might need to check this later
#
# to indicate the influence of each independent variable on the likelihood
# of involvement in a dispute, first calculate a baseline probability against
# which comparisons are made by setting each of the contiguos measures
# at their mean and making ALLIES and CONTIG 0
# then adjusted each independent variable in turn by adding,
# for each of the continuous variables, one standard deviation
# to its mean value or, for ALLIES and CONTIG, setting them to 1
# mean values and std. dev in appendix (liberal peace)
