---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# cdwtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

cdwtools stands for **Clinical Data Warehouse tools**. This package aims to help clinicians, statisticians & data scientists work with CDW data, with data visualization, data cleaning, exploratory data analysis & model building tools, using a Shiny web application.

## Installation

<!--You can install the released version of cdwtools from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cdwtools")
```
-->

You can install the development version from Github, with :
``` r
devtools::install_github("BorisDelange/cdwtools")
```

## Overview

![](https://github.com/BorisDelange/cdwtools/blob/master/inst/app/www/get_started/patient_lvl_first_connection.jpg?raw=true)

The core principle of the App is the **modularity** : each user chooses which pages to display (called **modules** in the App), which **figures** display in each page, which **thesaurus items** each figure uses.

The App contains two sides : a **user side**, which requires no particular skill in R and a **developper side**, where new plugins can be developped.
A **plugin** is a Shiny code used to render and manipulate data. Some plugins are available in the **Articles section** of this website. You can **create new plugins** and share it with other people.

## Deployment

The App can be deployed **locally** or installed on a **server**. See *Deploy the App on ShinyProxy* in Articles section for more details.

## Security

The App uses [Shiny Manager](https://github.com/datastorm-open/shinymanager) to secure the access.
In the developper mode, you have direct access to the R environment and the database, so developper access has to be used carefully.
It is recommended to install the App on a secure local server, as health data will be handled.
