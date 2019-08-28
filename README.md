# MERCAT
Molecular Epidemology Researcher's Collection of Analytical Tools

MERCAT is an R package for analysing molecular epidemiological data. 
Copyright (C) 2019 Zach Aandahl, Natalia Vaudagnotto, Sangeeta Bhatia,
Arthur Street, Andrew Francis and Mark Tanaka

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


## Installing R and required R packages
To install MERCAT you first need to install R. If you are new to R we recommend 
using RStudio. If you already have R we suggest upgrading to the latest version
of R. Once you have installed R, you will need to install all of the following
packages within R before installing MERCAT. 

The following packages are required for the main mercat package:
> install.packages(c("igraph", "dplyr", "rlang", "forcats", "stringr", "tidyr", "purrr", "reshape2", "ggplot2", "ggforce", "scales", "magrittr", "shiny"))

The following packages are required to run m_interactive():
> install.packages(c("shinyjs", "jsonlite", "ggthemes", "Cairo", "tools"))

Note: you may need to install these packages *before* installing mercat from 
source. You may also need to install the packages one at a time if there are 
any compilation errors. 


## Installing MERCAT from GitHub
You need the devtools package for R to install MERCAT from GitHub:
> install.packages("devtools")

After you have installed devtools you can install MERCAT with the following 
commands:
> library(devtools)

> install_github("zaandahl/mercat", build_opts = c("--no-resave-data", "--no-manual"))

If you do not wish to install the vignette you can use:
> install_github("zaandahl/mercat")

## Installing MERCAT from a source tarfile
Download the latest version of MERCAT from http://www.tanakalab.unsw.edu.au/mercat/
and save the mercat.tar.gz file to an accesible location on your computer (e.g. your Desktop).

Note for Mac users: You may need to right click and choose "Save as" to prevent the
file from uncompressing after download. 

Start an R console from the same location where the file is located (or change your working 
directory with the setwd() command inside of R).

Install the mercat.tar.gz source tarfile with the following command in R:
> install.packages("mercat.tar.gz", repos = NULL, type = "source")


## Loading MERCAT
Load the mercat library:
> library(mercat)

## MERCAT interactive
MERCAT interactive is a full-featured browser-based Shiny application that 
allows you to examine data through a number of visualisations. Use the following 
command to run the interactive version of mercat:
> m_interactive()

## Simple Molecular Epidemiology Format (SMEF)
The main file format for MERCAT is Simple Molecular Epidemiology Format or
SMEF. To learn more about this format and how you can get your data into 
MERCAT use the following command:
> help(smef)

## MERCAT help
You can get a list of commands available within MERCAT using the following command:
> help(package = "mercat")

## The MERCAT vignette
MERCAT includes a vignette which will guide you through a number of the main
features included with the software. Use the following command to view the
vignette:
> vignette("mercat")

## Viewing the MERCAT license
To view the MERCAT license enter the following command:
> file.show(system.file("extdata/lgpl-3.0.txt", package = "mercat"))

## Contacts
Zach Aandahl, z.aandahl@unsw.edu.au

Mark Tanaka, m.tanaka@unsw.edu.au

Natalia Vaudagnotto, natvau@gmail.com

Sangeeta Bhatia, sangeetabhatia03@gmail.com

Arthur Street, arthur@racingtadpole.com 

Andrew Francis, a.francis@westernsydney.edu.au

