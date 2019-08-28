#' mercat: Molecular Epidemiology Researcher's Collection of Analytical Tools
#'
#' MERCAT is an R package for analysing and visualising molecular epidemiological data.
#' 
#' Copyright (C) 2019 Zach Aandahl, Natalia Vaudagnotto, Sangeeta Bhatia,
#' Arthur Street, Andrew Francis and Mark Tanaka
#'
#' This library is free software; you can redistribute it and/or
#' modify it under the terms of the GNU Lesser General Public
#' License as published by the Free Software Foundation; either
#' version 3.0 of the License, or (at your option) any later version.
#' 
#' This library is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#' Lesser General Public License for more details.
#'
#' You should have received a copy of the GNU Lesser General Public
#' License along with this library; if not, write to the Free Software
#' Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#' 
#' @section MERCAT interactive:
#' 
#' MERCAT interactive is a full-featured browser-based Shiny application that 
#' allows you to examine data through a number of visualisations. Use the following 
#' command to run the interactive version of mercat:
#' 
#' \code{m_interactive()}
#' 
#' @section Simple Molecular Epidemiology Format (SMEF):
#' 
#' The main file format for MERCAT is Simple Molecular Epidemiology Format or
#' SMEF. To learn more about this format and how you can get your data into 
#' MERCAT use the following command:
#' 
#' \code{help(smef)}
#' 
#' @section MERCAT help:
#' 
#' You can get a list of commands from MERCAT using the following command:
#' 
#' \code{help(package = "mercat")}
#' 
#' @section The MERCAT vignette:
#' 
#' MERCAT includes a vignette which will guide you through a number of the main
#' features included with the software. Use the following command to view the
#' vignette:
#' 
#' \code{vignette("mercat")}
#'
#' @section Viewing the MERCAT license:
#' 
#' To view the MERCAT license enter the following command:
#' 
#' \code{file.show(system.file("extdata/lgpl-3.0.txt", package = "mercat"))}
#'
#' @section Contacts:
#'
#' \itemize{
#' \item{Zach Aandahl, z.aandahl@unsw.edu.au}
#' \item{Mark Tanaka, m.tanaka@unsw.edu.au}
#' \item{Natalia Vaudagnotto, natvau@gmail.com}
#' \item{Sangeeta Bhatia, sangeetabhatia03@gmail.com}
#' \item{Arthur Street, arthur@racingtadpole.com}
#' \item{Andrew Francis, a.francis@westernsydney.edu.au}
#' }
#' 
#' @docType package
#' @name mercat
NULL