---
title:  Welcome to Bio 297!
description:  "Welcome to the main course website for Bioinformatics and Computational Biology. Please use our [sakai site](https://sakai.wlu.edu/portal/site/2015_16_FALL-BIOL_297_01) to access the syllabus and submit assignments."
output_formats:
  page: 
    render: 
      html_document:
        template:   ../templates/index-template.html
        toc: false
  slides: false
  PDF: false
---

{{> site-contents}}

## Learning R

Beginner:

* [tryR](http://tryr.codeschool.com/).  A new [code school](http://www.codeschool.com) that walks you through a very basic introduction to R.  A great way to start out if this is your first time working at a command line or with a scripting language.

* [Quick-R](http://www.statmethods.net/).  A great collection of quick-and-dirty How-to's for common data analysis tasks in R.  I'd recommend starting with a search here if you're stuck trying to figure out how to do something that should be simple.

* [R-Bloggers](http://www.r-bloggers.com/).  This site is a collection of R-related blogs from around the web. It's a great site to browse if you want to be inspired by cool things other people are doing in R.

* [R Documentation](http://www.rdocumentation.org/).  Documentation for R and all the current CRAN packages are available on this website with an advanced search interface and community submitted comments.

* [RStudio help](https://support.rstudio.com/hc/en-us/categories/200035113-Documentation).  This term you'll be doing all of your work in R using the fantastic [RStudio](http://www.rstudio.com/ide/) environment.  See the extensive help documentation to learn about all of the features of this platform.

Advanced:

* [Advanced R Programming](http://adv-r.had.co.nz/).  A new WIP book on advanced R programming by Hadley Wickham.  The current draft is incomplete in parts but also freely available!

* [R Inferno](http://www.burns-stat.com/pages/Tutor/R_inferno.pdf).  A quirky but useful exploration of common R pitfalls and many of the less-than-intuitive aspects of R language semantics (told in the model of Dante's work by the same name).

* [Evaluating the Design of the R Language](http://r.cs.purdue.edu/pub/ecoop12.pdf).  For programming language geeks this 2012 paper takes a pass at describing a formal semantics for the core langauge and provides a deep analysis of the run-time behavior of the reference R interpreter.

* [Parellel Computation on the GPU](http://www.r-tutor.com/gpu-computing).  This is a nice guide to a couple of R pacakges that let you use CUDA GPUs to perform computationally intensive operations.  All of the PCs in the IQ Center 2D lab have dual high-end nVidia GPUs.


