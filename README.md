# data-frame

[![Build Status](https://dev.azure.com/alexharsanyi0641/racket-packages/_apis/build/status/alex-hhh.data-frame?branchName=master)](https://dev.azure.com/alexharsanyi0641/racket-packages/_build/latest?definitionId=4&branchName=master)

## Installation

    raco pkg install data-frame

## Overview

A data frame implementation for Racket which is vaguely inspired by various
data frame implementations from other programming languages and environments.
However, this implementation follows its own path and does not aim to be
compatible with anything.  It has been successfully used in
[ActivityLog2](https://github.com/alex-hhh/ActivityLog2), where it can easily
handle data sets of about 40 columns and 10000 rows -- bigger data sets work
too, but these are the data set sizes that I use.

The code is commented and there is [user documentation][ud] too, plus [a blog
post][bp] provides a tutorial for its features, but at a glance, these are:

* can read data from SQL queries and read/write data to/from CSV and GPX files

* code to construct new data frames is simple (the code to read/write CSV
  files is 200 lines of Racket code, the code to read data from SQL is 70
  lines)

* different methods for selecting/filtering of a subset from a data frame,
  including an `in-data-frame` generator for use in `for` loops.

* reference and update individual elements, these can be used for data cleanup
  (but there is room for improvement here)

* Efficient row lookup for data series that are sorted

* Interpolated value lookup for when there are large gaps between data
  samples.

* Statistics (average, min, max, etc) based on `math/statistics`, including
  using sample weights, when data is collected at irregular intervals

* Generating histograms of data, including using sample weights when data is
  collected at irregular intervals.

* Line simplification and scatter plot simplification algorithms for when you
  want to plot several thousand data points in a reasonable amount of time.

* Spline interpolation for when you want to plot a smooth line for data that
  sampled at big intervals

* Least-Square fitting of data (polynomial, power and exponential) for when
  you want to draw a trend line on a scatter plot

There are a few other things for which there is a foundation, but no user API
yet, for example the data-frame implementation supports efficient appending of
elements to both ends, but there is no user API for that.

[ud]: http://docs.racket-lang.org/data-frame/index.html
[bp]: https://alex-hhh.github.io/2018/08/racket-data-frame.html
