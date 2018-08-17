# data-frame

[![Build Status](https://travis-ci.org/alex-hhh/data-frame.svg?branch=master)](https://travis-ci.org/alex-hhh/data-frame)

A data frame implementation for Racket.  This is vaguely inspired by various
other data frame implementations from other programming languages and
environments.  However, this implementation follows its own path and does not
aim to be compatible with anything.  It has been successfully used in
[ActivityLog2](https://github.com/alex-hhh/ActivityLog2), where it can easily
handle data sets of about 40 columns and 10000 rows.

You probably want to install this using the Racket package manager, which will
also install the documentation:

    racko pkg install data-frame
    
At this time, the official source for this code is in the ActivityLog2
project, at [this
location](https://github.com/alex-hhh/ActivityLog2/tree/master/rkt/data-frame),
which also contains the unit tests for this code.  The Racket package itself
is experimental, if you like it, consider providing feedback, or even better,
improvements.
