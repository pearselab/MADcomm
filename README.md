[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/pearselab/MADcomm.svg?branch=master)](https://travis-ci.org/pearselab/MADcomm)
[![codecov](https://codecov.io/gh/pearselab/MADcomm/branch/master/graph/badge.svg)](https://codecov.io/gh/willpearse/MADcomm)
MADcomm - Make A Database of ecological community data
===============================================================
Will Pearse (will.pearse@usu.edu)

Part of the MAD world of packages that Make A Database from existing
data. *Use of MADcomm, and all MADworld packages, requires you to
cite the underlying trait data it downloads* - the function
`citations` will give you this citation information for whatever data
you are working with.

# Installation

```{R}
# install.packages("devtools") # (If devtools not installed)
library(devtools)
install_github("willpearse/MADcomm")
```

# Getting started

Pick a directory on your hard-drive that you can use as a 'cache' to
store data downloaded from individual papers/repositories using
MADcomm. Mine, for example, is `~/Code/MADcomm/cache`. This is
optional, but recommended, as otherwise it will take a very long time
to use MADcomm every time you use it. Once you've chosen that, run
the following:

```{R}
library(MADcomm)
data <- MADcomm("~/Code/MADcomm/cache")
```

This will take a while the first time, but as long as you always use
that same cache location, it will be almost instantaneous after
that.

Once you have that data, you can optionally 'clean' it to harmonisee
species' names across sites. Note that the nomenclature used in
MADcomm isn't guaranteed to be the one you prefer - read on to learn
more about the internal structure of MADcomm to do such cleaning for
yourself. Also note that, right now, this function doesn't do
anything - but it will soon, so please get into the habit of using it.

```{R}
clean.data <- clean.MADcomm(data)
```

You can now subset your data according to particular set of species or
sites:

```{R}
# Grab the first 10 sites in the dataset
clean.data[sites(clean.data)[1:10],]

# Grab only the data with Will's favourite oaks
clean.data[,c("quercus_robur","quercus_ilex")]
```

# MADcomm structure

A MADcomm data object is really just four `data.frame`s in a list:
(1:`data`) the species seen at each site, (2:`spp.metadata`) meta-data
information about species, (3:`site.metadata`) meta-data information
about sites (location, etc.), and (4:`study.metadata`) meta-data
information about each study. 

Note that the last column in each of the `data.frame`s is special:
it's `metadata`. This is set of `key:value` pairs, separated by `;`,
that allow you to extract additional information about each trait
observation (e.g., the `latitude` at which it was recorded).

# Contributing to MADcomm and its internals

Thank you for your interest in the package! We have a detailed set of
instructions for how the package works up available online
https://github.com/willpearse/MADcomm/wiki. Please follow those
instructions!

