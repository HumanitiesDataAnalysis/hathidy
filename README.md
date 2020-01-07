The HathiTrust has over 15 million books, both in and out of copyright. This is an R package to load and manipulate page-level wordcount data that the HathiTrust Research Center has made available in R. It assumes you're using the `tidyverse` system of packages.


# Installation

Install from github.

```{r}
if (!require(remotes)) install.packages("remotes")
remotes::install_github("HumanitiesDataAnalysis/hathidy")
```

# About

See [the vignette](https://humanitiesdataanalysis.github.io/hathidy/articles/Hathidy.html) for details, and the [function definition](https://humanitiesdataanalysis.github.io/hathidy/reference/hathi_counts.html) for the basic API.

## Basic use

Functionality here is almost entirely through one function: `hathi_counts`, 
which takes as an argument one htid or a vector of htids. It returns a tibble object with 
information about full word counts for the requested book, including pages, parts of speech, and so on.

You can access further metadata given an htid, oclc number, or other identifier using ropensci's [Hathi package](https://github.com/ropensci/hathi).

Although this data is fetched by default from the web, I have an opinionated take on how you should store this information; using the Hathi's pairtree format. It will nag you
to designate a local folder in which to store data; it will also cache CSV files there. You can also path it a filename path directly.

```{r}

```
