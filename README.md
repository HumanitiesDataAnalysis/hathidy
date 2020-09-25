The HathiTrust has over 17 million books, both in and out of copyright, for which page-level wordcount data is available. This is an R package to load and manipulate page-level wordcount data that the HathiTrust Research Center has made available in R.

**Note**: This package only works with [version 2.0 of the HTRC extended features released in 2020](https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329). If you wish to work with the version 1.0 features or their "pairtree" format, you should install from the [1.1 release.](https://github.com/HumanitiesDataAnalysis/hathidy/releases/tag/v1.1)

The name is a nod to the `tidyverse` family of packages on which it is built; the basic format of this package is to return data frames with token, page, and part-of-speech information that can then be used directly with `tidyverse` packages, and in packages like `stylo` the same way that `tidytext` parsing is.

This provides a subset of the tools for working with extended-feature files in the Python package maintained by Peter Organisciak: [https://github.com/htrc/htrc-feature-reader]. The goal here is to provide a simple, id-based form of access and caching. This should work well for exploratory data analysis in classroom and workshop settings; and research that uses htids and dataframes as here should be more easily reproducible and translatable to other frameworks.

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

The extracted features also include a great deal of metadata; any of this can be bound to the frame as well by passing it as an argument.

You can access further metadata given an htid, oclc number, or other identifier using ropensci's [Hathi package](https://github.com/ropensci/hathi). (**Note--for the time being, this seems not be available on CRAN for the latest versions of R**).

Although this data is fetched by default from the web, I have an opinionated take on how you should store this information; using the Hathi's stubbytree format. It will nag you
to designate a local folder in which to store data; it will also cache Apache Arrow 'feather' files there. You can also pass it a filename path directly, but I discourage this practice; Hathi Trust filenames are different than identifiers (for example, every occurence of the character "/" is replaced with "="), and your life will be easier if you never use the filename system.

### Caching and Apache Arrow

Rather than caching to csv as in version 1.0, `hathidy` now caches to the newly solidified [Apache Arrow](https://arrow.apache.org/) "feather" format.
(Note that this version 2.0 of 'feather' inside R.) Feather/Arrow is a fast, stable, and efficient binary serialization format for data tables that
allows speedy interchange across almost all languages; it's perhaps jumping the gun to include it in a package like this just a couple months after its
release, but TBH I hope that I can encourage people to use it.

By default, this package gzips feather files before storing to disk. This is more efficient but significantly slower than the native compression formats built
into arrow; I do it this way because I tend to pass arrow files around among R, Python, and Javascript pretty regularly, and JS support for internal arrow compression is lacking.

### Reproducibility.

One of the goals here is to support reproducible research workflows. One important thing to note is that the details of file storage are not important for reproducible code, so these are handled through calls to set various options.

The most important are:

1. The location of a directory where feature count files are stored. Downloading to this directory happens automatically.
   By default, this downloading happens to a temporary directory; but this is impolite and unnecessary.
2. The *structure* of that directory. Hathi Extended Features and this package use,
   by default, a system called 'stubbytree' in which files are nested inside folders
   to avoid overfilling if you have millions of files. 

A typical researcher might have many projects using Hathi EF data. A typical workflow might involve writing drafts of the paper
using a single directory to store everything, like: `options('hathidy_dir' = /Users/bschmidt/hathi-ef)`. If working with more than a few hundred files,
it would make sense to fill up that directory using rsync or the python ef tools, rather than downloading one at a time over http.

And then before uploading to a repository, copy the related files inside your project into a local folder, and 
rerun the code with something like this: `options('hathidy_dir' = './hathi_data_files')`
