This is an R package to load and manipulate page-level wordcount data from the Hathi Trust.

It's currently set up unstably for my Humanities Data Analysis class, and a few of the vignettes are broken.

It's functionality is almost entirely through one function: `hathi_counts`, 
which takes as an argument an htid or list of htids. It returns a tibble object with 
information about full word counts for the requested book.
