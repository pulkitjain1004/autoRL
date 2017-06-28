![PGRdup](inst/extdata/PGRdup.svg)

PGRdup: Discover Probable Duplicates in Plant Genetic Resources Collections
---------------------------------------------------------------------------

##### *J. Aravind, J. Radhamani, Kalyani Srinivasan, B. Ananda Subhash and R. K. Tyagi*

###### Copyright (C) 2014, [ICAR-NBPGR](http://www.nbpgr.ernet.in/) ; License: [GPL-2 | GPL-3](https://www.r-project.org/Licenses/)

The `R` package `PGRdup` was developed as a tool to aid genebank managers in the identification of probable duplicate accessions from plant genetic resources (PGR) passport databases.

This package primarily implements a workflow designed to fetch groups or sets of germplasm accessions with similar passport data particularly in fields associated with accession names within or across PGR passport databases.

The functions in this package are primarily built using the following R packages:

-   [`data.table`](https://cran.r-project.org/package=data.table)
-   [`igraph`](https://cran.r-project.org/package=igraph)
-   [`stringdist`](https://cran.r-project.org/package=stringdist)
-   [`stringi`](https://cran.r-project.org/package=stringi)

Installation
------------

The package can be installed from CRAN as follows:

``` r
install.packages('PGRdup', dependencies=TRUE)
```

Workflow
--------

The series of steps involve in the workflow along with the associated functions are are illustrated below:

#### Step 1

**Function(s) :**

-   `DataClean`
-   `MergeKW`
-   `MergePrefix`
-   `MergeSuffix`

Use these functions for the appropriate data standardisation of the relevant fields in the passport databases to harmonize punctuation, leading zeros, prefixes, suffixes etc. associated with accession names.

#### Step 2

**Function(s) :**

-   `KWIC`

Use this function to extract the information in the relevant fields as keywords or text strings in the form of a searchable Keyword in Context (KWIC) index.

#### Step 3

**Function(s) :**

-   `ProbDup`

Execute fuzzy, phonetic and semantic matching of keywords to identify probable duplicate sets either within a single KWIC index or between two indexes using this function. For fuzzy matching the levenshtein edit distance is used, while for phonetic matching, the double metaphone algorithm is used. For semantic matching, synonym sets or ‘synsets’ of accession names can be supplied as an input and the text strings in such sets will be treated as being identical for matching. Various options to tweak the matching strategies used are also available in this function.

#### Step 4

**Function(s) :**

-   `DisProbDup`
-   `ReviewProbDup`
-   `ReconstructProbDup`

Inspect, revise and improve the retrieved sets using these functions. If considerable intersections exist between the initially identified sets, then `DisProbDup` may be used to get the disjoint sets. The identified sets may be subjected to clerical review after transforming them into an appropriate spreadsheet format which contains the raw data from the original database(s) using `ReviewProbDup` and subsequently converted back using `ReconstructProbDup`.

#### Adjuncts

**Function(s) :**

-   `ValidatePrimKey`
-   `DoubleMetaphone`
-   `ParseProbDup`
-   `AddProbDup`
-   `SplitProbDup`
-   `MergeProbDup`
-   `ViewProbDup`
-   `KWCounts`
-   `read.genesys`

Use these helper functions if needed. `ValidatePrimKey` can be used to check whether a column chosen in a data.frame as the primary primary key/ID confirms to the constraints of absence of duplicates and NULL values.

`DoubleMetaphone` is an implementation of the Double Metaphone phonetic algorithm in `R` and is used for phonetic matching.

`ParseProbDup` and `AddProbDup` work with objects of class `ProbDup`. The former can be used to parse the probable duplicate sets in a `ProbDup` object to a `data.frame` while the latter can be used to add these sets data fields to the passport databases. `SplitProbDup` can be used to split an object of class `ProbDup` according to set counts. `MergeProbDup` can be used to merge together two objects of class `ProbDup`. `ViewProbDup` can be used to plot the summary visualizations of probable duplicate sets retrieved in an object of class `ProbDup`.

`KWCounts` can be used to compute keyword counts from PGR passport database fields(columns), which can give a rough indication of the completeness of the data.

`read.genesys` can be used to import PGR data in a Darwin Core - germplasm zip archive downloaded from genesys database into the R environment.

Tips
----

-   Use [`fread`](https://www.rdocumentation.org/packages/data.table/functions/fread) to rapidly read large files instead of `read.csv` or `read.table` in `base`.
-   In case the PGR passport data is in any DBMS, use the appropriate [`R`-database interface packages](http://www.burns-stat.com/r-database-interfaces/) to get the required table as a `data.frame` in `R`.

Note
----

-   The `ProbDup` function can be memory hungry with large passport databases. In such cases, ensure that the system has sufficient memory for smooth functioning (See `?ProbDup`).

Citing `PGRdup`
---------------

``` r
citation('PGRdup')
```

    #> 
    #> To cite the R package 'PGRdup' in publications use:
    #> 
    #>   Aravind, J., J. Radhamani, Kalyani Srinivasan, B. Ananda
    #>   Subhash, and R. K. Tyagi ().  PGRdup: Discover Probable
    #>   Duplicates in Plant Genetic Resources Collections. R package
    #>   version 0.2.2.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {PGRdup: Discover Probable Duplicates in Plant Genetic Resources Collections},
    #>     author = {{Aravind J} and {Radhamani J} and {Kalyani Srinivasan} and {Ananda Subhash B} and Rishi Kumar Tyagi},
    #>     note = {R package version 0.2.2},
    #>   }
