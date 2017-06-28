PGRdup 0.2.3.1
---------------------------------------
###OTHER NOTES:
 - Registered native routines in the C code for `DoubleMetaphone`.

***

PGRdup 0.2.3
---------------------------------------
###UPDATED FUNCTIONS:
 - `KWCounts` - Fixed error in case of large number of exceptions and fixed bug regarding non-exact removal of keyword exceptions.
 - `ProbDup` - Changed code with a column vector specifying the columns having `with=FALSE` argument to the new preferred syntax in `data.table`.
 - `ViewProbDup` - Fixed error 'formal argument "axis.ticks.y" matched by multiple actual arguments'.
 - `ViewProbDup` - Fixed bug in case when all factor names in `select` argument are not present in `factor.db1` and/or `factor.db2`, the function stops. Now it gives an warning and stops only when none of the factor names in `select` are present in `factor.db*`.
 
###OTHER NOTES:
 - Added `rmarkdown` to suggests field in DESCRIPTION, as prompted by Jan Górecki.

***

PGRdup 0.2.2.1
---------------------------------------
###OTHER NOTES:
 - Fixed memory access error in src/fdouble_metaphone.c (Thanks to Prof. Brian Ripley)
 
***

PGRdup 0.2.2
---------------------------------------
###NEW FUNCTIONS:
 - `read.genesys` - Convert 'Darwin Core - Germplasm' zip archive to a flat file.
 - `ViewProbDup` - Visualize the probable duplicate sets retrived in a `ProbDup` object.

###UPDATED FUNCTIONS:
 - `ReconstructProbDup` - Fixed bug regarding failure to retrieve db2 fields when method "c" is used.
 - `ProbDup` - Updated code after bugfix in `stringdist` package (`stringdistmatrix`: output was transposed when length(a)==1).
 
###OTHER NOTES:
 - Changed the contact email addresses of four authors (including maintainer) in DESCRIPTION.
 - Updated the vignette and README.md with the details of new functions.

***

PGRdup 0.2.1
---------------------------------------

###NEW FUNCTIONS:
 - `SplitProbDup` - Split an object of class `ProbDup`.
 - `MergeProbDup` - Merges two objects of class `ProbDup`.
 - `KWCounts` - Generates keyword counts from database fields.
 - `print.KWIC` - Prints summary of an object of class `KWIC` to console.
 - `print.ProbDup` - Pprints summary of an object of class `ProbDup` to console.

###UPDATED FUNCTIONS:
 - `ProbDup` - Modified the phonetic matching for better handling of strings with digits.
 - `ProbDup` - Fixed throwing of error when no duplicate sets are retrieved.
 - `ProbDup` - Fixed issue regarding memory out error when large number of exceptions are there.
 - `ProbDup` - Further converted code to use `data.table` package for greater efficiency and speed.
 - `ProbDup` - Fixed bug regarding inconsistent matching when method "b" is used.
 - `ProbDup` - Reduced the dimensions of the string matching matrices produced for greater efficiency and speed.
 - `MergeKW` - Modified for better handling of regex special characters.
 - `ReconstructProbDup` - Modified to ignore sets with counts less than 2 after reconstruction.

###OTHER NOTES:
 - Edited README.md formatting.
 - Added `diagram`, `microbenchmark` and `wordcloud` (required for vignette) to suggests field in DESCRIPTION.
 - Added imports to functions from `methods`, `stats` and `utils` as `R CMD check --as-cran` now checks code usage (via `codetools`) with only the base package attached.
 - Dropped the abbreviation PGR in the title in DESCRIPTION as it is mentioned in the description text.
 
###VIGNETTE
 - Added vignette "An Introduction to PGRdup package".

***

PGRdup 0.2.0
---------------------------------------
- First release
