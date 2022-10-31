## Resubmission

This is a resubmission. In this version I have:

* replaced \dontrun{} with \donttest{} since examples are not executable in < 5 sec

* Revised functions so none write messages to the console. 


## Resubmission

This is a resubmission. In this version I have:

* Used only undirected quotation marks in the description text. e.g. `lm4` --> 'lm4'

* Revised the Description field to provide more details about the package functionality. 

* Included references describing the methods in the package in the description field of the DESCRIPTION file in the form authors (year, ISBN:...)


## R CMD check results

There were no ERRORs or WARNINGs.

There are 4 NOTEs:

-   This is a new submission.

-   Possibly misspelled words in DESCRIPTION:
    Bosker (15:666)
    Bryk (15:579)
    Hox (15:612)
    Raudenbush (15:566)
    Snijders (15:655)
    al (15:619)
    et (15:616)
    intraclass (15:226)
    reliabilities (15:397)

    These words are not misspelled.

-   checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'

    As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.
