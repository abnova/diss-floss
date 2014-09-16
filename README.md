diss-floss
==========

Repository of software project for my Ph.D. dissertation research. Implements reproducible research framework for data collection, analysis and reporting results, using `R` statistical language and environment.

Related information can be found in the repository's wiki at https://github.com/abnova/diss-floss/wiki.

**Installation Notes**

Path to the *project's home directory*, where it's installed should be specified in R environment file, either system-wide (requires corresponding permissions), or, better, local (file ".Renviron" in user's home directory), as follows:

```bash
DISS_FLOSS_HOME = <path to project home directory>
```

Successful import of open source software repositories' data and startup companies data requires specifying corresponding data sources' credentials as values of environment variables. This also should be done via R environment file, preferably the same ".Renviron" file, as above:

```bash
# SourceForge Research Data Archive (SRDA)
SRDA_USER  = <SRDA user name>
SRDA_PASS  = <SRDA password>

# CrunchBase
CB_API_KEY = <CrunchBase API key>
```
