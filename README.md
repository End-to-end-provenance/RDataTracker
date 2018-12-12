RDataTracker, rdtLite and rdt
=============================

The *RDataTracker* repository contains R source code and associated files for *rdtLite* and *rdt*. These tools collect provenance as an R script executes or during an R console session and store the provenance in PROV-JSON format for use by other applications. [rdtLite](https://github.com/End-to-end-provenance/rdtLite) is a streamlined tool that will meet the needs of most users. [rdt](https://github.com/End-to-end-provenance/rdt) is a research tool that uses an annotated version of the original R script to collect provenance for statements inside functions and control constructs. *rdt* and *rdtLite* belong to a collection of [R Tools](https://github.com/End-to-end-provenance/End-to-end-provenance.github.io/blob/master/RTools.md) developed as part of a larger project on [End-to-end-provenance](https://github.com/End-to-end-provenance/End-to-end-provenance.github.io/blob/master/README.md).

A script is run nightly to create separate repositories for *rdtLite* and *rdt* to facilitate installation from GitHub. If you would like to install *rdtLite* or *rdt* directly from GitHub using devtools, use one of the following commands:

```
devtools::install_github("End-to-end-provenance/rdtLite")
devtools::install_github("End-to-end-provenance/rdt")
```

Please note that neither can be installed directly from the *RDataTracker* repository using devtools.

If you would like to build and install *rdtLite* and *rdt* and run regression tests using Apache Ant, clone the *RDataTracker* repository and see the file README_Build_and_Test.md.
