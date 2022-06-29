This README file provides instructions on how to build *rdt* and *rdtLite* and run regression tests from the *RDataTracker* repository using Apache Ant.

## Building a Package

*rdt* and *rdtLite* are built using the ant files build-rdt.xml and build-rdtLite.xml, respectively. This can be done at the command line as follows:

```
ant -file build-<rdt|rdtLite>.xml <target>
```

where one of the following is substituted for target:

* build - this creates the tar.gz file that can be shared with others and installed
  as a library
* check - this builds the tar file (as build does) and then runs R CMD check on it
* install - this builds the tar file, checks it, and installs it as an R library
* cran-check - this builds the tar file and then runs the extra checks required by CRAN
* clean - this deletes the directory files that are copied to for a build.  It may occasionally 
  be useful if you think the build is not working properly.
  
--------------------------------------------------------------------

## Running Regression Tests

Regression testing of *rdt* and *rdtLite* is done using the ant file tests.xml.

To run a test, enter the following at the command line:

```
ant -file tests.xml <testName>-<rdt|rdtLite>
```

where testName is the name of an individual test.

To run all of the tests, enter the following:

```
ant -file tests.xml test-all-<rdt|rdtLite>

```


For more details on regression testing, please see the comments in tests.xml.
