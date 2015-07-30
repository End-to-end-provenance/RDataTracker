This README provides instructions on how to build RDataTracker and run the regression tests.

Building RDataTracker is controlled by the build.xml ant file.  You can run this on the 
command line as:

ant <target>

substituting the name of a target for <target>.  The targets are:

- build - this creates the tar.gz file that can be shared with others and installed
  as a library
- check - this builds the tar file (like build does) and then runs R CMD check on it
- install - this builds the tar file, checks in, and installs it as an R library
- cran-check - this builds the tar file and then runs the extra checks required by CRAN
- dropbox - this builds the tar file (but no check) and installs it on dropbox.
  It also copies the DDGCheckpoint.R file to dropbox.
- clean - this deletes the directory files are copied to for a build.  It may occasionally 
  be useful if you think the build is not working properly.
  
--------------------------------------------------------------------

Regression testing is controlled by the test-no-instrumentation.xml ant script

To build and install the library and run all the tests say:

ant -file test-no-instrumentation.xml   

To run an individual test on the current library, please read the comment at
the top of the test-no-instrumentation.xml file.

