This README provides instructions on how to build RDataTracker and run the regression tests.

Building RDataTracker is controlled by the build.xml ant file.  You can run this on the 
command line as:

ant <target>

substituting the name of a target for <target>.  The targets are:

- build - this creates the tar.gz file that can be shared with others and installed
  as a library
- check - this builds the tar file (as build does) and then runs R CMD check on it
- install - this builds the tar file, checks it, and installs it as an R library
- cran-check - this builds the tar file and then runs the extra checks required by CRAN
- clean - this deletes the directory files that are copied to for a build.  It may occasionally 
  be useful if you think the build is not working properly.
  
--------------------------------------------------------------------

Regression testing is controlled by the tests.xml ant script

To run all the tests, first be sure that you have the version of the 
library that you want to test installed. You can do this by entering:

ant install

Then, to run all of the regression tests, enter:

ant -file tests.xml   

To run an individual test on the current library, please read the comment at
the top of the tests.xml file.
