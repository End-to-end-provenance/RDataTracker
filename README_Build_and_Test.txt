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

Regression testing is controlled by the test.xml ant script

To build and install the library and run all the tests say:

ant -file test.xml   

To run an individual test on the current library (no new install), say:

ant -file test.xml <target>

Look at the test.xml file for the names of the targets that identify individual tests
or groups of tests.  These targets appear after the line:

  <!--############################ End FUNCTIONS ############################-->

When you run a test, it will create several files names something like:

<target>.out - standard output from executing the test
ddg/ddg.txt - ddg from executing the test
source_<target>.out - standarad output from executing the test with ddg.source
ddg-source/ddg.txt - ddg from executing the test with ddg.source

The first 2 output files are running the R script in a batch environment.

The ddg.source results are similar, but not identical to what you would get 
running in RStudio, selecting all and clicking Run.  Not all tests produce
ddg.source outputs.

After the tests are run, the .out files and the ddg.txt files are compared
to expected_ and source_expected files with the same names.  The differences
are displayed on standard output.  If the tests work as before, the only 
differences you should see are in the execution time.  The calculate square
roots tests use random numbers and so produce differences on each execution.  
Some tests, particularly those that create plots, may produce different 
.out files depending on whether they are run on Mac or PC.

If the tests stop with Build failed, see what the last test was that was attempted.
Look in its .out file(s).  Most likely, there is something that caused it to 
not work.  Perhaps a library function has changed and the test needs to be
updated.  Perhaps the test needs a network connection and you are not online.
It is usually easy to understand the problem by looking at the .out files.

If a test displays differences and the expected value is the correct value, it 
means that you have introduced a bug into the RDataTracker library.  Figure out
what it is and fix it.

If a test displays differences and the new version is the correct value, you need
to update the expected files. Determine which files to update based on the test
output.  Then copy the new result to the expected result file.  Then, edit the
expected result, replacing all occurrences of the value used for WorkingDirectory with
[DIR].  This enables the comparison functions to ignore changes in the working directory
when comparing results with expected, making the tests nicely portable from
one computer to another.  For example, if ddg.txt has:

WorkingDirectory="/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/consoleSource"  

replace all occurrences of 

/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/consoleSource

with 

[DIR]

Then, the WorkingDirectory line should read:

WorkingDirectory="[DIR]"  

ALL USES OF THE STRING SHOULD BE REPLACED WITH [DIR] both in the expected 
ddg.txt file and the expected .out file.

Since the source output is not exactly the same as what you get when running in 
RStudio, it is good to manually test in RStudio at least occasionally.  Two longer
tests that have saved expected results are in the Aaron directory.  There will be
differences due to timestamps so a careful comparison is not possible, but you can at 
least see that there are not large chunks missing or added and that it runs
to completion.  Also, load the ddgs into DDG Explorer and check the error log
at the bottom of the window to make sure there were no problems load the file.  
  
Run in RStudio by loading the file, selecting all and clicking Run:
- Aaron/aaron-min-test.r - compare the ddg written to ddg-min/ddg.txt 
  with expected_minimal_console_ddg.tst
- Aaron/aaron-annotated-test.r - compare the ddg written to ddg-min/ddg.txt 
  with expected_annotated_console_ddg.tst