# rdt 3.3
* Save the modification time of files that are used by the script in the provenance
* Save absolute path to the provenance directory in the JSON.
* Store hash values of the main and sourced scripts in the provenance.
* When run in console mode (that is, interactively), the commands are
saved in a file called Console.R in the provenance scripts directory.
They are also saved in a separate console folder where they are saved in a file that includes the timestamp.
* Record in the provenance variables that are defined before the script starts but used within the script as being "from the environment".
* Updated for R 4.0
* Stops gracefully if there is an error when exiting
* Fixed several bugs
    * Fixed a problem when a sourced script reads a file.
    * Fixed a problem with creating plots when running R Markdown in console mode.
    * Fixed a problem with creating error nodes with long error messages.
    * Added error checking to I/O tracing    


# rdt 3.2.1

* Minor changes to documentation
* Moved provSummarizeR and provViz from the Suggests list to the Imports list

# rdt 3.2

* Save hash values for URLs
* Added a vignette
* Added support for RMarkdown
* Save standard output in the provenance
* Store arguments passed to prov.run in the provenance
* prov.run now accepts all the parameters of R's source function
* Improved support for environments
* Fixed problem with sourced scripts so they show up in the sourced script list, not the input file list.
* Fixed bug in how hash values are computed
* Fixed bug in matching up files that are both read and written by the same script

# rdt 3.1.1

* Fixed problem with sourced scripts so they show up in the sourced script list, not the input file list.
* Fixed bug in how hash values are computed
* Save hash values for URLs

# rdt 3.1.0

* With R 3.6.0, line numbers are correctly calculated and displayed.
* The elapsedTime field in procedure nodes now displays the time it took for the procedure itself to execute,
rather than the time taken to execute that procedure since the start of the script's execution.
* A new field is added in the environment node, totalElapsedTime, which displays the total amount of time all
procedures in the script took to execute.

# rdt 3.0.3

* Added optional parameter to prov.run to collect provenance for inputs/outputs only
and not for individual statements
* Renamed prov.display to prov.visualize
* Added prov.summarize function
* Modified prov.source so that it can be called even when provenance is not initialized.  In
that case it just sources the script.
* Fixed a bug that prevented creation of data->proc edges for calls to locally-defined 
functions that take no parameters.

# rdt 3.0.2

* Updated .ddg.installedpackages to work with the new type of return value from devtools::session_info
* Updated man pages
* Changed script numbers to start at 1 instead of 0
* Changed package name from RDataTracker to rdt


# RDataTracker 3.0.1

* Improved values stores in data nodes
* Corrected some problems with the value types recorded for data
* Added Device type for data nodes to represent base graphics devices
