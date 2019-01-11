# rdtLite 1.0.3

* Added prov.visualize function
* Added prov.summarize function
* Modified prov.source so that it can be called even when provenance is not initialized.  In
that case it just sources the script.
* Fixed a bug that prevented creation of data->proc edges for calls to locally-defined 
functions that take no parameters.

# rdtLite 1.0.2

* Updated .ddg.installedpackages to work with the new type of return value from devtools::session_info
* Updated man pages
* Changed script numbers to start at 1 instead of 0
* Changed package name from provR to rdtLite
* Added support for RMarkdown files

# provR 1.0.1

* Improved values stores in data nodes
* Corrected some problems with the value types recorded for data
* Added Device type for data nodes to represent base graphics devices
