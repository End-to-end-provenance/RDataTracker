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
