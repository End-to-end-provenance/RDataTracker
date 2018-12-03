# rdt 3.0.3

* Modified prov.source so that it can be called even when provenance is not initialized.  In
that case it just sources the script.

# rdt 3.0.2

* Updated .ddg.installedpackages to work with the new type of return value from devtools::session_info
* Updated man pages
* Changed script numbers to start at 1 instead of 0
* Changed package name from RDataTracker to rdt


# RDataTracker 3.0.1

* Improved values stores in data nodes
* Corrected some problems with the value types recorded for data
* Added Device type for data nodes to represent base graphics devices
