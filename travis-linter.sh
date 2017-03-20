#!/bin/bash
set -e

exitstatus=0

Rscript -e "lintr::lint_package()"
outputbytes=`Rscript -e "lintr::lint_package()" | grep ^ | wc -c`
if [ $outputbytes -gt 0 ]
then
	exitstatus=1
fi

exit $exitstatus
