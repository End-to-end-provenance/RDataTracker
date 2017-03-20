#!/bin/bash
set -e

exitstatus=0

for file in R/*.R
do
    Rscript -e "lintr::lint(\"$file\", linters=lintr::with_defaults(trailing_whitespace_linter=NULL, line_length_linter=lintr::line_length_linter(80), snake_case_linter=NULL))"
    outputbytes=`Rscript -e "lintr::lint(\"$file\", linters=lintr::with_defaults(trailing_whitespace_linter=NULL, line_length_linter=lintr::line_length_linter(80), snake_case_linter=NULL))" | grep ^ | wc -c`
    if [ $outputbytes -gt 0 ]
    then
        exitstatus=1
    fi
done

exit $exitstatus
