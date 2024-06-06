#!/bin/bash
summary=2
desc=16
reference=7
budget=2
facility=2
man=2

page=1
pdftk output/main.pdf cat $page-$((page+summary-1)) output submitted/summary.pdf;
page=$((page+summary))
pdftk output/main.pdf cat $page-$((page+desc-1)) output submitted/desc.pdf;
page=$((page+desc))
pdftk output/main.pdf cat $page-$((page+reference-1)) output submitted/reference.pdf;
page=$((page+reference))
pdftk output/main.pdf cat $page-$((page+budget-1)) output submitted/budget.pdf;
page=$((page+budget))
pdftk output/main.pdf cat $page-$((page+facility-1)) output submitted/facility.pdf;
page=$((page+facility))
pdftk output/main.pdf cat $page-$((page+man-1)) output submitted/dataman.pdf;
