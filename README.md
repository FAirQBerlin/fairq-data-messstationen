# fairq-data-messstationen

This repository contains R code that retrieves data from
https://luftdaten.berlin.de and stores it in a Clickhouse database.


## How to get started

- Create an .Renviron file in the project folder, see `.Renviron_template` for 
the structure
- Build the R package
- Create database as described in https://github.com/fairqBerlin/fairq-data/tree/public/inst/db (schema fairq_raw)


## Most important file

`inst/scripts/craw_messstationen.R` is a script that retrieves data about 
measurement stations and current measurements. It is intended to be run on an 
hourly basis, but it always retrieves data from the past two months to ensure 
that any updated values are included.


## Input and output

### Input

- https://luftdaten.berlin.de

## Output

- Database, schema fairq_raw
