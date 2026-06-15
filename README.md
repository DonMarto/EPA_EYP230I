# EPA_EYP230I
Repository for Final Project for EYP230I, in accordance to the EPA Fuel Economy Database.

Para efectos del avance, se realiza un análisis exploratorio inicial de los datos, revisión de la fuente de datos y los reportes anuales de la EPA, con el fin de plantear una pregunta de estudio, y un modelo de regresión que la busque responder.


# Directory Organization

## EDA
The EDA directory contains two versions of the same file. It's primary role is to explore the data, and transform them accordingly, deleting unused columns and making adjustments if needed. The more elaborate, and fully commented version corresponds to the Jupyter Notebook, which is also used for obtaining Plots and other data. But a version of R is also produced, but is limited to data filtering and treatment. It is done for completeness, such that the full project could be replicated only in R.

## Informe
This directory hold the final report in a PDF format, as well as the full ZIP project.

## R_script
This directory holds the final version of the analysis done in one single script, alongside a copy of the cleaned data.

## data
The data directory hold the data and some guides provided by EPA.

## regression
Holds miscellaneous scripts used by the collaborators of this project. It contains partial analysis or proofs of concept, but are not part of the final analysis.
