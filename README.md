# SARS-CoV-2-Wastewater-Mutation-Resarch-Tool
**Overview**

This is a tool for visualizing and exploring complex data with a user-friendly interface which also allows easy insight into capture the trends of cryptic mutation in wastewater samples.


To create 'SARS-CoV-2 Wastewater Mutation Search Tool', R Shiny app was used for the front-end and MySQL for the back-end.

This tool consists of four interconnected pages: the Main page, Samples page, Sample info page, and Mutation info page,

each offering a different level of detail and analysis, which allows users to narrow down their analysis by moving from page to page.


In addition, depending on the dataset that users wish to use, the dataset selection box is set on the Main page, allowing them to update the existing dataset.

**Data source**
Two sets of data utlised were previously obtained from the European Nucleo@de Archive (ENA) (accession number: PRJEB55313, PRJEB53325) which consists of FASTQ files collected from the sewer networks in Liverpool and across England, as detailed in Franziska S. Brunner et al. 
(https://doi.org/10.1016/j.watres.2022.119306)/ https://doi.org/10.1016/j.watres.2023.120804)

The MRC-University of Glasgow Centre for Virus Research is part of the COG-UK (COVID-19 Genomics UK) consortium, which allows us to access these resources for research purposes.

**Dataset**
The main dataset for this project is the Liverpool dataset, all 1341 sequenced wastewater samples were used in this project, which were collected from eight locations, both sewer networks and wastewater treatment plants across the city of Liverpool. 
These samples were collected between the 1st of November 2020 and the 21st of June 2021.

For the UK test dataset, 20 samples from the available 23536 samples were randomly chosen with a collec@on date between the 1st of October 2021 and the 3rd of December 2022.

