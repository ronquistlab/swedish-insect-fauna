# swedish-insect-fauna

This repository contains data and scripts used to generate analyses and figures for the paper:

   Ronquist F et al (32 authors): Completing Linnaeus's inventory of the Swedish insect fauna: only 5,000 species left?
   Preprint on bioRxiv: https://www.biorxiv.org/content/10.1101/687392v2

The directory is organized into ten subdirectories. These directories are described in the following. We also give instructions on how to run the analyses and generate the plots presented in the paper.

quantitative_data
-----------------
Contains the abundance data from the Malaise trap inventory. The raw data are provided as Excel (xls) files, and the essential taxonomic and observation information in separate tsv files. The combined processed data are available in the file ‘quant-combined-data.tsv’. To regenerate this file, use the ‘createFiles.R’ script followed by the ‘correctFiles.R’ and ‘combineFiles.R’ scripts. The script ‘problemFiles.R’ can be used to locate posssible problems in the generated files. The ‘addToTableS3.R’ and ‘addToTableS4.R’ scripts are used to generate some information for Table S3 and Table S4. The file ‘overview_quantitative_data.csv” contains information from Table S3 used in the processing of the raw data files.

non-quantitative_data
---------------------
Contains the incidence data from the Malaise trap inventory. The raw data are provided as Excel (xls) files, and the essential taxonomic and observation information in separate tsv files. The combined processed data are available in the file ‘non-quant-combined-data.tsv’. To regenerate that file, use the ‘createFiles.R’ script followed by the ‘correctFiles.R’ and ‘combineFiles.R’ scripts (this will take some time). The script ‘problemFiles.R’ can be used to locate possible problems in the generated files. The ‘addToTableS3.R’ and ‘addToTableS4.R’ scripts are used to generate some information for Table S3 and Table S4. The file ‘overview_non-quantitative_data.csv” contains information from Table S3 used in the processing of the raw data files.

smtp_sample_data
----------------
This directory contains files that allow estimation of the total number of specimens in the catch (mentioned in the main text) and the proportion of the catch containing specimens of particular taxonomic groups (data shown in Table S4). The raw data include data on the abundance of all taxa in 16 inventory samples (‘StatSamples_2017-09-21.xls’ and related Excel files) a dump of all the sorting data for 1481 inventory samples (of 1919) sorted to a level beyond “Arthropoda”. The estimates of the total catch are summarized in the file ‘result_summary.xlsx’. To regenerate the estimates of the total catch, run the script ‘generateTotalStats.R’. To regenerate the estimates of the proportion of the catch containing the analysis groups, run the script ‘generatePropStats.R’.

misc_data
---------
This directory contains a file (in csv and xlsx format) with information about the species known in 2003 of critical groups used in the analysis (‘Checklist2003_2016-09-16.xlsx’ and ‘Checklist2003_2016-09-16.csv’). The information is provided in terms of absence and presence data for these groups in a dump from DynTaxa made in 2016. The R script ‘generateChecklist2003.R’ takes the csv file and generates a text table (‘checklist2003.tsv’) that is easy to use in R. The R script ‘generateTablesFromExcel.R’ generates similar text tables from the supporting tables (Tables S1–S6), which are provided in xlsx format in the parent directory. The generated files, which are also provided, are used by R scripts in several of the other directories.

smtp_map_data
-------------
This directory contains the charts for Fig 1, and Fig 2. To regenerate the charts, run the script ‘generateRegionHabitatCharts.R’.

richness-estimates
------------------
This directory contains the data files and R scripts required to regenerate the richness estimates (Table S5, a subset of which appears in Table 1 in the main paper) and the accuracy and bias plots (Figs 3–4). The raw data needed to regenerate the richness estimates are found in the ‘quantitative_data’, ‘non-quantitative_data’ and ‘misc_data’ folders. To regenerate the estimates, run the ‘generateRichnessEstimates.R’, ‘generateOverallRichnessEstimates.R’ and ‘generateRichnessEstimates-known2003.R’ scripts. Generate the accuracy and bias plots using ‘generateAccuracePlots.R’. The ‘cneFxn.R’ and ‘sqdevPlot.R’ files contain definitions of helper functions for the R scripts.

smtp_accumulation
-----------------
This directory contains the script necessary to generate the species accumulation plots for the Malaise trap inventory data (Fig 5).

composition_analysis
--------------------
This directory contains the data and scripts needed to generate the analyses of the composition of the Swedish insect fauna and how our knowledge of the total size and composition of the fauna has changed over time (Figs 7, 8). The key data file is the dump of the insect part of the DynTaxa checklist (‘Hexapoda-2017-02-08.csv’) and the life-history and taxonomic checklist information from Table S1 (‘swedish_insect_fauna.tsv’ in the ‘misc_data’ directory). Some information for Table S1 is generated from the DynTaxa dump using the script ‘generateStats.R’. Fig 7 and Fig 8 are generated by the script ‘generateCompositionCharts.R’.

smtp_trends
-----------
This directory contains the data and scripts needed to generate the bar plots illustrating changes in some aspects of the composition of the Swedish insect fauna over the nemoral to boreal transition, as indicated by the Malaise trap inventory data (Fig 9). The data files include several of the data files used or generated in the ‘richness-estimates’ directory. In addition, there is a file containing trap data from Table S2 (‘trap_data.csv’). To regenerate the figures, first run the ‘generateTrendData.R’ script, followed by the ‘generateTrendPlots.R’ script.

fauna_europaea_analysis
-----------------------
This directory contains the data and scripts needed to generate the analyses and plots of the insect data from Fauna Europaea (Figs 10–11). The key data file is the Fauna Europaea dump (‘Insecta_FaEu_version_2.6+distr 3.xlsx’), and the csv files generated from the different sheets of this file and containing the taxon information (’FaEu_Taxa.csv’), distribution information (’FaEu_Distribution.csv’) and country code information (’FaEu_CountryCodes.csv’). The Fauna Europaea dump did not contain information about the subfamily classification of genera in the Ichneumonidae, Braconidae and Staphylinidae, so this information was retrieved by online searches of the database at http://faunaeur.org (’FaEu_subfamily_genera.csv’). Additional data files include information on the land areas of European countries (‘country_areas.csv’) and their latitudes (‘country_latitudes.csv’). Finally, the analysis relies on a file containing the life-history data from Table S6 (‘FaEu_bio_info.tsv’ in the ‘misc_data’ folder). To regenerate the figures, run the ‘generateFaEuPlotData.R’ script first and then the ‘generateFaEuPlots.R’ script.


