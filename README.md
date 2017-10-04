# Bern_PD_Dream
The code used for the Parkinson's Digital Biomarker DREAM Challenge of Team Bern

## Instructions

1. Run the script `downloadData.R` to download the raw challenge data
2. Run the script `getFeatures.R` to create the files `featureTabTraining_Meta.Rda` and `featureTabALL_NoMeta.Rda`
3. Put the created files in the folder `data`
4. Use Snakemake (http://snakemake.readthedocs.io/en/stable/) to run the rest of the pipeline, by simple typing
```
snakemake all --cores 4
```
