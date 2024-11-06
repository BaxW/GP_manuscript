Hybrid Poplar Genomic Prediction
================
Baxter Worthing
2024-09-13

This repository contains code and drafts for my Hybrid Poplar Genomic
Prediction manuscript

All of the analyses and figures for the manuscript can be reproduced
using the code contained here

Cross validation of the genomic prediction models is implemented in
Snakemake. The sankefile located in the workflow directory can be used
to recreate the cross validation, but users should note that it is
designed to be run on a local HPC (UVM VACC), so partition names, wall
time, cpu usage and other resource designations may need to be adjusted
to run it elsewhere.
