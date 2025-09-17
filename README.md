# Mathematics Requirements, Self-Efficacy, and the Gender Gap in STEM

This repository contains the code and analysis for my senior honors thesis in Economics at the University of Chicago.

**Author:** Jess Xiong\
**Advisor:** Juanna S. Joensen

## Abstract

This project investigates how state-level high school math graduation requirements affect students’ self-perception of mathematical ability and long-term STEM retention, with a particular focus on gender differences. Using NCES longitudinal data (ELS:2002), I employ Difference-in-Differences (DiD) and matching methods to estimate causal effects. Results show that increasing math requirements from 3 to 4 years has a significant positive effect on self-efficacy, but suggests heterogenous impacts by gender, highlighting the importance of designing accessible and appealing math education policies.

## Repository structure

## How to run

1.  Clone the repository.
2.  Open the RStudio project.
3.  Run: \`\`\`r install.packages("renv") renv::restore() source("scripts/01_cleaning.R") source("scripts/02_matching.R") \# optional source("scripts/03_did.R") source("scripts/04_heterogeneity.R") source("scripts/05_figures_tables.R")

Outputs (figures/tables) will appear in the output/ folder.
