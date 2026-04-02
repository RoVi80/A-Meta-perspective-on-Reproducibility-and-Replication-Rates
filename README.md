# A Meta-Meta Perspective on Replication Criteria and Reported Replication Rates

This repository contains the code, data, and figures for the paper on replication criteria and reported replication rates across large-scale replication projects in the social and behavioral sciences and related fields.

## Abstract

Large-scale replication projects have become central to debates about reproducibility in the social and behavioral sciences and related fields. Of the 274 positive results claimed in 164 published papers between 2009 and 2018 across the social and behavioral sciences, depending on the exact criterion applied to evaluate replication success, between 31% and 77% replicated successfully. A key conclusion of this project was that the conditions that accept or reject replicability need further investigation. Building on that insight, the present study takes a meta-perspective on reproducibility by examining replication projects themselves. In particular, it focuses on how the choice of replication criterion may shape the conclusions that are reported. Two widely used criteria are the p-value-based replication criterion (PVRC), which assesses whether an originally significant effect remains statistically significant in the same direction in the replication, and the confidence-interval-based replication criterion (CIRC), which assesses whether the confidence intervals of the original and replication studies overlap. Using a sample of 31 replication projects across psychology, behavioral and social sciences, psychiatry, and behavioral ecology, this study shows substantial heterogeneity in reported replication rates both across projects and within projects depending on the criterion applied. Meta-analytic evidence suggests no overall time trend toward improved replicability and no robust association of replication rates with field, authorship patterns, or journal impact. At the same time, funnel-plot patterns indicate that reported CIRC estimates may be selectively biased toward more extreme values. These findings raise the possibility of publication bias and reporting in favor of "extreme results" operating not only in original studies, but also in the meta-literature on replication itself.

## Repository Contents

- `replicants_final.R`: main R script used to generate the publication figures and meta-analytic model objects.
- `final_dataset_summarized_new_stage2.xlsx`: summarized dataset used by the script.
- `Three_funnel_PVRC.jpg`: funnel plot for below- and above-average PVRC values.
- `Three_funnel_CIRC.jpg`: funnel plot for below- and above-average CIRC values.
- `final_plot.jpg`: comparison plot of PV and CI estimates ordered by final replication rate.

## How to Run the Code

### Requirements

The code was tested in RStudio with R and the following packages:

```r
install.packages(c("ggplot2", "ggrepel", "metafor", "readxl"))
```

### Files Needed

Make sure the following files are in the same repository:

- `replicants_final.R`
- `final_dataset_summarized_new_stage2.xlsx`

### Running in RStudio

Open the repository folder in RStudio, or set the working directory to the folder containing `replicants_final.R`.

Then run:

```r
source("replicants_final.R")
```

This will create the following plot objects:

- `funnel_below_above_p_values`
- `funnel_below_above_ci_values`
- `final_plot`

It will also create the following meta-analytic model objects:

- `pv_reg`
- `ci_reg`
- `pv_reg_funnel`
- `ci_reg_funnel`
- `final_reg_funnel`

When sourced interactively in RStudio, the three plots are also shown automatically in the Plots pane.

### Saving the Figures

To save the figures as `.jpg` files in the script folder, run:

```r
save_publication_figures()
```

This writes:

- `Three_funnel_PVRC.jpg`
- `Three_funnel_CIRC.jpg`
- `final_plot.jpg`

## Notes

- The script uses a project-relative file search to locate the summarized dataset in common RStudio working-directory setups.
- Some meta-analytic models may report warnings that studies with missing values were omitted from model fitting. This is expected because not every replication project contains every measure required for each model.

## Citation

If you use this repository, please cite the associated paper. Bibliographic details can be added here once the final citation is available.

## License

Add your preferred license information here.
