# Survey Evaluation: AI Usage in Associations

What is the current state of the use of artificial intelligence in associations in Germany? [Digital Vereint](https://digital-vereint.de/) investigated this question in a recent survey of around 220 Bavarian associations. In collaboration with the [Civic Data Lab](https://civic-data.de) team, the results of the survey were evaluated and visualized. The project not only provides an insight into the current status of AI use in associations, but also offers valuable insights into what organizations should pay attention to when conducting their own surveys. 

🔎 Find the final report in `report.qmd` and the plots in `report_files/figure-html`

## Data Processing

- Raw data was processed in two steps that correspond to the scripts `R-tools/data_cleaning.R`and `R-tools/create_new_cols.R`. 
- The final dataset can be found in `data/final.csv`
- Additional data for open questions was created with `intermediate/open_questions.qmd`.

## Setup

This repository is utilizing [quarto](https://quarto.org/docs/get-started/) in conjunction with [renv](https://quarto.org/docs/projects/virtual-environments.html#using-renv). Having quarto and renv installed run `renv::restore()` to install required packages.


