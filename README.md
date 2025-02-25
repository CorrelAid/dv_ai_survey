# Evaluation of a survey on AI in associations from Digital Vereint

What is the current state of the use of artificial intelligence in associations in Germany? “[Digital Vereint](https://digital-vereint.de/)”, a project of lagfa bayern e.V., investigated this question in a recent survey of around 220 Bavarian associations. Digital Vereint helps volunteers and associations to take advantage of the opportunities offered by digitalization. In collaboration with the [Civic Data Lab](https://civic-data.de) team, the results of the survey were evaluated and visualized in detail as part of a data project. The project not only provides an insight into the current status of AI use in associations, but also offers valuable insights into what organizations should pay attention to when conducting their own surveys. 

🔎 Find the final report in `report.qmd`.

## Data Processing

- Raw data was processed in two steps that correspond to the scripts `R-tools/data_cleaning.R`and `R-tools/create_new_cols.R`. 
- The final dataset can be found in `data/final.csv`
- Additional data for open questions was created with `intermediate/open_questions.qmd`.

## Setup

This repository is utilizing [quarto](https://quarto.org/docs/get-started/) in conjunction with [renv](https://quarto.org/docs/projects/virtual-environments.html#using-renv). Having quarto and renv installed run `renv::restore()` to install required packages.


