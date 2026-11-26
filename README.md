# LLteacher data and code

Data and R code to reproduce the tables and figures in the paper.

## Data (`data/`)

| File | Contents |
|---|---|
| `llteacher_data.csv` | One row per student x assignment (781 rows, 134 students): widget self-ratings and message activity. |
| `survey_data.csv` | One row per survey respondent (121 students): answers to Q1-Q17. |
| `data_dictionary.csv` | Description of every column in the two data files. |

The two data files are linked by `student_id`. Notes:

- **Identifiers.** Students are identified only by a study ID (`student_id`).
- **Survey.** 121 of the 134 students completed the survey; the other 13 have no row in `survey_data.csv`.
- **Widget ratings.** Assignments 3-6 were recorded by the platform. Assignments 1-2 were extracted by hand from students' free-text answers.
- **`n_messages = 0`.** 6 student x assignment rows have widget ratings but no student message on the two graded problems. Message-activity analyses use rows with `n_messages > 0`; widget analyses use all rows with a rating.
- The conversation transcripts are not shared, so the data files are the starting point for all analyses.

## Code

Run each script from this folder, e.g. `Rscript image_generation/survey_overview.R` or `Rscript analysis_gains_vs_baseline.R`. R packages: `tidyverse`, `lme4`, `patchwork`. Figures are written to `figs/` and the table to `tables/`.

| File | Produces |
|---|---|
| `prep.R` | Loads the data files and builds the shared tables. Sourced by every script. |
| `image_generation/message_activity.R` | Messages per student and message length by assignment, engagement clusters (`interactions_by_assignment`, `avgchars_box_by_assignment`, `avgchars_vs_nmsg_clusters`). |
| `image_generation/widget_change.R` | Widget self-ratings and gains by assignment (`widget_diff_by_assignment`, `widget_diff_dist_by_assignment`). |
| `image_generation/gains_by_start.R` | Gains by starting score (`gains_by_start_group`). |
| `image_generation/survey_overview.R` | Survey Likert items and liked-most/least items (`survey_likert`, `priority_q14_q15`). |
| `image_generation/hallucination.R` | Hallucination questions Q16-Q17 (`hallucination_q16_q17`). |
| `analysis_gains_vs_baseline.R` | Mixed-effects logistic model of big vs. small improvement; writes `tables/improvement_logistic_table.tex`. |

Several figures use unseeded random jitter, so the point positions in those plots differ slightly between runs. No reported numbers depend on it.

## License

- **Code** (`prep.R`, `analysis_gains_vs_baseline.R`, `image_generation/`): MIT License, see `LICENSE`.
- **Data and outputs** (`data/`, `figs/`, `tables/`): Creative Commons Attribution 4.0 International (CC BY 4.0), see `LICENSE-data`. Please cite the paper when reusing them.
