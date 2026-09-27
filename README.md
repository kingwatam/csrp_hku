# HKU CSRP research projects (2019-2020)

R code from my work as a Data Analyst (Senior Research Assistant) at the Centre for Suicide Research and Prevention, The University of Hong Kong, from September 2019 to December 2020.

The unit worked on suicide prevention, school mental health, poverty and health services. I worked with Coroners' Court data, school surveys, census microdata, media-monitoring data and administrative data.

This repository contains analysis code only. Coroners' Court records, pupil and teacher records, census microdata, census boundary files, media corpora and other identifiable material are not included.

## At a glance

- **School mental health:** multi-year pupil surveys from the Thematic Network on Developing Students' Positive Attitudes and Values, with universal and selective components
- **Suicide epidemiology:** Coroners' Court data, school-level clustering, helium-inhalation suicides, online discussion and decriminalisation
- **Poverty and health services:** a case-control study, census microdata, small-area mortality mapping and monograph indexing
- **Methods:** multi-year data merging, mixed models, clustering, difference-in-differences, time-series analysis, survival-free count modelling and cross-validation
- **Outputs:** cleaned analysis files, report tables, figures, search terms and reproducible research summaries
- **Data policy:** code is published without participant-level or identifiable data

## Repository layout

| Folder | Project | Contents |
|---|---|---|
| `qtn/` | Thematic Network on Developing Students' Positive Attitudes and Values | Multi-year pupil survey cleaning, repeated-measures modelling, teacher measures and programme-report figures |
| `school_clusters/` | School clustering and student suicide | Comparison of four clustering approaches and linkage to student suicide counts |
| `cc/` | Coroners' Court suicide epidemiology | Record recoding, descriptive analysis and difference-in-differences analysis |
| `poverty/` | Poverty, census and health services | Case-control analysis, census microdata, small-area mortality mapping and monograph indexing |
| `helium/` | Helium-inhalation suicides | Case summaries, news coding, timeline visualization and scan statistics |
| `early_warning/` | Suicide-related online discussion | Daily time-series analysis and media search-term curation |
| `suicide_law/` | Decriminalisation of suicide | Cross-national comparison |
| `covid19/` | COVID-19 fatality estimation | Four approaches for estimating fatality from epidemic time series |
| `multivariate/` | Correlated count outcomes | Bivariate Poisson and multivariate count models |
| `misc/` | Forecasting examples | Repeated cross-validation, time-series cross-validation and PDF utilities |
| `helper_functions.R` | Shared analysis library | Data management, reporting and R utility functions |

## Thematic Network on Developing Students' Positive Attitudes and Values

`qtn/` contains the largest body of work in the repository. It covers the Thematic Network on Developing Students' Positive Attitudes and Values in Hong Kong primary and secondary schools across multiple school years, with universal and selective programme components.

### Multi-year data merging

`qtn/data_merging.R` merges four waves of secondary-school data and three waves of primary-school data.

Tasks handled in the merge include:

- Reconciling school codes that changed between years
- Handling different item banks used in e-learning and paper questionnaires
- Removing duplicate submissions
- Deriving age from date of birth
- Scoring validated measures including GHQ-12 distress, mental-health knowledge, positive and negative thinking, life satisfaction, empathy and gratitude
- Reversing scored items where required
- Combining waves with different column sets
- Saving an RDS file for faster loading

One outcome had been scored incorrectly in an earlier dataset version. Rather than re-keying the cohort, corrected values were recovered by matching on the other variables and school, class and pupil identifiers, with a uniqueness check before replacement.

### Pupil-level analysis

`qtn/qtn_paper.R` analyses repeated pupil measurements using:

- `nlme::lme` and `lme4::lmer`
- Pupil-within-school random effects
- School-level random effects
- School-specific random slopes for the intervention effect
- Likelihood-ratio comparisons of mixed and ordinary linear models
- `merTools` simulations for school random effects
- Pairwise school contrasts using explicit linear combinations
- Delta-method standard errors for sums of coefficients
- Bootstrap estimates for school effects
- Pre/post point-range figures with 84% confidence intervals

### Teacher measures

The teacher-efficacy files analyse ordinal ratings and repeated measurements. A cumulative-link mixed model was implemented with an adaptive quadrature loop, but the multilevel version was switched off for the 2019-20 analysis because too few teachers had matched pre- and post-test data. That decision is recorded in the code.

A separate file handles teacher-rated Strengths and Difficulties Questionnaire data for the selective cohort. It documents manual corrections to files with inconsistent column names, duplicate submissions and missing waves.

### Programme-report figures

`qtn1920_charts.R` generates pre/post figures directly from the results workbook. Axis limits are derived from each instrument's documented score range. Figures are saved at 300 dpi using the Cairo graphics device so that Chinese instrument and school labels render correctly.

## School clustering and student suicide

`school_clusters/school_cluster_analysis.R` clusters roughly 450 Hong Kong secondary schools using 14 indicators covering resources, teaching quality, student-teacher ratio, student support, academic and extracurricular results, school ethos, learning atmosphere, conduct, management and reputation.

Four clustering families are compared:

- k-means with an elbow check
- Gaussian mixture models with information-criterion selection
- High-dimensional data clustering
- Dirichlet-process mixture models

Principal components are used before clustering, and the code checks whether standardization or normalization changes the grouping. Cluster labels are recoded so that the same group keeps the same label across reruns.

Cluster membership is joined to student suicide counts and cluster profiles are drawn with radar charts. Negative results are retained in the code, including the finding that the Dirichlet-process model did not work well without PCA and that high-dimensional clustering selected more groups than seemed useful.

`school_cluster_merge.R` prepares school metadata from nested JSON, normalizes Chinese text and merges school staffing information.

## Coroners' Court suicide data

`cc/cc_data_cleaning.R` is the main data-cleaning file. It performs codebook-driven recoding of demographic, place-of-birth, occupation, employment, marital-status, income, mental-health and method-of-death fields.

Method of death is derived from ICD-10 codes, with free-text descriptions used to resolve categories not fully captured by the code alone. Occupation categories are built from free-text descriptions.

`cc_minority_analysis.R` is descriptive. It produces two- and three-way frequency tables for ethnicity, place of birth, age, sex, method of death, mental health and employment, and writes the results to multi-sheet Excel workbooks.

`cc_eastern_analysis.R` uses difference-in-differences to compare suicide rates in Eastern District with a comparison district before, during and after an intervention period. Rather than reporting a single window, the code searches possible pre- and post-period boundaries and reports the full set of specifications, together with plots of modelled and observed trends.

## Poverty, health services and census data

### Case-control study

`poverty/case_control/poverty_analysis.R` covers a poverty and health-services study. It handles bilingual questionnaires, maps free-text addresses to districts and regions, and constructs measures including life satisfaction, social support, hope, transport affordability and service use.

The same associations are estimated in three ways:

- Logistic regression for odds ratios
- Log-binomial regression for risk ratios
- Modified Poisson GEE for robust risk ratios

Service-use models are repeated with ordinary and robust standard errors. Item-level R-squared decompositions are used to see which questions drive composite service scales. Descriptive tests are selected according to variable type.

### Census microdata

`poverty/monograph/census16.R` processes the 2016 Census 5% sample. It separates household and person records, reconstructs serial numbers and derives household types, including single-parent households by matching a child's parent serial number within the household.

A sample-based poverty line is calculated as half the median household income at each household size. Official government thresholds are retained in the code as an alternative.

### Small-area mortality

`poverty/monograph/geospatial/stpu.R` creates a concordance between territory-wide and small territory-wide units, calculates poverty rates across four censuses and produces WHO age-standardized mortality and standardized premature mortality rates by sex and area. Shapefiles are read with `sf`.

`book_indexing.R` reduces the completed monograph to one-, two- and three-word frequency tables to support keyword indexing.

## Online early warning of suicide

`early_warning/early_warning.R` compares daily case counts with daily suicide-related online discussion. It uses Poisson regression with pre/post indicators, checks residual autocorrelation and refits the model as GEE with an AR(1) correlation structure.

Alternative event dates are examined in a grid search, with model fit reported at each date.

`meltwater_search_terms.R` contains a Boolean media query combining Hong Kong news agencies with Chinese suicide-related terms and exclusions for irrelevant coverage.

The committed SaTSCan result for this period was not significant. The null result is retained with the method used.

## Helium-inhalation suicides

`helium/` covers 46 cases recorded between 2011 and 2017.

- `news_articles.R` converts manually coded news articles into analysable data
- `helium_analysis.R` calculates monthly rates and replicates the scan statistic in R
- `helium_timeline.Rmd` produces a knitted timeline
- A custom `ggplot2` timeline scales case markers by the number of related news articles

The committed SaTSCan output identifies a significant temporal cluster from August 2014 to January 2015.

## Decriminalisation of suicide

`suicide_law/suicide_law_analysis.R` builds a country panel for 2008 and 2012. It combines sex-specific suicide rates with country-level characteristics and whether suicide was a criminal offence. The analysis includes sex differences, stratified summaries and faceted plots.

## COVID-19 fatality estimation

`covid19/fatality_modelling.R` implements four approaches for estimating fatality from incidence, recovery and death data:

- Non-negative least-squares inverse solution
- Proportional allotment estimator
- Constrained non-linear least squares
- Constrained elastic-net Poisson GLM

The methods are compared for Hubei COVID-19 and the 2003 SARS outbreaks in Hong Kong and Beijing. Disease duration is selected by deviance rather than assumed in advance.

## Correlated count outcomes

`multivariate/bivariate.R` compares:

- Bivariate Poisson models
- Diagonal-inflated models
- Multivariate generalized linear mixed models
- A hand-written EM implementation

The score equations are also re-derived and solved numerically as a check. Standard errors use 1,000 bootstrap replications run in parallel, and offset-versus-weight equivalence is tested.

## Forecasting and cross-validation

`misc/primary_model/cross_validate.R` is a separate forecasting example using:

- Repeated ten-fold cross-validation with 1,000 repeats
- Expanding-window time-series cross-validation for ARIMA models with regressors
- Gaussian and binomial models
- Lagged predictors
- Bootstrap prediction intervals

The folder also contains small PDF extraction and benchmarking utilities.

## Shared analysis library

`helper_functions.R` started at HKU as a practical response to moving from Stata to R. Its original purpose was to recreate Stata commands I used frequently and to shorten common R idioms.

It contains:

- `summ()` and `tab()`
- `iferror()` and `ifwarning()`
- `starred_p()`
- `recode_age()`
- `get_freqtable()`
- `convert2NA()` and `convert2value()`
- `import_func()`
- `trycatchNA()` and related error-tolerant wrappers
- `write_excel()` for multi-sheet Excel output

The same library was later extended at CUHK and Calgary, but those later functions are not part of this repository.

## Reproducibility notes

- The repository is code-only by design.
- Most scripts set one project-root variable so that paths can be changed in a single place.
- Random seeds are documented for several stochastic procedures.
- The `.gitignore` excludes data and selected collaborator folders.
- R Markdown is used for the helium timeline.
- Two SaTSCan output files are included as evidence of both a significant and a null result.
- There is no dependency lockfile or automated pipeline.
- Some earlier specifications remain in commented code to document the analyses considered.

## Related outputs

Quantitative analyses from this period contributed to:

- Thematic Network on Developing Students' Positive Attitudes and Values programme reports and mixed-methods research
- A book chapter on suicide among ethnic minorities in Hong Kong
- A monograph and final report on poverty alleviation
- School-cluster and suicide-prevalence analyses

Yip PS. *Social unrest and the poverty problem in Hong Kong*. Springer Singapore; 2021. https://doi.org/10.1007/978-981-33-6629-9

(I produced the quantitative results and visualizations for the monograph.)
