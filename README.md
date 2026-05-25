# PEAXAI

`PEAXAI` (**Probabilistic Efficiency Analysis Using Explainable Artificial Intelligence**) is an R package for estimating technical efficiency through a probabilistic framework that integrates Data Envelopment Analysis (DEA), machine learning classifiers, and explainable artificial intelligence (XAI) tools.

The package uses DEA-derived efficiency labels to train supervised machine learning models that estimate the probability that each decision-making unit (DMU) is efficient. This probability-based approach allows users to move beyond a purely deterministic efficient/inefficient classification and to analyse efficiency under uncertainty.

In addition, `PEAXAI` provides post-hoc interpretability tools to explain the estimated efficiency probabilities. These tools include global and local feature importance, Shapley value explanations, permutation importance, sensitivity analysis, counterfactual improvement recommendations, efficiency rankings, and peer identification based on user-defined probability thresholds.

The package is designed for researchers and practitioners interested in productivity and efficiency analysis, benchmarking, policy evaluation, operational research, and explainable machine learning applications.

Please check the vignette for more details.

## Metadata Table

| Metadata item | Description |
|---|---|
| Software name | `PEAXAI` |
| Full name | Probabilistic Efficiency Analysis Using Explainable Artificial Intelligence |
| Current version | 1.0.2 |
| Programming language | R |
| Minimum R version | R >= 3.5 |
| Repository | GitHub |
| Repository URL | https://github.com/rgonzalezmoyano/PEAXAI |
| Issue tracker | https://github.com/rgonzalezmoyano/PEAXAI/issues |
| License | GPL-3 |
| Main purpose | Probability-based technical efficiency analysis using DEA, machine learning and explainable artificial intelligence |
| Main methodology | DEA-based efficiency labelling, supervised machine learning classification, probability-based efficiency assessment, XAI, counterfactual analysis and peer identification |
| Main outputs | Efficiency probabilities, efficiency rankings, global and local explanations, counterfactual targets and benchmark peers |
| Example data | Spanish food industry firms from the SABI database |
| Documentation | Function documentation and package vignette |
| Associated manuscript | *Probability-based Technical Efficiency Analysis through Machine Learning* |
| Authors | Ricardo González Moyano, Juan Aparicio, José Luis Zofío and Víctor España |
| Maintainer | Ricardo González Moyano |
| Contact | ricardo.gonzalezm@umh.es |

## Installation

You can install the released version of the package from CRAN with:

```r
install.packages("PEAXAI")
```

And the development version from GitHub with:

```r
install.packages("devtools")
devtools::install_github("rgonzalezmoyano/PEAXAI")
```

## Dependencies

`PEAXAI` depends on R and imports the following packages:

```text
Benchmarking
caret
deaR
dplyr
kernelshap
iml
isotone
lime
np
PRROC
pROC
rminer
rms
stats
```

The following packages are suggested for examples, documentation and vignettes:

```text
ggplot2
knitr
rmarkdown
```

## Main functions

The main functions exported by `PEAXAI` are:

| Function | Description |
|---|---|
| `PEAXAI_fitting()` | Fits machine learning classifiers using DEA-derived efficiency labels and selects the best model according to user-defined performance metrics. |
| `PEAXAI_predict()` | Predicts the probability that each DMU is efficient. |
| `PEAXAI_global_importance()` | Computes global feature importance using XAI methods such as sensitivity analysis, SHAP or permutation importance. |
| `PEAXAI_local_importance()` | Computes local explanations for individual DMUs. |
| `PEAXAI_counterfactuals()` | Computes counterfactual input-output targets required for a DMU to reach a given efficiency probability threshold. |
| `PEAXAI_ranking()` | Produces efficiency rankings based on predicted probabilities or attainable counterfactual improvements. |
| `PEAXAI_peer()` | Identifies benchmark peers according to estimated efficiency probabilities and user-defined thresholds. |
| `label_efficiency()` | Generates DEA-based efficiency labels for the training process. |
| `SMOTE_data()` | Generates synthetic observations to address class imbalance. |
| `SMOTE_Z_data()` | Generates synthetic observations based on frontier-related combinations. |

## Basic example

The following example illustrates the main workflow of the package using the example dataset included in `PEAXAI`.

```r
library(PEAXAI)

data("firms", package = "PEAXAI")

# Select firms from the Autonomous Community of Valencia
data <- subset(firms, autonomous_community == "Comunidad Valenciana")
data <- data[, -ncol(data)]

# Define inputs and output
x <- c(1:4)
y <- c(5)

# Define returns to scale
RTS <- "vrs"

# Define machine learning method
methods <- list(
  "nnet" = list(
    tuneGrid = expand.grid(
      size = c(1, 5, 10, 20),
      decay = 10^seq(-5, -1, by = 1)
    ),
    preProcess = c("center", "scale"),
    entropy = TRUE,
    skip = TRUE,
    maxit = 1000,
    MaxNWts = 100000,
    trace = FALSE,
    weights = NULL
  )
)

# Cross-validation settings
trControl <- list(
  method = "cv",
  number = 5
)

# Metrics used to select the best model
metric_priority <- c("Balanced_Accuracy", "F1", "ROC_AUC")

# Define imbalance treatment
imbalance_rate <- seq(0.05, 0.4, 0.05)

# Set seed for reproducibility
seed <- 1

# Fit PEAXAI model
models <- PEAXAI_fitting(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  imbalance_rate = imbalance_rate,
  methods = methods,
  trControl = trControl,
  metric_priority = metric_priority,
  hold_out = NULL,
  verbose = TRUE,
  seed = seed
)

# Extract final model
final_model <- models[["best_model_fit"]][["nnet"]]

# Estimate efficiency probabilities
probabilities <- PEAXAI_predict(
  data = data,
  x = x,
  y = y,
  final_model = final_model
)

head(probabilities)
```

## Explainable AI analysis

`PEAXAI` allows users to compute global and local explanations for the estimated efficiency probabilities.

For example, global feature importance can be obtained as follows:

```r
importance_method <- list(
  name = "SHAP",
  nsim = 200
)

relative_importance <- PEAXAI_global_importance(
  final_model = final_model,
  x = x,
  y = y,
  explain_data = data,
  reference_data = data,
  importance_method = importance_method
)

relative_importance
```

## Counterfactual efficiency targets

Given a set of probability thresholds, `PEAXAI` can estimate the input-output changes required for a DMU to reach a desired probability of being efficient.

```r
efficiency_thresholds <- seq(0.75, 0.95, 0.1)

directional_vector <- list(
  relative_importance = relative_importance,
  baseline = "mean"
)

targets <- PEAXAI_counterfactuals(
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  efficiency_thresholds = efficiency_thresholds,
  directional_vector = directional_vector,
  n_expand = 0.25,
  n_grid = 50,
  max_y = 1,
  min_x = 1
)

head(targets[["0.85"]][["counterfactual_dataset"]])
head(targets[["0.85"]][["inefficiencies"]])
```

## Efficiency rankings

Efficiency rankings can be computed either from the predicted probabilities or from attainable counterfactual improvements.

```r
ranking_predicted <- PEAXAI_ranking(
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  rank_basis = "predicted"
)

head(ranking_predicted)
```

If counterfactual targets have already been computed, an attainable-efficiency ranking can be obtained as follows:

```r
ranking_attainable <- PEAXAI_ranking(
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  efficiency_thresholds = efficiency_thresholds,
  targets = targets,
  rank_basis = "attainable"
)

head(ranking_attainable[["0.85"]])
```

## Peer identification

`PEAXAI` also identifies benchmark peers for each DMU according to estimated efficiency probabilities and selected probability thresholds.

```r
peers <- PEAXAI_peer(
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  targets = targets,
  efficiency_thresholds = efficiency_thresholds,
  weighted = FALSE,
  relative_importance = relative_importance
)

head(peers)
```

## Repository structure

The repository is organised as follows:

```text
PEAXAI/
├── R/
│   └── R source files containing the main functions of the package
├── data/
│   └── Example datasets included in the package
├── man/
│   └── Function documentation generated with roxygen2
├── vignettes/
│   └── Package vignette with a complete worked example
├── DESCRIPTION
│   └── R package metadata, dependencies, authors and license information
├── NAMESPACE
│   └── Exported functions and imported methods
├── README.md
│   └── General description, installation instructions and usage examples
└── LICENCE.txt
    └── Full license text
```

## Data

The package includes example data that can be loaded directly in R.

The vignette illustrates the use of `PEAXAI` with a dataset of Spanish food industry firms obtained from the SABI database for the year 2023. The dataset includes firms with more than 50 employees and contains financial and operational variables relevant for productivity and efficiency analysis.

The output variable used in the vignette is:

```text
operating_income
```

The input variables used in the vignette are:

```text
total_assets
employees
fixed_assets
personnel_expenses
```

The variable `autonomous_community` indicates the geographical location of each firm within one of Spain's autonomous communities.

## Documentation

After installing the package, the documentation of each function can be accessed in R using:

```r
?PEAXAI_fitting
?PEAXAI_predict
?PEAXAI_global_importance
?PEAXAI_local_importance
?PEAXAI_counterfactuals
?PEAXAI_ranking
?PEAXAI_peer
```

The package vignette can be opened with:

```r
browseVignettes("PEAXAI")
```

## License

This software is distributed under the GPL-3 license.

Please see the `LICENCE.txt` file for the full license text.

## Citation

If you use `PEAXAI`, please cite the associated manuscript:

```bibtex
@article{gonzalez_moyano_peaxai,
  title   = {Probability-based Technical Efficiency Analysis through Machine Learning},
  author  = {González Moyano, Ricardo and Aparicio, Juan and Zofío, José Luis and España, Víctor},
  journal = {In review},
  year    = {2025}
}
```

## Contact

For questions, bug reports or suggestions, please use the GitHub Issues page:

```text
https://github.com/rgonzalezmoyano/PEAXAI/issues
```

or contact the maintainer:

```text
Ricardo González Moyano
ricardo.gonzalezm@umh.es
```
