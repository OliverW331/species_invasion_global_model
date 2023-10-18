# Species Invasion Global Model

## Introduction

This project aims to understand and predict patterns of species invasions on a global scale. By integrating various ecological and socio-economic factors, the model assesses how different variables contribute to the biodiversity changes observed worldwide.

## Prerequisites

Before you begin, ensure you have met the following requirements:
- R software (version x.x.x or later)
- Required R packages: [list here]
- Data files formatted according to the specifications outlined in the preprocessing scripts

## Installation

To install the Species Invasion Global Model, follow these steps:

1. Clone the repository to your local machine:
git clone https://github.com/OliverW331/species_invasion_global_model.git

2. Navigate to the project directory and install the required R packages.

## Usage

This project contains several scripts that preprocess data, run the model, and perform post-processing. Here are some examples:

### Data Preprocessing

- `baci_preprocess.R`: This script prepares the BACI trade data for analysis. It cleans and formats the data according to the model's requirements.
- `bilateral_distance_preprocess.R`: Processes geographical distance data between countries, essential for the model's spatial aspects.
- `gdp_pop_preprocess.R`: Prepares socio-economic data (GDP, population) for integration into the model.

### Model Execution

- `global_invasion_model.R`: The main script that runs the species invasion model. It takes the preprocessed data as input and generates predictions and insights regarding species invasions.
- `global_invasion_model_grid_search.R`: Performs a grid search for parameter optimization in the species invasion model.

### Post-processing

- `pp_funcs.R`: Contains various post-processing functions that analyze and visualize the model's output data.

To run the main model script, execute the following command in the project's root directory. Make sure the path names in the scripts are correct.:
Rscript model/global_invasion_model.R


## Contributing to Species Invasion Global Model

To contribute to the project, follow these steps:

1. Fork the repository.
2. Create a new branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`.
4. Push to the original branch: `git push origin <project_name>/<location>`.
5. Create the pull request.

Alternatively, see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).


