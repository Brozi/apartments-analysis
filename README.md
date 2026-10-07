# Apartment Price Analysis and Prediction in Kraków

## Project Overview

This repository contains a group data analysis project conducted in **R** using a real-world dataset of apartment listings in Kraków, Poland.

The project explores the factors associated with apartment prices through data cleaning, exploratory analysis, statistical testing, visualization, regression modeling, and unsupervised learning.

> **Start here:** [Open `apartments.R`](apartments.R)  
> This is the final, consolidated version of the analysis. The remaining `.R` files contain earlier or supporting versions of the work.

## Objectives

The main objectives of the project were to:

- Explore the structure and distribution of the apartment listing data.
- Identify relationships between apartment characteristics and prices.
- Clean and transform the dataset for statistical analysis and modeling.
- Detect and remove influential observations and outliers.
- Build regression models for apartment price prediction.
- Analyze spatial patterns in apartment prices across Kraków.
- Group apartments into meaningful clusters using unsupervised learning methods.

## Analysis Workflow

The final analysis script includes the following stages:

1. **Package configuration and data import**
2. **Initial data exploration**
3. **Data type conversion and preprocessing**
4. **Exploratory Data Analysis**
5. **Outlier detection and removal**
6. **Variable transformation, including logarithmic transformations**
7. **Statistical testing**
8. **Data visualization**
9. **Spatial price analysis and interactive mapping**
10. **Preparation of data for machine learning**
11. **Multiple linear regression modeling**
12. **Model diagnostics and validation using a test dataset**
13. **Apartment segmentation using PAM clustering**

## Key Techniques

The project demonstrates practical experience with:

- Exploratory Data Analysis (EDA)
- Data cleaning and preprocessing
- Outlier detection using the Interquartile Range (IQR)
- Influence analysis using Cook’s distance
- Logarithmic transformations for skewed variables
- Correlation analysis using Spearman’s rank correlation
- Normality testing using the Lilliefors test
- Kruskal–Wallis statistical testing
- One-hot encoding of categorical variables
- Multiple linear regression
- Regression diagnostics
- Train/test model validation
- Spatial data analysis
- PAM clustering, also known as Partitioning Around Medoids

## Visualizations

The analysis includes several types of visualizations, including:

- Histograms
- Boxplots
- Scatter plots
- Linear trend lines
- LOESS smoothing curves
- Correlation plots
- Price comparisons across districts
- Interactive maps showing the spatial distribution of apartment prices

## Technologies and Libraries

The project was developed in **R** using the following packages:

### Data Processing

- `tidyverse`
- `fastDummies`
- `openxlsx`

### Visualization

- `ggplot2`
- `ggcorrplot`
- `factoextra`

### Spatial Analysis

- `sf`
- `leaflet`
- `viridis`
- `ggspatial`

### Statistics and Modeling

- `car`
- `e1071`
- `nortest`
- `cluster`
- `moments`

## Repository Structure

| File | Description |
| --- | --- |
| [`apartments.R`](apartments.R) | Final, consolidated analysis script and recommended starting point |
| [`apartments.R`](archive/apartments.R) | Earlier version of the main analysis |
| [`code.R`](archive/code.R) | Supporting analysis code |
| [`model.R`](archive/model.R) | Regression and modeling-related code |
| [`wykresy_cenam2.R`](archive/wykresy_cenam2.R) | Supporting visualization code |

## Recommended Review Path

For a quick overview of the project, review the files in this order:

1. [`apartments.R`](apartments.R) — final analysis
2. [`model.R`](archive/model.R) — additional modeling code
3. [`code.R`](archive/code.R) — supporting analysis
4. [`apartments.R`](archive/apartments.R) — earlier project version

## Skills Demonstrated

This project demonstrates the ability to:

- Work with real-world, imperfect datasets.
- Apply a complete data analysis workflow.
- Combine statistical reasoning with practical programming.
- Build and assess predictive regression models.
- Communicate analytical findings through visualizations.
- Work collaboratively on a data science project using R.
