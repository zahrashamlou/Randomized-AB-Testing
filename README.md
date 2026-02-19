# Product Analytics A/B Testing — Onboarding Experiment

This project presents an end-to-end analysis of a randomized A/B test evaluating a new onboarding experience on a digital platform.
The objective of the experiment was to measure the impact of the new onboarding flow on:

   - Conversion to paid subscription
   - Early user engagement
   - Expected lifetime value (LTV)

## The analysis includes:

- Experiment design and setup
- Data cleaning and user-level aggregation
- Funnel and conversion analysis
- Estimation of treatment effects using statistical methods
- Evaluation of trade-offs between acquisition and retention
- Business impact assessment and recommendations
- Results show that while the new onboarding improved paid conversion, it reduced early post-payment engagement, highlighting a trade-off between short-term growth and long-term retention.

## This repository contains:

Analysis report
- R Code Base
- Methodology and assumptions
- Supporting materials demonstrating the experimentation workflow

> ⚠️ The dataset used in this project is anonymized and does not contain confidential company information.

```
Randomized-AB-Testing/
│
├── README.md
├── AB Testing Analysis Report.pdf
├── .gitignore
│
├── data/
│   ├── assignments-1-.csv
│   └── events-1-.csv
│
├── codes/
│   ├── A1_0_data_cleaning.R
│   ├── A1_1_data_exploration.R
│   ├── A1_2_conversion_analysis.R
│   └── A1_3_dynamic_activity.R
│
├── results/

```


```
