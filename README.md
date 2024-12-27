# Student Data Analysis

This repository contains an R script for analyzing a dataset related to student performance. The analysis includes data preprocessing, statistical tests, hypothesis testing, handling missing values, and logistic regression modeling to determine factors influencing student grades.

## Overview

The dataset consists of various features related to students, including demographic data, academic performance indicators, and participation in activities. The primary goal is to analyze and model the factors affecting grades (categorized as `High` or `Low` performance).

## Key Steps in the Analysis

### 1. Data Loading and Exploration
- The dataset is loaded using `read.csv`.
- Libraries used include:
  - `tidyverse`
  - `DataExplorer`
  - `skimr`
- Initial exploration:
  - View column names
  - Identify missing values
  - Visualize dataset summary using `plot_intro`.

### 2. Data Transformation
- Renamed feature columns for better readability (e.g., `X1` -> `X1_age`).
- Created derived features:
  - `Scholar_type`: Categorized scholarship levels as `L` (Low) or `H` (High).
  - `GRADE_2`: Categorized grades into `High` or `Low`.

### 3. Statistical Tests
#### Hypothesis Testing: Association Between Scholarship Type and Grades
- Null Hypothesis (H0): Scholarship type and grades are independent.
- Chi-squared test of independence applied.
- Residual analysis performed using `corrplot`.

### 4. Handling Missing Values
- Simulated missingness:
  - MCAR (Missing Completely At Random)
  - MNAR (Missing Not At Random)
- Methods used to handle missing data:
  - **Deletion**: Removing rows with missing values.
  - **Imputation**:
    - Mode imputation for categorical variables.
    - Multiple Imputation using `mice` with PMM (Predictive Mean Matching) and `polyreg` for categorical variables.

### 5. Logistic Regression Modeling
- Built logistic regression models to determine significant predictors of grade category (`GRADE_2`).
- Evaluated models using:
  - Coefficients and p-values for individual predictors.
  - Likelihood Ratio Test (LRT) to assess overall model significance.
  - AIC values for model comparison.
- Significant predictors included:
  - `X20_attende_seminars` (Attending seminars).
  - `X18_reading_freq_nonsci` (Non-scientific reading frequency).
  - `X21_impact_projects` (Participation in impactful projects).

## Notable Findings
- Attending seminars (`X20_attende_seminars`) consistently showed a significant positive association with high grades.
- Missing data handling improved model reliability while maintaining original patterns of significance.
- Variables like parental education and accommodation type did not significantly affect grade outcomes.

## Dependencies
Ensure the following R packages are installed:

```r
install.packages(c("tidyverse", "DataExplorer", "skimr", "missMethods", "mice", "glm2", "lmtest", "corrplot"))
```

## File Structure
- **analysis.R**: Main script containing the full analysis workflow.
- **README.md**: Documentation for understanding and reproducing the analysis.

## Usage
1. Clone the repository:
   ```bash
   git clone <https://github.com/jwalith/R-Programming>
   ```
2. Run the `analysis.R` script in RStudio or an R environment.
3. Replace the dataset path (`Student_Data.csv`) with the appropriate file path on your system.

## Results Visualization
- Visualizations included:
  - Boxplots for grade comparisons (e.g., `GRADE by GENDER`, `GRADE by AGE`).
  - Histograms for grade distribution.
  - Bar plots for categorical variables (e.g., `Scholarship Type vs Grade`).

## Future Work
- Expand analysis to include additional predictors and interaction terms.
- Test alternative models, such as Random Forest or Gradient Boosting, for classification.

## License
This project is licensed under the MIT License.
