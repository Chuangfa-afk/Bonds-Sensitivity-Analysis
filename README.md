# Sensitivity Analysis of Fixed Income Securities

This project was completed as part of the Finance 529Q course in Spring 2024 under the guidance of Professor David A. Hsieh. The main objective was to conduct a comprehensive sensitivity analysis on a bond portfolio consisting of four US treasury securities. The analysis aimed to understand how changes in yield affect the portfolio's market value, employing various quantitative and computational finance techniques.

## Team Assignment 2 Overview

### Question 1: Portfolio Analysis
- **Objective**: Analyze a bond portfolio to find the modified duration, convexity, and the impact of yield changes on the portfolio's market value.
- **Methodology**:
  - Calculation of individual bond's modified duration and convexity.
  - Aggregation of portfolio-level metrics using market value weights.
  - Estimation of market value changes through first and second-order approximations.

### Question 2: Nonlinear Yield Regression
- **Objective**: Estimate the parameters of a nonlinear yield regression model and compare predicted prices to actual market prices for both bonds and coupon strips.
- **Data**: `TeamAssignment2_Q2_bond.csv` containing US treasury securities' details for settlement on 2023-12-15.
- **Approach**:
  - Utilization of the nonlinear regression model to estimate spot rates.
  - Prediction of prices for coupon and principal strips, comparing these predictions to actual prices.

### Question 3: Par Rates and Discount Factors
- **Objective**: Determine discount factors and spot rates for default-free bonds and calculate the present value of an annuity.
- **Data**: `TeamAssignment2_Q3.csv` containing par rates for bonds with annual coupon payments.
- **Tasks**:
  - Calculation of discount factors for specified maturities.
  - Utilization of spot rates to approximate the present value of an annuity.

## Submission Components

1. **PowerPoint Slides PDF**: A concise presentation detailing findings for each question, emphasizing clarity, and effective communication.
2. **R Script**: Comprehensive code file with detailed comments for each part of the assignment, showcasing the application of various computational finance techniques.

## Skills and Tools Used

- **Quantitative Analysis**: Employed statistical modeling techniques for sensitivity analysis.
- **R Programming**: Extensive use of `jrvFinance`, `data.table`, `optimx`, and `ggplot2` packages for data manipulation, model estimation, and visualization.
- **Financial Modeling**: Developed models to estimate bond prices and yield curves, applying theoretical concepts to real-world data.

## Key Findings and Contributions

- Demonstrated the impact of yield changes on bond portfolio value, providing insights into risk management.
- Estimated yield curve parameters, offering a predictive outlook on bond and strip prices.
- Calculated present values under different interest rate scenarios, aiding in investment decision-making.

This project exemplifies the application of quantitative finance principles in analyzing fixed income securities, emphasizing practical skills in data analysis, modeling, and financial theory application.
