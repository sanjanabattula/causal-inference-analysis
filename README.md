# National Supported Work (NSW) Demonstration Study

## Introduction
The National Supported Work (NSW) Demonstration was a randomized experiment conducted in the mid-1970s to evaluate the impact of a government-funded job training program on improving the economic prospects of disadvantaged workers. This temporary employment program was designed to assist individuals lacking basic job skills by providing them with work experience and counseling in a supportive environment. By helping participants transition into the labor market, the program sought to address challenges such as unemployment and low earnings, particularly for marginalized demographic groups. The study's randomized design provides a robust framework to assess the program’s effectiveness in achieving its objectives.

The primary research question of this analysis is: Does participation in the government-supported job training program improve participants’ earnings from 1975 to 1978? As part of our experiment, we will also explore whether matching can allow us to better measure the response to treatment by addressing imbalances between the treatment and control groups. The study randomly assigned 297 participants to the treatment group, which received the program’s benefits, and 425 participants to the control group, which did not receive any intervention. The outcome of interest is the change in earnings from 1975 to 1978, operationalized as the difference between earnings in 1978 and earnings in 1975 (earnings78 - earnings75). A positive change would indicate an improvement in earnings over time. The analysis incorporates several pre-treatment covariates to control for potential confounders and to explore whether the program's effects varied across demographic subgroups. These covariates include age (measured in years), education (measured as total years of schooling), race/ethnicity (binary variables for Black and Hispanic participants), marital status (married or not), and educational attainment (whether the participant had a high school degree). Baseline earnings in 1975 (earnings75) were also included to account for initial differences in income levels.

A comparison of baseline characteristics between the treatment and control groups (Table 1) indicates that the two groups were largely similar prior to the intervention. Key covariates, including age, education, race/ethnicity, marital status, and baseline earnings, show no statistically significant differences, supporting the validity of the randomization process. The only exception is the proportion of participants without a high school degree, which is slightly lower in the treatment group, suggesting this variable may need to be accounted for in the analysis. By leveraging the randomized design of the NSW Demonstration, this analysis aims to provide causal estimates of the program's impact on earnings. Furthermore, the inclusion of pre-treatment covariates allows for an exploration of differential effects across demographic subgroups, offering a comprehensive understanding of the program’s effectiveness for disadvantaged populations. This study’s findings will contribute to the evidence base on the efficacy of job training interventions and their role in reducing economic disparities.

## Methods
All analyses were performed utilizing the tidyverse, optmatch and MatchIt packages for data management, matching and statistical evaluation. The study began with a baseline multiple linear regression analysis performed on the unmatched data, with the difference in earnings between 1978 and 1975 as the outcome variable. Covariates include age, education level, race(Black and Hispanic), marital status, and high school graduation status.

Propensity scores were calculated using a logistic regression model, where the treatment group was regressed on the covariates listed above. These scores, representing the probability of receiving treatment, were used to assess overlap between treatment and control groups. A boxplot was generated to confirm substantial overlap, indicating the feasibility of matching.

Two matching methods were applied on this dataset: pair matching and full matching. Pair matching utilized 1:1 nearest neighbour matching with a caliper on propensity scores, ensuring treated and control units were closely matched based on their scores. Full matching created matched strata, each containing at least one treated and one control unit, using the optmatch package to optimize balance across covariates. Visualizations of propensity score overlap, and covariate balance were created to support the assessment of matching quality.

To evaluate the effectiveness of matching, balance diagnostics were performed using standardized mean difference for all covariates. Love plots were generated to visualize covariate balance before and after matching for both methods. A threshold of 0.1 for absolute standardized differences was used to determine adequate balance. After matching, a multiple linear regression model was re-estimated using the matched dataset to control for residual covariate differences. Additionally, sensitivity analyses were conducted on the full-matching results to assess the robustness of the findings. This included calculating the treatment effect estimate and its confidence interval under different levels of assumed unmeasured confounding.

## Results
To establish a baseline comparison, multiple linear regression was conducted on the data prior to any matching. The results of this model overall estimated that job training increased earnings by $869 (95% CI -232.52, 1970.52) while controlling for all other covariates. However, the overall model was not significant (p = 0.08), and only one predictor had a significant relationship on earnings (marital status, p = 0.004). Then, propensity scores were calculated and are visualized in Figure 1; as expected, there is significant overlap between the two groups indicating that matching can be performed.

Two different matching methods were utilized, pair matching and full matching. Figure 2 displays the love plot before and after pair matching, and all values after the match are less than 0.1, indicating adequate balance. Figure 3 displays the love plot for the full matching and again all values after the match are less than 0.1, indicating adequate balance with this method as well.

A sensitivity analysis was then done on the full match. At gamma = 1, the estimate was that job training led to a $965 increase in earnings with a 95% confidence interval of (-127, 2383). As one can see, the confidence interval contains 0, indicating that even assuming no unmeasured confounding, there is not a significant effect of the job training on earnings. Finally, multiple linear regression was performed again after matching, this time estimating a $1170 (95% CI -44, 2383) increase in earnings due to job training. Again, the confidence interval contains zero, indicating that there is still no significant effect even after full matching to control for any biases.

## Discussion
The NSW study was a randomized study that assessed the impact of government job training programs on individuals’ earnings between 1975 and 1978. However, despite the randomization, there were still some imbalances between the groups. The goal of this report was to investigate if an effect could be measured after matching to account for these imbalances between the groups. The authors of this paper acknowledge that matching on a randomized study is not often done; this was an experiment to understand if matching could account for any imbalances in the groups and lead to a measurable effect.

As previously discussed, prior to matching, the multiple linear regression model was not significant (p = 0.08). In order to improve balance between the two groups, both pair matching and full matching were completed and analyzed. While both methods lead to adequate balance with all variables having absolute standardized mean differences less than 0.10, full matching was utilized for the rest of the analysis. This is because full matching allowed for all data points to be included, while pair matching required 128 control subjects to be omitted from the sample.

In the end, even after full matching, no measurable effect was seen by both sensitivity analysis and multiple linear regression, despite adequate balance between groups. This indicates with certainty that at least within the study’s sample, there is no statistically significant effect of job training on earnings. However, there is room for future research due to some of the drawbacks of the National Supported Work Demonstration. For example, the study utilized a fairly small follow-up window of 3 years, limiting our ability to understand the impact of earnings over time. It’s possible that the training, combined with a few years of experience, could lead to much higher earnings that were not able to be captured in this study. In conclusion, even though matching leads to more balance in the covariates amongst groups, a statistically significant effect was still not able to be measured, but there is need for further research on the long-term effects of government job training programs.
