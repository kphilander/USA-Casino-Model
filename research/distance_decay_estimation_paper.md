# Estimating Distance Decay in Casino Demand: Evidence from Pennsylvania and Ohio

**Working Paper Draft**

---

## Abstract

Distance decay functions are fundamental to gravity models used in retail and gaming site selection, yet the functional form is typically assumed rather than empirically estimated. Using monthly revenue data from 17 Pennsylvania casinos and 4 Ohio casinos combined with ZIP code-level demographic data, we estimate and compare alternative distance decay specifications. We find that [results TBD] provides the best fit, with important implications for casino demand modeling and regulatory impact assessment.

**Keywords:** gravity model, distance decay, casino demand, gaming revenue, spatial econometrics

---

## 1. Introduction

### 1.1 Research Question

What functional form of distance decay best characterizes consumer travel behavior to casinos?

### 1.2 Motivation

Gravity models are widely used to:
- Estimate demand for proposed casino locations
- Assess competitive impacts of new market entrants
- Inform regulatory decisions on casino licensing
- Guide corporate site selection decisions

The standard gravity model for casino demand takes the form:

$$D_{ij} = \frac{P_i \cdot W_i \cdot f(d_{ij})}{\sum_{k} f(d_{ik})}$$

Where:
- $D_{ij}$ = demand from zone $i$ to casino $j$
- $P_i$ = population of zone $i$
- $W_i$ = wealth/income measure of zone $i$
- $d_{ij}$ = distance from zone $i$ to casino $j$
- $f(d)$ = distance decay function

**The critical question:** What is the correct specification of $f(d)$?

### 1.3 Contribution

This paper makes three contributions:

1. **Empirical estimation** of distance decay using revealed preference (actual revenue) data
2. **Comparison of functional forms** to identify best-fitting specification
3. **Policy-relevant parameters** for use in regulatory and commercial applications

---

## 2. Literature Review

### 2.1 Gravity Models in Gaming

- Garrett (2004) - Missouri riverboat casino analysis
- Philander & Bernhard (2012) - Regional casino demand estimation
- Humphreys & Marchand (2013) - Canadian casino substitution patterns

### 2.2 Distance Decay Functional Forms

| Form | Equation | Properties |
|------|----------|------------|
| Exponential | $f(d) = e^{-\beta d}$ | Constant decay rate |
| Power | $f(d) = d^{-\beta}$ | Declining decay rate with distance |
| Gaussian | $f(d) = e^{-\beta d^2}$ | Rapid initial decay, slow tail |
| Log-logistic | $f(d) = \frac{1}{1 + (\frac{d}{\alpha})^\beta}$ | S-curve with threshold |
| Combined | $f(d) = d^{-\alpha} e^{-\beta d}$ | Flexible hybrid |

### 2.3 Evidence from Related Sectors

- Retail/shopping center studies (Huff, 1963; Reilly, 1931)
- Healthcare access (Luo & Wang, 2003)
- Recreation demand (Cesario, 1976)

Typical findings suggest β ≈ 1.5-2.5 for discretionary entertainment travel.

---

## 3. Data

### 3.1 Casino Revenue Data

**Pennsylvania** (Source: PA Gaming Control Board)
- 17 licensed casinos (Category 1-4)
- Monthly slot and table revenue by property
- Period: [TBD - suggest 2019-2024, excluding COVID months]

**Ohio** (Source: Ohio Casino Control Commission)
- 4 licensed casinos
- Monthly slot and table revenue by property
- Period: [TBD - matching PA]

| State | Properties | Months | Observations |
|-------|------------|--------|--------------|
| Pennsylvania | 17 | TBD | TBD |
| Ohio | 4 | TBD | TBD |
| **Total** | 21 | — | — |

### 3.2 Demographic Data

**Source:** U.S. Census Bureau, American Community Survey

**Unit of analysis:** ZIP code tabulation area (ZCTA)

**Variables:**
| Variable | Definition | Source |
|----------|------------|--------|
| $P_i$ | Adult population (21+) | ACS 5-year estimates |
| $W_i$ | Median household income | ACS 5-year estimates |
| $\rho_i$ | Population density | Calculated |

**Geographic scope:**
- Pennsylvania: ~1,700 ZCTAs
- Ohio: ~1,200 ZCTAs
- Border states (NY, NJ, WV, MD, DE, MI, IN, KY): ~X,XXX ZCTAs

### 3.3 Distance Calculations

- Haversine (great-circle) distance from ZIP centroid to casino location
- Calculated for all ZIP-casino pairs within 150-mile radius
- Alternative: Drive-time distance (Google/HERE API) for robustness

### 3.4 Summary Statistics

[Table: Summary statistics for key variables]

---

## 4. Methodology

### 4.1 Model Specification

**Casino-level revenue** is the sum of demand from all ZIP codes:

$$R_j = \sum_{i} D_{ij} = \sum_{i} \frac{P_i \cdot W_i^\gamma \cdot f(d_{ij})}{\sum_{k \in C} f(d_{ik})}$$

Where:
- $R_j$ = observed revenue at casino $j$
- $C$ = set of competing casinos accessible to ZIP $i$
- $\gamma$ = income elasticity parameter

**Market share formulation:**

$$S_j = \frac{R_j}{\sum_j R_j} = \frac{\sum_i P_i W_i^\gamma f(d_{ij}) / \sum_k f(d_{ik})}{\sum_j \sum_i P_i W_i^\gamma f(d_{ij}) / \sum_k f(d_{ik})}$$

### 4.2 Candidate Distance Decay Functions

We estimate and compare five specifications:

**Model 1: Exponential decay**
$$f(d) = e^{-\beta d}$$

**Model 2: Power decay**
$$f(d) = d^{-\beta}$$

**Model 3: Gaussian decay**
$$f(d) = e^{-\beta d^2}$$

**Model 4: Log-logistic decay**
$$f(d) = \frac{1}{1 + (d/\alpha)^\beta}$$

**Model 5: Combined exponential-power**
$$f(d) = d^{-\alpha} \cdot e^{-\beta d}$$

### 4.3 Estimation Approach

**Option A: Non-linear least squares (NLS)**

Minimize sum of squared errors between predicted and actual revenue shares:

$$\min_{\theta} \sum_j \left( S_j^{observed} - S_j^{predicted}(\theta) \right)^2$$

Where $\theta$ = {$\beta$, $\gamma$, ...} are parameters to estimate.

**Option B: Maximum likelihood estimation (MLE)**

Assume revenue shares follow Dirichlet distribution:

$$\mathbf{S} \sim Dirichlet(\alpha \cdot \mathbf{\pi}(\theta))$$

Where $\mathbf{\pi}(\theta)$ = predicted share vector.

**Option C: Bayesian estimation**

Prior distributions on parameters + MCMC sampling.

### 4.4 Model Comparison Criteria

| Criterion | Formula | Interpretation |
|-----------|---------|----------------|
| R² | $1 - \frac{SS_{res}}{SS_{tot}}$ | Variance explained |
| RMSE | $\sqrt{\frac{1}{n}\sum(S_j - \hat{S}_j)^2}$ | Average error magnitude |
| AIC | $2k - 2\ln(\hat{L})$ | Parsimony-adjusted fit |
| BIC | $k\ln(n) - 2\ln(\hat{L})$ | Stronger parsimony penalty |
| MAPE | $\frac{1}{n}\sum\left|\frac{S_j - \hat{S}_j}{S_j}\right|$ | Percentage error |

### 4.5 Robustness Checks

1. **State-specific estimation** — Do PA and OH have different decay functions?
2. **Temporal stability** — Are parameters stable across years?
3. **Urban vs. rural** — Does decay differ by population density?
4. **Facility type** — Category 1/2/3/4 differences in PA?
5. **Drive-time vs. distance** — Does travel time improve fit?

---

## 5. Results

### 5.1 Parameter Estimates

[Table: Estimated parameters by model specification]

### 5.2 Model Fit Comparison

[Table: R², RMSE, AIC, BIC by specification]

### 5.3 Predicted vs. Actual Revenue

[Figure: Scatter plot of predicted vs. actual revenue shares]

### 5.4 Distance Decay Curves

[Figure: Comparative plot of estimated decay functions]

### 5.5 Robustness Results

[Tables/figures for robustness checks]

---

## 6. Discussion

### 6.1 Interpretation of Results

- Which functional form fits best and why?
- Economic interpretation of estimated parameters
- Implied travel behavior of casino patrons

### 6.2 Comparison to Prior Literature

- How do our estimates compare to assumed values in practice?
- Consistency with related retail/recreation studies?

### 6.3 Practical Applications

**For regulators:**
- More accurate demand forecasting for license applications
- Better assessment of competitive impacts

**For operators:**
- Site selection optimization
- Market area definition

**For researchers:**
- Empirically grounded parameters for simulation models

### 6.4 Limitations

1. **Selection on observables** — Only casinos that were built are observed
2. **Equilibrium data** — Revenues reflect market equilibrium, not pure demand
3. **Supply heterogeneity** — Casinos differ in size/quality (partially addressed)
4. **Cross-border effects** — Interstate competition complicates analysis
5. **Temporal aggregation** — Monthly data masks day-of-week patterns

---

## 7. Conclusion

[Summary of findings and implications]

---

## References

Cesario, F. J. (1976). Value of time in recreation benefit studies. Land Economics, 52(1), 32-41.

Garrett, T. A. (2004). Casino gaming and local employment trends. Federal Reserve Bank of St. Louis Review, 86(1), 9-22.

Huff, D. L. (1963). A probabilistic analysis of shopping center trade areas. Land Economics, 39(1), 81-90.

Humphreys, B. R., & Marchand, J. (2013). New casinos and local labor markets. Labour Economics, 24, 151-160.

Luo, W., & Wang, F. (2003). Measures of spatial accessibility to health care in a GIS environment. Environment and Planning B, 30(6), 865-884.

Philander, K. S., & Bernhard, B. J. (2012). Informing the regulatory environment: Estimating the demand for destination and nondestination casino locations. UNLV Gaming Research & Review Journal, 16(1), 4.

Reilly, W. J. (1931). The law of retail gravitation. New York: Knickerbocker Press.

---

## Appendix A: Casino Locations and ZIP Code Matching

[Table: Full list of casinos with coordinates and matched ZCTAs]

## Appendix B: Derivation of Likelihood Function

[Mathematical derivation for MLE approach]

## Appendix C: Sensitivity Analysis

[Additional robustness results]

---

*Draft version: February 2026*
*Corresponding author: [TBD]*
