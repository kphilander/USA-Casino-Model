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

### 3.1 Study Region

The study region encompasses Pennsylvania and Ohio (where we observe casino-level revenue) plus all neighboring states that contribute demand to or compete for demand with PA/OH casinos.

**Demand sources (ZIP codes):**
| State | Role | Rationale |
|-------|------|-----------|
| Pennsylvania | Primary | Revenue observed |
| Ohio | Primary | Revenue observed |
| New York | Border | Feeds demand to PA casinos |
| New Jersey | Border | Feeds demand to PA casinos |
| Delaware | Border | Feeds demand to PA casinos |
| Maryland | Border | Feeds demand to PA casinos |
| West Virginia | Border | Feeds demand to PA and OH casinos |
| Kentucky | Border | Feeds demand to OH casinos |
| Indiana | Border | Feeds demand to OH casinos |
| Michigan | Border | Feeds demand to OH casinos |

**Competing casinos (supply):**
All casino locations in the above states must be included to properly model competition, even though we only observe revenue for PA and OH properties.

### 3.2 Casino Revenue Data (Dependent Variable)

**Pennsylvania** (Source: PA Gaming Control Board)
- 17 licensed casinos (Category 1-4)
- Monthly slot and table revenue by property
- Gaming positions: slot machines and table games reported

**Ohio** (Source: Ohio Casino Control Commission)
- 4 licensed casinos
- Monthly slot and table revenue by property
- Gaming positions: slot machines and table games reported

**Cross-section period:** Calendar year 2022

We use 2022 as our reference period for three reasons:
1. Casino location data (from existing geo-demand model) reflects 2022 market structure
2. 2022 represents post-COVID market stabilization with "normal" gaming patterns
3. ACS 2018-2022 5-year estimates provide matching demographic data

| State | Casinos with Revenue Data | Gaming Positions Available |
|-------|---------------------------|---------------------------|
| Pennsylvania | 14-17 | Yes (slots + tables from PGCB) |
| Ohio | 12 (4 casinos + 8 racinos) | Yes (slots + tables from OCCC) |
| **Total observed** | **26-29** | — |

**Note:** Ohio's 12 properties include 4 full casinos (JACK Cleveland, Hollywood Columbus, Hollywood Toledo, Hard Rock Cincinnati) and 8 racinos (VLT facilities at racetracks). Pennsylvania count depends on whether all Category 4 mini-casinos were operational in 2022.

### 3.3 Competing Casino Locations (No Revenue Observed)

Casinos in neighboring states compete for the same demand pool. We include their locations to properly model the choice set, but do not observe their revenue.

| State | Casinos in Database | Data Source |
|-------|---------------------|-------------|
| New York | 22 | `casinodata.rds` (2022) |
| New Jersey | 11 | `casinodata.rds` (2022) |
| Delaware | 3 | `casinodata.rds` (2022) |
| Maryland | 6 | `casinodata.rds` (2022) |
| West Virginia | 5 | `casinodata.rds` (2022) |
| Kentucky | 0 | No casinos in 2022 |
| Indiana | 12 | `casinodata.rds` (2022) |
| Michigan | 25 | `casinodata.rds` (2022) |
| **Border state total** | **84** | — |

**Note:** Casino locations from existing geo-demand model database reflect 2022 market structure. Includes commercial and tribal facilities.

### 3.4 Casino Supply Characteristics

To control for supply-side heterogeneity, we include casino capacity measures:

| Variable | Definition | Source |
|----------|------------|--------|
| $SLOTS_j$ | Number of slot machines at casino $j$ | State gaming reports |
| $TABLES_j$ | Number of table games at casino $j$ | State gaming reports |
| $POSITIONS_j$ | Total gaming positions ($SLOTS_j + 6 \times TABLES_j$) | Calculated |

The factor of 6 for tables reflects typical seats per table game. Alternative weightings can be tested for robustness.

### 3.5 Demographic Data (Demand Characteristics)

**Source:** U.S. Census Bureau, American Community Survey (ACS) 2018-2022 5-year estimates

**Unit of analysis:** ZIP code tabulation area (ZCTA)

**Variables:**
| Variable | Definition | Source |
|----------|------------|--------|
| $P_i$ | Adult population (21+) | ACS 2018-2022 |
| $W_i$ | Median household income | ACS 2018-2022 |
| $\rho_i$ | Population density | Calculated |

**Note:** The existing `allzips.rds` file contains demographic data that can be used directly or updated with 2018-2022 ACS estimates for consistency.

**Geographic coverage (from `allzips.rds`):**
| State | ZCTAs |
|-------|-------|
| Pennsylvania | 1,830 |
| Ohio | 1,232 |
| New York | 1,824 |
| New Jersey | 595 |
| Delaware | 68 |
| Maryland | 477 |
| West Virginia | 736 |
| Kentucky | 778 |
| Indiana | 806 |
| Michigan | 990 |
| **Total** | **9,336** |

### 3.6 Distance Calculations

- Haversine (great-circle) distance from ZIP centroid to casino location
- Calculated for all ZIP-casino pairs
- Demand allocation limited to casinos within reasonable travel distance (e.g., 150 miles)
- Alternative: Drive-time distance (API-based) for robustness

### 3.7 Summary Statistics

[Table: Summary statistics for key variables - to be populated]

---

## 4. Methodology

### 4.1 Model Specification

**Demand from ZIP $i$ to casino $j$:**

Each ZIP code's gambling demand is allocated across all accessible casinos based on distance and casino attractiveness:

$$D_{ij} = P_i \cdot W_i^\gamma \cdot \frac{A_j \cdot f(d_{ij})}{\sum_{k \in C_i} A_k \cdot f(d_{ik})}$$

Where:
- $D_{ij}$ = demand (in dollars) flowing from ZIP $i$ to casino $j$
- $P_i$ = adult population (21+) in ZIP $i$
- $W_i$ = median household income in ZIP $i$
- $\gamma$ = income elasticity of gambling demand
- $A_j$ = attractiveness/capacity of casino $j$ (see below)
- $f(d_{ij})$ = distance decay function (to be estimated)
- $C_i$ = set of casinos accessible to ZIP $i$ (within threshold distance)

**Casino attractiveness (supply-side control):**

$$A_j = POSITIONS_j^\delta = (SLOTS_j + 6 \times TABLES_j)^\delta$$

Where $\delta$ captures returns to scale in casino capacity. If $\delta = 1$, revenue scales linearly with gaming positions. If $\delta < 1$, diminishing returns to size.

**Casino-level revenue:**

$$R_j = \sum_{i} D_{ij} = \sum_{i} P_i \cdot W_i^\gamma \cdot \frac{A_j \cdot f(d_{ij})}{\sum_{k \in C_i} A_k \cdot f(d_{ik})}$$

**Key insight:** We only observe $R_j$ for PA and OH casinos, but the denominator includes *all* competing casinos (including those in border states). This properly accounts for demand leakage to out-of-state competitors.

**Market share formulation (for PA+OH casinos only):**

$$S_j = \frac{R_j}{\sum_{j \in \{PA,OH\}} R_j}$$

The predicted share depends on the full competitive landscape, but we only fit to observed PA/OH revenues.

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

### 4.3 Parameters to Estimate

| Parameter | Interpretation | Expected Range |
|-----------|----------------|----------------|
| $\beta$ (or $\alpha, \beta$) | Distance decay rate(s) | Context-dependent |
| $\gamma$ | Income elasticity of demand | 0.5 - 1.5 |
| $\delta$ | Returns to scale in casino capacity | 0.5 - 1.2 |

**Note:** The distance decay parameters vary by functional form—some models have one parameter ($\beta$), others have two ($\alpha$, $\beta$).

### 4.4 Estimation Approach

**Primary method: Non-linear least squares (NLS)**

Minimize sum of squared errors between predicted and actual revenues:

$$\min_{\theta} \sum_{j \in \{PA,OH\}} \left( R_j^{observed} - R_j^{predicted}(\theta) \right)^2$$

Where $\theta$ = {distance decay parameters, $\gamma$, $\delta$}.

Alternatively, fit to revenue shares to normalize scale:

$$\min_{\theta} \sum_{j \in \{PA,OH\}} \left( S_j^{observed} - S_j^{predicted}(\theta) \right)^2$$

**Alternative: Maximum likelihood estimation (MLE)**

Assume revenue shares follow Dirichlet distribution:

$$\mathbf{S} \sim Dirichlet(\phi \cdot \mathbf{\pi}(\theta))$$

Where $\mathbf{\pi}(\theta)$ = predicted share vector and $\phi$ = precision parameter.

**Alternative: Bayesian estimation**

Prior distributions on parameters + MCMC sampling. Useful for uncertainty quantification.

### 4.5 Model Comparison Criteria

| Criterion | Formula | Interpretation |
|-----------|---------|----------------|
| R² | $1 - \frac{SS_{res}}{SS_{tot}}$ | Variance explained |
| RMSE | $\sqrt{\frac{1}{n}\sum(S_j - \hat{S}_j)^2}$ | Average error magnitude |
| AIC | $2k - 2\ln(\hat{L})$ | Parsimony-adjusted fit |
| BIC | $k\ln(n) - 2\ln(\hat{L})$ | Stronger parsimony penalty |
| MAPE | $\frac{1}{n}\sum\left|\frac{S_j - \hat{S}_j}{S_j}\right|$ | Percentage error |

### 4.6 Robustness Checks

1. **State-specific estimation** — Do PA and OH have different decay functions?
2. **Distance threshold sensitivity** — How do results change with 100/150/200 mile cutoffs?
3. **Urban vs. rural** — Does decay differ by population density?
4. **Facility type** — Category 1/2/3/4 differences in PA?
5. **Drive-time vs. distance** — Does travel time improve fit?
6. **Capacity measure alternatives** — Different weightings for slots vs. tables
7. **Border casino inclusion** — Sensitivity to assumed locations of competing casinos

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

1. **Partial revenue observation** — We observe revenue only for PA and OH casinos, not for competing border state casinos. Model relies on correct specification of competition.
2. **Supply heterogeneity** — Gaming positions capture capacity but not quality (management, amenities, marketing). Residual variation may reflect unobserved casino attributes.
3. **Selection on observables** — Only casinos that were built are observed; cannot validate predictions for hypothetical locations.
4. **Cross-section identification** — Single time period limits ability to separate distance decay from market-specific effects.
5. **Border casino data quality** — Locations and characteristics of competing casinos may be less precisely measured than PA/OH properties.
6. **Homogeneous preferences assumed** — Model assumes uniform distance decay across all demographic groups.

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
