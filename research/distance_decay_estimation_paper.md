# Estimating Venue-Level Casino Revenue From Statewide Totals: A Gravity Model Approach

Kahlil Philander

*Working Paper — February 2026*

---

## Abstract

Gravity models are widely used to estimate casino demand, yet the distance decay functions that drive these models are typically assumed rather than empirically estimated. This study addresses this gap by estimating distance decay parameters using observed property-level gross gaming revenue (GGR) data from 92 casinos across nine U.S. states, representing $18.8 billion in 2022 GGR. The model is estimated to predict each venue's share of its own state's total GGR—a within-state share formulation that enables forecasting venue-level revenue in jurisdictions that report only statewide totals. Supply-side attractiveness is captured through hotel and table game availability. The gravity model with power distance decay explains 90% of the variance in within-state revenue shares (*R*² = .90), with a decay parameter of *β* = 2.28 and attractiveness multipliers of 1.82× for hotels and 1.43× for table games. Leave-one-market-out cross-validation confirms model stability (*R*² = .88, shrinkage = .02), and leave-one-state-out cross-validation demonstrates the model can predict venue-level allocation within a held-out state to within 3.31 percentage points. These results provide empirically grounded parameters for casino demand modeling and a practical tool for estimating venue-level GGR from aggregate state reports.

**Keywords:** gravity model, distance decay, casino demand, gaming revenue, within-state market share, cross-validation

**JEL Codes:** L83, R12, C51

---

## 1. Introduction

### Background

Gravity models have become a standard tool for estimating casino demand and assessing the competitive impacts of gaming expansion. These models, adapted from retail trade area analysis (Reilly, 1931; Huff, 1963), predict that consumer visitation to a gaming facility is proportional to the facility's attractiveness and inversely related to some function of travel distance. The general formulation allocates demand from geographic zones to competing facilities based on their relative accessibility-weighted attractiveness.

A critical component of any gravity model is the distance decay function, which specifies how demand diminishes as travel distance increases. Despite the importance of this function to model predictions, the vast majority of applied casino demand studies either assume a particular functional form (typically exponential or power) or adopt parameter values from other contexts without empirical validation. This practice introduces substantial uncertainty into demand forecasts and regulatory impact assessments.

A related practical challenge motivates this study. Many U.S. states publish only aggregate statewide GGR totals rather than property-level breakdowns. Analysts seeking to estimate individual venue revenues—for competitive analysis, market entry evaluation, or regulatory impact assessment—lack a systematic method for allocating statewide totals across venues. A model that accurately predicts each venue's share of its state's total GGR would directly address this need.

### Research Question

This paper addresses two related questions: (a) What functional form of distance decay best characterizes consumer travel behavior to casinos, and what are the empirically estimated parameters? (b) Can a gravity model accurately predict the within-state allocation of GGR across competing venues?

### Contributions

This paper makes three contributions to the gaming economics literature:

1. **Scale and scope.** This study provides the largest multi-state estimation of casino distance decay parameters to date, using property-level revenue data from nine states encompassing 92 venues and $18.8 billion in annual GGR.

2. **Within-state share framework.** By estimating the model to predict each venue's share of its own state's total GGR, this study provides a practical forecasting tool: given only a state-level GGR total, the model's predicted within-state shares yield venue-level revenue estimates.

3. **Cross-validation.** Leave-one-market-out and leave-one-state-out cross-validation procedures demonstrate the model's out-of-sample predictive accuracy, including its ability to forecast venue-level allocation in states not used for estimation.

---

## 2. Literature Review

### Gravity Models in Gaming

The application of gravity models to casino demand has a substantial history. Garrett (2004) employed a gravity framework to analyze Missouri riverboat casinos, finding significant distance effects on local employment outcomes. Philander and Bernhard (2012) developed a gravity-based approach to estimate demand for proposed casino locations, demonstrating the model's utility for regulatory applications. Humphreys and Marchand (2013) used spatial econometric methods to study substitution patterns among Canadian casinos, implicitly incorporating distance decay through geographic fixed effects.

More recently, gravity models have been applied to assess the competitive impacts of gaming expansion (Condliffe, 2012), estimate cannibalization effects from new market entrants (Landers, 2008), and model the geographic distribution of problem gambling prevalence (Philander, 2019). In each application, the distance decay function plays a central role in determining predicted outcomes.

### Distance Decay Functional Forms

The spatial interaction literature has developed several candidate functional forms for distance decay, each with distinct theoretical properties.

Exponential decay takes the form *f*(*d*) = exp(−*βd*), where demand decreases at a constant percentage rate per unit distance. This specification implies that the marginal effect of distance is greatest at short distances and diminishes as distance increases. The exponential form has been widely used in transportation and retail studies due to its tractability and intuitive interpretation.

Power decay takes the form *f*(*d*) = *d*^(−*β*), implying that demand follows a power law relationship with distance. Unlike exponential decay, the power function produces a heavier tail, meaning that distant demand sources retain relatively more weight. This specification has theoretical foundations in central place theory and has been applied in studies of urban retail hierarchies.

Gaussian decay takes the form *f*(*d*) = exp(−*βd*²), producing rapid initial decay followed by a very long tail. This specification is less common in retail applications but has been used in studies where demand is thought to be concentrated in a tight local catchment with minimal long-distance draw.

### Evidence From Related Sectors

Studies of distance decay in related discretionary entertainment contexts provide useful benchmarks. Cesario (1976) found power decay exponents between 1.5 and 2.5 for recreational travel, while more recent studies of shopping center patronage suggest exponential decay rates between 0.02 and 0.10 depending on facility type (Huff, 2003). Healthcare accessibility research has documented exponential decay parameters in the range of 0.03 to 0.08 for nonurgent services (Luo & Wang, 2003).

Casino visitation shares characteristics with both retail shopping (discretionary, repeat purchase) and recreational travel (destination-oriented, entertainment-focused), suggesting that decay parameters should fall within the ranges observed in these contexts. However, no prior study has directly estimated decay parameters specifically for casino demand using a multi-state sample, nor has any study framed the estimation objective in terms of within-state venue-level allocation.

---

## 3. Data

### Study Region

The study region encompasses nine primary states where property-level GGR is observed, plus 19 border states whose casinos compete for demand but for which revenue data are not available. The border states serve as supply-side controls: their casinos draw demand away from primary-state venues, and omitting them would bias estimated decay parameters.

**Table 1**

*Study Region Summary*

| Region | States | ZIP codes | Casinos |
|---|---|---|---|
| Primary (revenue observed) | PA, OH, MD, NY, MA, CT, IN, MO, IA | 7,688 | 92 |
| Border (competition only) | NJ, DE, WV, KY, MI, VT, NH, RI, VA, DC, WI, IL, MN, SD, NE, KS, AR, TN, OK | 12,659 | 262 |
| **Total** | **28** | **20,347** | **354** |

### Revenue Data

Property-level 2022 GGR data were obtained from each state's gaming regulatory authority or commission. Revenue figures include both slot machine and table game win except where noted.

**Table 2**

*Revenue Data by State*

| State | Source | Properties | 2022 GGR ($B) | Notes |
|---|---|---|---|---|
| PA | Pennsylvania Gaming Control Board | 14 | 3.23 | Slots + tables |
| NY | New York State Gaming Commission | 12 | 2.65 | Commercial + VLT |
| IN | Indiana Gaming Commission | 12 | 2.50 | Riverboats + racinos |
| OH | Ohio Casino Control Commission / Ohio Lottery | 11 | 2.33 | Casinos + racinos |
| MD | Maryland Lottery and Gaming Control Agency | 6 | 2.05 | |
| MO | Missouri Gaming Commission | 13 | 1.91 | Riverboat casinos |
| IA | Iowa Racing and Gaming Commission | 19 | 1.77 | Commercial only |
| CT | Connecticut Division of Special Revenue | 2 | 1.24 | Scaled; see below |
| MA | Massachusetts Gaming Commission | 3 | 1.13 | |
| **Total** | | **92** | **18.80** | |

*Note.* Connecticut reports slot revenue only. Total GGR was estimated by dividing slot revenue by (1 − 0.318), where 0.318 represents the table game share of total gaming revenue. This scalar was derived from Mohegan Tribal Gaming Authority's FY2022 quarterly supplemental earnings decks and SEC 10-K/10-Q filings, which report slot and table revenue separately. The FY2022 table share (31.8%) was cross-validated against FY2023 (31.0%) and FY2024 (32.2%).

One Indiana property (Caesars Southern Indiana) was absent from the casino location database and was manually geocoded (38.1796°N, 85.9054°W) with supply-side characteristics verified against public records.

Iowa's three tribal casinos (Blackbird Bend, Meskwaki, and Prairie Flower) do not report revenue publicly. These properties remain in the model as unobserved supply-side competitors—their locations draw demand away from nearby commercial casinos—but are excluded from the set of observed venues.

### Supply-Side Controls

Two binary supply-side attractiveness controls were constructed for each casino:

- **Has hotel** (*H*): Whether the property operates an on-site hotel. Initially coded from a comprehensive U.S. casino database containing property-level attributes for 1,476 venues, then verified against property websites and public records for all 92 observed venues. Eleven corrections were applied to reflect 2022 operational status: nine properties were reclassified from no-hotel to hotel (e.g., Grand Falls Casino Resort, River City Casino, Rhythm City Casino Resort), and two were reclassified from hotel to no-hotel (e.g., Hard Rock Casino Northern Indiana, whose planned hotel was not yet operational in 2022).

- **Has tables** (*T*): Whether the property offers table games. Several facility types are slots-only by regulatory design: Ohio video lottery terminal (VLT) racinos, New York VLT facilities, and Massachusetts's Plainridge Park Casino (a racino). Properties with zero reported table revenue were coded as *T* = 0.

Among the 354 casinos in the study region, 145 (41%) have hotels and 338 (95%) have table games. The 16 slots-only properties tend to be smaller facilities in secondary markets.

### Demographic Data

ZIP code-level demographic data were obtained from the American Community Survey (ACS) 2018–2022 five-year estimates. The demand weight for each ZIP code is defined as the product of adult population (aged 21+) and relative household income:

*w*ᵢ = *Pop*ᵢ × (*Inc*ᵢ / 50,000)

where *Inc*ᵢ is the median household income in ZIP code *i*. The income normalization at $50,000 scales the demand weight such that a ZIP code with median income contributes proportionally more demand than one with below-median income, consistent with the positive income elasticity of gambling expenditure documented in prior research.

### Distance Calculations and Market Clustering

Haversine (great-circle) distances were calculated between each of 20,347 ZIP code centroids and each of 354 casino locations, producing a distance matrix of over 7.2 million pairs. A 150-mile threshold was applied, beyond which demand contribution is assumed negligible.

Nearby casinos were clustered into "markets" using single-linkage hierarchical clustering on the pairwise distance matrix of casino locations. This ensures that co-located properties (e.g., two Philadelphia casinos separated by three miles) compete as a unit rather than being treated as independent demand sinks. Two clustering radii were evaluated (5 mi and 10 mi); the 5-mile radius minimized the sum of squared errors and was selected for all reported results, yielding 280 distinct markets (85 with observed revenue, 195 in border states).

---

## 4. Methodology

### Gravity Share Model

The core model follows the gravity tradition. Demand from ZIP code *i* for market *j* is:

*D*ᵢⱼ = *w*ᵢ × [*A*ⱼ × *f*(*d*ᵢⱼ)] / Σₖ [*A*ₖ × *f*(*d*ᵢₖ)]

where *w*ᵢ is the demand weight defined above, *A*ⱼ is the attractiveness of market *j*, *f*(*d*ᵢⱼ) is the distance decay function, and the sum in the denominator runs over all markets accessible to ZIP *i*. Total predicted demand at market *j* is:

*D*ⱼ = Σᵢ *D*ᵢⱼ

Attractiveness is parameterized as:

*A*ⱼ = exp(*α*_H × *H*ⱼ + *α*_T × *T*ⱼ)

where *H*ⱼ and *T*ⱼ are binary indicators for hotel and table game availability, and *α*_H and *α*_T are parameters to be estimated. This multiplicative specification allows each amenity to scale demand independently.

Three candidate distance decay functions are tested:

- **Exponential:** *f*(*d*) = exp(−*βd*)
- **Power:** *f*(*d*) = (*d* + 0.1)^(−*β*)
- **Gaussian:** *f*(*d*) = exp(−*βd*²)

The small constant (0.1 miles) in the power specification prevents division by zero for ZIP codes containing a casino.

#### Within-State Share Objective

The model is estimated by minimizing the sum of within-state share errors across all states. For each state *s*, define the within-state share of market *j* as:

Actual: *S*ⱼˢ = *Rev*ⱼ / Σₖ∈ₛ *Rev*ₖ

Predicted: *Ŝ*ⱼˢ = *D*ⱼ / Σₖ∈ₛ *D*ₖ

The objective function is:

min(β, α_H, α_T) Σₛ Σⱼ∈ₛ (*S*ⱼˢ − *Ŝ*ⱼˢ)²

This formulation has two advantages over minimizing errors on pooled (cross-state) revenue shares. First, it isolates the model's ability to predict competitive allocation within a state from its ability to predict aggregate state-level demand—the latter depends on factors (e.g., state population, regulatory environment) outside the model. Second, it directly produces the object of practical interest: venue-level shares of state GGR that can be applied to statewide totals from jurisdictions that do not report property-level data.

Optimization is performed using the L-BFGS-B algorithm with 1,000 maximum iterations and convergence tolerance of 10⁻².

### Cross-Validation

Two cross-validation procedures assess out-of-sample prediction accuracy.

**Leave-one-market-out (LOOCV).** Each of the 85 observed markets is held out in turn. The model is re-estimated on the remaining 84 markets, and the held-out market's within-state share is predicted. The held-out market's casinos remain in the supply set (they still draw demand), but their revenue is hidden from the estimator. This procedure produces 85 out-of-sample predictions.

**Leave-one-state-out (LOSO).** Each of the nine primary states is held out in turn. The model is re-estimated on the remaining eight states, and the held-out state's within-state venue allocation is predicted. This is the most demanding test: it asks whether distance decay and attractiveness parameters estimated in other states can predict venue-level allocation in an entirely unseen market.

### Robustness

Two robustness checks are reported. First, a log-linear (LN) model provides an alternative, reduced-form specification. In the LN model, a demand index is computed for each market as *D*ⱼ = Σᵢ [*w*ᵢ × *f*(*d*ᵢⱼ)] (without the attractiveness-weighted denominator), and log-revenue is regressed on the log of this index plus supply-side controls:

ln(*Rev*ⱼ) = *α* + *γ* × ln(*D*ⱼ) + *β*_H × *H*ⱼ + *β*_T × *T*ⱼ + *ε*ⱼ

The decay parameter *β* is optimized to maximize the OLS *R*² on log-revenue. The demand elasticity *γ* is freely estimated (unlike the gravity model, which implicitly constrains *γ* = 1). Predictions are converted to within-state shares for comparison: *Ŝ*ⱼ = exp(*ŷ*ⱼ) / Σₖ∈ₛ exp(*ŷ*ₖ).

Second, an eight-state model excluding Iowa is estimated to assess sensitivity to Iowa's inclusion. Iowa contains three tribal casinos whose supply-side characteristics and revenues are unobserved; excluding the state entirely tests whether the results are robust to this data limitation.

---

## 5. Results

### Distance Decay Parameter Estimates

Table 3 presents the estimated gravity model parameters for each distance decay specification. The power decay function provides the best fit, explaining approximately 90% of the variance in within-state revenue shares.

**Table 3**

*Gravity Model Parameter Estimates (Within-State Shares)*

| Decay form | *β* | *α*_Hotel | *α*_Tables | SSE | *R*² |
|---|---|---|---|---|---|
| **Power** | **2.279** | **0.597** | **0.358** | **0.111** | **.898** |
| Exponential | 0.072 | 0.628 | 0.422 | 0.122 | .889 |
| Gaussian | 0.002 | 0.730 | 0.515 | 0.143 | .870 |

*Note.* *N* = 85 observed markets across nine states. Parameters estimated by L-BFGS-B minimizing within-state share SSE. Optimal clustering radius = 5 miles (280 markets: 85 observed, 195 border). Power decay: *f*(*d*) = (*d* + 0.1)^(−*β*). All three specifications select the same optimal clustering radius.

### Attractiveness Multipliers

The estimated attractiveness parameters translate into multiplicative effects on predicted demand. Under the best-fitting power decay specification:

**Table 4**

*Attractiveness Multipliers (Power Decay Model)*

| Amenity combination | Multiplier |
|---|---|
| Hotel only | 1.82× |
| Table games only | 1.43× |
| Hotel + table games | 2.60× |

*Note.* Multipliers computed as exp(*α*). A casino with a hotel is predicted to attract 1.82 times the demand of an otherwise identical casino without a hotel, holding distance and all other factors constant.

These multipliers are economically meaningful. A full-service casino with both a hotel and table games is predicted to attract 2.60 times the demand of a slots-only facility without a hotel—a difference that substantially affects predicted market shares, particularly in states where some venues are racinos (slots-only, no hotel) competing against integrated resorts.

### Interpretation of Power Decay

The power decay exponent *β* = 2.28 means that demand falls with the inverse square of distance. At twice the distance, demand is 2^(−2.28) = 0.21 times as large—roughly one fifth. At three times the distance, demand is 3^(−2.28) = 0.083 times as large. This produces rapid local concentration with a moderate tail: a casino draws the majority of its demand from within 25 miles, but distant population centers with large populations can still contribute meaningfully.

This exponent falls within the 1.5–2.5 range documented by Cesario (1976) for recreational travel and is consistent with casinos functioning as regional entertainment destinations that attract primarily local and near-local patronage.

### Cross-Validation

**Table 5**

*Leave-One-Market-Out Cross-Validation Results*

| Model | MAPE (pp) | RMSE (pp) | *R*² |
|---|---|---|---|
| Gravity (power) | 2.89 | 3.95 | .879 |
| LN (exponential) | 2.88 | 3.95 | .879 |

*Note.* MAPE = mean absolute prediction error; RMSE = root mean square error; pp = percentage points of within-state share. 85 folds. Each fold re-estimates all parameters on the remaining 84 markets.

The gravity model's in-sample *R*² of .898 shrinks to .879 under LOOCV—a shrinkage of only .020, indicating that the three-parameter model is not overfit to the data. The LN model shows comparable out-of-sample performance despite its different in-sample metric (OLS *R*² on log-revenue = .630).

**Table 6**

*Leave-One-State-Out Cross-Validation: Within-State Share MAPE (Percentage Points)*

| State | Properties | Markets | State GGR ($M) | Gravity MAPE | LN MAPE |
|---|---|---|---|---|---|
| CT | 2 | 2 | 1,236 | 5.49 | 7.55 |
| IA | 19 | 15 | 1,765 | 1.55 | 2.28 |
| IN | 12 | 11 | 2,504 | 3.68 | 2.97 |
| MA | 3 | 3 | 1,132 | 3.86 | 6.85 |
| MD | 6 | 6 | 2,051 | 3.22 | 5.61 |
| MO | 13 | 12 | 1,905 | 2.00 | 2.15 |
| NY | 12 | 12 | 2,646 | 3.40 | 1.89 |
| OH | 11 | 11 | 2,333 | 3.43 | 2.17 |
| PA | 14 | 13 | 3,229 | 3.16 | 3.37 |
| **Mean** | | | | **3.31** | **3.87** |

*Note.* Each row holds out one state entirely. The model is re-estimated on the remaining eight states, then within-state venue shares are predicted for the held-out state. MAPE is computed over the held-out state's markets.

The LOSO results demonstrate that gravity model parameters estimated in other states transfer well to an unseen market. The overall mean within-state share MAPE of 3.31 percentage points means that, on average, the model's predicted venue share deviates from the actual share by about 3 percentage points. The gravity model outperforms the LN model in seven of nine states.

States with few venues (CT, MA) show larger prediction errors, as expected—with only two or three properties, each venue's share is large and any misallocation produces a sizable error in percentage-point terms. States with many venues (IA, MO) show the smallest errors, reflecting the law of large numbers and the model's ability to accurately rank-order venues by demand.

### Property-Level Predictions

Table 7 presents within-state share predictions for the 20 largest properties by revenue under the gravity model.

**Table 7**

*Predicted Versus Actual Within-State Revenue Shares: Top 20 Properties*

| Property | State | Rev ($M) | State GGR ($M) | Actual (%) | Predicted (%) | Error (pp) |
|---|---|---|---|---|---|---|
| MGM National Harbor | MD | 884.5 | 2,051 | 43.12 | 47.33 | +4.21 |
| Encore Boston Harbor | MA | 729.7 | 1,132 | 64.47 | 59.35 | −5.12 |
| Mohegan Sun | CT | 722.7 | 1,236 | 58.47 | 53.23 | −5.24 |
| Live! Casino & Hotel | MD | 705.4 | 2,051 | 34.39 | 25.31 | −9.08 |
| Resorts World Casino NYC | NY | 645.0 | 2,646 | 24.37 | 36.26 | +11.89 |
| Empire City Casino | NY | 613.7 | 2,646 | 23.19 | 21.25 | −1.94 |
| Parx Casino and Racing | PA | 598.7 | 3,228 | 18.54 | 12.04 | −6.50 |
| Wind Creek Bethlehem | PA | 515.8 | 3,228 | 15.98 | 11.34 | −4.64 |
| Foxwoods Resort Casino | CT | 513.3 | 1,236 | 41.53 | 46.77 | +5.24 |
| Hard Rock Casino N. Indiana | IN | 419.5 | 2,504 | 16.76 | 6.09 | −10.67 |
| Rivers Casino Pittsburgh | PA | 355.0 | 3,228 | 11.00 | 6.34 | −4.66 |
| Horseshoe Hammond | IN | 346.1 | 2,504 | 13.82 | 10.52 | −3.30 |
| Horseshoe Indianapolis | IN | 341.8 | 2,504 | 13.65 | 10.79 | −2.86 |
| Ameristar St. Charles | MO | 302.5 | 1,905 | 15.88 | 12.41 | −3.47 |
| MGM Northfield Park | OH | 285.9 | 2,333 | 12.25 | 9.48 | −2.77 |
| Hollywood Casino Columbus | OH | 263.6 | 2,333 | 11.30 | 17.05 | +5.75 |
| JACK Cleveland Casino | OH | 262.4 | 2,333 | 11.25 | 10.90 | −0.35 |
| MGM Springfield | MA | 259.1 | 1,132 | 22.89 | 28.37 | +5.48 |
| Jake's 58 | NY | 256.6 | 2,646 | 9.70 | 15.73 | +6.03 |
| Hard Rock Cincinnati | OH | 251.5 | 2,333 | 10.78 | 10.07 | −0.71 |

*Note.* Predicted shares from the power decay gravity model. Within-state shares sum to 100% within each state. Error = predicted − actual. Full 92-property table in Appendix A.

The model captures the broad distribution of revenues well, correctly identifying the dominant property in each state and appropriately scaling smaller venues. The largest absolute errors occur for properties whose competitive position reflects factors not in the model—for example, Resorts World Casino NYC (+11.89 pp) is a VLT-only facility in a dense urban market where the model's population-weighted demand index overpredicts share, and Hard Rock Casino Northern Indiana (−10.67 pp) is a newly opened property (2021) whose market share may not yet have reached equilibrium.

### Robustness

#### *Log-Linear Model*

**Table 8**

*Log-Linear Model Estimates (Best Decay Specification)*

| Parameter | Estimate | *SE* | *t* |
|---|---|---|---|
| Intercept | 12.065 | 0.610 | 19.77 |
| ln(*D*ⱼ) [demand elasticity] | 0.536 | 0.046 | 11.69 |
| Hotel | 0.230 | 0.129 | 1.79 |
| Tables | 0.286 | 0.169 | 1.70 |

*Note.* OLS regression of ln(Revenue) on ln(demand index) and supply controls. *n* = 85 markets. Exponential decay with *β* = 0.103. *R*² = .630, adjusted *R*² = .617.

The LN model's demand elasticity (*γ* = 0.536) is significantly less than unity (*t* = −10.11 for *H*₀: *γ* = 1), suggesting diminishing returns to demand—a feature the gravity model implicitly constrains away. Despite this, when LN predictions are converted to within-state shares, property-level MAPE (2.57 pp) is comparable to the gravity model (2.46 pp), indicating that both specifications capture similar competitive dynamics.

#### *Model Without Iowa*

**Table 9**

*Gravity Model Comparison: Nine-State Versus Eight-State (Without Iowa)*

| Specification | *N* | *β* | *α*_H | *α*_T | *R*² | Prop MAPE |
|---|---|---|---|---|---|---|
| Nine-state (with Iowa) | 92 | 2.279 | 0.597 | 0.358 | .898 | 2.46 |
| Eight-state (without Iowa) | 73 | 2.162 | 0.591 | 0.348 | .895 | 2.81 |

*Note.* Both models use power decay. Prop MAPE in percentage points of within-state share.

Parameter estimates are stable across specifications. The decay parameter shifts by only 5.1% (from 2.279 to 2.162), and attractiveness multipliers are virtually unchanged. The small increase in MAPE (0.35 pp) reflects the loss of 19 Iowa observations rather than any instability in the estimated relationship.

---

## 6. Discussion

### Interpretation of Results

The finding that power decay with *β* ≈ 2.28 best characterizes casino demand has several implications.

**Local market dominance.** The power decay exponent implies that demand falls to approximately one fifth at twice the distance and one twelfth at three times the distance. Casino revenues are heavily concentrated among nearby residents, consistent with casinos functioning as local entertainment venues rather than destination attractions.

**Supply-side differentiation matters.** The hotel multiplier (1.82×) and table game multiplier (1.43×) are both economically substantial. A full-service casino with both amenities is predicted to attract 2.60 times the demand of an otherwise identical slots-only facility without a hotel. This has direct implications for licensing decisions and capital investment: the model quantifies the competitive advantage conferred by amenity investment.

**Within-state share as a forecasting tool.** The *R*² of .90 on within-state shares demonstrates that distance and two binary supply controls explain the vast majority of how state GGR is allocated across venues. The practical application is straightforward: for a state that reports only aggregate GGR, one can construct the gravity model's predicted within-state shares for each venue and multiply by the statewide total to obtain venue-level estimates. The LOSO results (3.31 pp mean error) provide a confidence bound for this exercise.

### Model Stability

The LOOCV shrinkage of .020 (*R*² from .898 to .879) is small by any standard, indicating that the three-parameter model is not overfit. The LOSO results further confirm transferability: parameters estimated on eight states predict the ninth state's venue allocation to within 3.31 pp on average.

The robustness to Iowa's exclusion provides additional confidence. Iowa presents the most challenging data environment in the sample (tribal casinos with unobserved revenue, many small rural properties), yet its inclusion or exclusion shifts the decay parameter by about 5%.

### Comparison to Prior Literature

The estimated power decay exponent (*β* = 2.28) falls within Cesario's (1976) range of 1.5–2.5 for recreational travel. The dominance of power over exponential decay differs from some retail applications but is consistent with casino markets where some patrons travel considerable distances for resort-style properties, producing a heavier distance tail than the exponential form accommodates.

### Limitations

Several limitations should be noted.

First, the model is estimated on a single cross-section (2022). Temporal stability of the estimated parameters—particularly through demand shocks such as the COVID-19 pandemic—remains untested.

Second, supply-side attractiveness is captured by only two binary controls (hotel and tables). Unobserved characteristics such as management quality, marketing expenditure, entertainment offerings, and facility age likely affect revenues independently of location.

Third, Connecticut's table revenue is imputed using a scalar derived from Mohegan Sun's financial disclosures. While this scalar is cross-validated across three fiscal years, it remains an approximation.

Fourth, the model assumes homogeneous distance decay across demographic groups. Decay rates may vary by age, income, or urbanicity, and a richer model could allow for heterogeneous parameters.

Fifth, Iowa's three tribal casinos are included as supply-side competitors with assumed characteristics (hotel, tables) rather than observed attributes. The robustness check excluding Iowa mitigates but does not eliminate this concern.

---

## 7. Conclusion

This study provides the largest multi-state empirical estimation of casino demand distance decay to date, using property-level revenue data from 92 venues across nine U.S. states totaling $18.8 billion in 2022 GGR. The key findings are:

1. **Power distance decay** best characterizes casino demand (*R*² = .90 on within-state shares), with an exponent of *β* = 2.28 implying that demand falls with approximately the inverse square of distance.

2. **Supply-side amenities** have large effects on predicted demand. Casinos with hotels attract 1.82 times more demand than those without; table game availability adds another 1.43× multiplier.

3. **The model is stable out of sample.** LOOCV shrinkage is only .020 (from *R*² = .90 to .88), and leave-one-state-out cross-validation demonstrates that the model can predict a new state's venue-level allocation to within 3.31 percentage points.

4. **Within-state shares provide a practical forecasting tool.** Given only a state's total GGR, the model's predicted shares yield venue-level revenue estimates—enabling analysis in jurisdictions that do not report property-level data.

These empirically grounded parameters and the within-state share framework offer a more rigorous foundation for casino demand modeling than the commonly assumed values in the literature. Future research should extend this analysis to additional states as data become available, examine heterogeneity in decay rates across demographic segments, and assess the temporal stability of estimated parameters by replicating the analysis across multiple years.

---

## References

Cesario, F. J. (1976). Value of time in recreation benefit studies. *Land Economics*, *52*(1), 32–41.

Condliffe, S. (2012). Estimating the demand for casino gaming with longitudinal data. *Applied Economics*, *44*(36), 4769–4777.

Garrett, T. A. (2004). Casino gaming and local employment trends. *Federal Reserve Bank of St. Louis Review*, *86*(1), 9–22.

Huff, D. L. (1963). A probabilistic analysis of shopping center trade areas. *Land Economics*, *39*(1), 81–90.

Huff, D. L. (2003). Parameter estimation in the Huff model. *ArcUser*, October–December, 34–36.

Humphreys, B. R., & Marchand, J. (2013). New casinos and local labor markets: Evidence from Canada. *Labour Economics*, *24*, 151–160.

Landers, J. (2008). An assessment of casino competition's effect on Indian gaming. *Journal of Regional Analysis and Policy*, *38*(2), 115–129.

Luo, W., & Wang, F. (2003). Measures of spatial accessibility to health care in a GIS environment. *Environment and Planning B: Planning and Design*, *30*(6), 865–884.

Philander, K. S. (2019). Regional impacts of casino availability on gambling problems: Evidence from the Canadian Community Health Survey. *Tourism Management*, *71*, 173–178.

Philander, K. S., & Bernhard, B. J. (2012). Informing the regulatory environment: Estimating the demand for destination and nondestination casino locations. *UNLV Gaming Research & Review Journal*, *16*(1), 27–41.

Reilly, W. J. (1931). *The law of retail gravitation*. Knickerbocker Press.

---

## Appendix A: Property-Level Results

**Table A1**

*Within-State Revenue Shares: All 92 Properties (Power Decay Gravity Model)*

| Property | St | Rev ($M) | St Total ($M) | Actual (%) | Predicted (%) | Error (pp) |
|---|---|---|---|---|---|---|
| MGM National Harbor | MD | 884.5 | 2,051 | 43.12 | 47.33 | +4.21 |
| Encore Boston Harbor | MA | 729.7 | 1,132 | 64.47 | 59.35 | −5.12 |
| Mohegan Sun | CT | 722.7 | 1,236 | 58.47 | 53.23 | −5.24 |
| Live! Casino & Hotel | MD | 705.4 | 2,051 | 34.39 | 25.31 | −9.08 |
| Resorts World Casino NYC | NY | 645.0 | 2,646 | 24.37 | 36.26 | +11.89 |
| Empire City Casino | NY | 613.7 | 2,646 | 23.19 | 21.25 | −1.94 |
| Parx Casino and Racing | PA | 598.7 | 3,228 | 18.54 | 12.04 | −6.50 |
| Wind Creek Bethlehem | PA | 515.8 | 3,228 | 15.98 | 11.34 | −4.64 |
| Foxwoods Resort Casino | CT | 513.3 | 1,236 | 41.53 | 46.77 | +5.24 |
| Hard Rock Casino N. Indiana | IN | 419.5 | 2,504 | 16.76 | 6.09 | −10.67 |
| Rivers Casino Pittsburgh | PA | 355.0 | 3,228 | 11.00 | 6.34 | −4.66 |
| Horseshoe Hammond | IN | 346.1 | 2,504 | 13.82 | 10.52 | −3.30 |
| Horseshoe Indianapolis | IN | 341.8 | 2,504 | 13.65 | 10.79 | −2.86 |
| Ameristar St. Charles | MO | 302.5 | 1,905 | 15.88 | 12.41 | −3.47 |
| MGM Northfield Park | OH | 285.9 | 2,333 | 12.25 | 9.48 | −2.77 |
| Hollywood Casino Columbus | OH | 263.6 | 2,333 | 11.30 | 17.05 | +5.75 |
| JACK Cleveland Casino | OH | 262.4 | 2,333 | 11.25 | 10.90 | −0.35 |
| MGM Springfield | MA | 259.1 | 1,132 | 22.89 | 28.37 | +5.48 |
| Jake's 58 | NY | 256.6 | 2,646 | 9.70 | 15.73 | +6.03 |
| Hard Rock Cincinnati | OH | 251.5 | 2,333 | 10.78 | 10.07 | −0.71 |
| River City Casino | MO | 249.5 | 1,905 | 13.10 | 11.01 | −2.09 |
| Caesars Southern Indiana | IN | 248.2 | 2,504 | 9.91 | 15.49 | +5.58 |
| Harrah's Hoosier Park | IN | 245.6 | 2,504 | 9.81 | 8.39 | −1.42 |
| Prairie Meadows Casino | IA | 235.0 | 1,765 | 13.31 | 13.72 | +0.41 |
| Eldorado Scioto Downs | OH | 234.8 | 2,333 | 10.06 | 6.80 | −3.26 |
| Hollywood Casino St. Louis | MO | 234.4 | 1,905 | 12.30 | 14.18 | +1.88 |
| Resorts World Catskills | NY | 228.7 | 2,646 | 8.64 | 5.82 | −2.82 |
| Hollywood Casino Toledo | OH | 227.7 | 2,333 | 9.76 | 9.98 | +0.22 |
| Miami Valley Gaming | OH | 224.8 | 2,333 | 9.64 | 5.90 | −3.74 |
| Live! Casino Philadelphia | PA | 222.4 | 3,228 | 6.89 | 6.85 | −0.04 |
| Rivers Casino Philadelphia | PA | 216.7 | 3,228 | 6.71 | 6.68 | −0.03 |
| Mohegan Sun Pocono | PA | 215.5 | 3,228 | 6.68 | 5.74 | −0.94 |
| Horseshoe Casino Council Bluffs | IA | 211.1 | 1,765 | 11.96 | 10.04 | −1.92 |
| Horseshoe Casino Baltimore | MD | 209.9 | 2,051 | 10.23 | 10.95 | +0.72 |
| Ameristar Casino East Chicago | IN | 205.0 | 2,504 | 8.19 | 6.23 | −1.96 |
| Rivers Casino Schenectady | NY | 201.9 | 2,646 | 7.63 | 5.58 | −2.05 |
| Ameristar Casino Kansas City | MO | 198.7 | 1,905 | 10.43 | 8.11 | −2.32 |
| Hollywood Casino Meadows | PA | 189.5 | 3,228 | 5.87 | 4.76 | −1.11 |
| Ameristar Casino Council Bluffs | IA | 187.7 | 1,765 | 10.63 | 8.93 | −1.70 |
| Mount Airy Casino Resort | PA | 184.2 | 3,228 | 5.71 | 6.90 | +1.19 |
| Jack Thistledown | OH | 183.2 | 2,333 | 7.85 | 8.86 | +1.01 |
| Hollywood Casino Penn National | PA | 180.6 | 3,228 | 5.59 | 6.10 | +0.51 |
| Argosy Casino Riverside | MO | 177.1 | 1,905 | 9.30 | 9.22 | −0.08 |
| Harrah's North Kansas City | MO | 176.8 | 1,905 | 9.28 | 7.39 | −1.89 |
| Bally's Evansville | IN | 176.6 | 2,504 | 7.06 | 11.66 | +4.60 |
| Harrah's Philadelphia | PA | 172.9 | 3,228 | 5.35 | 7.52 | +2.17 |
| Hollywood Casino Lawrenceburg | IN | 170.2 | 2,504 | 6.80 | 7.07 | +0.27 |
| del Lago Resort & Casino | NY | 163.2 | 2,646 | 6.17 | 2.26 | −3.91 |
| Hollywood Gaming Mahoning Valley | OH | 159.4 | 2,333 | 6.83 | 5.75 | −1.08 |
| Hollywood Gaming Dayton | OH | 150.9 | 2,333 | 6.47 | 8.20 | +1.73 |
| Horseshoe St. Louis | MO | 150.5 | 1,905 | 7.90 | 10.51 | +2.61 |
| Plainridge Park Casino | MA | 143.1 | 1,132 | 12.64 | 12.28 | −0.36 |
| Saratoga Casino Hotel | NY | 141.1 | 2,646 | 5.33 | 3.04 | −2.29 |
| Valley Forge Casino Resort | PA | 136.0 | 3,228 | 4.21 | 14.83 | +10.62 |
| Blue Chip Casino | IN | 133.9 | 2,504 | 5.35 | 5.51 | +0.16 |
| Riverside Casino & Golf Resort | IA | 129.9 | 1,765 | 7.36 | 6.39 | −0.97 |
| Bally's Kansas City | MO | 119.9 | 1,905 | 6.29 | 4.89 | −1.40 |
| Rhythm City Casino Resort | IA | 117.6 | 1,765 | 6.66 | 4.22 | −2.44 |
| Finger Lakes Gaming | NY | 116.5 | 2,646 | 4.40 | 2.34 | −2.06 |
| Live! Casino Pittsburgh | PA | 109.9 | 3,228 | 3.40 | 4.85 | +1.45 |
| Presque Isle Downs & Casino | PA | 109.0 | 3,228 | 3.38 | 2.17 | −1.21 |
| Tioga Downs Casino Resort | NY | 103.3 | 2,646 | 3.90 | 2.63 | −1.27 |
| Diamond Jo Worth | IA | 102.9 | 1,765 | 5.83 | 5.61 | −0.22 |
| Isle of Capri Casino Waterloo | IA | 100.0 | 1,765 | 5.67 | 6.02 | +0.35 |
| Ocean Downs Casino | MD | 96.6 | 2,051 | 4.71 | 6.88 | +2.17 |
| Hard Rock Hotel Sioux City | IA | 96.0 | 1,765 | 5.44 | 4.91 | −0.53 |
| Grand Falls Casino Resort | IA | 95.0 | 1,765 | 5.38 | 6.27 | +0.89 |
| Belterra Casino Resort | IN | 91.8 | 2,504 | 3.67 | 6.10 | +2.43 |
| Hollywood Casino Perryville | MD | 90.5 | 2,051 | 4.41 | 4.81 | +0.40 |
| Belterra Park Gaming | OH | 88.7 | 2,333 | 3.80 | 7.01 | +3.21 |
| Isle of Capri Boonville | MO | 88.5 | 1,905 | 4.65 | 4.41 | −0.24 |
| French Lick Resort Casino | IN | 80.3 | 2,504 | 3.21 | 5.66 | +2.45 |
| Batavia Downs Gaming | NY | 77.3 | 2,646 | 2.92 | 1.74 | −1.18 |
| Diamond Jo Casino Dubuque | IA | 74.2 | 1,765 | 4.21 | 3.29 | −0.92 |
| Isle Casino Hotel Bettendorf | IA | 73.7 | 1,765 | 4.17 | 2.64 | −1.53 |
| Harrah's Casino Council Bluffs | IA | 73.0 | 1,765 | 4.14 | 3.47 | −0.67 |
| Century Casino Cape Girardeau | MO | 72.1 | 1,905 | 3.78 | 3.94 | +0.16 |
| Hamburg Gaming | NY | 70.9 | 2,646 | 2.68 | 1.22 | −1.46 |
| Rocky Gap Casino Resort | MD | 64.3 | 2,051 | 3.14 | 4.71 | +1.57 |
| Q Casino | IA | 51.7 | 1,765 | 2.93 | 2.29 | −0.64 |
| Lakeside Hotel Casino | IA | 51.1 | 1,765 | 2.89 | 3.42 | +0.53 |
| Century Casino Caruthersville | MO | 48.3 | 1,905 | 2.54 | 6.54 | +4.00 |
| St. Jo Frontier Casino | MO | 48.1 | 1,905 | 2.53 | 2.26 | −0.27 |
| Catfish Bend Casino | IA | 44.9 | 1,765 | 2.54 | 4.46 | +1.92 |
| Rising Star Casino Resort | IN | 44.5 | 2,504 | 1.78 | 6.49 | +4.71 |
| Mark Twain Casino | MO | 38.5 | 1,905 | 2.02 | 5.13 | +3.11 |
| Wild Rose Casino Jefferson | IA | 35.9 | 1,765 | 2.03 | 3.84 | +1.81 |
| Wild Rose Casino Clinton | IA | 33.2 | 1,765 | 1.88 | 5.11 | +3.23 |
| Wild Rose Casino Emmetsburg | IA | 30.9 | 1,765 | 1.75 | 3.16 | +1.41 |
| Vernon Downs Casino Hotel | NY | 28.4 | 2,646 | 1.07 | 2.13 | +1.06 |
| Lady Luck Casino Nemacolin | PA | 22.2 | 3,228 | 0.69 | 3.88 | +3.19 |
| Casino Queen Marquette | IA | 21.4 | 1,765 | 1.21 | 2.19 | +0.98 |

*Note.* Within-state shares sum to 100% within each state. Properties sorted by revenue. Error = predicted − actual.

---

## Appendix B: State Revenue Sources

| State | Regulatory Authority | Report Type |
|---|---|---|
| PA | Pennsylvania Gaming Control Board | Monthly slot and table revenue by property |
| OH | Ohio Casino Control Commission; Ohio Lottery Commission | Monthly casino revenue; VLT revenue by facility |
| MD | Maryland Lottery and Gaming Control Agency | Monthly GGR by facility |
| NY | New York State Gaming Commission | Monthly reports: commercial casinos and VLT facilities |
| MA | Massachusetts Gaming Commission | Monthly gross gaming revenue by licensee |
| CT | Connecticut Division of Special Revenue | Monthly slot revenue (table revenue imputed; see text) |
| IN | Indiana Gaming Commission | Monthly win by licensee |
| MO | Missouri Gaming Commission | Monthly adjusted gross receipts by boat |
| IA | Iowa Racing and Gaming Commission | Annual GGR by licensee (commercial only) |

---

## Appendix C: Replication

Analysis code and data are available at: https://github.com/kphilander/USA-Casino-Model/tree/main/research

The analysis can be replicated by running:

```r
source("run_analysis.R")
```

Required inputs:
- `allzips.rds`: ZIP code demographics (ACS 2018–2022)
- `casinodata.rds`: Casino locations and attributes
- `data/[state]_revenue_2022.csv`: Revenue data for each of nine states

Runtime is approximately 15 minutes on a standard laptop (LOOCV with 85 folds is the bottleneck).

---

*Corresponding author: Kahlil Philander (kphilander@gamblingpolicy.com)*
