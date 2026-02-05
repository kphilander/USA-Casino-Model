# Distance Decay Estimation Research

This directory contains the analysis code for estimating casino demand distance decay functions using Pennsylvania and Ohio revenue data.

## Paper

See `distance_decay_estimation_paper.md` for the full research paper draft.

## Analysis Pipeline

Run scripts in order:

```bash
Rscript 01_data_preparation.R   # Prepare ZIP and casino data
Rscript 02_distance_calculations.R  # Calculate all distances
Rscript 03_estimation.R         # Estimate decay parameters
Rscript 04_visualization.R      # Create figures
```

## Data Requirements

Before running the analysis, you need to collect:

1. **PA 2022 Revenue** - From [PA Gaming Control Board](https://gamingcontrolboard.pa.gov/news-and-transparency/revenue)
   - Slot revenue by property
   - Table revenue by property
   - Number of gaming positions

2. **OH 2022 Revenue** - From [Ohio Casino Control Commission](https://casinocontrol.ohio.gov/)
   - Casino revenue (4 properties)
   - Racino revenue (8 properties) from Ohio Lottery

Save collected data as:
- `data/pa_revenue_2022.csv`
- `data/oh_revenue_2022.csv`

## Directory Structure

```
research/
├── distance_decay_estimation_paper.md  # Research paper
├── 01_data_preparation.R               # Data prep script
├── 02_distance_calculations.R          # Distance calculations
├── 03_estimation.R                     # Model estimation
├── 04_visualization.R                  # Figures and tables
├── data/                               # Data files (gitignored)
├── results/                            # Estimation results
└── figures/                            # Output figures
```

## Models Estimated

1. **Exponential**: f(d) = exp(-βd)
2. **Power**: f(d) = d^(-β)
3. **Gaussian**: f(d) = exp(-βd²)
4. **Log-logistic**: f(d) = 1 / (1 + (d/α)^β)
5. **Combined**: f(d) = d^(-α) × exp(-βd)
