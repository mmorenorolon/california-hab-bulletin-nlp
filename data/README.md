# Data Documentation

## Data Source

Primary source:

SCCOOS. *California HAB Bulletin Archive*. https://sccoos.org/archive-ca-hab-bulletins/. Date Accessed: 11/03/2026

The archive contains narrative harmful algal bloom (HAB) bulletins describing:

- Bloom conditions
- Phytoplankton observations
- Toxin measurements
- Marine wildlife impacts
- Shellfish advisories
- Public health communication
- Environmental monitoring summaries

## Data Format

The original source data are unstructured web documents.

Source types include:

- HTML bulletin pages
- Archived email bulletins
- PDF bulletins (not included in the current workflow)

The modeling workflow currently processes HTML bulletin content only.

## Study Period

Project corpus:

- Earliest bulletin year: 2018
- Latest bulletin year: 2026

Exact bulletin dates are inferred from bulletin titles and URLs.

## Access Method

Data are collected programmatically using the R package:

- `rvest`

The workflow:

1. Access archive page
2. Extract bulletin URLs
3. Download bulletin content
4. Extract article text
5. Remove navigation and boilerplate text
6. Create modeling datasets

## Data Lifecycle

### Raw Data

Location:

data/raw/

Contains:

- bulletin_index.csv
- original scraped bulletin records

Raw data should closely resemble source content.

### Processed Data

Location:

data/processed/

Contains:

- cleaned bulletin corpus
- segmented text observations
- labeled modeling datasets

Processed data are suitable for modeling and visualization.

## Redistribution

The original bulletin content is owned by SCCOOS and associated data providers.

This repository contains:

- derived datasets
- metadata
- code

Users should consult SCCOOS for current terms of use before redistributing source bulletin content.

Source:

https://sccoos.org/.