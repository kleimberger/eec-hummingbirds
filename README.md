# The ecology, evolution, and conservation of hummingbirds and their interactions with flowering plants

[![DOI](https://zenodo.org/badge/353152221.svg)](https://zenodo.org/badge/latestdoi/353152221)

Code associated with review article [published in Biological Reviews](https://doi.org/10.1111/brv.12828) (first dissertation chapter)

This project had three main analysis goals:

1.	How much have articles about hummingbirds increased over time?
    -	I selected key words related to ecology, evolution, conservation and downloaded data from Web of Science about the number of hummingbird articles per year.
    -	I then plotted the number of hummingbird-related research articles published over time. The number of published articles has **more than doubled since in the past 20 years.**

<p align="center">
<img src="https://github.com/kleimberger/eec-hummingbirds/blob/main/results/hummingbird_papers_over_time/Papers_over_time_lines_labeled_gray.png" width="500">
</p>

2.	To what extent does hummingbird territoriality limit pollen dispersal?
    -	This question can be studied using fluorescent dye as a pollen analogue; researchers apply it to a centrally located plant, then examine other plants within a certain distance. The expectation is that territorial hummingbirds move shorter distances, therefore limiting pollen dispersal.
    -	I digitized figures from past papers using this approach, then fit exponential decay curves to visualize the relationship between distance and presence of pollen – for each hummingbird group (territorial vs. non-territorial). The curves for each hummingbird group overlap substantially, suggesting that the **effects of territoriality on pollen dispersal may be weaker than previously thought.**

<p align="center">
<img src="https://github.com/kleimberger/eec-hummingbirds/blob/main/results/pollen_dispersal/Pollen_dispersal_curves_metrics.png" width="500">
</p>

3.	Of the 360 hummingbird species, 10% are considered at risk of extinction. Why might this be? What factors explain why certain hummingbirds have poor conservation outlook?

<p align="center">
<img src="https://github.com/kleimberger/eec-hummingbirds/blob/main/results/extinction_risk_and_hummingbird_traits/IUCN_extinction_risk_multiplot_rdylbu.png" width="500">
</p>

    -	I downloaded data on extinction risk from the International Union for the Conservation of Nature (IUCN). ‘At-risk’ species were Threatened (Red List categories Vulnerable, Endangered, or Critically Endangered) or had a declining population trend. I ran separate models for Red List category and population trend.
    -	Predictor variables were species traits, compiled from existing datasets (e.g., geographic range size, elevational range, body mass, bill length, forest dependency, migratory behavior, and more)
    -	Used phylogenetic logistic regression, which accounts for non-independence among closely related hummingbird species: Is hummingbird at risk of extinction? (1/0) ~ X1 + … + X8. I included all predictors in the same model and checked for multicollinearity.
    -	Out of 8 different predictor variables, the most important predictors of hummingbird extinction risk were **range size** and **reliance on forests**.

<p align="center">
<img src="https://github.com/kleimberger/eec-hummingbirds/blob/main/results/extinction_risk_and_hummingbird_traits/Standardized_coefs_bootstrapCI_plot.png" width="500">
</p>
