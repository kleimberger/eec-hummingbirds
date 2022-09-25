This Extinction_risk_and_hummingbird_traits_README.txt file was generated on 2021-05-20 by Kara Leimberger.
Additional description of methods and dataset are available in Appendix S1 of associated manuscript (Leimberger et al. 2021).

GENERAL INFORMATION

1. Title of Dataset: Extinction risk and hummingbird traits

2. Author Information
	A. Principal Investigator Contact Information
		Name: Kara Leimberger	
		Institution: Oregon State University
		Address: Corvallis, OR USA
		Email: kara.leimberger@oregonstate.edu -or- kleimberger@gmail.com

	B. Associate or Co-investigator Contact Information
		Name: Matthew Betts	
		Institution: Oregon State University
		Address: Corvallis, OR USA
		Email: matt.betts@oregonstate.edu

3. Date of data collection (single date, range, approximate date): NA

4. Geographic location of data collection: NA 

5. Information about funding sources that supported the collection of the data: NA

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: None

2. Links to publications that cite or use the data: 

<INSERT PUBLICATION HERE>

3. Links to other publicly accessible locations of the data: None

4. Links/relationships to ancillary data sets: None

5. Was data derived from another source? Yes; data were compiled from a variety of other sources.
	A. If yes, list source(s): 

BETTS, M.G., WOLF, C., RIPPLE, W.J., PHALAN, B., MILLERS, K.A., DUARTE, A., BUTCHART, S.H.M. & LEVI, T. (2017) Global forest loss disproportionately erodes biodiversity in intact landscapes. Nature 547, 441–444.
BIRDLIFE INTERNATIONAL (2020) IUCN Red List for birds. http://datazone.birdlife.org/.
IUCN (2020) The International Union for Conservation of Nature (IUCN) Red List of Threatened Species. http://www.iucnredlist.org [accessed 24 July 2020].
JETZ, W., THOMAS, G.H., JOY, J.B., HARTMANN, K. & MOOERS, A.O. (2012) The global diversity of birds in space and time. Nature 491, 444–448.
MCGUIRE, J.A., WITT, C.C., REMSEN, J.V., CORL, A., RABOSKY, D.L., ALTSHULER, D.L. & DUDLEY, R. (2014) Molecular phylogenetics and the diversification of hummingbirds. Current Biology 24, 910–916.
PIGOT, A.L., SHEARD, C., MILLER, E.T., BREGMAN, T.P., FREEMAN, B.G., ROLL, U., SEDDON, N., TRISOS, C.H., WEEKS, B.C. & TOBIAS, J.A. (2020) Macroevolutionary convergence connects morphological form to ecological function in birds. Nature Ecology & Evolution 4, 230–239.
QUINTERO, I. & JETZ, W. (2018) Global elevational diversity and diversification of birds. Nature 555, 246–250.
TOBIAS, J.A. & PIGOT, A.L. (2019) Integrating behaviour and ecology into global biodiversity conservation strategies. Philosophical Transactions of the Royal Society B: Biological Sciences 374, 20190012.
WILMAN, H., BELMAKER, J., SIMPSON, J., ROSA, C. DE LA, RIVADENEIRA, M.M. & JETZ, W. (2014) EltonTraits 1.0: species-level foraging attributes of the world’s birds and mammals. Ecology 95, 2027–2027.

6. Recommended citation for this dataset: 

<INSERT PUBLICATION HERE>

DATA & FILE OVERVIEW

1. File List:

(1) Phylogenetic_regression.R
(2) Making_IUCN_summary_tables_and_barcharts.R
(3) Hummingbird_IUCN_and_trait_data_with_derived_vars_20200518.csv
(4) BEAST_MCC_10000_tree.tre

2. Relationship between files, if important:

File 1 (R script) relies on Files 3-4
File 2 (R script) relies on File 3 

3. Additional related data collected that was not included in the current data package: None

4. Are there multiple versions of the dataset? No
	A. If yes, name of file(s) that was updated: NA
		i. Why was the file updated? NA
		ii. When was the file updated? NA

DATA-SPECIFIC INFORMATION FOR: Hummingbird_IUCN_and_trait_data_with_derived_vars_20200518.csv

1. Number of variables: 24

2. Number of cases/rows: 366

3. Variable List: 

See Leimberger et al. 2021 (Table S6) for additional details about these variables.

Clade: hummingbird clade (Hermits, Mangoes, Emeralds, Topazes, Coquettes, Mountain Gems, Brilliants, Patagona, Bees). Assigned by examining location of genus in McGuire et al. 2014 hummingbird phylogeny (found in supplementary material). In small number of cases, clade was assigned from Jetz et al. 2012 BirdTree phylogeny (if not present in hummingbird phylogeny).
Taxon_ID: species identification number, from IUCN 2020
Species_name_IUCN: scientific name, from IUCN 2020
Species_name: scientific name from Jetz et al. 2012 (BirdTree) phylogeny, used in the phylogenetic regression analysis.
Common_name: common name, from BirdLife International 2020
Redlist_category: IUCN RedList category/status (Least Concern, Near Threatened, Vulnerable, Endangered, Critically Endangered, Data Deficient), from IUCN 2020
Threatened: whether species is considered 'Threatened' (i.e., Vulnerable, Endangered, or Critically Endangered), 0/1 [DERIVED VARIABLE]
Population_trend: IUCN population trend (Decreasing, Stable, Increasing, Unknown), from IUCN 2020
Decreasing: whether species has a 'Decreasing' population trend, 0/1 [DERIVED VARIABLE]
Body_mass: hummingbird mass (g), from Wilman et al. 2014
Bill_length: hummingbird bill length (mm) measured from base of bill to tip by Pigot et al. 2020. Data are from JA Tobias and provided in Table S7 of associated manuscript (Leimberger et al. 2021)
Range_size: area of breeding season Extent of Occurrence, calculated from BirdLife International and NatureServe range maps by Tobias & Pigot 2019
Elevation_min: minimum elevation of geographic range (m), from Quintero & Jetz 2018
Elevation_max: maximum elevation of geographic range (m), from Quintero & Jetz 2018
Elevation_range: Elevation_max - Elevation_min [DERIVED VARIABLE]
Human_footprint: Mean ‘human footprint’ value within the species’ geographic range, from Betts et al. 2017
Migratory_status: migratory behaviour of hummingbird species (full/latitudinal migrant, altitudinal migrant, not a migrant), from BirdLife International 2020
Migratory_score: whether species migrates (i.e., full or altitudinal migrant), 0/1 [DERIVED VARIABLE]
Forest_depend_status: forest dependency (high, medium, low), from BirdLife International 2020
Forest_depend_score: whether species has a 'high' forest dependency score, 0/1 [DERIVED VARIABLE]
Hermit: whether species is in subfamily Phaethornithinae (i.e., genera Eutoxeres, Threnetes, Glaucis, Ramphodon, Anopetia, Phaethornis), 0/1 [DERIVED VARIABLE]
Body_mass_log: log-transformed body mass, base e [DERIVED VARIABLE]
Bill_length_log: log-transformed bill length, base e [DERIVED VARIABLE]
Range_size_log: log-transformed range size, base e [DERIVED VARIABLE]

4. Missing data codes: 

NA: data not available

5. Specialized formats or other abbreviations used: None
