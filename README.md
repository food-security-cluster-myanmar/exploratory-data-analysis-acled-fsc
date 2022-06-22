
# Introduction

In 2021, Myanmar experienced more conflict events than any other country.

Despite existing in a state of civil war for the past 70 years, conflict in Myanmar had remained at a relatively low level when compared with the other high conflict countries, such as Syria, Yemen and Afghanistan.

However, following the military takeover on 1 February 2021, conflict in Myanmar quickly increased, and by the end of the year it had overtaken Syria as the most conflict-affected country.

While the Russia-Ukraine war in 2022 might shift Myanmar from being the most conflict-affected country, it is likely to remain a conflict hotspot. As such, a deeper exploration of conflict in Myanmar is warranted, especially as it relates to food security and food security programming.

The following report uses data from the Armed Conflict Location and Event Data Project, or ACLED, to analyse and provide an overview of the conflict situation in Myanmar and what that means for food security.

<br><br>

# Datasets used for this report

## Townships

From 2021 version of the 5Ws. Administrative level 3 place codes. Public. [Link](https://raw.githubusercontent.com/food-security-cluster-myanmar/exploratory-data-analysis-acled-fsc/master/data/townships.csv). 

## ACLED

Extracted 2022-01-25. Conflict reference table, at incident level. Provided by MIMU. Read into .csv for public sharing. [Link](https://raw.githubusercontent.com/food-security-cluster-myanmar/exploratory-data-analysis-acled-fsc/master/data/acled_20220125.csv); only used for this report.  

Find updated dataset at Link. 

## Shapefiles 

MIMU shapefiles. Administrative levels 1 and 3 shapefiles. Public. Links for [Admin level 1](https://geonode.themimu.info/layers/geonode%3Ammr_polbnda2_adm1_mimu_250k) and [Admin level 3](https://geonode.themimu.info/layers/geonode%3Ammr_polbnda_adm3_mimu_250k). 

## Actors 

Extracted 2022-02-09. Conflict reference table -- long table, by incident and actor. 
Only observations from 2021 used in the report. Public. [Report link](https://raw.githubusercontent.com/food-security-cluster-myanmar/exploratory-data-analysis-acled-fsc/master/data/actors.csv) (outdated); updated Link.

## ACLED Words

Textmining dataset. Created using `tidytext`. Contains all words from conflict descriptions of each conflict incident, filtered by stopword. Public. [Link](https://raw.githubusercontent.com/food-security-cluster-myanmar/exploratory-data-analysis-acled-fsc/master/data/acled_words.csv).

## Vulnerability in Myanmar

Census reference dataset. Dataset used for the creation of the MIMU-HARP Vulnerability Index bassed on 2015 Census data. Public. [Link](https://raw.githubusercontent.com/food-security-cluster-myanmar/exploratory-data-analysis-acled-fsc/master/data/vulmmr.csv).

