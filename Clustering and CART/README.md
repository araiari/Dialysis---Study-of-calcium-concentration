Using averaged data, fitted curves were interpolated by means of piecewise linear curves, with a total of 122 curves with 5 covariates each, 
corresponding to the 5 instant of times of the measure.

The functional approach of KMA failed to classify these curves. 
Therefore, we perform clusterization with KNN in R^2 by looking at the value of ionized calcium at the end and its derivative at the beginning.

Based on these clusters, features to explain the labels were learnt with a Classification Tree.
The covariates which were considered to be important from the previous results were taken as features.
The tree was then pruned obtaining that the clusters differs depending on the usage of beta blockers,
the age of the patient, flow of blood and the initial value of phosphate calcium.

