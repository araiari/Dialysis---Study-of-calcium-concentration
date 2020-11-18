We tried to fit a model in order to predict the values of total calcium at the end of the dialysis session using the averaged data. 
The covariates, selected from preliminary analysis, are: sodium prescribed, age, total calcium at the beginning and the type of therapy. 

The first linear models were not satisfactory at all. We noticed an improvement in the goodness of fit when including the mixed effects of the hospitals in the model.
he residual standard error, computed as variability within the hospitals, is 0.3645 and the hospital standard error, computed as variability between the hospitals, is 0.3893 and so it explains the 55.9% of the total variability.

This approach to the problem was then generalized in order to take into account the grouping structure induced by the different patients in the different hospitals. 
We fitted a mixed effects model, using the dataset containing the data for all sessions, with two random effects, one for the hospitals and one for the patients. 
This model has a residual standard error of 0.3548, an hospital standard error of 0.4532 and a patient standard error of 0.3843, hence it explains 70% of the total variability.


