# Statistical Analysis on Dialysis

The aim of the research is to find a criterion to classify a patient according to his features. 
In particular, we aim to predict the growth of ionized and total calcium concentration in blood 
during the dialysis process through several variables. 

We are collaborating with the Departments of Mathematics of Politecnico di Milano and MOX, 
that gave us the dataset to work with. 
Since the data are private, we will share the methods and the results without providing the original data.


### Preliminary analysis
We started with a preliminary analysis to understand the relevance and the relationship between available features. 

PCA and correlation analysis for continuous variables and MCA for cathegoriacal variables were performed. 
PCA did not show any particoular dependence and did not allow us to reduce the dimensionality of the dataset.
Interesntig results were given by MCA, which evidentiate an high dependency of the data from the hospital.

In the respective folder, one can find the results.

### Linear mixed-effect model
We tried to fit a model in order to predict the values of total calcium at the end of the dialysis session using the averaged data. 
The covariates, selected from preliminary analysis, are: sodium prescribed, age, total calcium at the beginning and the type of therapy.

Linear model did not perform well in describing the variability of the data. 
In light of the previous results, in order to satisfy the hypothesis of independence, a mixed effects model was fitted.
Then we generalized this approach to take into account the grouping structure induced by the different patients in the different hospitals.

In the respective folder, one can find the results.

### Clustering and CART
For each observation there are available five measurements of ionized calcium in different instant of dialysis. 
We aimed at clutering the interpolated curves using KNN algorithm, 
and thereafter learning the features to explain the labels through a CART.




