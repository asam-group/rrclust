# Clustering the Swiss Pension Register

The anonymous data of the Swiss Pension Register (PR) are typically used to estimate and project (in the short, middle and long term) the revenues and the expenditures of the Old-Age and Survivors’ Insurance (OASI). In this perspective, it is essential to have a clear look at the register's main statistical features. To better understand it and benefit more from its richness, we propose analysing the raw data by an appropriate clustering method.

We face three main difficulties: 
i) As not only continuous but also nominal or categorical variables structure the register, we have to choose a clustering method that considers any types of variables; 
ii) The a priori number of clusters should be in the first step determined, and thus the question of how to fix it is essential; 
iii) The method should run over big data.

Recently, A. Foss et al. (2016) and A. H. Foss and Markatou (2018) proposed the kamila Method (KAy-means for MIxed LArge data), which is specifically designed to manage a clustering process for mixed distributions. 
Furthermore, a simple rewriting of the kamila's algorithm permits an easy implementation in a map-reduce framework like Hadoop, thus being run on very large data sets. 
On the other hand, Tibshirani and Walther (2005) advocate the use of the "Prediction Strength" as a measure to find the optimal number of clusters. 

We applied the kamila clustering method on the more than 2 000 000 observations of the PR data. 
The technique allows us to determine the optimal number of clusters. 
On this basis, we can analyse the partition of our data. Indeed, each cluster is then analysed, and its principal features are described. 
As a result, it becomes possible to recognise the similarities and dissimilarities between the OASI pensioners subgroups according to their socio-demographic characteristics. 
These pieces of information are crucial to predicting revenues and expenditures of the OASI.
