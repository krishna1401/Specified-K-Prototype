# Specified-K-Prototype

## Definition
> **K-Prototye** is a clustering algorithm used for clustering large Data Sets with mixed attributes i.e. numerical and categorical attributes.

## Simple steps for K-Prototype Algorithm
1. Select k initial prototypes, randomly from the available dataset
2. Allocate each object in dataset, to the cluster nearest to it. Allocation is done on basis of **Dis-similarity Measure**
3. After allocation of all points, reset the similarity of the cluster against the current prototype
4. Stop, when no object change its cluster.

## Dissimilarity Measure
Dissimilarity measure for each object with the cluster prototype is calculating using formula:
```
E = E(n) + E(c)
```
E(n) is the sum of distance measure for **numerical attributes** calculated using **Euclidean Distance**
E(c) is the sum of distance measure for **categorical attributes** calculated using **Information Gain**

## Implementation of Algorithm
Here are the 4 steps for implementing the complete K-Prototype Algorithm:
1. **Read Parameters** - It reads the Network Traffic Data from Kyoto's University Honeypots provided in the link: (http://www.takakura.com/Kyoto_data/).
2. **Feature Selection** - It selects the set of most informational attributes based on **_variance_** for Numerical Attributes and **_information gain_** for Categorical Attributes.
3. **Clustering** - It creates only 2 clusters from the given dataset providing information about the prototype of both the clusters.
4. **Prediction** - It predicts the Labels for the test data and also create a **_Confusion Matrix_** with few other details.
```
Detailed Description is provided in ReadMe folder for each step of the Algorithm listed above.
```

## Running the Code
Clone the given repository and download the dataset available at the given link above. Execute main.r file and select the input file.
