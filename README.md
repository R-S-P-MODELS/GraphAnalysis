# GraphAnalysis
This is a web App for Graph Analysis, it expectes a tabular file with the adjacency matrix of the graph in its content


This app uses both the igraph and the plotly package, if you already got the dependences you can use it by runGithub on the R interface.


If you dont the dockerfile is provided here that will create a docker image with all required components to run the container

This App lets the user calculate several metrics.

Metrics that are calculated on the graph are displayed in a table

Metrics that are calculated by vertex are displayed in a interactive graphic with plotly

Metrics that are calculated between vertices pairs are displayed in a interactive matrix heatmap

A tool for displaying cumminitioes using cluster_louvain is also displayed

To install via dockerfile just clone this repository and use docker build .

You can also pull the image from dockerhub with docker pull rspmodels/graphanalysis
