# Automating Record Linkage

This repository consist of my R scripts which I have developed while working with Texas A&M Engineering Experiment Station.

• Using Predictive Analytics to create uniform big data by linking multiple datasets with Patient information.

• Used R to establish performance parameters to link entities.

• Combined Machine Learing algorithms with probabilistic and rule-based approaches to increase efficiency.

_______________________________________________________________________________________________________________________________________

Record linkage (RL) is the task of finding records in a data set that refer to the same entity across different data sources (e.g., data files, books, websites, and databases).

My work is concentrated on linking same people in multiple databases. At present, this work is about construction of different features/parameters which can be used to determine the similarity between two entities. Primarly, SVM and Random Forests algorithms are used and evaluation of models is done on NC state voter database.

The code is split in following major steps:
1) How to build positive/negative pairs from entire database.
2) Generating features for those pairs.
3) Running either svm or random forests for prediction.

For detailed description on how to build parameters/features and an overview of the project please refer to the Record_Linkage.pptx file.
Other than the code, relevant research papers, log of activities, NC State voter database and results are also present in sub-directories.

Note: At the end of each script, output is saved in a csv format. This csv file is read in the next script.

Portions of this research were conducted with the advanced computing resources provided by Texas A&M High Performance Research Computing.
