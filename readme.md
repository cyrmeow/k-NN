#README

##Description
In `kNN_income.R` and `kNN_iris.R` we implemented kNN classifiers for income dataset and iris dataset, respectively. It is a weighted kNN classifier, performing k-nearest neighbor classification of a test set using a training set. For each row of the test set, the k nearest training set vectors (according to cosine similarity for income dataset and Euclidean distance for iris dataset) are found, and the classification decision is made by calculating the weighted posteriors of each class and pick the one with a posterior larger than a threshold (default = 0.5) to be the predicted class.


##Files
This submission contains the following files
* readme.md/readme.html, readme file
* kNN_income.R, the kNN classifier for the income dataset
* kNN_iris.R, the kNN classifier for the Iris dataset
* run.sh, a bashscript that allows user to run the classifier
* hw02-report.pdf, the required report of Homework2

##Instructions
1. Unzip the source files, make sure that the raw datasets of both training and test data stay in the same path as the R source files.
2. Open up a command line terminal, change the working directory to the path where source files and raw files are in.
3. Type `./run.sh` or `bash run.sh` to run the classifier
4. The results of classification would be in "results_income.csv" and "results_iris.csv"


##Outputs
The kNN classifier outputs the classification results into files, which are "results_income.csv" and "results_iris.csv".
For example, the "results_income.csv" are organized as:

| actural-class | predicted-class | posterior |  
| ------------- | ----------------| ----------|  
|<=50K          | <=50K           |0.778      |
|<=50K          | <=50K           |0.889      |
|>50K           | <=50K           |0.556      |
|...            |...              |...        |

where the first column is the actual classes of the test dataset, and the second column is the predicted classes classified by the kNN classifier, and the third column is the posterior probabilty of the predicted class.
The "results_iris.csv" has similar structure with "results_income.csv".
