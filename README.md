# Shurety-et-al.-Heterarchy-Code
The code used in the proof of concept titled: A comparative approach to quantify the heterarchical structures of complex systems.

Step 1: Generate 1000 random theoretical networks each with 100 nodes, using the Erdős–Rényi model. 

Step 2: Calculate the modularity of each theoretical network using the modualrity function of igraph. 

Step 3: Calculate the Probability Hieararchy Core (PHS) for each theoretical network. To get a single metric value: the PHS is calculated for n/2 levels. A linear regression model is run for each level iteration (PHS value vs number of levels) and the AIC was calculated to single our the optimum PHS value. 

Step 4: Metrics are corrected for network size. A linear regression was fitted to a plot of the metric value and the number of nodes within the network. The residuals of the linear regression are then used for further analysis.

Step 4: The residuals are then corrected for differences in magnitude. The residuals were re-scaled for each metric independently, subtracting the mean and dividing by the standard deviation, to translate metrics to deviation units with a zero mean.

Step 5: Plot the theoretical results to construct the heterarchy matrix. 

Step 6: Inport real-world data from a range of repositories and calculate the network modularity using the modualrity function in igraph. 

Step 7: Calculate the PHS for the real-world network. To get a single metric value: the PHS is calculated for n/2 levels. A linear regression model is run for each level iteration (PHS value vs number of levels) and the AIC was calculated to single our the optimum PHS value. 

Step 8: Correct for size in the real-world network. Run a linear regression of the number of nodes vs metric of the theoretical networks (done in step 4), to predict metric values of each real-world network. Then subtract the predicted metric value from the actual metric value.

Step 9: The metrics of the real-world network are then corrected for differences in magnitude. The residuals were re-scaled for each metric independently, subtracting the mean and dividing by the standard deviation, to translate metrics to deviation units with a zero mean.

Step 10: Plot the real-world networks on the heterarchy matrix constructed in step 5. 

Step 11: Calculate the Euclidean distance of each network from the centre of the heterarchy matrix and run statistical analyses.

Step 12: Calculate the angle (degrees) of each network from the Centre of the heterarchy matrix and run statistical analyses.
