# The Origins of Formula 1: A Bayesian Approach
Winter 2023 Methods of Applied Statistics II, University of Toronto

> <p align="justify"><b>Abstract:</b> <i>Formula 1 as an event generates a huge amount of data every grand-prix weekend. However, this wasn’t always the case. For this work robust models using a Bayesian framework were built in order to estimate the probabilities of a driver scoring points, focusing on the early 1950s, since this was the time were there was the least amount of data available; therefore, benefiting the most from simulation models. The data was scrapped from the FIA’s website using R. Two Bayesian Logistic Regression models, a hierarchical and a fixed effects one, were built using Stan and R. To evaluate the models the ELPD criterion and ROC curves were used. Although, the hierarchical model showed greater performance the difference was almost negligible. After these evaluations, it was possible to conclude that only some teams and the starting position can affect the odds of a driver scoring points in a given race. This can be seen as a first analysis in this area, which has a lot of potential for subsequent works.</i></p>



Running order:
scrapping.R -> data_prep.R -> Project_writeup.qmd

Check out my blogpost for a gentle explanation of the [webscraping](https://abraham-mv.github.io/posts/scraping/) and [bayesian analysis](https://abraham-mv.github.io/posts/bayesian_f1/) parts.
