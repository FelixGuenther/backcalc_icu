## Code to estimate the daily numbers of new ICU admissions from the number of occupied beds
This repository contains code and data to estimate the daily numbers of new ICU admissions from the number of occupied beds in Germany and German federal states based on data provided by the DIVI.

The developed Bayesian hierarchical model is presented in the .pdf **methods_back_calculation.pdf** in the *methods* Folder.
The other folders consist of:

  - **code**: Code for performing the back-calculation based on the DIVI data in Germany and the separate federal states, including the STAN-code for the Bayesian hierarchical model. For the German data we compare the results of the back-calculation with the true (known) number of new admissions per day
  - **data**: Contains the DIVI data on the number of occupied ICU beds
  - **results**: Contains results of the back-calculation
