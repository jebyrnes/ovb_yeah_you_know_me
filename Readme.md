# Code for manuscript and appendices of Byrnes, J.E.K. and Dee, L.E. 2024. Causal inference with observational data and unobserved confounding variables. Ecology Letters.

## Github Repository
This code comes from the authors' shared github repository. Its current version can be found at https://github.com/jebyrnes/ovb_yeah_you_know_me if you wish for fork or pull it.

## To replicate our analysis
In order to replicate the simulations we conducted in the paper, you can run code from Supporting Information S6. The corresponding code in an RMarkdown document is located in the `supporting_information_s6_s7` directory. 

## Shiny Apps 

To better understand how confounding can alter analyses, we've included code for SI8 and SI9, both apps, have corresponding directories where you can see their code. As of September 2024, the apps are running on a Shiny server at UMass Boston. For the app that explores a single simulated run, see https://shiny.umb.edu/shiny/users/jarrett.byrnes/shiny_ovb/ and for the app that looks at the distribution of results from replicated simulation runs, see https://shiny.umb.edu/shiny/users/jarrett.byrnes/ovb_sims/ or run them yourself based on the code in this repository. 

For more about shiny, see https://shiny.posit.co/.

## Directories
- *supporting_information_s6_s7* is R markdown files for both simulations used within the manuscript as well as a guide to implementing the causal methods in the manuscript with a sample data set.

- *supporting_information_s8_app_one_run* provides code for an R Shiny App to explore one simulated run of our temperature-snail system to evaluate the performance of different modeling techniques depending on the data generating process in the system. It is currently running at https://shiny.umb.edu/shiny/users/jarrett.byrnes/shiny_ovb/
  
- *supporting_information_s9_s9_simulations* provides code for an R Shiny App to explore the output from thousands of simulated runs of our temperature-snail system to evaluate the performance of different modeling techniques depending on the data generating process in the system. It is currently running at https://shiny.umb.edu/shiny/users/jarrett.byrnes/ovb_sims/.

- *scripts* provides helper scripts and functions for the supporting information files.  

- *data* provides the sample data set used in Supporting Information S7, the code-through example of techniques.  

## Funding
Support and funding for this project comes from the NCEAS working group,
"Scaling-up productivity responses to changes in biodiversity," where the authors conceived of the paper. The authors have also been supported by the NSF as part of the PIE-LTER Program (award #1637630), Woods Hole Sea Grant, and the Stone Living Lab to J.E.K.B.; and by NSF OCE #2049360, NSF CAREER #2340606, and NASA BioSCape award #80NSSC22K0796 to L.E.D.

## Additional Queries
For specific questions, feel free to contact jarrett.byrnes@umb.edu - and we welcome any pull requests with additional code comments that users find helpful to better understand what we did.

