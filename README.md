# COVID Cost-effectiveness Analysis

This repository contains the technical documentation for the cost-effectiveness analysis of COVID-19 Vaccination Strategies in the Western Pacific Region

Please cite this as:
Carvalho N, Akpan E, Wang Y, Bourke M, Abraham P, McVernon J. Cost-effectiveness Analysis of COVID-19 Vaccination Strategies in the Western Pacific Region: Description of methods, version 1.0, 2023 [27 February]. Available at https://github.com/spectrum-spark/covid-CEA

Correspondence to: natalie.carvalho@unimelb.edu.au


### Code authorship

Edifofon Akphan
Yingying Wang



### Instructions

This repository also contains code to conduct cost-effectiveness analysis, as part of work examining cost-effectiveness of boosting allocations

This code here takes in model outputs from [epidemic simulations](https://github.com/spectrum-spark/covid_singlestrain_scenarios/tree/singlestrain-paper), which are already stored in the [/data](/data) folder.


Base plots: https://github.com/spectrum-spark/covid-CEA/blob/main/2_Plots_Base.R

Tornado plots (sensitivity analysis): https://github.com/spectrum-spark/covid-CEA/blob/main/3_Plots_DSA.R

To produce the plots in the main paper, run [2_Plots_Paper.R](/2_Plots_Paper.R); the plots are saved in [/plots](/plots) folder.

---

*The work in this repository presented to the [Advisory Committee on Immunization and Vaccines-related Implementation Research (IVIR-AC) in February 2023](https://terrance.who.int/mediacentre/data/sage/IVIR-AC_Pink%20Book%20Feb2023.pdf).* It is also an accompaniment to the following paper and report:

Thao P. Le, Eamon Conway, Edifofon Akpan, Isobel Abell, Patrick Abraham, Christopher M. Baker, Patricia T. Campbell, Deborah Cromer, Michael J. Lydeamore, Yasmine McDonough, Ivo Mueller, Gerard Ryan, Camelia Walker, Yingying Wang, Jodie McVernon and Natalie Carvalho,  **Cost-effective boosting allocations in the post-Omicron era of COVID-19 management**, in preparation, 2023.


Eamon Conway, Thao P. Le, Isobel Abell, Patrick Abraham, Edifofon Akpan, Christopher Baker, Mackenzie Bourke, Patricia T. Campbell, Natalie Carvalho, Deborah Cromer, Alexandra B. Hogan, Michael J. Lydeamore, Yasmine McDonough, Ivo Mueller, Gerald Ryan, Camelia Walker, Yingying Wang, and Jodie McVernon, **A flexible immunity model-based framework for evaluation of likely impacts of emerging variants & vaccines: Technical Report**, 2023.

