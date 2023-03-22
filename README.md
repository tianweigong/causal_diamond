# causal_diamond

material for a project of how people learn generative and preventative causal structures in continuous time. 

## :fire: IMPORTANT :fire: : You can watch all stimuli [here](https://eco.ppls.ed.ac.uk/~s1940738/demo/diamond/).


## Data_analysis

#### analysis files

* `dataAnalysis_exp1.R`, `dataAnalysis_exp2.R`, `dataAnalysis_exp3.R` -- analyzing data from different experiments
* `dataAnalysis_comb.Rmd` -- the main analysis based on data from Experiment 2 and 3.

#### data files

* `exp1a.Rda`, `exp1b.Rda`, `exp2.Rda`, `exp_pilot.Rda`

#### stimulus files

* `sti_exp1a`, `sti_exp1b`, `sti_exp2`, `sti_exp_pilot` -- stimuli in three experiments

#### modeling

* `fun_fea.R`, `fun_nor.R`  -- normative and summary-statistic models, and their predictions for stimuli in Experiment 1-2
* `sim_fea.R`, `sim_fea`, `df.expect.Rda` -- generating simlations for summary-statistic models
* `fun_softmax.R` -- softmax functions
* `md_exp1a.R`, `md_exp1b.R`,  `model_exp_pilot` -- code to get the model predictions for each stimulus
* `mdall_exp1a.Rda`, `mdall_exp1b.Rda`, `mdall_exp2.Rda` -- the model predictions for each stimulus
* `softmax_exp1a.R`, `softmax_exp1b.R`, `softmax_exp2.R`,`softmax_exp_pilot.R`, `softmax_report.R` -- code to fit participants' data on the aggragate and individual levels, and their results
* `softmax_exp1a_cond.R`, `softmax_exp1b_cond.R`, `softmax_exp1b_gt.R`, `softmax_exp1b_ngt.R`  -- other fitting results seperate by conditions or with or without ground truths. 

## procedure

HTML for Experiment 1-2.