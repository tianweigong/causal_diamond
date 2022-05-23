# causal_diamond

material for a project of how people learn generative and preventative causal structures in continuous time. 

## Data_analysis

#### analysis files

* `dataAnalysis_exp1.R`, `dataAnalysis_exp2.R`, `dataAnalysis_exp3.R` -- analyzing data from different experiments
* `dataAnalysis_comb.Rmd` -- the main analysis based on data from Experiment 2 and 3.

#### data files

* `exp1.Rda`, `exp2.Rda`, `exp3.Rda`

#### stimulus files

* `sti_exp1`, `sti_exp2`, `sti_exp3` -- stimuli in three experiments
* `eff_exp2.Rda`, `eff_exp3.Rda` -- some stimulus information

#### modeling

* `fun_fea.R`, `fun_nor.R`, `mdall_exp1.Rda`, `mdall_exp2.Rda`, `mdall_exp3.Rda`  -- normative and summary-statistic models, and their predictions for stimuli in Experiment 1-3
* `sim_fea.R`, `sim_fea`, `df.expect.Rda` -- generating simlations for summary-statistic models
* `fun_softmax.R` -- softmax functions
* `md_all_exp1.R`, `md_all_exp2.R`, `md_all_exp3.R`, `modelFit_exp1.Rda`, `modelFit_exp2.Rda`, `modelFit_exp3.Rda`-- aggregate model fits and their results
* `softmax_exp1.R`, `softmax_exp2.R`, `softmax_exp3.R`, `softmax_exp1`, `softmax_exp2`, `softmax_exp3`. `softmax_report.R` -- individual fits and their results 
* `softmax_exp2_cond.R`, `softmax_exp3_cond.R`, `softmax_exp3_gt.R`, `softmax_exp3_ngt.R`  -- other fitting results seperate by conditions or with or without ground truths. 

## procedure

HTML for Experiment 1-3.