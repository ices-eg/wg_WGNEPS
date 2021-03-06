# WGNEPS

## Introduction

This is the code repository for [The Working Group on Nephrops Surveys (WGNEPS)](https://www.ices.dk/community/groups/Pages/WGNEPS.aspx), which is the international coordination group for _Nephrops_ UWTV and trawl surveys within ICES. For underwater television surveys (UWTV), this group builds on the work of WKNEPHTV 2007, WKNEPHBID 2010, SGNEPS 2009, 2010 & 2012, WKNEPH 2009 and WKNEPS 2016 & 2018.

This repository includes input files, R scripts and additional functions needed to run the code.
This repository does not include output files, as stated in _.gitignore_. However, output files could be uploaded in the future.

## Contents of this repository

There are four main folders in this repo (A, B, C, D)

* Agreed common code for all the institutes:

  * [A - Code for developing a reference set (WKNEPS 2018)](https://github.com/ices-eg/wg_WGNEPS/tree/master/A_Developing_a_reference_set)

* All international institutes are invited to share their code under the following folders:

  * [B - Code for running Lin's CCC test: pre-survey, training of Counters vs. Reference set counts](https://github.com/ices-eg/wg_WGNEPS/tree/master/B_Lins_training_pre_survey)
    * Marine Institute (Ireland)

  * [C - Code for running Lin's CCC test: during survey, to compare Counters vs. Counters](https://github.com/ices-eg/wg_WGNEPS/tree/master/C_Lins_during_survey)
    * Marine Institute (Ireland)
    
  * [D - Code for plotting survey data](https://github.com/ices-eg/wg_WGNEPS/tree/master/D_Survey_plots)
    * Marine Institute (Ireland)


## Details

### A - Code for developing a reference set (WKNEPS 2018)
The code follows this flowchart decision tree created in WKNEPS 2018:

<img src="A_Developing_a_reference_set/repo_images/0A_Reference_set_Flow_chart_decision_tree.png" width="500">
