## main simulations
qsub -N and_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp and
qsub -N or_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp or
qsub -N additive_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp additive
qsub -N and_cov_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp and_cov
qsub -N or_cov_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp or_cov
qsub -N additive_cov_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp additive_cov
qsub -N simple_subgroup submit_job.sh 01_main_subgroup_simulations --nreps 500 --save --dgp simple

## simulations with larger sample size (n = 1000)
qsub -N and_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp and
qsub -N or_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp or
qsub -N additive_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp additive
qsub -N and_cov_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp and_cov
qsub -N or_cov_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp or_cov
qsub -N additive_cov_subgroup_large_n submit_job.sh 01_main_subgroup_simulations --nreps 500 --nsamples 1000 --save --dgp additive_cov

## simulations with varying n
qsub -N and_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp and
qsub -N or_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp or
qsub -N additive_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp additive
qsub -N and_cov_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp and_cov
qsub -N or_cov_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp or_cov
qsub -N additive_cov_subgroup_n submit_job.sh 02_vary_n --nreps 500 --save --dgp additive_cov

## simulations with varying correlation in X
qsub -N and_cor_cov_subgroup submit_job.sh 03_vary_correlation --nreps 100 --save --dgp and_cor_cov
qsub -N or_cor_cov_subgroup submit_job.sh 03_vary_correlation --nreps 100 --save --dgp or_cor_cov
qsub -N additive_cor_cov_subgroup submit_job.sh 03_vary_correlation --nreps 100 --save --dgp additive_cor_cov

## simulations with rulefit student model
qsub -N and_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp and
qsub -N or_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp or
qsub -N additive_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp additive
qsub -N and_cov_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp and_cov
qsub -N or_cov_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp or_cov
qsub -N additive_cov_rulefit submit_job.sh 04_rulefit_simulations --nreps 100 --save --dgp additive_cov

## causal tree simulations with varying cps
qsub -N and_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp and
qsub -N or_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp or
qsub -N additive_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp additive
qsub -N and_cov_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp and_cov
qsub -N or_cov_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp or_cov
qsub -N additive_cov_causal_tree_cp submit_job_cparray.sh 05_causal_tree_cp --nreps 500 --save --dgp additive_cov

## simulations with varying crossfit reps
qsub -N and_crossfit submit_job.sh 06_vary_crossfit --nreps 100 --save --dgp and
qsub -N or_crossfit submit_job.sh 06_vary_crossfit --nreps 100 --save --dgp or
qsub -N additive_crossfit submit_job.sh 06_vary_crossfit --nreps 100 --save --dgp additive

## aids case study
qsub -N aids submit_job.sh 07_case_study_aids --nreps 500 --save

## timing simulations
qsub -N timing submit_job.sh 08_timing_simulations --nreps 500 --save

## jaccard toy example
qsub -N jaccard_illustration -pe smp 1 submit_job.sh 09_jaccard_illustration

## render simChef document
qsub -N render_docs -pe smp 1 submit_job.sh 00_render_docs.sh
