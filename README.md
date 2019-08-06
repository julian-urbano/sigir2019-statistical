This repository contains the data and source code for the following paper:

* J. Urbano, H. Lima and A. Hanjalic, "[Statistical Significance Testing in Information Retrieval: An Empirical Analysis of Type I, Type II and Type III Errors](http://julian-urbano.info/files/publications/076-statistical-significance-testing-information-retrieval-empirical-analysis-type-i-type-ii-type-iii-errors.pdf)", *International ACM SIGIR Conference on Research and Development in Information Retrieval*, 2019.

A [single ZIP file](https://github.com/julian-urbano/sigir2019-statistical/archive/master.zip) can be downloaded as well.

## Project Structure

* `data/` Input data files.
* `output/` Generated output files.
* `R/` Source code in R.
* `scratch/` Temporary files generated in the process.

All code is written for [R](https://www.r-project.org). You will need the following packages installed from CRAN: `rio`, `simIReff`, `VineCopula`, `stringr`, `doParallel` and `Rcpp`.

## How to reproduce the results in the paper 

The source files in `R/` need to be run in order. You can run each file individually by running `Rscript R/<file>.R`. They will store intermediate data in `scratch/` and the final data in `output/`.

**It is important that you always run from the base directory**.

1. `R/01-margins.R` fits all marginal distributions.
2. `R/02-margins_transform.R` transforms distributions of experimental runs to a certain expected value.
3. `R/03-bicops.R` fits all bivariate copulas.
4. `R/11-type_1.R` computes all p-values under the null hypothesis. Type I error rates at various alpha levels are stored in `output/type_1/`.
5. `R/12-type_2.R` computes all p-values under the alternative hypothesis with different effect sizes. Power at various alpha levels are stored in `output/type_2_by_alpha/`, and at various effect sizes in `output/type_2_by_delta`.
6. `R/13-type_3.R` computes Type III error rates. Rates at various alpha levels are stored in `output/type_3_by_alpha/`, and at various effect sizes in `output/type_3_by_delta`.
7. `R/99-paper.R` generates all figures and stores them in `output/`.

It takes months to run all the code, so it is ready to run in parallel. Most of the above code parallelizes using function `foreach` in R's package [`doParallel`](https://cran.r-project.org/web/packages/doParallel/index.html). In particular, it will use all available cores in the machine. Edit file `R/common.R` to modify this behavior and other parameters.

## Custom test collections, topic set sizes, significance levels or effect sizes

You can easily run the code with your data or parameters. For a different test collection, add the matrix of topic-by-system scores in `data/` using the name `<collection>_<measure>.csv` (see for instance file [`data/adhoc8_ap.csv`](/data/adhoc8_ap.csv)). Then, edit file `R/common.R` to add the new data. Other parameters may also be changed from there:

```r
.COLLECTIONS <- c("adhoc5", "adhoc6", "adhoc7", "adhoc8",
                  "web2010", "web2011", "web2012", "web2013")
.MEASURES <- c("ap", "p10", "rr", "ndcg20", "err20")

.N_TOPICS <- c(25, 50, 100) # topic set sizes
.DELTAS <- seq(.01, .1, .01) # effect sizes
.ALPHAS <- c(1:9*.001, 1:9*.01, .1) # significance levels
```

For more specific modifications, edit the corresponding source file in `R/` (see above). Note also that the script `R/99-paper.R` is only intended to generate the figures in the paper. If you customize something and want a similar analysis, you will need to extend this script yourself.

## Test implementations

We also provide an implementation of all five tests and, in particular, C++ implementations (file `R/test.cpp`) and R wrappers (file `R/ir_tests.R`) for the bootstrap and permutation tests. The easiest way to use them is as follows:

```r
> # import tests
> source("R/ir_tests.R")
> 
> # as example, use systems 125 and 126 as baseline and experimental
> dat <- rio::import("data/adhoc8_ap.csv")
> baseline <- dat[,125]
> experimental <- dat[,126]
> 
> baseline
[1] 0.0075 0.0712 0.7546 0.1367 0.0548 ...
> experimental
 [1] 0.0258 0.2125 0.7863 0.1649 0.0797 ...
>
> test_t(baseline, experimental)
[1] 0.0006596986 0.0013193972
> test_wilcoxon(baseline, experimental)
[1] 0.001722594 0.003445188
> test_sign(baseline, experimental)
[1] 0.004779939 0.009559879
> test_bootstrap(baseline, experimental, 1e6)
[1] 0.000424 0.000586
> test_permutation(baseline, experimental, 1e6)
[1] 0.000603 0.001188
```

Every test's function returns two p-values: 1-tailed and 2-tailed.

## All plots and error rates

Due to space restrictions, in the paper we only report a selection of plots. From this repository you may find all plots and data:

* `output/type_1/` contains all data and plots of Type I errors by alpha level (like Figures 2 and 3 of the paper).
* `output/type_2_by_delta/` contains all data and plots of power (Type II errors) by effect size delta (Figure 4).
* `output/type_2_by_alpha/` contains all data and plots of power (Type II errors) by significance level alpha (Figure 5).
* `output/type_3_by_delta/` contains all data and plots of Type III errors by effect size delta (Figure 6).
* `output/type_3_by_alpha/` contains all data and plots of Type III errors by significance level alpha (not in the paper).

## License

* The TREC results in `data/` are anonymized and posted here with permission from the organizers.
* Databases and their contents are distributed under the terms of the [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).
* Software is distributed under the terms of the [MIT License](https://opensource.org/licenses/MIT).

When using this archive, please [cite](CITE.bib) the above paper:

    @inproceedings{urbano2019statistical,
      author = {Urbano, Juli\'{a}n and Lima, Harlley and Hanjalic, Alan},
      booktitle = {International ACM SIGIR Conference on Research and Development in Information Retrieval},
      title = {{Statistical Significance Testing in Information Retrieval: An Empirical Analysis of Type I, Type II and Type III Errors}},
      year = {2019},
	  pages = {505--514}
    }
