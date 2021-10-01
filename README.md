<h1>
  This repository has been moved and it is no longer maintained.
  <sub>Please read below.</sub>
</h1>

This repository is part of the research master thesis titled ***Sample Size Recommendations for Estimating Cross-Sectional Network Models*** where we performed simulations for several network models to provide sample size recommendations. The primary purpose of this repository is to allow researchers to replicate the results presented in the thesis (i.e., using the repository version at commit [9ca8392](https://github.com/mihaiconstantin/netpaw/tree/9ca839210efe107bc8ccff38bc383d1d59e0b351)). The thesis and the simulation scripts are freely available on OSF at [osf.io/zkaxu](https://osf.io/zkaxu). 

**If you are interested in sample size analysis for psychological networks**, you are cordially invited to take a look at following paper:

- Constantin, M. A., Schuurman, N. K., & Vermunt, J. (2021). A General Monte Carlo Method for Sample Size Analysis in the Context of Network Models. *PsyArXiv*. https://doi.org/10.31234/osf.io/j5v7u

In this paper, which we submitted for publication, we introduce a general method to perform sample size calculations for psychological networks.
To facilitate researchers to conduct such sample size calculations we provide an `R` package called `powerly` available both on GitHub at [mihaiconstantin/powerly](https://github.com/mihaiconstantin/powerly) and [CRAN](https://www.r-pkg.org/pkg/powerly). The package `powerly` is stable and under active development to support new models. To request a new model, performance measure, or statistic, please open an issue at [github.com/mihaiconstantin/powerly/issues](https://github.com/mihaiconstantin/powerly/issues).

---

## netPower
**Sample Size Recommendations for Estimating Cross-Sectional Network Models.** 

<!-- Badges. -->
<a href="https://www.repostatus.org/#moved"><img src="https://www.repostatus.org/badges/latest/moved.svg" alt="Repository status"/></a>

### Installation
- the package can be installed as `devtools::install_github('mihaiconstantin/netPower')`


### Description
The data used in the study can be loaded via `data("netPowerData")`.
The code block below illustrates the main four functions found in this package. 


##### 1. build_design(participants, nodes, architectures, connectedness, models)
- builds the simulation design  

```r
# Specifying the factors.
participants    = seq(50, 1000, 50)
nodes           = c(10, 20, 30)
architectures   = c(random = 1, small_world = 2, scale_free = 3)
connectedness   = c(low = 1, medium = 2, large = 3)
models          = c(ising = 1, ggm = 2)

# Building the design.
design = build_design(participants, 
                      nodes, 
                      architectures, 
                      connectedness,
                      models)
             
head(design)
#   participants nodes architectures connectedness models
# 1           50    10             1             1      1
# 2           50    10             1             1      2
# 3           50    10             1             2      1
# 4           50    10             1             2      2
# 5           50    10             1             3      1
# 6           50    10             1             3      2
```


##### 2. run_cell(participants, nodes, density, architecture)
- applies the simulation procedure on a single design cell (i.e., referred to as row in the `design` matrix above)

```r
result_cell = run_cell(participants  = 300,
                       nodes         = 10, 
                       architecture  = 2,
                       connectedness = 3,
                       model         = 2)

result_cell
#  Simulation cell results:  
#       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#    -> config: 300 participants | 10 nodes | small world architecture | 3 connectedness | ggm model. 
#       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#    -> sensitivity: 0.85 
#    -> specificity: 0.88 
#    -> type one error: 0.12 
#    -> type two error: 0.15 
#    -> edge correlation: 0.9335286 
#    -> equal # nodes: yes 
# Try plot(result) for a visual inspection.

plot(result_cell)
```

![Plot for `run_cell` result object.](https://constantinmihai.com/tmp/run_cell_plot.png)

- the output contains a list containing two sub-lists: `$raw`, `$computed`:
    - `$raw` contains the true and estimated model parameters and the data
    - `$computed` contains the outcome measures of interests


##### 3. run_cells(cells)
- applies the simulation procedure on a selected number of cells (i.e., it is a wrapper around `run_cell`)

```r
result_cells = run_cells(design[c(1, 97, 165), ])
# -> Running simulation for 3 cells:
#   -> config: 50 par | 10 nod | 1 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 100 par | 30 nod | 2 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 200 par | 10 nod | 1 arc | 2 con | 1 mod. Cell done. ✓
# -> Completed all 3 cells.
```


##### 4. replicate_cells(cells, replications)
- applies the simulation procedure on a selected number of cell and replicates the procedure `n` number of times (i.e., it is a wrapper around `run_cells`)

```r
replicated cells = replicate_cells(design[c(1, 97, 165), ], 2)
# Design replications requested: 2.

# ------------------------------
# Replication: 1.
# ------------------------------
# -> Running simulation for 3 cells:
#   -> config: 50 par | 10 nod | 1 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 100 par | 30 nod | 2 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 200 par | 10 nod | 1 arc | 2 con | 1 mod. Cell done. ✓
# -> Completed all 3 cells.

# ------------------------------
# Replication: 2.
# ------------------------------
# -> Running simulation for 3 cells:
#   -> config: 50 par | 10 nod | 1 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 100 par | 30 nod | 2 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 200 par | 10 nod | 1 arc | 2 con | 1 mod. Cell done. ✓
# -> Completed all 3 cells.

# Completed all 2 replications.
```

---

***Note:*** *For more details please check the documentation of each function (i.e., work in progress).*
