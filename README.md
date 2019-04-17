# **`netpower`**
**Sample Size Recommendations for Estimating Cross-Sectional Network Models.** 

---

## ⚠️ Notice:
The version on this branch is currently under heavy development and many things will change.

## Checklist:
- ✅ generate graphs
- ✅ generate parameters
- ✅ generate data
- ✅ estimate models
- ⚠️ compare generated & estimated models 
- ❌ expand combinations of simulation factors into a data frame
- ❌ run and replicate combinations of design factors
- ❌ connect and store the results to a MySQL database
- ❌ post the results to an API endpoint using encrypted pre-generated tokens

---

## Installation
- the package can be installed as `devtools::install_github('mihaiconstantin/netPower')`


## Description
The data used in the study can be loaded via `data("netPowerData")`.
The code block below illustrates the main four functions found in this package. 


#### 1. build_design(participants, nodes, architectures, connectedness, models)
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


#### 2. run_cell(participants, nodes, density, architecture)
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

![Plot for `run_cell` result object.](https://constantinmihai.com/)

- the output contains a list containing two sub-lists: `$raw`, `$computed`:
    - `$raw` contains the true and estimated model parameters and the data
    - `$computed` contains the outcome measures of interests


#### 3. run_cells(cells)
- applies the simulation procedure on a selected number of cells (i.e., it is a wrapper around `run_cell`)

```r
result_cells = run_cells(design[c(1, 97, 165), ])
# -> Running simulation for 3 cells:
#   -> config: 50 par | 10 nod | 1 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 100 par | 30 nod | 2 arc | 1 con | 1 mod. Cell done. ✓
#   -> config: 200 par | 10 nod | 1 arc | 2 con | 1 mod. Cell done. ✓
# -> Completed all 3 cells.
```


#### 4. replicate_cells(cells, replications)
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
