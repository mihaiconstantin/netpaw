# netTinker
**Sample size recommendations for estimating network models.** 


## Work in progress
- things to implement:
    - `ising_scale_free()`
    - `ising_small_world()`


## Installation
- the package can be installed as `devtools::install_github('mihaiconstantin/netTinker')`
- it depends on:
    - [`qgraph`](https://github.com/SachaEpskamp/qgraph)
    - [`IsingFit`](https://github.com/cvborkulo/IsingFit)
    - [`IsingSampler`](https://github.com/SachaEpskamp/IsingSampler)
    - [`bootnet`](https://github.com/SachaEpskamp/bootnet)


## Description
The code block below illustrates the main four functions found in this package. 

#### 1. build_design(participants, nodes, densities, architectures)
- builds the simulation design  

```r
# Specifying the factors.
participants    = c(50, 200, 500, 1000)
nodes           = c(10, 20, 30)
densities       = c(.1, .2, .5, .8)
architectures   = c(random = 1, small_world = 2, scale_free = 3, empirical = 4)
    
# Building the factorial design.
design = build_design(participants, 
                      nodes, 
                      densities, 
                      architectures)
             
> head(design)
     participants nodes density architecture
[1,]           50    10     0.1            1
[2,]           50    10     0.1            2
[3,]           50    10     0.1            3
[4,]           50    10     0.1            4
[5,]           50    10     0.2            1
[6,]           50    10     0.2            2
```

- **note**: ***currently the simulation cannot be performed for architectures 2 and 3 (i.e., small world and scale-free)***. For now, please run configurations or design cells that do not include values of 2 and 3 for the architecture factor.


#### 2. run_cell(participants, nodes, density, architecture)
- applies the simulation procedure on a single design cell (i.e., referred to as row in the `design` matrix above)

```r
result_cell = run_cell(participants = 500,
                       nodes        = 15, 
                       density      = .2,
                       architecture = 1)
```

- outputs a list containing three sub-lists: `$config`, `$true`, and `$estimated`:
    - `$config` contains a vector with the cell configuration
    - `$graph` contains a matrix representing the network structure
    - `$thresholds` holds a vector representing the model thresholds


#### 3. run_cells(cells)
- applies the simulation procedure on a selected number of cells (i.e., it is a wrapper around `run_cell`)

```r
result_cells = run_cells(design[c(1, 97, 165), ])
```

- outputs a list containing the output of `run_cell` for each selected cell:
    - `result_cells`
        - `[[1]]`
            - `$config`
            - `$graph`
            - `$thresholds`
        - `[[2]]`
            - `$config`
            - `$graph`
            - `$thresholds`
        - ...


#### 4. run_cells_with_replication(cells, replications)
- applies the simulation procedure on a selected number of cell and replicates the procedure `n` number of times (i.e., it is a wrapper around `run_cells`)

```r
run_cells_with_replication = run_cells_with_replication(design[c(1, 97, 165), ], 10)
```

- outputs a list containing a list indicating the replication number. Each replication list holds the same output as `run_cells`:
    - `run_cells_with_replication`
        - `[[1]]` *(replication 1)*
            - `result_cells` *(output)*
        - `[[2]]` (replication 2)
            - `result_cells` *(output)*
        - ...

---

***Note:*** *For more details please check the documentation of each function.*
