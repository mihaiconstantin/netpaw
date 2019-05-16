# Simulation outline (building blocks) ------------------------------------

# Generating graphs.
graph <- gen.graph("smallworld", 15, neighborhood = 2, p.rewire = .3)


# Generating models.
model.ggm <- gen.model("ggm", "random", 10, p = .4, positive.edge.ratio = 1)

# Print the model.
model.ggm

# Plot the model.
plot(model.ggm)

model.ising <- gen.model("ising", "random", 10, p = .4, positive.edge.ratio = .8)

# Print the model.
model.ising

# Plot the model.
plot(model.ising)


# Generating data.
data.ggm <- gen.data(1000, model.ggm, levels = 5)

# Print data information
data.ggm


# When it fails to generate data.
data.ising <- gen.data(5, model.ising)

# Print data information
data.ising


# Fit the model.
fit <- estimate.model(data.ggm)

# Print the fit.
fit

# Plot the fit.
plot(fit)


# Compare true and fit.
outcomes <- extract.results(model.ggm, fit)

# Print outcomes (work in progress!)
outcomes


# Simulation design specification -----------------------------------------

# Advanced users.
spec <- list(
    sample.size = c(50, 100, 500),
    node = c(10, 15, 20),
    graph = c("random", "smallworld"),
    graph.options = list(
        random = list(
            p = c(.3, .5)
        ),
        smallworld = list(
            p = c(.1, .2),
            neighborhood = 2
        )
    ),
    positive.edge.ratio = c(.3, .5),
    model = c("ggm", "ising"),
    model.options = list(
        ggm = list(
            constant = 1.5,
            min = .5,
            max = 1
        ),
        ising = list(
            mean = 0,
            sd = 1
        )
    ),
    replication = 100
)

design.a <- build.design(spec)

# Print the design.
design.a


# R enthusiasts

# Without confirmation.
design <- build.design(with.confirmation = FALSE)


# With confirmation.
design <- build.design(with.confirmation = TRUE)


# The design matrix.
View(design$matrix)


# Running a single cell ---------------------------------------------------

x = run.cell(50, "ggm", "random", 5, p = .3, positive.edge.ratio = .5)

# Information contained.
x$config
x$true.model
x$fit
x$data
x$outcomes


# Things left to do:

# INTERNAL ONLY! Running multiple cells ---------------------------------------------------


# INTERNAL ONLY! IN PROGRESS. Running a single cell ---------------------------------------------------

x = run.cells(design)


# INTERNAL ONLY! IN PROGRESS. Running multiple cells with replication ---------------------------------------------------

x = replicate.cells(design)

# Store in database.
# - each row of the design matrix
# - its associated outcomes
