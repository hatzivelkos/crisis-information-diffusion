# Production experiment configurations

This directory documents the exact configurations used for the production
simulation experiments reported in the manuscript.

The files in this directory are configuration manifests, not standalone
simulation runners. The implementation of the simulation model is provided
in `../src/`.

Unless otherwise stated, the simulations use:

`../src/05_UpdateRules.wl`

The residence-time exponent sensitivity experiment uses:

`../src/05_UpdateRules_AlphaSensitivity.wl`

## Configuration files

### `benchmark.wl`

Benchmark scenario.

Main settings:
- `c = 0.45`
- `d = 0.35`
- `tau = 5`
- `Tmax = 300`
- `R = 100`

Uses the fixed benchmark network and misinformation source set.

### `delay_credibility.wl`

Response delay × official-information strength experiment.

Main settings:
- `c = {0.30, 0.45, 0.60}`
- `d = 0.35`
- `tau = 0,...,10`
- `Tmax = 300`
- `R = 100`

Uses the fixed benchmark network and misinformation source set.

### `alpha_sensitivity.wl`

Residence-time exponent sensitivity experiment.

Main settings:
- `alpha = {0.5, 1.0, 1.5}`
- `c = 0.45`
- `d = 0.35`
- `tau = {0, 5, 10}`
- `Tmax = 300`
- `R = 100`

Uses the fixed benchmark network and misinformation source set.

### `d_sensitivity.wl`

Misinformation-strength sensitivity experiment.

Main settings:
- `d = {0.25, 0.35, 0.45}`
- `c = 0.45`
- `tau = 5`
- `Tmax = 300`
- `R = 100`

Uses the fixed benchmark network and misinformation source set.

### `topology_robustness.wl`

Robustness analysis across independently generated network realizations.

Main settings:
- sparse, benchmark, and dense network regimes
- `tau = {0, 5, 10}`
- `c = 0.45`
- `d = 0.35`
- `Tmax = 200`
- 20 network realizations per regime
- 20 replications per network and response delay

### `topology_factorial.wl`

Full factorial variation of ordinary-agent connectivity and media reach.

Main settings:
- `k_A = {4, 6, 8}`
- `m_B = {3, 4, 5}`
- `tau = {0, 5, 10}`
- `c = 0.45`
- `d = 0.35`
- `Tmax = 200`
- 20 network realizations per factorial cell
- 20 replications per network and response delay

The factorial analysis combines the three original topology cells with six
additional off-diagonal configurations.

## Reproducibility information

Each configuration manifest records, where applicable:

- network-construction parameters;
- network, media-boost, misinformation-source, and simulation seeds;
- parameter grids;
- simulation horizon and replication counts;
- initial-source selection procedure;
- the update-rule module used;
- realized network or source-set checks for fixed-network experiments;
- production output-folder identifiers.

For the fixed-network experiments, the benchmark graph uses:

- network seed `123456`
- media-boost seed `222`
- misinformation-source seed `98765`

The realized misinformation source set is:

`{100, 87, 13, 17}`

For the network-realization experiments:

- network seeds are `123456,...,123475`
- source seeds are `98765,...,98784`

Corresponding network indices are matched across the compared topology
configurations. Within each realized network, the misinformation source set is
held fixed across the tested response delays.

## Parameter names

The configuration manifests preserve the parameter names used by the Wolfram
Language implementation wherever possible.

In the manuscript:

- `k` is denoted by `k_A`
- `MediaDegreeMultiplier` is denoted by `m_B`

The production output-folder names recorded in the manifests are identifiers
from the original simulation workflow.