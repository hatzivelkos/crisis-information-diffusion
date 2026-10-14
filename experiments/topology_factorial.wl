(* ========================================================================= *)
(* Factorial topology production configuration                               *)
(*                                                                           *)
(* Production configuration for the full factorial variation of ordinary-    *)
(* agent connectivity kA and media-connectivity multiplier mB.               *)
(*                                                                           *)
(* The factorial analysis combines the three original topology-regime        *)
(* experiments with six additional off-diagonal (kA,mB) configurations.      *)
(* Twenty independently generated networks are used for each factorial cell. *)
(* Each network is simulated for tau = 0, 5, and 10, with 20 replications    *)
(* per network and response delay.                                           *)
(*                                                                           *)
(* This file documents the production settings; it is not a simulation       *)
(* runner.                                                                   *)
(* ========================================================================= *)

topologyFactorialConfig = <|

    "FactorialGrid" -> <|
        "k" -> {4, 6, 8},
        "MediaDegreeMultiplier" -> {3, 4, 5}
    |>,

    "ManuscriptNotation" -> <|
        "k" -> "k_A",
        "MediaDegreeMultiplier" -> "m_B"
    |>,

    "DiffusionParameters" -> <|
        "c" -> 0.45,
        "d" -> 0.35,
        "tau" -> {0, 5, 10}
    |>,

    "Simulation" -> <|
        "Tmax" -> 200,
        "NetworkRealizationsPerCell" -> 20,
        "ReplicationsPerNetworkPerTau" -> 20,
        "muClearance" -> 0.05,
        "StoreSignals" -> False,
        "StoreUpdates" -> False,
        "ResampleSourcesPerRun" -> False
    |>,

    "CommonNetworkSpec" -> <|
        "NetworkType" -> "SmallWorld",
        "n" -> 100,
        "nMedia" -> 4,
        "rewiringProbability" -> 0.1,
        "MediaAssignmentMethod" -> "TopDegree",
        "BoostMediaConnectivity" -> True
    |>,

    "SeedScheme" -> <|

        (* Same seed indices are used across all factorial cells. *)

        "NetworkSeeds" ->
            123456 + Range[0, 19],

        "MediaBoostSeeds" ->
            1123456 + Range[0, 19],

        "SourceSeeds" ->
            98765 + Range[0, 19],

        "SimulationSeeds" ->
            555000 + 1000 Range[0, 19],

        "NetworkSeedMatchedAcrossCells" -> True,
        "SourceSeedMatchedAcrossCells" -> True,
        "SimulationSeedMatchedAcrossCells" -> True

    |>,

    "SourceSpec" -> <|
        "nMisinformationSources" -> 4,
        "MisinformationSourceMethod" -> "Random",
        "SourceSetHeldFixedAcrossTauWithinNetwork" -> True
    |>,

    "ProductionFolders" -> <|

        "k4m3" ->
            "topology_sparse_networks20_R20_T200",

        "k4m4" ->
            "topology_k4m4_networks20_R20_T200",

        "k4m5" ->
            "topology_k4m5_networks20_R20_T200",

        "k6m3" ->
            "topology_k6m3_networks20_R20_T200",

        "k6m4" ->
            "topology_normal_networks20_R20_T200",

        "k6m5" ->
            "topology_k6m5_networks20_R20_T200",

        "k8m3" ->
            "topology_k8m3_networks20_R20_T200",

        "k8m4" ->
            "topology_k8m4_networks20_R20_T200",

        "k8m5" ->
            "topology_dense_networks20_R20_T200"

    |>,

    "OriginalTopologyCells" -> {
        "k4m3",
        "k6m4",
        "k8m5"
    },

    "AdditionalFactorialCells" -> {
        "k4m4",
        "k4m5",
        "k6m3",
        "k6m5",
        "k8m3",
        "k8m4"
    },

    "UpdateRuleModule" ->
        "src/05_UpdateRules.wl",

    "NumberOfFactorialCells" -> 9,

    "NumberOfNetworkRealizations" ->
        9 * 20,

    "NumberOfNetworkDelayConfigurations" ->
        9 * 20 * 3,

    "TotalSimulationRuns" ->
        9 * 20 * 3 * 20,

    "NewSimulationRunsForFactorialExtension" ->
        6 * 20 * 3 * 20,

    "ReusedSimulationRunsFromTopologyRobustness" ->
        3 * 20 * 3 * 20

|>;