(* ========================================================================= *)
(* Network-topology robustness production configuration                      *)
(*                                                                           *)
(* Production configuration for the robustness analysis across independently *)
(* generated sparse, benchmark, and dense network realizations.              *)
(* Twenty network realizations are used in each topology regime.             *)
(* Corresponding seed indices are paired across the three regimes.           *)
(* This file documents the production settings.                              *)
(* ========================================================================= *)

topologyRobustnessConfig = <|

    "DiffusionParameters" -> <|
        "c" -> 0.45,
        "d" -> 0.35,
        "tau" -> {0, 5, 10}
    |>,

    "Simulation" -> <|
        "Tmax" -> 200,
        "NetworkRealizationsPerScenario" -> 20,
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

    "TopologyScenarios" -> <|

        "sparse" -> <|
            "k" -> 4,
            "MediaDegreeMultiplier" -> 3,
            "ProductionFolder" ->
                "topology_sparse_networks20_R20_T200"
        |>,

        "benchmark" -> <|
            "k" -> 6,
            "MediaDegreeMultiplier" -> 4,
            "ProductionFolder" ->
                "topology_normal_networks20_R20_T200"
        |>,

        "dense" -> <|
            "k" -> 8,
            "MediaDegreeMultiplier" -> 5,
            "ProductionFolder" ->
                "topology_dense_networks20_R20_T200"
        |>

    |>,

    "SeedScheme" -> <|

        "NetworkSeeds" ->
            Range[123456, 123475],

        "MediaBoostSeeds" ->
            Range[1123456, 1123475],

        "SourceSeeds" ->
            Range[98765, 98784],

        "SimulationSeeds" ->
            555000 + 1000 Range[0, 19]

    |>,

    "SourceSpec" -> <|
        "nMisinformationSources" -> 4,
        "MisinformationSourceMethod" -> "Random",
        "SourceSetHeldFixedAcrossTau" -> True
    |>,

    "UpdateRuleModule" ->
        "src/05_UpdateRules.wl",

    "NumberOfTopologyScenarios" -> 3,
    "NumberOfNetworkDelayConfigurations" -> 180,
    "TotalSimulationRuns" -> 3600

|>;