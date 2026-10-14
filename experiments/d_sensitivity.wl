(* ========================================================================= *)
(* Misinformation-strength sensitivity production configuration               *)
(*                                                                           *)
(* Exact parameter grid used for the d-sensitivity experiment.               *)
(* The benchmark network and misinformation source set are held fixed across *)
(* all parameter configurations. This file documents the production settings.*)
(* ========================================================================= *)

dSensitivityConfig = <|

    "ParameterGrid" -> <|
        "d" -> {0.25, 0.35, 0.45},
        "c" -> 0.45,
        "tau" -> 5
    |>,

    "Simulation" -> <|
        "Tmax" -> 300,
        "R" -> 100,
        "SimulationSeed" -> 555,
        "muClearance" -> 0.05,
        "StoreSignals" -> False,
        "StoreUpdates" -> False,
        "ResampleSourcesPerRun" -> False
    |>,

    "NetworkSpec" -> <|
        "NetworkType" -> "SmallWorld",
        "n" -> 100,
        "nMedia" -> 4,
        "k" -> 6,
        "rewiringProbability" -> 0.1,
        "MediaAssignmentMethod" -> "TopDegree",
        "BoostMediaConnectivity" -> True,
        "MediaDegreeMultiplier" -> 4,
        "Seed" -> 123456,
        "MediaBoostSeed" -> 222
    |>,

    "SourceSpec" -> <|
        "nMisinformationSources" -> 4,
        "MisinformationSourceMethod" -> "Random",
        "Seed" -> 98765
    |>,

    "UpdateRuleModule" ->
        "src/05_UpdateRules.wl",

    (* Reproduction checks for the fixed benchmark network and source set. *)

    "NetworkSummary" -> <|
        "VertexCount" -> 100,
        "EdgeCount" -> 364,
        "Density" -> 0.07353535353535354,
        "MeanDegree" -> 7.28,
        "MaxDegree" -> 24,
        "AgentCount" -> 96,
        "MediaNodeCount" -> 4,
        "MeanAgentDegree" -> 6.583333333333333,
        "MeanMediaDegree" -> 24.0,
        "MinMediaDegree" -> 24,
        "MaxMediaDegree" -> 24,
        "ConnectedGraphQ" -> True
    |>,

    "SourceSetCheck" -> <|
        "MisinformationSourceCount" -> 4,
        "SM" -> {100, 87, 13, 17}
    |>,

    "NumberOfConfigurations" -> 3

|>;