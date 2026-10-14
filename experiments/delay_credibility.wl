(* ========================================================================= *)
(* Response-delay x official-information credibility production configuration *)
(*                                                                           *)
(* Exact parameter grid used for the delay x credibility experiment.         *)
(* The benchmark network and misinformation source set are held fixed across *)
(* all parameter configurations. This file documents the production settings.*)
(* ========================================================================= *)

delayCredibilityConfig = <|

    "ParameterGrid" -> <|
        "cValues" -> {0.30, 0.45, 0.60},
        "d" -> 0.35,
        "tauValues" -> Range[0, 10]
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

    "ScenarioIDPattern" ->
        "exp_normal_c*_d0p35_tau*_sm4_T300_R100",

    "NumberOfConfigurations" -> 33

|>;