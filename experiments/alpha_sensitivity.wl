(* ========================================================================= *)
(* Residence-time exponent sensitivity production configuration              *)
(*                                                                           *)
(* Exact parameter grid used for the alpha-sensitivity experiment.           *)
(* The benchmark network and misinformation source set are held fixed across *)
(* all parameter configurations. The exponent alpha is applied symmetrically *)
(* to both residence-time-dependent transition strengths.                    *)
(* This file documents the production settings.                              *)
(* ========================================================================= *)

alphaSensitivityConfig = <|

    "ParameterGrid" -> <|
        "alpha" -> {0.5, 1.0, 1.5},
        "tau" -> {0, 5, 10},
        "c" -> 0.45,
        "d" -> 0.35
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
        "src/05_UpdateRules_AlphaSensitivity.wl",

    "ResidenceTimeRule" -> <|
        "MisinformationToInformation" -> "c/(a+1)^alpha",
        "InformationToMisinformation" -> "d/(b+1)^alpha"
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

    "NumberOfConfigurations" -> 9

|>;