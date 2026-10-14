(* ========================================================================= *)
(* Benchmark production configuration                                        *)
(*                                                                           *)
(* Exact configuration used for the benchmark experiment reported in the     *)
(* manuscript. This file documents the production settings.                  *)
(* ========================================================================= *)

benchmarkConfig = <|

    "ScenarioID" ->
        "exp_normal_c0p45_d0p35_tau5_sm4_T300_R100",

    "Parameters" -> <|
        "c" -> 0.45,
        "d" -> 0.35,
        "tau" -> 5,
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

    (* Reproduction checks: realized outputs. *)

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

    "InitialConditionSummary" -> <|
        "MisinformationSourceCount" -> 4,
        "InitialUCount" -> 96,
        "InitialMCount" -> 4,
        "InitialICount" -> 0,
        "SM" -> {100, 87, 13, 17}
    |>,

    "ManuscriptOutputs" -> {
        "Benchmark scenario",
        "tab:benchmark_summary",
        "fig:benchmark_sd_band"
    }

|>;