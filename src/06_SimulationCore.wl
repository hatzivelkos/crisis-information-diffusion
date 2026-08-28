(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`Networks`"],
   Get[FileNameJoin[{$SrcDir, "02_Networks.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`Initialization`"],
   Get[FileNameJoin[{$SrcDir, "03_Initialization.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`Signals`"],
   Get[FileNameJoin[{$SrcDir, "04_Signals.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`UpdateRules`"],
   Get[FileNameJoin[{$SrcDir, "05_UpdateRules.wl"}]]
];


BeginPackage[
   "Dezinformacije`SimulationCore`",
   {
      "Dezinformacije`TypesAndValidation`",
      "Dezinformacije`Networks`",
      "Dezinformacije`Initialization`",
      "Dezinformacije`Signals`",
      "Dezinformacije`UpdateRules`"
   }
];


MakeSimulationState::usage =
   "MakeSimulationState[runID, networkData, initialCondition] builds the t = 0 simulation state.";

StateLongTable::usage =
   "StateLongTable[simState] converts a simulation state to long-table rows.";

RunOneStep::usage =
   "RunOneStep[simState, params] runs one full simulation step.";

RunSingleSimulation::usage =
   "RunSingleSimulation[networkData, initialCondition, params, runID] runs one complete simulation.";

RunReplications::usage =
   "RunReplications[networkData, sourceOrInitialSpec, params] runs R replications.";

StateCounts::usage =
   "StateCounts[simState] returns counts of U, M, I in one simulation state.";

StateCountsFromHistory::usage =
   "StateCountsFromHistory[history] returns state counts by time from a long-format history.";

SimulationSummary::usage =
   "SimulationSummary[result] returns a compact summary of one simulation result.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Simulation-state construction                                          *)
(* ---------------------------------------------------------------------- *)

MakeSimulationState[
   runID_Integer,
   networkData_Association,
   initialCondition_Association
] :=
   Module[
      {
         simState,
         validation
      },


      If[
         ! KeyExistsQ[initialCondition, "InitialStates"] ||
         ! KeyExistsQ[initialCondition, "InitialDurationsA"] ||
         ! KeyExistsQ[initialCondition, "InitialDurationsB"] ||
         ! KeyExistsQ[initialCondition, "SM"],

         Return[
            ValidationFailure[
               "InvalidInitialCondition",
               {
                  "Initial condition must contain SM, InitialStates, InitialDurationsA, and InitialDurationsB."
               }
            ]
         ]
      ];


      simState =
         <|
            "RunID" ->
               runID,

            "t" ->
               0,

            "Graph" ->
               networkData["Graph"],

            "NodeTypes" ->
               networkData["NodeTypes"],

            "States" ->
               initialCondition["InitialStates"],

            "DurationsA" ->
               initialCondition["InitialDurationsA"],

            "DurationsB" ->
               initialCondition["InitialDurationsB"],

            "MisinformationSources" ->
               initialCondition["SM"]
         |>;


      validation =
         ValidateSimulationState[
            KeyTake[
               simState,
               {
                  "t",
                  "States",
                  "DurationsA",
                  "DurationsB"
               }
            ]
         ];


      If[
         ! ValidationSucceededQ[validation],

         validation,

         simState
      ]
   ];


MakeSimulationState[
   runID_,
   networkData_,
   initialCondition_
] :=
   ValidationFailure[
      "InvalidSimulationStateInput",
      {
         "MakeSimulationState expects runID, networkData, and initialCondition."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Long-format state table                                                *)
(* ---------------------------------------------------------------------- *)

StateLongTable[
   simState_Association
] :=
   Module[
      {
         graph,
         vertices,
         nodeTypes,
         states,
         durationsA,
         durationsB,
         runID,
         t,
         misinformationSources
      },


      graph =
         simState["Graph"];

      vertices =
         VertexList[graph];

      nodeTypes =
         simState["NodeTypes"];

      states =
         simState["States"];

      durationsA =
         simState["DurationsA"];

      durationsB =
         simState["DurationsB"];

      runID =
         simState["RunID"];

      t =
         simState["t"];

      misinformationSources =
         Lookup[
            simState,
            "MisinformationSources",
            {}
         ];


      If[
         ! SameSetQList[
            vertices,
            Keys[states]
         ],

         Return[
            ValidationFailure[
               "InvalidStateLongTableInput",
               {
                  "States must contain exactly all graph vertices."
               }
            ]
         ]
      ];


      Table[
         <|
            "runID" ->
               runID,

            "t" ->
               t,

            "nodeID" ->
               v,

            "nodeType" ->
               CanonicalNodeType[
                  nodeTypes[v]
               ],

            "state" ->
               states[v],

            "av" ->
               durationsA[v],

            "bv" ->
               durationsB[v],

            "isSM" ->
               MemberQ[
                  misinformationSources,
                  v
               ]
         |>,
         {v, vertices}
      ]
   ];


StateLongTable[simState_] :=
   ValidationFailure[
      "InvalidStateLongTableInput",
      {
         "StateLongTable expects a simulation-state Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* One-step update                                                        *)
(* ---------------------------------------------------------------------- *)

RunOneStep[
   simState_Association,
   params_Association
] :=
   Module[
      {
         validation,
         signalState,
         updated,
         newSimState
      },


      validation =
         ValidateParameters[
            params
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      validation =
         ValidateSimulationState[
            KeyTake[
               simState,
               {
                  "t",
                  "States",
                  "DurationsA",
                  "DurationsB"
               }
            ]
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      (* Signals are generated from the states at the current time t. *)

      signalState =
         BuildSignalState[
            simState
         ];

      If[
         Head[signalState] === Failure,
         Return[signalState]
      ];


      (* All node updates are simultaneous.
         Media-node campaign activation is handled internally
         by UpdateRules from the node type and parameter tau. *)

      updated =
         UpdateStatesAndDurations[
            simState,
            signalState,
            params
         ];

      If[
         Head[updated] === Failure,
         Return[updated]
      ];


      newSimState =
         <|
            "RunID" ->
               simState["RunID"],

            "t" ->
               updated["t"],

            "Graph" ->
               simState["Graph"],

            "NodeTypes" ->
               simState["NodeTypes"],

            "States" ->
               updated["States"],

            "DurationsA" ->
               updated["DurationsA"],

            "DurationsB" ->
               updated["DurationsB"],

            "MisinformationSources" ->
               simState["MisinformationSources"]
         |>;


      <|
         "SimulationState" ->
            newSimState,

         "SignalState" ->
            signalState,

         "Updates" ->
            updated["Updates"]
      |>
   ];


RunOneStep[
   simState_,
   params_
] :=
   ValidationFailure[
      "InvalidRunOneStepInput",
      {
         "RunOneStep expects a simulation-state Association and params Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* One complete simulation                                                *)
(* ---------------------------------------------------------------------- *)

RunSingleSimulation[
   networkData_Association,
   initialCondition_Association,
   params_Association,
   runID_Integer : 1
] :=
   Module[
      {
         validation,
         simSeed,
         simState,
         step,
         history,
         signalHistory,
         updateHistory,
         tmax,
         storeSignalsQ,
         storeUpdatesQ
      },


      validation =
         ValidateParameters[
            params
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      tmax =
         params["Tmax"];


      simSeed =
         Lookup[
            params,
            "SimulationSeed",
            Lookup[
               params,
               "Seed",
               Missing["NotAvailable"]
            ]
         ];


      If[
         IntegerQ[simSeed],

         SeedRandom[
            simSeed + runID - 1
         ]
      ];


      storeSignalsQ =
         TrueQ[
            Lookup[
               params,
               "StoreSignals",
               False
            ]
         ];

      storeUpdatesQ =
         TrueQ[
            Lookup[
               params,
               "StoreUpdates",
               False
            ]
         ];


      simState =
         MakeSimulationState[
            runID,
            networkData,
            initialCondition
         ];

      If[
         Head[simState] === Failure,
         Return[simState]
      ];


      history =
         StateLongTable[
            simState
         ];

      If[
         Head[history] === Failure,
         Return[history]
      ];


      signalHistory = {};
      updateHistory = {};


      While[
         simState["t"] < tmax,

         step =
            RunOneStep[
               simState,
               params
            ];


         If[
            Head[step] === Failure,
            Return[step]
         ];


         If[
            storeSignalsQ,

            signalHistory =
               Join[
                  signalHistory,

                  SignalLongTable[
                     step["SignalState"]["Signals"],
                     simState["NodeTypes"],
                     simState["RunID"],
                     simState["t"]
                  ]
               ]
         ];


         If[
            storeUpdatesQ,

            updateHistory =
               Join[
                  updateHistory,
                  step["Updates"]
               ]
         ];


         simState =
            step["SimulationState"];


         history =
            Join[
               history,
               StateLongTable[
                  simState
               ]
            ];
      ];


      <|
         "RunID" ->
            runID,

         "History" ->
            history,

         "FinalState" ->
            simState,

         "Signals" ->
            signalHistory,

         "Updates" ->
            updateHistory,

         "NetworkData" ->
            networkData,

         "InitialCondition" ->
            initialCondition,

         "Parameters" ->
            params
      |>
   ];


RunSingleSimulation[
   networkData_,
   initialCondition_,
   params_,
   runID_: 1
] :=
   ValidationFailure[
      "InvalidRunSingleSimulationInput",
      {
         "RunSingleSimulation expects networkData, initialCondition, params, and optional runID."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Replications                                                           *)
(* ---------------------------------------------------------------------- *)

RunReplications[
   networkData_Association,
   sourceOrInitialSpec_Association,
   params_Association
] :=
   Module[
      {
         validation,
         rCount,
         resampleSourcesQ,
         initialCondition,
         sourceSpec,
         runInitialCondition,
         runSourceSpec,
         results
      },


      validation =
         ValidateParameters[
            params
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      rCount =
         params["R"];

      resampleSourcesQ =
         TrueQ[
            Lookup[
               params,
               "ResampleSourcesPerRun",
               False
            ]
         ];


      (* The second argument may already be a complete initial condition,
         or it may be a source specification from which SM is selected. *)

      If[
         KeyExistsQ[
            sourceOrInitialSpec,
            "InitialStates"
         ],

         initialCondition =
            sourceOrInitialSpec;

         sourceSpec =
            Lookup[
               initialCondition,
               "SourceSpec",
               <||>
            ],

         sourceSpec =
            sourceOrInitialSpec;

         initialCondition =
            BuildInitialCondition[
               networkData,
               sourceSpec,
               params
            ];

         If[
            Head[initialCondition] === Failure,
            Return[initialCondition]
         ];
      ];


      results =
         Table[

            runInitialCondition =
               If[
                  resampleSourcesQ &&
                  ! KeyExistsQ[
                     sourceOrInitialSpec,
                     "InitialStates"
                  ],

                  runSourceSpec =
                     Join[
                        sourceSpec,
                        <|
                           "Seed" ->
                              Lookup[
                                 sourceSpec,
                                 "Seed",
                                 100000
                              ] +
                              runID - 1
                        |>
                     ];

                  BuildInitialCondition[
                     networkData,
                     runSourceSpec,
                     params
                  ],

                  initialCondition
               ];


            If[
               Head[runInitialCondition] === Failure,
               Return[runInitialCondition]
            ];


            RunSingleSimulation[
               networkData,
               runInitialCondition,
               params,
               runID
            ],

            {runID, 1, rCount}
         ];


      If[
         AnyTrue[
            results,
            Head[#] === Failure &
         ],

         Return[
            First @
               Select[
                  results,
                  Head[#] === Failure &
               ]
         ]
      ];


      <|
         "Replications" ->
            results,

         "NetworkData" ->
            networkData,

         "Parameters" ->
            params,

         "SourceSpec" ->
            sourceSpec,

         "ResampleSourcesPerRun" ->
            resampleSourcesQ
      |>
   ];


RunReplications[
   networkData_,
   sourceOrInitialSpec_,
   params_
] :=
   ValidationFailure[
      "InvalidRunReplicationsInput",
      {
         "RunReplications expects networkData, sourceSpec or initialCondition, and params."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Compact summaries                                                      *)
(* ---------------------------------------------------------------------- *)

StateCounts[
   simState_Association
] :=
   Counts[
      Values[
         simState["States"]
      ]
   ];


StateCounts[simState_] :=
   ValidationFailure[
      "InvalidStateCountsInput",
      {
         "StateCounts expects a simulation-state Association."
      }
   ];


StateCountsFromHistory[
   history_List
] :=
   Module[
      {
         times
      },

      times =
         Sort @
            DeleteDuplicates[
               history[[All, "t"]]
            ];


      Table[
         Module[
            {
               rows,
               counts
            },

            rows =
               Select[
                  history,
                  #["t"] === t &
               ];

            counts =
               Counts[
                  rows[[All, "state"]]
               ];


            <|
               "t" ->
                  t,

               "U" ->
                  Lookup[
                     counts,
                     "U",
                     0
                  ],

               "M" ->
                  Lookup[
                     counts,
                     "M",
                     0
                  ],

               "I" ->
                  Lookup[
                     counts,
                     "I",
                     0
                  ]
            |>
         ],
         {t, times}
      ]
   ];


StateCountsFromHistory[history_] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "StateCountsFromHistory expects a list of long-format history rows."
      }
   ];


SimulationSummary[
   result_Association
] :=
   Module[
      {
         historyCounts,
         finalCounts
      },

      historyCounts =
         StateCountsFromHistory[
            result["History"]
         ];

      finalCounts =
         Last[
            historyCounts
         ];


      <|
         "RunID" ->
            result["RunID"],

         "Tmax" ->
            result["Parameters"]["Tmax"],

         "InitialCounts" ->
            First[
               historyCounts
            ],

         "FinalCounts" ->
            finalCounts,

         "PeakMCount" ->
            Max[
               historyCounts[[All, "M"]]
            ],

         "PeakICount" ->
            Max[
               historyCounts[[All, "I"]]
            ],

         "HistoryRows" ->
            Length[
               result["History"]
            ]
      |>
   ];


SimulationSummary[result_] :=
   ValidationFailure[
      "InvalidSimulationResult",
      {
         "SimulationSummary expects one simulation result Association."
      }
   ];


End[];

EndPackage[];