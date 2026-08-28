(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`Networks`"],
   Get[FileNameJoin[{$SrcDir, "02_Networks.wl"}]]
];

BeginPackage[
   "Dezinformacije`Initialization`",
   {
      "Dezinformacije`TypesAndValidation`",
      "Dezinformacije`Networks`"
   }
];


SelectMisinformationSources::usage =
   "SelectMisinformationSources[networkData, sourceSpec] selects the initial misinformation source set SM.";

InitializeStates::usage =
   "InitializeStates[vertices, SM, mediaNodes, tau] initializes node states at t = 0. If tau = 0, all media nodes start in state I.";

InitializeDurations::usage =
   "InitializeDurations[states] initializes duration variables from initial states.";

BuildInitialCondition::usage =
   "BuildInitialCondition[networkData, sourceSpec, params] builds the complete initial condition.";

InitialConditionSummary::usage =
   "InitialConditionSummary[initialCondition] returns a compact summary of the initial condition.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Internal helpers                                                       *)
(* ---------------------------------------------------------------------- *)

TakeTopDegreeNodes[
   graph_Graph,
   candidates_List,
   k_Integer
] :=
   Module[
      {degrees},

      degrees =
         AssociationThread[
            candidates ->
               (VertexDegree[graph, #] & /@ candidates)
         ];

      Take[
         SortBy[
            candidates,
            {-degrees[#], #} &
         ],
         UpTo[k]
      ]
   ];


TakeRandomNodes[
   candidates_List,
   k_Integer
] :=
   RandomSample[
      candidates,
      Min[k, Length[candidates]]
   ];


ValidateSourceCount[
   k_,
   candidates_List,
   label_String
] :=
   Which[

      ! NonNegativeIntegerQ[k],

         ValidationFailure[
            "InvalidSourceCount",
            {
               label <>
               " must be a nonnegative integer."
            }
         ],


      k > Length[candidates],

         ValidationFailure[
            "InvalidSourceCount",
            {
               label <>
               " cannot be larger than the number of available candidate nodes. " <>
               "Requested " <>
               ToString[k] <>
               ", available " <>
               ToString[Length[candidates]] <>
               "."
            }
         ],


      True,

         True
   ];


ResolveSourceSet[
   graph_Graph,
   candidates_List,
   sourceSpec_Association,
   explicitKey_String,
   countKey_String,
   methodKey_String,
   defaultMethod_String,
   label_String
] :=
   Module[
      {
         explicitSources,
         k,
         method,
         validation
      },


      (* Explicitly specified source set *)

      If[
         KeyExistsQ[
            sourceSpec,
            explicitKey
         ],

         explicitSources =
            sourceSpec[explicitKey];

         If[
            ! ListQ[explicitSources],

            Return[
               ValidationFailure[
                  "InvalidSourceSet",
                  {
                     explicitKey <>
                     " must be a list of nodes."
                  }
               ]
            ]
         ];

         If[
            ! SubsetQList[
               explicitSources,
               candidates
            ],

            Return[
               ValidationFailure[
                  "InvalidSourceSet",
                  {
                     explicitKey <>
                     " must be a subset of the allowed candidate nodes."
                  }
               ]
            ]
         ];

         Return[
            DeleteDuplicates[
               explicitSources
            ]
         ]
      ];


      (* Source set selected by count and method *)

      k =
         Lookup[
            sourceSpec,
            countKey,
            1
         ];

      method =
         Lookup[
            sourceSpec,
            methodKey,
            defaultMethod
         ];

      validation =
         ValidateSourceCount[
            k,
            candidates,
            countKey
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      Switch[
         method,

         "Random",

            TakeRandomNodes[
               candidates,
               k
            ],


         "TopDegree",

            TakeTopDegreeNodes[
               graph,
               candidates,
               k
            ],


         "First",

            Take[
               candidates,
               UpTo[k]
            ],


         "None",

            {},


         _,

            ValidationFailure[
               "InvalidSourceSelectionMethod",
               {
                  "Unsupported source-selection method for " <>
                  label <>
                  ": " <>
                  ToString[method]
               }
            ]
      ]
   ];


(* ---------------------------------------------------------------------- *)
(* Legacy-source validation                                               *)
(* ---------------------------------------------------------------------- *)

ValidateNoDeprecatedOfficialSourceSpecification[
   sourceSpec_Association
] :=
   Module[
      {
         deprecatedKeys,
         presentKeys
      },

      deprecatedKeys = {
         "BI",
         "OfficialSources",
         "nOfficialSources",
         "OfficialSourceMethod"
      };

      presentKeys =
         Select[
            deprecatedKeys,
            KeyExistsQ[sourceSpec, #] &
         ];

      If[
         presentKeys === {},

         True,

         ValidationFailure[
            "DeprecatedOfficialSourceSpecification",
            {
               "Deprecated official-source specification detected: " <>
               StringRiffle[presentKeys, ", "] <>
               ". The model no longer selects a subset BI of media nodes. " <>
               "All media nodes B become permanent official-information spreaders when the campaign is activated."
            }
         ]
      ]
   ];


(* ---------------------------------------------------------------------- *)
(* Misinformation-source selection                                        *)
(* ---------------------------------------------------------------------- *)

SelectMisinformationSources[
   networkData_Association,
   sourceSpec_Association
] :=
   Module[
      {
         graph,
         agents
      },

      graph =
         networkData["Graph"];

      agents =
         networkData["Agents"];

      ResolveSourceSet[
         graph,
         agents,
         sourceSpec,
         "SM",
         "nMisinformationSources",
         "MisinformationSourceMethod",
         "Random",
         "SM"
      ]
   ];


SelectMisinformationSources[
   networkData_,
   sourceSpec_
] :=
   ValidationFailure[
      "InvalidSourceSpec",
      {
         "SelectMisinformationSources expects networkData and sourceSpec Associations."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* State and duration initialization                                      *)
(* ---------------------------------------------------------------------- *)

InitializeStates[
   vertices_List,
   SM_List,
   mediaNodes_List,
   tau_Integer
] :=
   AssociationThread[
      vertices,

      Which[

         MemberQ[SM, #],

            "M",


         tau === 0 &&
         MemberQ[mediaNodes, #],

            "I",


         True,

            "U"

      ] & /@ vertices
   ];


InitializeStates[
   vertices_,
   SM_,
   mediaNodes_,
   tau_
] :=
   ValidationFailure[
      "InvalidStateInitialization",
      {
         "InitializeStates expects a vertex list, SM list, media-node list, and integer tau."
      }
   ];


InitializeDurations[
   states_Association
] :=
   Module[
      {
         vertices,
         durationsA,
         durationsB
      },

      vertices =
         Keys[states];

      durationsA =
         AssociationThread[
            vertices,
            If[
               states[#] === "M",
               1,
               0
            ] & /@ vertices
         ];

      durationsB =
         AssociationThread[
            vertices,
            If[
               states[#] === "I",
               1,
               0
            ] & /@ vertices
         ];

      <|
         "DurationsA" -> durationsA,
         "DurationsB" -> durationsB
      |>
   ];


InitializeDurations[states_] :=
   ValidationFailure[
      "InvalidDurationInitialization",
      {
         "InitializeDurations expects a states Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Full initial condition                                                 *)
(* ---------------------------------------------------------------------- *)

BuildInitialCondition[
   networkData_Association,
   sourceSpec_Association,
   params_Association
] :=
   Module[
      {
         graph,
         vertices,
         nodeTypes,
         mediaNodes,
         tau,
         SM,
         validation,
         states,
         durations
      },


      (* Validate model parameters *)

      validation =
         ValidateParameters[
            params
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      (* Reject source specifications belonging to the previous model *)

      validation =
         ValidateNoDeprecatedOfficialSourceSpecification[
            sourceSpec
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      (* Network information *)

      graph =
         networkData["Graph"];

      vertices =
         VertexList[graph];

      nodeTypes =
         networkData["NodeTypes"];

      mediaNodes =
         networkData["MediaNodes"];

      tau =
         params["tau"];


      (* Random seed used for source selection *)

      If[
         KeyExistsQ[
            sourceSpec,
            "Seed"
         ],

         SeedRandom[
            sourceSpec["Seed"]
         ]
      ];


      (* Initial misinformation sources *)

      SM =
         SelectMisinformationSources[
            networkData,
            sourceSpec
         ];

      If[
         Head[SM] === Failure,
         Return[SM]
      ];


      (* Validate the initial misinformation source set.
         There is no separate official-source set:
         all media nodes B become official sources at activation. *)

      validation =
         ValidateInitialSets[
            graph,
            SM,
            nodeTypes
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      (* Initial node states *)

      states =
         InitializeStates[
            vertices,
            SM,
            mediaNodes,
            tau
         ];

      If[
         Head[states] === Failure,
         Return[states]
      ];


      (* Initial state durations *)

      durations =
         InitializeDurations[
            states
         ];

      If[
         Head[durations] === Failure,
         Return[durations]
      ];


      (* Validate the complete initial state *)

      validation =
         ValidateSimulationState[
            <|
               "t" -> 0,
               "States" -> states,
               "DurationsA" ->
                  durations["DurationsA"],
               "DurationsB" ->
                  durations["DurationsB"]
            |>
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      (* Complete initial condition *)

      <|
         "SM" -> SM,
         "InitialStates" -> states,
         "InitialDurationsA" ->
            durations["DurationsA"],
         "InitialDurationsB" ->
            durations["DurationsB"],
         "SourceSpec" -> sourceSpec
      |>
   ];


BuildInitialCondition[
   networkData_,
   sourceSpec_,
   params_
] :=
   ValidationFailure[
      "InvalidInitialConditionInput",
      {
         "BuildInitialCondition expects networkData, sourceSpec, and params Associations."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Summary                                                                *)
(* ---------------------------------------------------------------------- *)

InitialConditionSummary[
   initialCondition_Association
] :=
   Module[
      {
         states,
         counts
      },

      states =
         initialCondition[
            "InitialStates"
         ];

      counts =
         Counts[
            Values[states]
         ];

      <|
         "MisinformationSourceCount" ->
            Length[
               initialCondition["SM"]
            ],

         "InitialUCount" ->
            Lookup[
               counts,
               "U",
               0
            ],

         "InitialMCount" ->
            Lookup[
               counts,
               "M",
               0
            ],

         "InitialICount" ->
            Lookup[
               counts,
               "I",
               0
            ],

         "SM" ->
            initialCondition["SM"]
      |>
   ];


InitialConditionSummary[
   initialCondition_
] :=
   ValidationFailure[
      "InvalidInitialCondition",
      {
         "InitialConditionSummary expects an initialCondition Association."
      }
   ];


End[];

EndPackage[];