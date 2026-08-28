(* ::Package:: *)

BeginPackage["Dezinformacije`TypesAndValidation`"];

$AllowedStates::usage =
   "$AllowedStates is the list of allowed node states.";

$AllowedNodeTypes::usage =
   "$AllowedNodeTypes is the list of allowed canonical node types.";

$AllowedNetworkTypes::usage =
   "$AllowedNetworkTypes is the list of supported network type labels.";

StateCode::usage =
   "StateCode[state] returns the numeric code of a state.";

CanonicalNodeType::usage =
   "CanonicalNodeType[type] returns the canonical node type label, \"A\" or \"B\".";

ValidProbabilityQ::usage =
   "ValidProbabilityQ[x] returns True if x is a number in [0,1].";

NonNegativeIntegerQ::usage =
   "NonNegativeIntegerQ[x] returns True if x is a nonnegative integer.";

PositiveIntegerQ::usage =
   "PositiveIntegerQ[x] returns True if x is a positive integer.";

ValidationFailure::usage =
   "ValidationFailure[tag, errors] creates a standard Failure object.";

ValidationSucceededQ::usage =
   "ValidationSucceededQ[result] returns True if a validation result is True.";

ValidateParameters::usage =
   "ValidateParameters[params] validates the main model parameters.";

ValidateNetworkSpec::usage =
   "ValidateNetworkSpec[netSpec] validates a network specification association.";

ValidateNodeTypes::usage =
   "ValidateNodeTypes[graph, nodeTypes] validates node type assignments.";

ValidateInitialSets::usage =
   "ValidateInitialSets[graph, SM, nodeTypes] validates the initial misinformation-source set.";

ValidateSimulationState::usage =
   "ValidateSimulationState[simState] validates states and duration variables.";

ValidateExperimentConfig::usage =
   "ValidateExperimentConfig[config] validates a full experiment configuration.";

AssertValid::usage =
   "AssertValid[validationResult] returns True or stops with a validation message.";

AgentNodes::usage =
   "AgentNodes[nodeTypes] returns ordinary-agent nodes.";

MediaNodes::usage =
   "MediaNodes[nodeTypes] returns media or institutional nodes.";

SameSetQList::usage =
   "SameSetQList[x, y] returns True if two lists contain the same elements.";

SubsetQList::usage =
   "SubsetQList[x, y] returns True if every element of x is contained in y.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Basic constants                                                        *)
(* ---------------------------------------------------------------------- *)

$AllowedStates = {"U", "M", "I"};

$StateCodes = <|
   "U" -> 0,
   "M" -> 1,
   "I" -> 2
|>;

$AllowedNodeTypes = {"A", "B"};

$AllowedNetworkTypes = {
   "Random",
   "SmallWorld",
   "ScaleFree",
   "Modular",
   "Imported"
};

StateCode[state_String] :=
   Lookup[
      $StateCodes,
      state,
      Missing["InvalidState", state]
   ];

CanonicalNodeType[type_] :=
   Which[
      type === "A" ||
      type === "Agent" ||
      type === "OrdinaryAgent",
         "A",

      type === "B" ||
      type === "Media" ||
      type === "Institutional" ||
      type === "MediaInstitutional",
         "B",

      True,
         Missing["InvalidNodeType", type]
   ];


(* ---------------------------------------------------------------------- *)
(* Elementary predicates                                                  *)
(* ---------------------------------------------------------------------- *)

ValidProbabilityQ[x_] :=
   NumericQ[x] &&
   0 <= N[x] <= 1;

NonNegativeIntegerQ[x_] :=
   IntegerQ[x] &&
   x >= 0;

PositiveIntegerQ[x_] :=
   IntegerQ[x] &&
   x >= 1;

SubsetQList[x_List, y_List] :=
   Complement[x, y] === {};

SameSetQList[x_List, y_List] :=
   SubsetQList[x, y] &&
   SubsetQList[y, x];

ValidationFailure[
   tag_String,
   errors_List
] :=
   Failure[
      tag,
      <|"Errors" -> errors|>
   ];

ValidationSucceededQ[result_] :=
   TrueQ[result];

AssertValid[result_] :=
   If[
      ValidationSucceededQ[result],

      True,

      Print["Validation failed:"];
      Print[result["Errors"]];
      Abort[]
   ];


(* ---------------------------------------------------------------------- *)
(* Node-type helpers                                                      *)
(* ---------------------------------------------------------------------- *)

AgentNodes[nodeTypes_Association] :=
   Keys @
      Select[
         nodeTypes,
         CanonicalNodeType[#] === "A" &
      ];

MediaNodes[nodeTypes_Association] :=
   Keys @
      Select[
         nodeTypes,
         CanonicalNodeType[#] === "B" &
      ];


(* ---------------------------------------------------------------------- *)
(* Parameter validation                                                   *)
(* ---------------------------------------------------------------------- *)

ValidateParameters[
   params_Association
] :=
   Module[
      {
         errors = {},
         requiredKeys,
         deprecatedKeys
      },

      requiredKeys = {
         "c",
         "d",
         "tau",
         "Tmax",
         "R"
      };

      deprecatedKeys = {
         "thetaB",
         "MediaAdoptionMode"
      };


      (* Required parameters *)

      Do[
         If[
            ! KeyExistsQ[params, key],

            AppendTo[
               errors,
               "Missing required parameter: " <> key
            ]
         ],
         {key, requiredKeys}
      ];


      (* Deprecated parameters from the previous model *)

      Do[
         If[
            KeyExistsQ[params, key],

            AppendTo[
               errors,
               "Deprecated parameter detected: " <>
               key <>
               ". Media nodes no longer use a misinformation threshold before campaign activation."
            ]
         ],
         {key, deprecatedKeys}
      ];


      (* Diffusion parameters *)

      If[
         KeyExistsQ[params, "c"] &&
         ! ValidProbabilityQ[params["c"]],

         AppendTo[
            errors,
            "Parameter c must be a number in [0,1]."
         ]
      ];

      If[
         KeyExistsQ[params, "d"] &&
         ! ValidProbabilityQ[params["d"]],

         AppendTo[
            errors,
            "Parameter d must be a number in [0,1]."
         ]
      ];


      (* Timing and simulation parameters *)

      If[
         KeyExistsQ[params, "tau"] &&
         ! NonNegativeIntegerQ[params["tau"]],

         AppendTo[
            errors,
            "Parameter tau must be a nonnegative integer."
         ]
      ];

      If[
         KeyExistsQ[params, "Tmax"] &&
         ! PositiveIntegerQ[params["Tmax"]],

         AppendTo[
            errors,
            "Parameter Tmax must be a positive integer."
         ]
      ];

      If[
         KeyExistsQ[params, "R"] &&
         ! PositiveIntegerQ[params["R"]],

         AppendTo[
            errors,
            "Parameter R must be a positive integer."
         ]
      ];


      (* Optional parameters *)

      If[
         KeyExistsQ[params, "Seed"] &&
         ! IntegerQ[params["Seed"]],

         AppendTo[
            errors,
            "Optional parameter Seed must be an integer."
         ]
      ];

      If[
         KeyExistsQ[params, "muClearance"] &&
         ! ValidProbabilityQ[params["muClearance"]],

         AppendTo[
            errors,
            "Optional parameter muClearance must be a number in [0,1]."
         ]
      ];


      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidParameters",
            errors
         ]
      ]
   ];

ValidateParameters[params_] :=
   ValidationFailure[
      "InvalidParameters",
      {
         "Parameters must be given as an Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Network-spec validation                                                *)
(* ---------------------------------------------------------------------- *)

ValidateNetworkSpec[
   netSpec_Association
] :=
   Module[
      {
         errors = {},
         n,
         nMedia,
         networkType
      },


      (* Required keys *)

      If[
         ! KeyExistsQ[netSpec, "NetworkType"],

         AppendTo[
            errors,
            "Missing required network specification key: NetworkType."
         ]
      ];

      If[
         ! KeyExistsQ[netSpec, "n"],

         AppendTo[
            errors,
            "Missing required network specification key: n."
         ]
      ];

      If[
         ! KeyExistsQ[netSpec, "nMedia"],

         AppendTo[
            errors,
            "Missing required network specification key: nMedia."
         ]
      ];


      (* Network type *)

      If[
         KeyExistsQ[netSpec, "NetworkType"],

         networkType = netSpec["NetworkType"];

         If[
            ! MemberQ[
               $AllowedNetworkTypes,
               networkType
            ],

            AppendTo[
               errors,
               "Unsupported NetworkType: " <>
               ToString[networkType] <>
               ". Allowed values are: " <>
               StringRiffle[
                  $AllowedNetworkTypes,
                  ", "
               ] <>
               "."
            ]
         ]
      ];


      (* Number of nodes *)

      If[
         KeyExistsQ[netSpec, "n"],

         n = netSpec["n"];

         If[
            ! PositiveIntegerQ[n],

            AppendTo[
               errors,
               "Network size n must be a positive integer."
            ]
         ]
      ];


      (* Number of media nodes *)

      If[
         KeyExistsQ[netSpec, "nMedia"],

         nMedia = netSpec["nMedia"];

         If[
            ! PositiveIntegerQ[nMedia],

            AppendTo[
               errors,
               "nMedia must be a positive integer."
            ]
         ]
      ];


      (* There must also be at least one ordinary agent *)

      If[
         KeyExistsQ[netSpec, "n"] &&
         KeyExistsQ[netSpec, "nMedia"],

         n = netSpec["n"];
         nMedia = netSpec["nMedia"];

         If[
            PositiveIntegerQ[n] &&
            PositiveIntegerQ[nMedia] &&
            nMedia >= n,

            AppendTo[
               errors,
               "nMedia must be smaller than n so that the network contains ordinary agents."
            ]
         ]
      ];


      (* Optional network parameters *)

      If[
         KeyExistsQ[netSpec, "p"] &&
         ! ValidProbabilityQ[netSpec["p"]],

         AppendTo[
            errors,
            "Optional network parameter p must be a number in [0,1]."
         ]
      ];

      If[
         KeyExistsQ[
            netSpec,
            "rewiringProbability"
         ] &&
         ! ValidProbabilityQ[
            netSpec["rewiringProbability"]
         ],

         AppendTo[
            errors,
            "Optional parameter rewiringProbability must be a number in [0,1]."
         ]
      ];

      If[
         KeyExistsQ[netSpec, "k"] &&
         ! PositiveIntegerQ[netSpec["k"]],

         AppendTo[
            errors,
            "Optional parameter k must be a positive integer."
         ]
      ];

      If[
         KeyExistsQ[netSpec, "communities"] &&
         ! PositiveIntegerQ[
            netSpec["communities"]
         ],

         AppendTo[
            errors,
            "Optional parameter communities must be a positive integer."
         ]
      ];


      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidNetworkSpec",
            errors
         ]
      ]
   ];

ValidateNetworkSpec[netSpec_] :=
   ValidationFailure[
      "InvalidNetworkSpec",
      {
         "Network specification must be given as an Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Node-type validation                                                   *)
(* ---------------------------------------------------------------------- *)

ValidateNodeTypes[
   graph_Graph,
   nodeTypes_Association
] :=
   Module[
      {
         errors = {},
         vertices,
         keys,
         badTypes,
         canonicalTypes
      },

      vertices = VertexList[graph];
      keys = Keys[nodeTypes];

      If[
         ! SameSetQList[keys, vertices],

         AppendTo[
            errors,
            "NodeTypes must assign exactly one type to every vertex of the graph."
         ]
      ];

      canonicalTypes =
         CanonicalNodeType /@
         Values[nodeTypes];

      badTypes =
         Pick[
            Values[nodeTypes],
            MissingQ /@ canonicalTypes
         ];

      If[
         badTypes =!= {},

         AppendTo[
            errors,
            "Invalid node type values detected: " <>
            ToString[
               DeleteDuplicates[badTypes]
            ]
         ]
      ];

      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidNodeTypes",
            errors
         ]
      ]
   ];

ValidateNodeTypes[graph_, nodeTypes_] :=
   ValidationFailure[
      "InvalidNodeTypes",
      {
         "ValidateNodeTypes expects a Graph and an Association of node types."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Initial-source validation                                              *)
(* ---------------------------------------------------------------------- *)

ValidateInitialSets[
   graph_Graph,
   SM_List,
   nodeTypes_Association
] :=
   Module[
      {
         errors = {},
         vertices,
         agents,
         mediaNodes
      },

      vertices = VertexList[graph];

      agents =
         AgentNodes[nodeTypes];

      mediaNodes =
         MediaNodes[nodeTypes];


      (* The model requires media/institutional nodes.
         All of them become official-information spreaders
         when the campaign is activated. *)

      If[
         mediaNodes === {},

         AppendTo[
            errors,
            "The network must contain at least one media or institutional node B."
         ]
      ];


      (* Initial misinformation sources *)

      If[
         SM === {},

         AppendTo[
            errors,
            "SM must contain at least one initial misinformation source."
         ]
      ];

      If[
         ! SubsetQList[
            SM,
            vertices
         ],

         AppendTo[
            errors,
            "SM must be a subset of the graph vertex set."
         ]
      ];

      If[
         ! SubsetQList[
            SM,
            agents
         ],

         AppendTo[
            errors,
            "SM must be a subset of ordinary-agent nodes A."
         ]
      ];


      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidInitialSets",
            errors
         ]
      ]
   ];

ValidateInitialSets[
   graph_,
   SM_,
   nodeTypes_
] :=
   ValidationFailure[
      "InvalidInitialSets",
      {
         "ValidateInitialSets expects a Graph, an SM list, and a node-type Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Simulation-state validation                                            *)
(* ---------------------------------------------------------------------- *)

ValidateSimulationState[
   simState_Association
] :=
   Module[
      {
         errors = {},
         requiredKeys,
         states,
         durationsA,
         durationsB,
         stateKeys,
         aKeys,
         bKeys,
         invalidStates,
         invalidA,
         invalidB,
         v,
         x,
         av,
         bv
      },

      requiredKeys = {
         "t",
         "States",
         "DurationsA",
         "DurationsB"
      };

      Do[
         If[
            ! KeyExistsQ[simState, key],

            AppendTo[
               errors,
               "Simulation state is missing key: " <>
               key
            ]
         ],
         {key, requiredKeys}
      ];

      If[
         errors =!= {},

         Return[
            ValidationFailure[
               "InvalidSimulationState",
               errors
            ]
         ]
      ];


      If[
         ! NonNegativeIntegerQ[
            simState["t"]
         ],

         AppendTo[
            errors,
            "Simulation time t must be a nonnegative integer."
         ]
      ];


      states =
         simState["States"];

      durationsA =
         simState["DurationsA"];

      durationsB =
         simState["DurationsB"];


      If[
         ! AssociationQ[states],

         AppendTo[
            errors,
            "States must be an Association."
         ]
      ];

      If[
         ! AssociationQ[durationsA],

         AppendTo[
            errors,
            "DurationsA must be an Association."
         ]
      ];

      If[
         ! AssociationQ[durationsB],

         AppendTo[
            errors,
            "DurationsB must be an Association."
         ]
      ];

      If[
         errors =!= {},

         Return[
            ValidationFailure[
               "InvalidSimulationState",
               errors
            ]
         ]
      ];


      stateKeys =
         Keys[states];

      aKeys =
         Keys[durationsA];

      bKeys =
         Keys[durationsB];


      If[
         ! SameSetQList[
            stateKeys,
            aKeys
         ] ||
         ! SameSetQList[
            stateKeys,
            bKeys
         ],

         AppendTo[
            errors,
            "States, DurationsA, and DurationsB must have the same node keys."
         ]
      ];


      invalidStates =
         Select[
            Values[states],
            ! MemberQ[
               $AllowedStates,
               #
            ] &
         ];

      If[
         invalidStates =!= {},

         AppendTo[
            errors,
            "Invalid node states detected: " <>
            ToString[
               DeleteDuplicates[
                  invalidStates
               ]
            ]
         ]
      ];


      invalidA =
         Select[
            Values[durationsA],
            ! NonNegativeIntegerQ[#] &
         ];

      invalidB =
         Select[
            Values[durationsB],
            ! NonNegativeIntegerQ[#] &
         ];


      If[
         invalidA =!= {},

         AppendTo[
            errors,
            "All values in DurationsA must be nonnegative integers."
         ]
      ];

      If[
         invalidB =!= {},

         AppendTo[
            errors,
            "All values in DurationsB must be nonnegative integers."
         ]
      ];


      If[
         errors === {},

         Do[
            x = states[v];
            av = durationsA[v];
            bv = durationsB[v];

            Which[

               x === "U" &&
               ! (
                  av === 0 &&
                  bv === 0
               ),

                  AppendTo[
                     errors,
                     "If state is U, both duration variables must be zero. Node: " <>
                     ToString[v]
                  ],


               x === "M" &&
               ! (
                  av >= 1 &&
                  bv === 0
               ),

                  AppendTo[
                     errors,
                     "If state is M, DurationsA must be positive and DurationsB must be zero. Node: " <>
                     ToString[v]
                  ],


               x === "I" &&
               ! (
                  av === 0 &&
                  bv >= 1
               ),

                  AppendTo[
                     errors,
                     "If state is I, DurationsA must be zero and DurationsB must be positive. Node: " <>
                     ToString[v]
                  ],


               True,
                  Null
            ],

            {v, stateKeys}
         ]
      ];


      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidSimulationState",
            errors
         ]
      ]
   ];

ValidateSimulationState[simState_] :=
   ValidationFailure[
      "InvalidSimulationState",
      {
         "Simulation state must be given as an Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Full experiment-config validation                                      *)
(* ---------------------------------------------------------------------- *)

ValidateExperimentConfig[
   config_Association
] :=
   Module[
      {
         errors = {},
         result
      },


      (* Parameters *)

      If[
         ! KeyExistsQ[
            config,
            "Parameters"
         ],

         AppendTo[
            errors,
            "Experiment config is missing key: Parameters."
         ],

         result =
            ValidateParameters[
               config["Parameters"]
            ];

         If[
            ! ValidationSucceededQ[result],

            errors =
               Join[
                  errors,
                  result["Errors"]
               ]
         ]
      ];


      (* Network specification *)

      If[
         ! KeyExistsQ[
            config,
            "NetworkSpec"
         ],

         AppendTo[
            errors,
            "Experiment config is missing key: NetworkSpec."
         ],

         result =
            ValidateNetworkSpec[
               config["NetworkSpec"]
            ];

         If[
            ! ValidationSucceededQ[result],

            errors =
               Join[
                  errors,
                  result["Errors"]
               ]
         ]
      ];


      (* Optional source specification *)

      If[
         KeyExistsQ[
            config,
            "SourceSpec"
         ] &&
         ! AssociationQ[
            config["SourceSpec"]
         ],

         AppendTo[
            errors,
            "Optional SourceSpec must be an Association."
         ]
      ];


      (* Optional output specification *)

      If[
         KeyExistsQ[
            config,
            "OutputSpec"
         ] &&
         ! AssociationQ[
            config["OutputSpec"]
         ],

         AppendTo[
            errors,
            "Optional OutputSpec must be an Association."
         ]
      ];


      If[
         errors === {},
         True,
         ValidationFailure[
            "InvalidExperimentConfig",
            errors
         ]
      ]
   ];

ValidateExperimentConfig[config_] :=
   ValidationFailure[
      "InvalidExperimentConfig",
      {
         "Experiment config must be given as an Association."
      }
   ];


End[];

EndPackage[];