(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

BeginPackage[
   "Dezinformacije`Signals`",
   {
      "Dezinformacije`TypesAndValidation`"
   }
];


TransmittingMisinformationQ::usage =
   "TransmittingMisinformationQ[state] returns True if a node currently transmits misinformation.";

TransmittingInformationQ::usage =
   "TransmittingInformationQ[state] returns True if a node currently transmits official information.";

MisinformationTransmitterNodes::usage =
   "MisinformationTransmitterNodes[states] returns nodes currently transmitting misinformation.";

InformationTransmitterNodes::usage =
   "InformationTransmitterNodes[states] returns nodes currently transmitting official information.";

NeighborNodes::usage =
   "NeighborNodes[graph, v] returns the neighbours of node v.";

CountIncomingSignalsForNode::usage =
   "CountIncomingSignalsForNode[graph, v, states] returns <|\"NI\" -> ..., \"NM\" -> ...|> for node v.";

CountIncomingSignals::usage =
   "CountIncomingSignals[graph, states] counts incoming official-information and misinformation signals for all nodes.";

BuildSignalState::usage =
   "BuildSignalState[simState] builds a compact Association containing signal counts and transmitter sets.";

SignalLongTable::usage =
   "SignalLongTable[signals, nodeTypes, runID, t] converts signal counts to long-table Association rows.";

SignalSummary::usage =
   "SignalSummary[signals] returns aggregate information about signal counts.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Transmission rules                                                     *)
(* ---------------------------------------------------------------------- *)

(* Transmission depends only on the current state of a node.

   Nodes in state M transmit misinformation.
   Nodes in state I transmit official information.
   Nodes in state U transmit neither signal.

   No distinction between ordinary and media nodes is required here.
   Before campaign activation, media nodes follow the same state-based
   transmission rule as ordinary agents. Once the campaign is activated,
   the update rules place all media nodes in state I, after which they are
   automatically counted as official-information transmitters.
*)

TransmittingMisinformationQ[state_] :=
   state === "M";

TransmittingInformationQ[state_] :=
   state === "I";


MisinformationTransmitterNodes[
   states_Association
] :=
   Keys @
      Select[
         states,
         TransmittingMisinformationQ
      ];


InformationTransmitterNodes[
   states_Association
] :=
   Keys @
      Select[
         states,
         TransmittingInformationQ
      ];


MisinformationTransmitterNodes[states_] :=
   ValidationFailure[
      "InvalidStates",
      {
         "MisinformationTransmitterNodes expects a states Association."
      }
   ];


InformationTransmitterNodes[states_] :=
   ValidationFailure[
      "InvalidStates",
      {
         "InformationTransmitterNodes expects a states Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Neighbour extraction                                                   *)
(* ---------------------------------------------------------------------- *)

NeighborNodes[
   graph_Graph,
   v_
] :=
   Module[
      {
         edges,
         pairs,
         neighbours
      },

      If[
         ! MemberQ[
            VertexList[graph],
            v
         ],

         Return[
            ValidationFailure[
               "InvalidNode",
               {
                  "The node is not a vertex of the graph: " <>
                  ToString[v]
               }
            ]
         ]
      ];


      edges =
         EdgeList[graph];

      pairs =
         List @@@ edges;


      neighbours =
         DeleteDuplicates @
            Join[

               Cases[
                  pairs,
                  {v, x_} :> x
               ],

               Cases[
                  pairs,
                  {x_, v} :> x
               ]

            ];


      neighbours
   ];


NeighborNodes[
   graph_,
   v_
] :=
   ValidationFailure[
      "InvalidNeighbourInput",
      {
         "NeighborNodes expects a Graph and a node."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Signal counting                                                        *)
(* ---------------------------------------------------------------------- *)

CountIncomingSignalsForNode[
   graph_Graph,
   v_,
   states_Association
] :=
   Module[
      {
         vertices,
         neighbours,
         missingNeighbours,
         neighbourStates,
         nI,
         nM
      },

      vertices =
         VertexList[graph];


      If[
         ! MemberQ[
            vertices,
            v
         ],

         Return[
            ValidationFailure[
               "InvalidNode",
               {
                  "The node is not a vertex of the graph: " <>
                  ToString[v]
               }
            ]
         ]
      ];


      neighbours =
         NeighborNodes[
            graph,
            v
         ];

      If[
         Head[neighbours] === Failure,
         Return[neighbours]
      ];


      missingNeighbours =
         Complement[
            neighbours,
            Keys[states]
         ];

      If[
         missingNeighbours =!= {},

         Return[
            ValidationFailure[
               "InvalidStates",
               {
                  "The states Association does not contain all neighbours of node " <>
                  ToString[v] <>
                  ". Missing neighbours: " <>
                  ToString[missingNeighbours]
               }
            ]
         ]
      ];


      neighbourStates =
         states /@ neighbours;


      nI =
         Count[
            neighbourStates,
            "I"
         ];

      nM =
         Count[
            neighbourStates,
            "M"
         ];


      <|
         "NI" -> nI,
         "NM" -> nM
      |>
   ];


CountIncomingSignalsForNode[
   graph_,
   v_,
   states_
] :=
   ValidationFailure[
      "InvalidSignalCountingInput",
      {
         "CountIncomingSignalsForNode expects a Graph, a node, and a states Association."
      }
   ];


CountIncomingSignals[
   graph_Graph,
   states_Association
] :=
   Module[
      {
         vertices,
         invalidStates,
         signalRows
      },

      vertices =
         VertexList[graph];


      (* Every graph vertex must have exactly one state *)

      If[
         ! SameSetQList[
            vertices,
            Keys[states]
         ],

         Return[
            ValidationFailure[
               "InvalidStates",
               {
                  "The states Association must assign exactly one state to every vertex of the graph."
               }
            ]
         ]
      ];


      (* Only U, M, and I are valid *)

      invalidStates =
         Complement[
            DeleteDuplicates[
               Values[states]
            ],
            $AllowedStates
         ];

      If[
         invalidStates =!= {},

         Return[
            ValidationFailure[
               "InvalidStates",
               {
                  "The states Association contains invalid state labels: " <>
                  ToString[invalidStates]
               }
            ]
         ]
      ];


      (* Count signals received from neighbours *)

      signalRows =
         CountIncomingSignalsForNode[
            graph,
            #,
            states
         ] & /@ vertices;


      If[
         AnyTrue[
            signalRows,
            Head[#] === Failure &
         ],

         Return[
            First @
               Select[
                  signalRows,
                  Head[#] === Failure &
               ]
         ]
      ];


      AssociationThread[
         vertices -> signalRows
      ]
   ];


CountIncomingSignals[
   graph_,
   states_
] :=
   ValidationFailure[
      "InvalidSignalCountingInput",
      {
         "CountIncomingSignals expects a Graph and a states Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Interface for a simulation-state object                                *)
(* ---------------------------------------------------------------------- *)

BuildSignalState[
   simState_Association
] :=
   Module[
      {
         validation,
         graph,
         states,
         signals
      },


      If[
         ! KeyExistsQ[
            simState,
            "Graph"
         ],

         Return[
            ValidationFailure[
               "InvalidSimulationState",
               {
                  "BuildSignalState expects simState to contain key \"Graph\"."
               }
            ]
         ]
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


      graph =
         simState["Graph"];

      states =
         simState["States"];


      signals =
         CountIncomingSignals[
            graph,
            states
         ];

      If[
         Head[signals] === Failure,
         Return[signals]
      ];


      <|
         "t" ->
            simState["t"],

         "Signals" ->
            signals,

         "MisinformationTransmitters" ->
            MisinformationTransmitterNodes[
               states
            ],

         "InformationTransmitters" ->
            InformationTransmitterNodes[
               states
            ]
      |>
   ];


BuildSignalState[simState_] :=
   ValidationFailure[
      "InvalidSimulationState",
      {
         "BuildSignalState expects a simulation-state Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Conversion to table rows                                               *)
(* ---------------------------------------------------------------------- *)

SignalLongTable[
   signals_Association,
   nodeTypes_Association,
   runID_: Missing["RunID"],
   t_: Missing["t"]
] :=
   Module[
      {
         nodes,
         missingNodes
      },

      nodes =
         Keys[signals];

      missingNodes =
         Complement[
            nodes,
            Keys[nodeTypes]
         ];


      If[
         missingNodes =!= {},

         Return[
            ValidationFailure[
               "InvalidSignalTableInput",
               {
                  "nodeTypes does not contain all nodes present in signals."
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
               nodeTypes[v],

            "NI" ->
               signals[v]["NI"],

            "NM" ->
               signals[v]["NM"]
         |>,
         {v, nodes}
      ]
   ];


SignalLongTable[
   signals_,
   nodeTypes_,
   runID_: Missing["RunID"],
   t_: Missing["t"]
] :=
   ValidationFailure[
      "InvalidSignalTableInput",
      {
         "SignalLongTable expects signals and nodeTypes Associations."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Signal summaries                                                       *)
(* ---------------------------------------------------------------------- *)

SignalSummary[
   signals_Association
] :=
   Module[
      {
         rows,
         niValues,
         nmValues
      },

      rows =
         Values[signals];

      niValues =
         (#["NI"] & /@ rows);

      nmValues =
         (#["NM"] & /@ rows);


      <|

         "NodesReceivingInformation" ->
            Count[
               niValues,
               _?(# > 0 &)
            ],

         "NodesReceivingMisinformation" ->
            Count[
               nmValues,
               _?(# > 0 &)
            ],

         "TotalInformationSignals" ->
            Total[
               niValues
            ],

         "TotalMisinformationSignals" ->
            Total[
               nmValues
            ],

         "MaxInformationSignalsToOneNode" ->
            If[
               Length[niValues] > 0,
               Max[niValues],
               0
            ],

         "MaxMisinformationSignalsToOneNode" ->
            If[
               Length[nmValues] > 0,
               Max[nmValues],
               0
            ]

      |>
   ];


SignalSummary[signals_] :=
   ValidationFailure[
      "InvalidSignals",
      {
         "SignalSummary expects a signals Association."
      }
   ];


End[];

EndPackage[];