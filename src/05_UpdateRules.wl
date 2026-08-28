(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

If[
   ! MemberQ[$Packages, "Dezinformacije`Signals`"],
   Get[FileNameJoin[{$SrcDir, "04_Signals.wl"}]]
];

BeginPackage[
   "Dezinformacije`UpdateRules`",
   {
      "Dezinformacije`TypesAndValidation`",
      "Dezinformacije`Signals`"
   }
];


AggregateInformationStrength::usage =
   "AggregateInformationStrength[NI, c] computes 1 - (1 - c)^NI.";

AggregateMisinformationStrength::usage =
   "AggregateMisinformationStrength[NM, d] computes 1 - (1 - d)^NM.";


UninformedTransitionProbabilities::usage =
   "UninformedTransitionProbabilities[NI, NM, c, d] returns transition probabilities from state U.";

MisinformedTransitionProbabilities::usage =
   "MisinformedTransitionProbabilities[NI, a, c] returns transition probabilities from state M.";

InformedTransitionProbabilities::usage =
   "InformedTransitionProbabilities[NM, b, d] returns transition probabilities from state I.";


DrawState::usage =
   "DrawState[probabilities] draws one state from an Association of state probabilities.";

UpdateUninformedNode::usage =
   "UpdateUninformedNode[NI, NM, c, d] randomly updates a node in state U.";

UpdateMisinformedNode::usage =
   "UpdateMisinformedNode[NI, a, c] randomly updates a node in state M.";

UpdateInformedNode::usage =
   "UpdateInformedNode[NM, b, d] randomly updates a node in state I.";


UpdateOrdinaryAgent::usage =
   "UpdateOrdinaryAgent[state, av, bv, NI, NM, params] updates an ordinary agent.";

UpdateMediaNode::usage =
   "UpdateMediaNode[state, av, bv, NI, NM, t, params] updates a media node. Before campaign activation it follows the ordinary-agent rule; from activation onward it is permanently informed.";


UpdateDurations::usage =
   "UpdateDurations[oldState, newState, oldA, oldB] updates duration variables.";

UpdateNode::usage =
   "UpdateNode[v, simState, signals, params] updates one node and its durations.";

UpdateStatesAndDurations::usage =
   "UpdateStatesAndDurations[simState, signalState, params] updates all node states and durations.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Probability helpers                                                    *)
(* ---------------------------------------------------------------------- *)

ClipProbability[p_] :=
   Min[
      Max[N[p], 0],
      1
   ];


AggregateInformationStrength[
   NI_Integer,
   c_?ValidProbabilityQ
] :=
   ClipProbability[
      1 - (1 - c)^NI
   ];


AggregateMisinformationStrength[
   NM_Integer,
   d_?ValidProbabilityQ
] :=
   ClipProbability[
      1 - (1 - d)^NM
   ];


AggregateInformationStrength[
   NI_,
   c_
] :=
   ValidationFailure[
      "InvalidAggregateInformationInput",
      {
         "AggregateInformationStrength expects an integer NI and c in [0,1]."
      }
   ];


AggregateMisinformationStrength[
   NM_,
   d_
] :=
   ValidationFailure[
      "InvalidAggregateMisinformationInput",
      {
         "AggregateMisinformationStrength expects an integer NM and d in [0,1]."
      }
   ];


NormalizeProbabilities[
   prob_Association
] :=
   Module[
      {
         states,
         values,
         total
      },

      states =
         Keys[prob];

      values =
         ClipProbability /@
         Values[prob];

      total =
         Total[values];

      If[
         total <= 0,

         AssociationThread[
            states ->
               ConstantArray[
                  0,
                  Length[states]
               ]
         ],

         AssociationThread[
            states ->
               values/total
         ]
      ]
   ];


DrawState[
   prob_Association
] :=
   Module[
      {p},

      p =
         NormalizeProbabilities[
            prob
         ];

      RandomChoice[
         Values[p] ->
            Keys[p]
      ]
   ];


DrawState[prob_] :=
   ValidationFailure[
      "InvalidStateProbabilities",
      {
         "DrawState expects an Association of probabilities."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Transition probabilities                                               *)
(* ---------------------------------------------------------------------- *)

UninformedTransitionProbabilities[
   NI_Integer,
   NM_Integer,
   c_?ValidProbabilityQ,
   d_?ValidProbabilityQ
] :=
   Module[
      {
         AI,
         AM,
         A,
         pI,
         pM,
         pU
      },

      If[
         NI < 0 ||
         NM < 0,

         Return[
            ValidationFailure[
               "InvalidSignalCounts",
               {
                  "NI and NM must be nonnegative integers."
               }
            ]
         ]
      ];


      AI =
         AggregateInformationStrength[
            NI,
            c
         ];

      AM =
         AggregateMisinformationStrength[
            NM,
            d
         ];


      If[
         AI + AM == 0,

         Return[
            <|
               "U" -> 1.,
               "M" -> 0.,
               "I" -> 0.
            |>
         ]
      ];


      A =
         ClipProbability[
            1 -
            (1 - AI) (1 - AM)
         ];


      pI =
         ClipProbability[
            A AI/(AI + AM)
         ];

      pM =
         ClipProbability[
            A AM/(AI + AM)
         ];

      pU =
         ClipProbability[
            1 - A
         ];


      NormalizeProbabilities[
         <|
            "U" -> pU,
            "M" -> pM,
            "I" -> pI
         |>
      ]
   ];


UninformedTransitionProbabilities[
   NI_,
   NM_,
   c_,
   d_
] :=
   ValidationFailure[
      "InvalidUninformedTransitionInput",
      {
         "UninformedTransitionProbabilities expects nonnegative integer signal counts and c,d in [0,1]."
      }
   ];


MisinformedTransitionProbabilities[
   NI_Integer,
   a_Integer,
   c_?ValidProbabilityQ
] :=
   Module[
      {pMI},

      If[
         NI < 0 ||
         a < 1,

         Return[
            ValidationFailure[
               "InvalidMisinformedTransitionInput",
               {
                  "NI must be nonnegative and a must be positive."
               }
            ]
         ]
      ];


      (* A longer uninterrupted stay in state M reduces the
         effectiveness of each incoming corrective signal. *)

      pMI =
         ClipProbability[
            1 -
            (1 - c/(a + 1))^NI
         ];


      NormalizeProbabilities[
         <|
            "M" -> 1 - pMI,
            "I" -> pMI
         |>
      ]
   ];


MisinformedTransitionProbabilities[
   NI_,
   a_,
   c_
] :=
   ValidationFailure[
      "InvalidMisinformedTransitionInput",
      {
         "MisinformedTransitionProbabilities expects NI, a and c with NI >= 0, a >= 1, c in [0,1]."
      }
   ];


InformedTransitionProbabilities[
   NM_Integer,
   b_Integer,
   d_?ValidProbabilityQ
] :=
   Module[
      {pIM},

      If[
         NM < 0 ||
         b < 1,

         Return[
            ValidationFailure[
               "InvalidInformedTransitionInput",
               {
                  "NM must be nonnegative and b must be positive."
               }
            ]
         ]
      ];


      (* A longer uninterrupted stay in state I reduces the
         effectiveness of each incoming misinformation signal. *)

      pIM =
         ClipProbability[
            1 -
            (1 - d/(b + 1))^NM
         ];


      NormalizeProbabilities[
         <|
            "I" -> 1 - pIM,
            "M" -> pIM
         |>
      ]
   ];


InformedTransitionProbabilities[
   NM_,
   b_,
   d_
] :=
   ValidationFailure[
      "InvalidInformedTransitionInput",
      {
         "InformedTransitionProbabilities expects NM, b and d with NM >= 0, b >= 1, d in [0,1]."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Random state updates                                                   *)
(* ---------------------------------------------------------------------- *)

UpdateUninformedNode[
   NI_Integer,
   NM_Integer,
   c_?ValidProbabilityQ,
   d_?ValidProbabilityQ
] :=
   Module[
      {prob},

      prob =
         UninformedTransitionProbabilities[
            NI,
            NM,
            c,
            d
         ];

      If[
         Head[prob] === Failure,
         Return[prob]
      ];

      DrawState[prob]
   ];


UpdateMisinformedNode[
   NI_Integer,
   a_Integer,
   c_?ValidProbabilityQ
] :=
   Module[
      {prob},

      prob =
         MisinformedTransitionProbabilities[
            NI,
            a,
            c
         ];

      If[
         Head[prob] === Failure,
         Return[prob]
      ];

      DrawState[prob]
   ];


UpdateInformedNode[
   NM_Integer,
   b_Integer,
   d_?ValidProbabilityQ
] :=
   Module[
      {prob},

      prob =
         InformedTransitionProbabilities[
            NM,
            b,
            d
         ];

      If[
         Head[prob] === Failure,
         Return[prob]
      ];

      DrawState[prob]
   ];


(* ---------------------------------------------------------------------- *)
(* Ordinary-agent update rule                                             *)
(* ---------------------------------------------------------------------- *)

UpdateOrdinaryAgent[
   state_String,
   av_Integer,
   bv_Integer,
   NI_Integer,
   NM_Integer,
   params_Association
] :=
   Module[
      {
         c,
         d
      },

      c =
         params["c"];

      d =
         params["d"];


      Switch[
         state,

         "U",

            UpdateUninformedNode[
               NI,
               NM,
               c,
               d
            ],


         "M",

            UpdateMisinformedNode[
               NI,
               av,
               c
            ],


         "I",

            UpdateInformedNode[
               NM,
               bv,
               d
            ],


         _,

            ValidationFailure[
               "InvalidState",
               {
                  "Invalid state for ordinary-agent update: " <>
                  ToString[state]
               }
            ]
      ]
   ];


UpdateOrdinaryAgent[
   state_,
   av_,
   bv_,
   NI_,
   NM_,
   params_
] :=
   ValidationFailure[
      "InvalidOrdinaryAgentUpdateInput",
      {
         "UpdateOrdinaryAgent expects state, durations, signal counts, and params Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Media-node update rule                                                 *)
(* ---------------------------------------------------------------------- *)

UpdateMediaNode[
   state_String,
   av_Integer,
   bv_Integer,
   NI_Integer,
   NM_Integer,
   t_Integer,
   params_Association
] :=
   Module[
      {
         tau
      },

      tau =
         params["tau"];


      (* The update maps the state at time t to the state at time t+1.

         Therefore, if t+1 >= tau, the official campaign is active in
         the resulting state. Every media node B is then permanently
         set to I.

         Before activation, media nodes follow exactly the same
         stochastic update rule as ordinary agents.
      *)

      If[
         t + 1 >= tau,

         "I",

         UpdateOrdinaryAgent[
            state,
            av,
            bv,
            NI,
            NM,
            params
         ]
      ]
   ];


UpdateMediaNode[
   state_,
   av_,
   bv_,
   NI_,
   NM_,
   t_,
   params_
] :=
   ValidationFailure[
      "InvalidMediaNodeUpdateInput",
      {
         "UpdateMediaNode expects state, durations, signal counts, time, and params Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Duration update                                                        *)
(* ---------------------------------------------------------------------- *)

UpdateDurations[
   oldState_String,
   newState_String,
   oldA_Integer,
   oldB_Integer
] :=
   Switch[

      newState,

      "U",

         <|
            "a" -> 0,
            "b" -> 0
         |>,


      "M",

         <|
            "a" ->
               If[
                  oldState === "M",
                  oldA + 1,
                  1
               ],

            "b" -> 0
         |>,


      "I",

         <|
            "a" -> 0,

            "b" ->
               If[
                  oldState === "I",
                  oldB + 1,
                  1
               ]
         |>,


      _,

         ValidationFailure[
            "InvalidNewState",
            {
               "Invalid new state in UpdateDurations: " <>
               ToString[newState]
            }
         ]
   ];


UpdateDurations[
   oldState_,
   newState_,
   oldA_,
   oldB_
] :=
   ValidationFailure[
      "InvalidDurationUpdateInput",
      {
         "UpdateDurations expects old state, new state, and integer old durations."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Node-level update                                                      *)
(* ---------------------------------------------------------------------- *)

UpdateNode[
   v_,
   simState_Association,
   signals_Association,
   params_Association
] :=
   Module[
      {
         nodeTypes,
         states,
         durationsA,
         durationsB,
         t,
         state,
         av,
         bv,
         nodeType,
         NI,
         NM,
         newState,
         newDurations
      },


      nodeTypes =
         simState["NodeTypes"];

      states =
         simState["States"];

      durationsA =
         simState["DurationsA"];

      durationsB =
         simState["DurationsB"];

      t =
         simState["t"];


      state =
         states[v];

      av =
         durationsA[v];

      bv =
         durationsB[v];

      nodeType =
         CanonicalNodeType[
            nodeTypes[v]
         ];


      NI =
         signals[v]["NI"];

      NM =
         signals[v]["NM"];


      newState =
         Switch[

            nodeType,

            "A",

               UpdateOrdinaryAgent[
                  state,
                  av,
                  bv,
                  NI,
                  NM,
                  params
               ],


            "B",

               UpdateMediaNode[
                  state,
                  av,
                  bv,
                  NI,
                  NM,
                  t,
                  params
               ],


            _,

               ValidationFailure[
                  "InvalidNodeType",
                  {
                     "Invalid node type for node " <>
                     ToString[v] <>
                     ": " <>
                     ToString[nodeTypes[v]]
                  }
               ]
         ];


      If[
         Head[newState] === Failure,
         Return[newState]
      ];


      newDurations =
         UpdateDurations[
            state,
            newState,
            av,
            bv
         ];


      If[
         Head[newDurations] === Failure,
         Return[newDurations]
      ];


      <|
         "nodeID" -> v,

         "oldState" -> state,
         "newState" -> newState,

         "oldA" -> av,
         "oldB" -> bv,

         "newA" ->
            newDurations["a"],

         "newB" ->
            newDurations["b"],

         "NI" -> NI,
         "NM" -> NM,

         "nodeType" -> nodeType
      |>
   ];


UpdateNode[
   v_,
   simState_,
   signals_,
   params_
] :=
   ValidationFailure[
      "InvalidNodeUpdateInput",
      {
         "UpdateNode expects node, simState, signals, and params."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Full-state update                                                      *)
(* ---------------------------------------------------------------------- *)

UpdateStatesAndDurations[
   simState_Association,
   signalState_Association,
   params_Association
] :=
   Module[
      {
         validation,
         graph,
         vertices,
         signals,
         updates,
         newStates,
         newDurationsA,
         newDurationsB
      },


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

      vertices =
         VertexList[graph];

      signals =
         signalState["Signals"];


      If[
         ! SameSetQList[
            vertices,
            Keys[signals]
         ],

         Return[
            ValidationFailure[
               "InvalidSignals",
               {
                  "Signal state must contain signal counts for exactly all graph vertices."
               }
            ]
         ]
      ];


      updates =
         UpdateNode[
            #,
            simState,
            signals,
            params
         ] & /@ vertices;


      If[
         AnyTrue[
            updates,
            Head[#] === Failure &
         ],

         Return[
            First @
               Select[
                  updates,
                  Head[#] === Failure &
               ]
         ]
      ];


      newStates =
         AssociationThread[
            vertices,
            (#["newState"] & /@ updates)
         ];


      newDurationsA =
         AssociationThread[
            vertices,
            (#["newA"] & /@ updates)
         ];


      newDurationsB =
         AssociationThread[
            vertices,
            (#["newB"] & /@ updates)
         ];


      validation =
         ValidateSimulationState[
            <|
               "t" ->
                  simState["t"] + 1,

               "States" ->
                  newStates,

               "DurationsA" ->
                  newDurationsA,

               "DurationsB" ->
                  newDurationsB
            |>
         ];


      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      <|
         "t" ->
            simState["t"] + 1,

         "States" ->
            newStates,

         "DurationsA" ->
            newDurationsA,

         "DurationsB" ->
            newDurationsB,

         "Updates" ->
            updates
      |>
   ];


UpdateStatesAndDurations[
   simState_,
   signalState_,
   params_
] :=
   ValidationFailure[
      "InvalidFullStateUpdateInput",
      {
         "UpdateStatesAndDurations expects simState, signalState, and params Associations."
      }
   ];


End[];

EndPackage[];