(* ::Package:: *)

(* ========================================================================= *)
(* 05_UpdateRules_AlphaSensitivity.wl                                        *)
(*                                                                           *)
(* Dedicated extension of 05_UpdateRules.wl for the residence-time exponent  *)
(* sensitivity experiment used for Major Comment 1(d).                       *)
(*                                                                           *)
(* The standard production update rules are loaded first and remain          *)
(* unchanged except for the residence-time-dependent M -> I and I -> M        *)
(* transitions.                                                              *)
(*                                                                           *)
(* The generalized effective signal strengths are                            *)
(*                                                                           *)
(*     c/(a + 1)^alpha                                                       *)
(*                                                                           *)
(* and                                                                       *)
(*                                                                           *)
(*     d/(b + 1)^alpha.                                                      *)
(*                                                                           *)
(* If "alpha" is absent from params, alpha = 1 is used, reproducing the       *)
(* original production model exactly.                                        *)
(* ========================================================================= *)


(* ------------------------------------------------------------------------- *)
(* Load the standard production update rules                                 *)
(* ------------------------------------------------------------------------- *)

If[
   ! MemberQ[$Packages, "Dezinformacije`UpdateRules`"],

   Get[
      FileNameJoin[
         {
            $SrcDir,
            "05_UpdateRules.wl"
         }
      ]
   ]
];


BeginPackage[
   "Dezinformacije`UpdateRules`",
   {
      "Dezinformacije`TypesAndValidation`",
      "Dezinformacije`Signals`"
   }
];


Begin["`Private`"];


(* ------------------------------------------------------------------------- *)
(* Residence-time exponent validation                                        *)
(* ------------------------------------------------------------------------- *)

ValidAlphaQ[alpha_] :=
   NumericQ[alpha] &&
   alpha > 0;


(* ------------------------------------------------------------------------- *)
(* Generalized transition probability: M -> I                                *)
(* ------------------------------------------------------------------------- *)

MisinformedTransitionProbabilities[
   NI_Integer,
   a_Integer,
   c_?ValidProbabilityQ,
   alpha_?ValidAlphaQ
] :=
   Module[
      {
         pMI
      },

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


      (* Each incoming corrective signal has effective strength

               c/(a + 1)^alpha

         after a consecutive residence time a in state M.
      *)

      pMI =
         ClipProbability[
            1 -
            (1 - c/(a + 1)^alpha)^NI
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
   c_,
   alpha_
] :=
   ValidationFailure[
      "InvalidMisinformedTransitionInput",
      {
         "MisinformedTransitionProbabilities expects NI >= 0, a >= 1, c in [0,1], and alpha > 0."
      }
   ];


(* ------------------------------------------------------------------------- *)
(* Generalized transition probability: I -> M                                *)
(* ------------------------------------------------------------------------- *)

InformedTransitionProbabilities[
   NM_Integer,
   b_Integer,
   d_?ValidProbabilityQ,
   alpha_?ValidAlphaQ
] :=
   Module[
      {
         pIM
      },

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


      (* Each incoming misinformation signal has effective strength

               d/(b + 1)^alpha

         after a consecutive residence time b in state I.
      *)

      pIM =
         ClipProbability[
            1 -
            (1 - d/(b + 1)^alpha)^NM
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
   d_,
   alpha_
] :=
   ValidationFailure[
      "InvalidInformedTransitionInput",
      {
         "InformedTransitionProbabilities expects NM >= 0, b >= 1, d in [0,1], and alpha > 0."
      }
   ];


(* ------------------------------------------------------------------------- *)
(* Random state update: M                                                    *)
(* ------------------------------------------------------------------------- *)

UpdateMisinformedNode[
   NI_Integer,
   a_Integer,
   c_?ValidProbabilityQ,
   alpha_?ValidAlphaQ
] :=
   Module[
      {
         prob
      },

      prob =
         MisinformedTransitionProbabilities[
            NI,
            a,
            c,
            alpha
         ];

      If[
         Head[prob] === Failure,
         Return[prob]
      ];

      DrawState[
         prob
      ]
   ];


UpdateMisinformedNode[
   NI_,
   a_,
   c_,
   alpha_
] :=
   ValidationFailure[
      "InvalidMisinformedNodeUpdateInput",
      {
         "UpdateMisinformedNode expects NI >= 0, a >= 1, c in [0,1], and alpha > 0."
      }
   ];


(* ------------------------------------------------------------------------- *)
(* Random state update: I                                                    *)
(* ------------------------------------------------------------------------- *)

UpdateInformedNode[
   NM_Integer,
   b_Integer,
   d_?ValidProbabilityQ,
   alpha_?ValidAlphaQ
] :=
   Module[
      {
         prob
      },

      prob =
         InformedTransitionProbabilities[
            NM,
            b,
            d,
            alpha
         ];

      If[
         Head[prob] === Failure,
         Return[prob]
      ];

      DrawState[
         prob
      ]
   ];


UpdateInformedNode[
   NM_,
   b_,
   d_,
   alpha_
] :=
   ValidationFailure[
      "InvalidInformedNodeUpdateInput",
      {
         "UpdateInformedNode expects NM >= 0, b >= 1, d in [0,1], and alpha > 0."
      }
   ];


(* ------------------------------------------------------------------------- *)
(* Ordinary-agent update rule with residence-time exponent alpha             *)
(* ------------------------------------------------------------------------- *)

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
         d,
         alpha
      },

      c =
         params["c"];

      d =
         params["d"];

      alpha =
         Lookup[
            params,
            "alpha",
            1.
         ];


      If[
         ! ValidAlphaQ[alpha],

         Return[
            ValidationFailure[
               "InvalidResidenceTimeExponent",
               {
                  "Parameter alpha must be a positive numeric value."
               }
            ]
         ]
      ];


      Switch[
         state,

         "U",

            (* The uninformed-state rule is unchanged.
               Residence time does not enter this transition.
            *)

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
               c,
               alpha
            ],


         "I",

            UpdateInformedNode[
               NM,
               bv,
               d,
               alpha
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


End[];

EndPackage[];