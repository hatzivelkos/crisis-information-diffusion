(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

BeginPackage[
   "Dezinformacije`Metrics`",
   {"Dezinformacije`TypesAndValidation`"}
];


StateCountsByTime::usage =
   "StateCountsByTime[history] computes U, M, I counts and prevalences among ordinary agents A by time.";

MCount::usage =
   "MCount[history, t] returns the number of misinformed ordinary agents at time t.";

ICount::usage =
   "ICount[history, t] returns the number of informed ordinary agents at time t.";

MPrevalence::usage =
   "MPrevalence[history, t] returns misinformation prevalence among ordinary agents at time t.";

IPrevalence::usage =
   "IPrevalence[history, t] returns official-information prevalence among ordinary agents at time t.";


PeakMisinformation::usage =
   "PeakMisinformation[timeSeries] returns the peak misinformation prevalence among ordinary agents.";

TimeOfPeakMisinformation::usage =
   "TimeOfPeakMisinformation[timeSeries] returns the first time at which peak misinformation prevalence is reached.";

AucMisinformation::usage =
   "AucMisinformation[timeSeries] returns cumulative misinformation exposure among ordinary agents.";

NormalizedAucMisinformation::usage =
   "NormalizedAucMisinformation[timeSeries] returns normalized cumulative misinformation exposure.";

ClearanceTime::usage =
   "ClearanceTime[timeSeries] returns the first time at which no ordinary agent is misinformed, or Missing if not observed.";

MuClearanceTime::usage =
   "MuClearanceTime[timeSeries, mu] returns the first time at which misinformation prevalence among ordinary agents is at most mu.";

PostPeakMuClearanceTime::usage =
   "PostPeakMuClearanceTime[timeSeries, mu] returns the first time after the misinformation peak at which ordinary-agent misinformation prevalence is at most mu.";

InformationDominanceTime::usage =
   "InformationDominanceTime[timeSeries] returns the first time at which informed ordinary agents outnumber misinformed ordinary agents.";


TerminalMisinformationSlope::usage =
   "TerminalMisinformationSlope[timeSeries, params] estimates the linear slope of misinformation prevalence over the final part of the post-campaign simulation horizon.";

TerminalMisinformationChange::usage =
   "TerminalMisinformationChange[timeSeries, params] returns the change in misinformation prevalence from the beginning to the end of the terminal trend window.";

TerminalMisinformationTrend::usage =
   "TerminalMisinformationTrend[timeSeries, params] classifies the terminal misinformation trajectory as Declining, Increasing, or Flat.";

MeanConfidenceInterval::usage =
   "MeanConfidenceInterval[values, confidenceLevel] returns a Student-t confidence interval for the mean of numeric values.";


ComputeRunMetrics::usage =
   "ComputeRunMetrics[result] computes metrics for one simulation result.";

ComputeRunMetricsFromHistory::usage =
   "ComputeRunMetricsFromHistory[history, params, runID] computes metrics directly from a long-format history.";

ComputeReplicationMetrics::usage =
   "ComputeReplicationMetrics[replicationResult] computes metrics for all replications.";

AggregateReplicationMetrics::usage =
   "AggregateReplicationMetrics[runMetrics] aggregates run-level metrics across replications.";


MetricLongTable::usage =
   "MetricLongTable[runMetrics] returns selected run metrics in flat Association rows.";

NumericMetricSummary::usage =
   "NumericMetricSummary[values] returns mean, sd, min, max, median for a numeric vector.";

ObservedTimeQ::usage =
   "ObservedTimeQ[x] returns True if a clearance-type time was observed.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Basic helpers                                                          *)
(* ---------------------------------------------------------------------- *)

HistoryListQ[history_] :=
   ListQ[history] &&
   AllTrue[
      history,
      AssociationQ
   ];


ValidOutcomeHistoryQ[history_] :=
   HistoryListQ[history] &&
   history =!= {} &&
   AllTrue[
      history,
      ContainsAll[
         Keys[#],
         {
            "nodeID",
            "nodeType",
            "t",
            "state"
         }
      ] &
   ];


ObservedTimeQ[x_] :=
   IntegerQ[x] ||
   NumericQ[x];


SafeLookup[
   assoc_Association,
   key_,
   default_
] :=
   Lookup[
      assoc,
      key,
      default
   ];


NodeCountFromHistory[
   history_List
] :=
   Length @
      DeleteDuplicates[
         #["nodeID"] & /@ history
      ];


OrdinaryAgentRowQ[
   row_Association
] :=
   CanonicalNodeType[
      row["nodeType"]
   ] === "A";


OrdinaryAgentRows[
   history_List
] :=
   Select[
      history,
      OrdinaryAgentRowQ
   ];


AgentCountFromHistory[
   history_List
] :=
   Length @
      DeleteDuplicates[
         #["nodeID"] & /@
            OrdinaryAgentRows[
               history
            ]
      ];


TimesFromHistory[
   history_List
] :=
   Sort @
      DeleteDuplicates[
         #["t"] & /@ history
      ];


RowsAtTime[
   history_List,
   t_
] :=
   Select[
      history,
      #["t"] === t &
   ];


AgentRowsAtTime[
   history_List,
   t_
] :=
   Select[
      history,
      (
         #["t"] === t &&
         OrdinaryAgentRowQ[#]
      ) &
   ];


StateCountsInRows[
   rows_List
] :=
   Module[
      {counts},

      counts =
         Counts[
            #["state"] & /@ rows
         ];

      <|
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
   ];


FirstObservedTime[
   rows_List,
   predicate_
] :=
   Module[
      {selected},

      selected =
         Select[
            rows,
            predicate
         ];

      If[
         selected === {},

         Missing["NotObserved"],

         First[selected]["t"]
      ]
   ];


NumericMetricSummary[
   values_List
] :=
   Module[
      {numericValues},

      numericValues =
         Select[
            values,
            NumericQ
         ];

      If[
         numericValues === {},

         <|
            "Mean" ->
               Missing["NotAvailable"],

            "SD" ->
               Missing["NotAvailable"],

            "Min" ->
               Missing["NotAvailable"],

            "Median" ->
               Missing["NotAvailable"],

            "Max" ->
               Missing["NotAvailable"]
         |>,

         <|
            "Mean" ->
               N[
                  Mean[numericValues]
               ],

            "SD" ->
               If[
                  Length[numericValues] >= 2,
                  N[
                     StandardDeviation[
                        numericValues
                     ]
                  ],
                  0.
               ],

            "Min" ->
               Min[numericValues],

            "Median" ->
               Median[numericValues],

            "Max" ->
               Max[numericValues]
         |>
      ]
   ];


MeanConfidenceInterval[
   values_List,
   confidenceLevel_ : 0.95
] :=
   Module[
      {
         numericValues,
         n,
         mean,
         sd,
         se,
         alpha,
         criticalValue
      },

      numericValues =
         N @
            Select[
               values,
               NumericQ
            ];


      If[
         numericValues === {},

         Return[
            <|
               "Level" ->
                  confidenceLevel,

               "Lower" ->
                  Missing["NotAvailable"],

               "Upper" ->
                  Missing["NotAvailable"]
            |>
         ]
      ];


      n =
         Length[
            numericValues
         ];

      mean =
         Mean[
            numericValues
         ];


      If[
         n < 2,

         Return[
            <|
               "Level" ->
                  confidenceLevel,

               "Lower" ->
                  Missing["InsufficientReplications"],

               "Upper" ->
                  Missing["InsufficientReplications"]
            |>
         ]
      ];


      sd =
         StandardDeviation[
            numericValues
         ];

      se =
         sd/Sqrt[n];

      alpha =
         1 - confidenceLevel;

      criticalValue =
         Quantile[
            StudentTDistribution[
               n - 1
            ],
            1 - alpha/2
         ];


      <|
         "Level" ->
            confidenceLevel,

         "Lower" ->
            N[
               mean -
               criticalValue*se
            ],

         "Upper" ->
            N[
               mean +
               criticalValue*se
            ]
      |>
   ];


MeanConfidenceInterval[
   values_,
   confidenceLevel_ : 0.95
] :=
   ValidationFailure[
      "InvalidConfidenceIntervalInput",
      {
         "MeanConfidenceInterval expects a list of values."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Counts and prevalence time series                                      *)
(* ---------------------------------------------------------------------- *)

StateCountsByTime[
   history_List
] :=
   Module[
      {
         nAgents,
         times
      },


      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "StateCountsByTime expects long-format history rows containing nodeID, nodeType, t, and state."
               }
            ]
         ]
      ];


      nAgents =
         AgentCountFromHistory[
            history
         ];


      If[
         nAgents <= 0,

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "Cannot infer a positive number of ordinary-agent nodes from history."
               }
            ]
         ]
      ];


      times =
         TimesFromHistory[
            history
         ];


      Table[
         Module[
            {
               rows,
               counts
            },

            rows =
               AgentRowsAtTime[
                  history,
                  t
               ];

            counts =
               StateCountsInRows[
                  rows
               ];


            <|
               "t" ->
                  t,

               "U" ->
                  counts["U"],

               "M" ->
                  counts["M"],

               "I" ->
                  counts["I"],

               "muM" ->
                  N[
                     counts["M"]/
                     nAgents
                  ],

               "muI" ->
                  N[
                     counts["I"]/
                     nAgents
                  ]
            |>
         ],

         {t, times}
      ]
   ];


StateCountsByTime[history_] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "StateCountsByTime expects a list of Association rows."
      }
   ];


MCount[
   history_List,
   t_
] :=
   Module[
      {rows},

      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "MCount expects valid long-format history containing node types."
               }
            ]
         ]
      ];


      rows =
         AgentRowsAtTime[
            history,
            t
         ];


      Count[
         #["state"] & /@ rows,
         "M"
      ]
   ];


MCount[
   history_,
   t_
] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "MCount expects a history list and a time t."
      }
   ];


ICount[
   history_List,
   t_
] :=
   Module[
      {rows},

      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "ICount expects valid long-format history containing node types."
               }
            ]
         ]
      ];


      rows =
         AgentRowsAtTime[
            history,
            t
         ];


      Count[
         #["state"] & /@ rows,
         "I"
      ]
   ];


ICount[
   history_,
   t_
] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "ICount expects a history list and a time t."
      }
   ];


MPrevalence[
   history_List,
   t_
] :=
   Module[
      {
         nAgents,
         mCount
      },

      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "MPrevalence expects valid long-format history containing node types."
               }
            ]
         ]
      ];


      nAgents =
         AgentCountFromHistory[
            history
         ];


      If[
         nAgents <= 0,

         Return[
            Missing["NotAvailable"]
         ]
      ];


      mCount =
         MCount[
            history,
            t
         ];


      If[
         Head[mCount] === Failure,
         Return[mCount]
      ];


      N[
         mCount/nAgents
      ]
   ];


MPrevalence[
   history_,
   t_
] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "MPrevalence expects a history list and a time t."
      }
   ];


IPrevalence[
   history_List,
   t_
] :=
   Module[
      {
         nAgents,
         iCount
      },

      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "IPrevalence expects valid long-format history containing node types."
               }
            ]
         ]
      ];


      nAgents =
         AgentCountFromHistory[
            history
         ];


      If[
         nAgents <= 0,

         Return[
            Missing["NotAvailable"]
         ]
      ];


      iCount =
         ICount[
            history,
            t
         ];


      If[
         Head[iCount] === Failure,
         Return[iCount]
      ];


      N[
         iCount/nAgents
      ]
   ];


IPrevalence[
   history_,
   t_
] :=
   ValidationFailure[
      "InvalidHistory",
      {
         "IPrevalence expects a history list and a time t."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Time-series metrics                                                    *)
(* ---------------------------------------------------------------------- *)

PeakMisinformation[
   timeSeries_List
] :=
   Max[
      #["muM"] & /@
         timeSeries
   ];


PeakMisinformation[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "PeakMisinformation expects a time-series list."
      }
   ];


TimeOfPeakMisinformation[
   timeSeries_List
] :=
   Module[
      {peak},

      peak =
         PeakMisinformation[
            timeSeries
         ];


      FirstObservedTime[
         timeSeries,
         #["muM"] === peak &
      ]
   ];


TimeOfPeakMisinformation[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "TimeOfPeakMisinformation expects a time-series list."
      }
   ];


AucMisinformation[
   timeSeries_List
] :=
   Total[
      #["muM"] & /@
         timeSeries
   ];


AucMisinformation[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "AucMisinformation expects a time-series list."
      }
   ];


NormalizedAucMisinformation[
   timeSeries_List
] :=
   Module[
      {len},

      len =
         Length[
            timeSeries
         ];


      If[
         len == 0,

         Missing["NotAvailable"],

         N[
            AucMisinformation[
               timeSeries
            ]/
            len
         ]
      ]
   ];


NormalizedAucMisinformation[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "NormalizedAucMisinformation expects a time-series list."
      }
   ];


ClearanceTime[
   timeSeries_List
] :=
   FirstObservedTime[
      timeSeries,
      #["M"] === 0 &
   ];


ClearanceTime[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "ClearanceTime expects a time-series list."
      }
   ];


MuClearanceTime[
   timeSeries_List,
   mu_?ValidProbabilityQ
] :=
   FirstObservedTime[
      timeSeries,
      #["muM"] <= mu &
   ];


MuClearanceTime[
   timeSeries_,
   mu_
] :=
   ValidationFailure[
      "InvalidMuClearanceInput",
      {
         "MuClearanceTime expects a time-series list and mu in [0,1]."
      }
   ];


PostPeakMuClearanceTime[
   timeSeries_List,
   mu_?ValidProbabilityQ
] :=
   Module[
      {
         tPeak,
         postPeakRows
      },

      tPeak =
         TimeOfPeakMisinformation[
            timeSeries
         ];


      If[
         ! ObservedTimeQ[tPeak],

         Return[
            Missing["NotObserved"]
         ]
      ];


      postPeakRows =
         Select[
            timeSeries,
            #["t"] >= tPeak &
         ];


      FirstObservedTime[
         postPeakRows,
         #["muM"] <= mu &
      ]
   ];


PostPeakMuClearanceTime[
   timeSeries_,
   mu_
] :=
   ValidationFailure[
      "InvalidPostPeakMuClearanceInput",
      {
         "PostPeakMuClearanceTime expects a time-series list and mu in [0,1]."
      }
   ];


InformationDominanceTime[
   timeSeries_List
] :=
   FirstObservedTime[
      timeSeries,
      #["muI"] > #["muM"] &
   ];


InformationDominanceTime[timeSeries_] :=
   ValidationFailure[
      "InvalidTimeSeries",
      {
         "InformationDominanceTime expects a time-series list."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Terminal misinformation trend                                          *)
(* ---------------------------------------------------------------------- *)

(* By default, the terminal trend is estimated over the final 20% of the
   post-campaign horizon. This can optionally be changed by adding

      "TerminalTrendFraction" -> value

   to params, where value is in (0,1].

   For example, with tau = 5 and Tmax = 400, the default terminal window
   covers approximately the final 79 post-campaign updates. *)


TerminalTrendFractionValue[
   params_Association
] :=
   Module[
      {fraction},

      fraction =
         Lookup[
            params,
            "TerminalTrendFraction",
            0.20
         ];

      If[
         NumericQ[fraction] &&
         0 < fraction <= 1,

         N[fraction],

         0.20
      ]
   ];


TerminalTrendWindow[
   timeSeries_List,
   params_Association : <||>
] :=
   Module[
      {
         times,
         tMin,
         tMax,
         tau,
         fraction,
         postCampaignSpan,
         windowSpan,
         startTime,
         rows
      },


      If[
         timeSeries === {},

         Return[
            <|
               "StartTime" ->
                  Missing["NotAvailable"],

               "EndTime" ->
                  Missing["NotAvailable"],

               "Span" ->
                  Missing["NotAvailable"],

               "Fraction" ->
                  TerminalTrendFractionValue[
                     params
                  ],

               "Rows" ->
                  {}
            |>
         ]
      ];


      times =
         #["t"] & /@
            timeSeries;

      tMin =
         Min[times];

      tMax =
         Max[times];


      tau =
         Lookup[
            params,
            "tau",
            tMin
         ];


      If[
         ! NumericQ[tau],
         tau = tMin
      ];


      tau =
         Max[
            tMin,
            Min[
               tMax,
               tau
            ]
         ];


      fraction =
         TerminalTrendFractionValue[
            params
         ];


      postCampaignSpan =
         tMax - tau;


      If[
         postCampaignSpan < 1,

         Return[
            <|
               "StartTime" ->
                  Missing["InsufficientPostCampaignHorizon"],

               "EndTime" ->
                  tMax,

               "Span" ->
                  0,

               "Fraction" ->
                  fraction,

               "Rows" ->
                  {}
            |>
         ]
      ];


      windowSpan =
         Max[
            1,
            Floor[
               fraction*
               postCampaignSpan
            ]
         ];


      startTime =
         Max[
            tau,
            tMax - windowSpan
         ];


      rows =
         Select[
            timeSeries,
            #["t"] >= startTime &
         ];


      <|
         "StartTime" ->
            startTime,

         "EndTime" ->
            tMax,

         "Span" ->
            tMax - startTime,

         "Fraction" ->
            fraction,

         "Rows" ->
            rows
      |>
   ];


TerminalMisinformationSlope[
   timeSeries_List,
   params_Association : <||>
] :=
   Module[
      {
         window,
         rows,
         x,
         y,
         xMean,
         yMean,
         denominator
      },


      window =
         TerminalTrendWindow[
            timeSeries,
            params
         ];

      rows =
         window["Rows"];


      If[
         Length[rows] < 2,

         Return[
            Missing["InsufficientTerminalWindow"]
         ]
      ];


      x =
         N[
            rows[[All, "t"]]
         ];

      y =
         N[
            rows[[All, "muM"]]
         ];


      xMean =
         Mean[x];

      yMean =
         Mean[y];


      denominator =
         Total[
            (x - xMean)^2
         ];


      If[
         denominator == 0,

         Return[
            Missing["InsufficientTerminalWindow"]
         ]
      ];


      N[
         Total[
            (x - xMean)*
            (y - yMean)
         ]/
         denominator
      ]
   ];


TerminalMisinformationSlope[
   timeSeries_,
   params_ : <||>
] :=
   ValidationFailure[
      "InvalidTerminalTrendInput",
      {
         "TerminalMisinformationSlope expects a time-series list and an optional parameter Association."
      }
   ];


TerminalMisinformationChange[
   timeSeries_List,
   params_Association : <||>
] :=
   Module[
      {
         window,
         rows
      },

      window =
         TerminalTrendWindow[
            timeSeries,
            params
         ];

      rows =
         window["Rows"];


      If[
         Length[rows] < 2,

         Missing["InsufficientTerminalWindow"],

         N[
            Last[rows]["muM"] -
            First[rows]["muM"]
         ]
      ]
   ];


TerminalMisinformationChange[
   timeSeries_,
   params_ : <||>
] :=
   ValidationFailure[
      "InvalidTerminalTrendInput",
      {
         "TerminalMisinformationChange expects a time-series list and an optional parameter Association."
      }
   ];


TerminalTrendSlopeTolerance[
   timeSeries_List,
   params_Association : <||>
] :=
   Module[
      {
         explicitTolerance,
         window,
         rows,
         span,
         nAgents
      },


      explicitTolerance =
         Lookup[
            params,
            "TerminalTrendSlopeTolerance",
            Automatic
         ];


      If[
         NumericQ[explicitTolerance] &&
         explicitTolerance >= 0,

         Return[
            N[
               explicitTolerance
            ]
         ]
      ];


      window =
         TerminalTrendWindow[
            timeSeries,
            params
         ];

      rows =
         window["Rows"];


      If[
         Length[rows] < 2,

         Return[
            Missing["InsufficientTerminalWindow"]
         ]
      ];


      span =
         Last[rows]["t"] -
         First[rows]["t"];


      nAgents =
         First[rows]["U"] +
         First[rows]["M"] +
         First[rows]["I"];


      If[
         ! NumericQ[span] ||
         span <= 0 ||
         ! IntegerQ[nAgents] ||
         nAgents <= 0,

         Return[
            Missing["NotAvailable"]
         ]
      ];


      (* A slope whose implied total change over the terminal window
         is smaller than approximately one ordinary agent is treated
         as effectively flat. *)

      N[
         1/
         (nAgents*span)
      ]
   ];


TerminalMisinformationTrend[
   timeSeries_List,
   params_Association : <||>
] :=
   Module[
      {
         slope,
         tolerance
      },


      slope =
         TerminalMisinformationSlope[
            timeSeries,
            params
         ];


      tolerance =
         TerminalTrendSlopeTolerance[
            timeSeries,
            params
         ];


      If[
         ! NumericQ[slope] ||
         ! NumericQ[tolerance],

         Return[
            Missing["NotAvailable"]
         ]
      ];


      Which[
         slope < -tolerance,
            "Declining",

         slope > tolerance,
            "Increasing",

         True,
            "Flat"
      ]
   ];


TerminalMisinformationTrend[
   timeSeries_,
   params_ : <||>
] :=
   ValidationFailure[
      "InvalidTerminalTrendInput",
      {
         "TerminalMisinformationTrend expects a time-series list and an optional parameter Association."
      }
   ];


AggregateTrendFromCI[
   ci_Association
] :=
   Module[
      {
         lower,
         upper
      },

      lower =
         Lookup[
            ci,
            "Lower",
            Missing["NotAvailable"]
         ];

      upper =
         Lookup[
            ci,
            "Upper",
            Missing["NotAvailable"]
         ];


      If[
         ! NumericQ[lower] ||
         ! NumericQ[upper],

         Return[
            Missing["NotAvailable"]
         ]
      ];


      Which[
         upper < 0,
            "Declining",

         lower > 0,
            "Increasing",

         True,
            "NoClearTrend"
      ]
   ];


(* ---------------------------------------------------------------------- *)
(* Run-level metrics                                                      *)
(* ---------------------------------------------------------------------- *)

ComputeRunMetricsFromHistory[
   history_List,
   params_Association : <||>,
   runID_ : Missing["RunID"]
] :=
   Module[
      {
         timeSeries,
         n,
         nAgents,
         tmax,
         mu,
         peakM,
         tPeakM,
         aucM,
         aucMNorm,
         t0,
         tMu,
         tPostPeakMu,
         tIgtM,
         terminalWindow,
         terminalSlope,
         terminalChange,
         terminalTrend,
         terminalTolerance
      },


      If[
         ! ValidOutcomeHistoryQ[history],

         Return[
            ValidationFailure[
               "InvalidHistory",
               {
                  "ComputeRunMetricsFromHistory expects long-format history containing nodeID, nodeType, t, and state."
               }
            ]
         ]
      ];


      timeSeries =
         StateCountsByTime[
            history
         ];


      If[
         Head[timeSeries] === Failure,
         Return[timeSeries]
      ];


      n =
         NodeCountFromHistory[
            history
         ];

      nAgents =
         AgentCountFromHistory[
            history
         ];

      tmax =
         Max[
            timeSeries[[All, "t"]]
         ];

      mu =
         Lookup[
            params,
            "muClearance",
            0.05
         ];


      peakM =
         PeakMisinformation[
            timeSeries
         ];

      tPeakM =
         TimeOfPeakMisinformation[
            timeSeries
         ];

      aucM =
         AucMisinformation[
            timeSeries
         ];

      aucMNorm =
         NormalizedAucMisinformation[
            timeSeries
         ];

      t0 =
         ClearanceTime[
            timeSeries
         ];

      tMu =
         MuClearanceTime[
            timeSeries,
            mu
         ];

      tPostPeakMu =
         PostPeakMuClearanceTime[
            timeSeries,
            mu
         ];

      tIgtM =
         InformationDominanceTime[
            timeSeries
         ];


      terminalWindow =
         TerminalTrendWindow[
            timeSeries,
            params
         ];

      terminalSlope =
         TerminalMisinformationSlope[
            timeSeries,
            params
         ];

      terminalChange =
         TerminalMisinformationChange[
            timeSeries,
            params
         ];

      terminalTrend =
         TerminalMisinformationTrend[
            timeSeries,
            params
         ];

      terminalTolerance =
         TerminalTrendSlopeTolerance[
            timeSeries,
            params
         ];


      <|
         "RunID" ->
            runID,

         "n" ->
            n,

         "AgentCount" ->
            nAgents,

         "Tmax" ->
            tmax,

         "muClearance" ->
            mu,

         "TimeSeries" ->
            timeSeries,


         "InitialU" ->
            First[timeSeries]["U"],

         "InitialM" ->
            First[timeSeries]["M"],

         "InitialI" ->
            First[timeSeries]["I"],

         "FinalU" ->
            Last[timeSeries]["U"],

         "FinalM" ->
            Last[timeSeries]["M"],

         "FinalI" ->
            Last[timeSeries]["I"],

         "FinalMuM" ->
            Last[timeSeries]["muM"],

         "FinalMuI" ->
            Last[timeSeries]["muI"],


         "PeakMisinformation" ->
            peakM,

         "TimeOfPeakMisinformation" ->
            tPeakM,

         "AucMisinformation" ->
            aucM,

         "NormalizedAucMisinformation" ->
            aucMNorm,

         "ClearanceTime" ->
            t0,

         "MuClearanceTime" ->
            tMu,

         "PostPeakMuClearanceTime" ->
            tPostPeakMu,

         "InformationDominanceTime" ->
            tIgtM,


         (* Terminal trend metrics *)

         "TerminalTrendFraction" ->
            terminalWindow["Fraction"],

         "TerminalTrendStartTime" ->
            terminalWindow["StartTime"],

         "TerminalTrendEndTime" ->
            terminalWindow["EndTime"],

         "TerminalTrendSpan" ->
            terminalWindow["Span"],

         "TerminalTrendSlopeTolerance" ->
            terminalTolerance,

         "TerminalMisinformationSlope" ->
            terminalSlope,

         "TerminalMisinformationChange" ->
            terminalChange,

         "TerminalMisinformationTrend" ->
            terminalTrend,


         "ClearanceObservedQ" ->
            ObservedTimeQ[t0],

         "MuClearanceObservedQ" ->
            ObservedTimeQ[tMu],

         "PostPeakMuClearanceObservedQ" ->
            ObservedTimeQ[tPostPeakMu],

         "InformationDominanceObservedQ" ->
            ObservedTimeQ[tIgtM]
      |>
   ];


ComputeRunMetricsFromHistory[
   history_,
   params_ : <||>,
   runID_ : Missing["RunID"]
] :=
   ValidationFailure[
      "InvalidRunMetricsInput",
      {
         "ComputeRunMetricsFromHistory expects history, optional params, and optional runID."
      }
   ];


ComputeRunMetrics[
   result_Association
] :=
   Module[
      {
         history,
         params,
         runID
      },


      If[
         ! KeyExistsQ[
            result,
            "History"
         ],

         Return[
            ValidationFailure[
               "InvalidSimulationResult",
               {
                  "ComputeRunMetrics expects a simulation result containing key \"History\"."
               }
            ]
         ]
      ];


      history =
         result["History"];

      params =
         Lookup[
            result,
            "Parameters",
            <||>
         ];

      runID =
         Lookup[
            result,
            "RunID",
            Missing["RunID"]
         ];


      ComputeRunMetricsFromHistory[
         history,
         params,
         runID
      ]
   ];


ComputeRunMetrics[result_] :=
   ValidationFailure[
      "InvalidSimulationResult",
      {
         "ComputeRunMetrics expects a simulation result Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Replication-level metrics                                              *)
(* ---------------------------------------------------------------------- *)

ComputeReplicationMetrics[
   replicationResult_Association
] :=
   Module[
      {
         results,
         metrics
      },


      If[
         ! KeyExistsQ[
            replicationResult,
            "Replications"
         ],

         Return[
            ValidationFailure[
               "InvalidReplicationResult",
               {
                  "ComputeReplicationMetrics expects key \"Replications\"."
               }
            ]
         ]
      ];


      results =
         replicationResult["Replications"];


      metrics =
         ComputeRunMetrics /@
            results;


      If[
         AnyTrue[
            metrics,
            Head[#] === Failure &
         ],

         Return[
            First @
               Select[
                  metrics,
                  Head[#] === Failure &
               ]
         ]
      ];


      metrics
   ];


ComputeReplicationMetrics[
   replicationResult_
] :=
   ValidationFailure[
      "InvalidReplicationResult",
      {
         "ComputeReplicationMetrics expects a replication-result Association."
      }
   ];


MetricLongTable[
   runMetrics_List
] :=
   Table[
      <|
         "RunID" ->
            m["RunID"],

         "n" ->
            m["n"],

         "AgentCount" ->
            m["AgentCount"],

         "Tmax" ->
            m["Tmax"],

         "InitialM" ->
            m["InitialM"],

         "FinalM" ->
            m["FinalM"],

         "FinalI" ->
            m["FinalI"],

         "FinalMuM" ->
            m["FinalMuM"],

         "FinalMuI" ->
            m["FinalMuI"],

         "PeakMisinformation" ->
            m["PeakMisinformation"],

         "TimeOfPeakMisinformation" ->
            m["TimeOfPeakMisinformation"],

         "AucMisinformation" ->
            m["AucMisinformation"],

         "NormalizedAucMisinformation" ->
            m["NormalizedAucMisinformation"],

         "ClearanceTime" ->
            m["ClearanceTime"],

         "MuClearanceTime" ->
            m["MuClearanceTime"],

         "PostPeakMuClearanceTime" ->
            m["PostPeakMuClearanceTime"],

         "InformationDominanceTime" ->
            m["InformationDominanceTime"],


         "TerminalTrendFraction" ->
            m["TerminalTrendFraction"],

         "TerminalTrendStartTime" ->
            m["TerminalTrendStartTime"],

         "TerminalTrendEndTime" ->
            m["TerminalTrendEndTime"],

         "TerminalTrendSpan" ->
            m["TerminalTrendSpan"],

         "TerminalMisinformationSlope" ->
            m["TerminalMisinformationSlope"],

         "TerminalMisinformationChange" ->
            m["TerminalMisinformationChange"],

         "TerminalMisinformationTrend" ->
            m["TerminalMisinformationTrend"],


         "ClearanceObservedQ" ->
            m["ClearanceObservedQ"],

         "MuClearanceObservedQ" ->
            m["MuClearanceObservedQ"],

         "PostPeakMuClearanceObservedQ" ->
            m["PostPeakMuClearanceObservedQ"],

         "InformationDominanceObservedQ" ->
            m["InformationDominanceObservedQ"]
      |>,
      {m, runMetrics}
   ];


MetricLongTable[runMetrics_] :=
   ValidationFailure[
      "InvalidRunMetrics",
      {
         "MetricLongTable expects a list of run-metric Associations."
      }
   ];


AggregateReplicationMetrics[
   runMetrics_List
] :=
   Module[
      {
         r,
         peakValues,
         aucValues,
         aucNormValues,
         finalMValues,
         finalIValues,
         finalMuMValues,
         finalMuIValues,
         tPeakValues,
         clearanceTimes,
         muClearanceTimes,
         postPeakMuClearanceTimes,
         dominanceTimes,
         terminalSlopeValues,
         terminalChangeValues,
         terminalTrendValues,
         terminalSlopeCI
      },


      If[
         runMetrics === {},

         Return[
            ValidationFailure[
               "InvalidRunMetrics",
               {
                  "Cannot aggregate an empty list of run metrics."
               }
            ]
         ]
      ];


      r =
         Length[
            runMetrics
         ];


      peakValues =
         runMetrics[[
            All,
            "PeakMisinformation"
         ]];

      aucValues =
         runMetrics[[
            All,
            "AucMisinformation"
         ]];

      aucNormValues =
         runMetrics[[
            All,
            "NormalizedAucMisinformation"
         ]];

      finalMValues =
         runMetrics[[
            All,
            "FinalM"
         ]];

      finalIValues =
         runMetrics[[
            All,
            "FinalI"
         ]];

      finalMuMValues =
         runMetrics[[
            All,
            "FinalMuM"
         ]];

      finalMuIValues =
         runMetrics[[
            All,
            "FinalMuI"
         ]];

      tPeakValues =
         runMetrics[[
            All,
            "TimeOfPeakMisinformation"
         ]];


      terminalSlopeValues =
         Select[
            runMetrics[[
               All,
               "TerminalMisinformationSlope"
            ]],
            NumericQ
         ];


      terminalChangeValues =
         Select[
            runMetrics[[
               All,
               "TerminalMisinformationChange"
            ]],
            NumericQ
         ];


      terminalTrendValues =
         runMetrics[[
            All,
            "TerminalMisinformationTrend"
         ]];


      terminalSlopeCI =
         MeanConfidenceInterval[
            terminalSlopeValues,
            0.95
         ];


      clearanceTimes =
         Select[
            runMetrics[[
               All,
               "ClearanceTime"
            ]],
            ObservedTimeQ
         ];


      muClearanceTimes =
         Select[
            runMetrics[[
               All,
               "MuClearanceTime"
            ]],
            ObservedTimeQ
         ];


      postPeakMuClearanceTimes =
         Select[
            runMetrics[[
               All,
               "PostPeakMuClearanceTime"
            ]],
            ObservedTimeQ
         ];


      dominanceTimes =
         Select[
            runMetrics[[
               All,
               "InformationDominanceTime"
            ]],
            ObservedTimeQ
         ];


      <|

         "ReplicationCount" ->
            r,


         "PeakMisinformation" ->
            NumericMetricSummary[
               peakValues
            ],

         "AucMisinformation" ->
            NumericMetricSummary[
               aucValues
            ],

         "NormalizedAucMisinformation" ->
            NumericMetricSummary[
               aucNormValues
            ],

         "FinalM" ->
            NumericMetricSummary[
               finalMValues
            ],

         "FinalI" ->
            NumericMetricSummary[
               finalIValues
            ],

         "FinalMuM" ->
            NumericMetricSummary[
               finalMuMValues
            ],

         "FinalMuI" ->
            NumericMetricSummary[
               finalMuIValues
            ],

         "TimeOfPeakMisinformation" ->
            NumericMetricSummary[
               tPeakValues
            ],


         (* Terminal trend across replications *)

         "TerminalMisinformationSlope" ->
            NumericMetricSummary[
               terminalSlopeValues
            ],

         "TerminalMisinformationSlope95CI" ->
            terminalSlopeCI,

         "TerminalMisinformationChange" ->
            NumericMetricSummary[
               terminalChangeValues
            ],

         "TerminalMisinformationTrend" ->
            AggregateTrendFromCI[
               terminalSlopeCI
            ],

         "TerminalDecliningRunShare" ->
            N[
               Count[
                  terminalTrendValues,
                  "Declining"
               ]/
               r
            ],

         "TerminalIncreasingRunShare" ->
            N[
               Count[
                  terminalTrendValues,
                  "Increasing"
               ]/
               r
            ],

         "TerminalFlatRunShare" ->
            N[
               Count[
                  terminalTrendValues,
                  "Flat"
               ]/
               r
            ],


         "ClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "ClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "MuClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "MuClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "PostPeakMuClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "PostPeakMuClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "InformationDominanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "InformationDominanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "ClearanceTimeObserved" ->
            NumericMetricSummary[
               clearanceTimes
            ],

         "MuClearanceTimeObserved" ->
            NumericMetricSummary[
               muClearanceTimes
            ],

         "PostPeakMuClearanceTimeObserved" ->
            NumericMetricSummary[
               postPeakMuClearanceTimes
            ],

         "InformationDominanceTimeObserved" ->
            NumericMetricSummary[
               dominanceTimes
            ]

      |>
   ];


AggregateReplicationMetrics[
   runMetrics_List
] :=
   Module[
      {
         r,
         peakValues,
         aucValues,
         aucNormValues,
         finalMValues,
         finalIValues,
         finalMuMValues,
         finalMuIValues,
         tPeakValues,
         clearanceTimes,
         muClearanceTimes,
         postPeakMuClearanceTimes,
         dominanceTimes,

         terminalSlopeValues,
         terminalChangeValues,
         terminalTrendValues,
         terminalSlopeCI,

         unclearedRunMetrics,
         unclearedCount,
         unclearedFinalMuMValues,
         unclearedSlopeValues,
         unclearedChangeValues,
         unclearedTrendValues,
         unclearedSlopeCI,
         unclearedAggregateTrend,

         share
      },


      If[
         runMetrics === {},

         Return[
            ValidationFailure[
               "InvalidRunMetrics",
               {
                  "Cannot aggregate an empty list of run metrics."
               }
            ]
         ]
      ];


      r =
         Length[
            runMetrics
         ];


      share[
         count_,
         denominator_
      ] :=
         If[
            denominator > 0,
            N[count/denominator],
            Missing["NotApplicable"]
         ];


      (* ---------------------------------------------------------- *)
      (* Standard metrics                                           *)
      (* ---------------------------------------------------------- *)

      peakValues =
         runMetrics[[
            All,
            "PeakMisinformation"
         ]];


      aucValues =
         runMetrics[[
            All,
            "AucMisinformation"
         ]];


      aucNormValues =
         runMetrics[[
            All,
            "NormalizedAucMisinformation"
         ]];


      finalMValues =
         runMetrics[[
            All,
            "FinalM"
         ]];


      finalIValues =
         runMetrics[[
            All,
            "FinalI"
         ]];


      finalMuMValues =
         runMetrics[[
            All,
            "FinalMuM"
         ]];


      finalMuIValues =
         runMetrics[[
            All,
            "FinalMuI"
         ]];


      tPeakValues =
         runMetrics[[
            All,
            "TimeOfPeakMisinformation"
         ]];


      (* ---------------------------------------------------------- *)
      (* Terminal trend over all replications                       *)
      (* ---------------------------------------------------------- *)

      terminalSlopeValues =
         Select[
            runMetrics[[
               All,
               "TerminalMisinformationSlope"
            ]],
            NumericQ
         ];


      terminalChangeValues =
         Select[
            runMetrics[[
               All,
               "TerminalMisinformationChange"
            ]],
            NumericQ
         ];


      terminalTrendValues =
         runMetrics[[
            All,
            "TerminalMisinformationTrend"
         ]];


      terminalSlopeCI =
         MeanConfidenceInterval[
            terminalSlopeValues,
            0.95
         ];


      (* ---------------------------------------------------------- *)
      (* Terminal trend among runs without complete clearance       *)
      (* ---------------------------------------------------------- *)

      unclearedRunMetrics =
         Select[
            runMetrics,
            ! TrueQ[
               #["ClearanceObservedQ"]
            ] &
         ];


      unclearedCount =
         Length[
            unclearedRunMetrics
         ];


      unclearedFinalMuMValues =
         If[
            unclearedCount > 0,

            Select[
               unclearedRunMetrics[[
                  All,
                  "FinalMuM"
               ]],
               NumericQ
            ],

            {}
         ];


      unclearedSlopeValues =
         If[
            unclearedCount > 0,

            Select[
               unclearedRunMetrics[[
                  All,
                  "TerminalMisinformationSlope"
               ]],
               NumericQ
            ],

            {}
         ];


      unclearedChangeValues =
         If[
            unclearedCount > 0,

            Select[
               unclearedRunMetrics[[
                  All,
                  "TerminalMisinformationChange"
               ]],
               NumericQ
            ],

            {}
         ];


      unclearedTrendValues =
         If[
            unclearedCount > 0,

            unclearedRunMetrics[[
               All,
               "TerminalMisinformationTrend"
            ]],

            {}
         ];


      unclearedSlopeCI =
         MeanConfidenceInterval[
            unclearedSlopeValues,
            0.95
         ];


      unclearedAggregateTrend =
         Which[

            unclearedCount == 0,

            Missing[
               "AllRunsCleared"
            ],


            unclearedCount == 1,

            First[
               unclearedTrendValues
            ],


            True,

            AggregateTrendFromCI[
               unclearedSlopeCI
            ]
         ];


      (* ---------------------------------------------------------- *)
      (* Observed event times                                       *)
      (* ---------------------------------------------------------- *)

      clearanceTimes =
         Select[
            runMetrics[[
               All,
               "ClearanceTime"
            ]],
            ObservedTimeQ
         ];


      muClearanceTimes =
         Select[
            runMetrics[[
               All,
               "MuClearanceTime"
            ]],
            ObservedTimeQ
         ];


      postPeakMuClearanceTimes =
         Select[
            runMetrics[[
               All,
               "PostPeakMuClearanceTime"
            ]],
            ObservedTimeQ
         ];


      dominanceTimes =
         Select[
            runMetrics[[
               All,
               "InformationDominanceTime"
            ]],
            ObservedTimeQ
         ];


      (* ---------------------------------------------------------- *)
      (* Aggregate output                                           *)
      (* ---------------------------------------------------------- *)

      <|

         "ReplicationCount" ->
            r,


         "PeakMisinformation" ->
            NumericMetricSummary[
               peakValues
            ],


         "AucMisinformation" ->
            NumericMetricSummary[
               aucValues
            ],


         "NormalizedAucMisinformation" ->
            NumericMetricSummary[
               aucNormValues
            ],


         "FinalM" ->
            NumericMetricSummary[
               finalMValues
            ],


         "FinalI" ->
            NumericMetricSummary[
               finalIValues
            ],


         "FinalMuM" ->
            NumericMetricSummary[
               finalMuMValues
            ],


         "FinalMuI" ->
            NumericMetricSummary[
               finalMuIValues
            ],


         "TimeOfPeakMisinformation" ->
            NumericMetricSummary[
               tPeakValues
            ],


         (* ------------------------------------------------------- *)
         (* Terminal trend: all replications                        *)
         (* ------------------------------------------------------- *)

         "TerminalMisinformationSlope" ->
            NumericMetricSummary[
               terminalSlopeValues
            ],


         "TerminalMisinformationSlope95CI" ->
            terminalSlopeCI,


         "TerminalMisinformationChange" ->
            NumericMetricSummary[
               terminalChangeValues
            ],


         "TerminalMisinformationTrend" ->
            AggregateTrendFromCI[
               terminalSlopeCI
            ],


         "TerminalDecliningRunShare" ->
            share[
               Count[
                  terminalTrendValues,
                  "Declining"
               ],
               r
            ],


         "TerminalIncreasingRunShare" ->
            share[
               Count[
                  terminalTrendValues,
                  "Increasing"
               ],
               r
            ],


         "TerminalFlatRunShare" ->
            share[
               Count[
                  terminalTrendValues,
                  "Flat"
               ],
               r
            ],


         (* ------------------------------------------------------- *)
         (* Terminal trend: only runs not cleared by Tmax           *)
         (* ------------------------------------------------------- *)

         "UnclearedRunCount" ->
            unclearedCount,


         "UnclearedRunShare" ->
            N[
               unclearedCount/r
            ],


         "UnclearedFinalMuM" ->
            NumericMetricSummary[
               unclearedFinalMuMValues
            ],


         "UnclearedTerminalMisinformationSlope" ->
            NumericMetricSummary[
               unclearedSlopeValues
            ],


         "UnclearedTerminalMisinformationSlope95CI" ->
            unclearedSlopeCI,


         "UnclearedTerminalMisinformationChange" ->
            NumericMetricSummary[
               unclearedChangeValues
            ],


         "UnclearedTerminalMisinformationTrend" ->
            unclearedAggregateTrend,


         "UnclearedTerminalDecliningRunShare" ->
            share[
               Count[
                  unclearedTrendValues,
                  "Declining"
               ],
               unclearedCount
            ],


         "UnclearedTerminalIncreasingRunShare" ->
            share[
               Count[
                  unclearedTrendValues,
                  "Increasing"
               ],
               unclearedCount
            ],


         "UnclearedTerminalFlatRunShare" ->
            share[
               Count[
                  unclearedTrendValues,
                  "Flat"
               ],
               unclearedCount
            ],


         (* ------------------------------------------------------- *)
         (* Event probabilities                                     *)
         (* ------------------------------------------------------- *)

         "ClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "ClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "MuClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "MuClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "PostPeakMuClearanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "PostPeakMuClearanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         "InformationDominanceProbability" ->
            N[
               Count[
                  runMetrics[[
                     All,
                     "InformationDominanceObservedQ"
                  ]],
                  True
               ]/
               r
            ],


         (* ------------------------------------------------------- *)
         (* Observed event times                                    *)
         (* ------------------------------------------------------- *)

         "ClearanceTimeObserved" ->
            NumericMetricSummary[
               clearanceTimes
            ],


         "MuClearanceTimeObserved" ->
            NumericMetricSummary[
               muClearanceTimes
            ],


         "PostPeakMuClearanceTimeObserved" ->
            NumericMetricSummary[
               postPeakMuClearanceTimes
            ],


         "InformationDominanceTimeObserved" ->
            NumericMetricSummary[
               dominanceTimes
            ]

      |>
   ];

End[];

EndPackage[];