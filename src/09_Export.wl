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
   ! MemberQ[$Packages, "Dezinformacije`Metrics`"],
   Get[FileNameJoin[{$SrcDir, "07_Metrics.wl"}]]
];

BeginPackage[
   "Dezinformacije`Export`",
   {
      "Dezinformacije`TypesAndValidation`",
      "Dezinformacije`Networks`",
      "Dezinformacije`Initialization`",
      "Dezinformacije`Metrics`"
   }
];


EnsureDirectory::usage =
   "EnsureDirectory[path] creates a directory if it does not already exist.";

ScenarioDirectoryName::usage =
   "ScenarioDirectoryName[scenarioID] returns a filesystem-safe scenario directory name.";

RunDirectoryName::usage =
   "RunDirectoryName[runID] returns a standardized run directory name.";


AssociationRowsToTable::usage =
   "AssociationRowsToTable[rows] converts a list of Associations to a rectangular table with a header row.";

ExportAssociationRowsCSV::usage =
   "ExportAssociationRowsCSV[rows, path] exports a list of Associations as CSV.";

ExportAssociationJSON::usage =
   "ExportAssociationJSON[assoc, path] exports an Association as JSON.";


CleanForJSON::usage =
   "CleanForJSON[x] converts Mathematica expressions to JSON-compatible expressions.";

FlattenAssociationOneLevel::usage =
   "FlattenAssociationOneLevel[assoc] flattens one level of nested Associations for CSV export.";


NodeTypeTable::usage =
   "NodeTypeTable[networkData] returns node-type and degree rows.";

SourceTable::usage =
   "SourceTable[initialCondition] returns rows for the initial misinformation source set SM.";

MetadataAssociation::usage =
   "MetadataAssociation[scenarioID, networkData, initialCondition, params] builds metadata for export.";


ExportHistoryCSV::usage =
   "ExportHistoryCSV[history, path] exports long-format state history.";

ExportTimeSeriesCSV::usage =
   "ExportTimeSeriesCSV[timeSeries, path] exports time-series metrics.";

ExportRunMetricsCSV::usage =
   "ExportRunMetricsCSV[runMetrics, path] exports one run's flat metrics.";

ExportRunMetricsJSON::usage =
   "ExportRunMetricsJSON[runMetrics, path] exports one run's metrics as JSON.";

ExportNetworkFiles::usage =
   "ExportNetworkFiles[networkData, dir] exports edge list and node types.";

ExportInitialConditionFiles::usage =
   "ExportInitialConditionFiles[initialCondition, dir] exports the initial misinformation source set.";


SaveRunOutput::usage =
   "SaveRunOutput[result, outDir, scenarioID] saves one simulation result.";

SaveReplicationOutput::usage =
   "SaveReplicationOutput[replicationResult, outDir, scenarioID] saves all replications and aggregated metrics.";

SaveExperimentManifest::usage =
   "SaveExperimentManifest[scenarioRows, path] exports a scenario manifest.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Directory and filename helpers                                         *)
(* ---------------------------------------------------------------------- *)

EnsureDirectory[
   path_String
] :=
   If[
      DirectoryQ[path],

      path,

      CreateDirectory[
         path,
         CreateIntermediateDirectories -> True
      ]
   ];


EnsureDirectory[path_] :=
   ValidationFailure[
      "InvalidDirectoryPath",
      {
         "EnsureDirectory expects a string path."
      }
   ];


ScenarioDirectoryName[
   scenarioID_
] :=
   StringReplace[
      ToString[scenarioID],
      {
         " " -> "_",
         "/" -> "_",
         "\\" -> "_",
         ":" -> "_",
         ";" -> "_",
         "," -> "_",
         "." -> "p",
         "=" -> "_",
         ">" -> "",
         "<" -> "",
         "|" -> "_",
         "\"" -> "",
         "'" -> "",
         "[" -> "_",
         "]" -> "_",
         "{" -> "_",
         "}" -> "_"
      }
   ];


RunDirectoryName[
   runID_Integer
] :=
   "run_" <>
   IntegerString[
      runID,
      10,
      6
   ];


RunDirectoryName[
   runID_
] :=
   "run_" <>
   ScenarioDirectoryName[
      runID
   ];


(* ---------------------------------------------------------------------- *)
(* CSV helpers                                                            *)
(* ---------------------------------------------------------------------- *)

AssociationRowsToTable[
   rows_List
] :=
   Module[
      {keys},

      If[
         rows === {},
         Return[{}]
      ];


      If[
         ! AllTrue[
            rows,
            AssociationQ
         ],

         Return[
            ValidationFailure[
               "InvalidAssociationRows",
               {
                  "AssociationRowsToTable expects a list of Associations."
               }
            ]
         ]
      ];


      keys =
         DeleteDuplicates @
         Flatten[
            Keys /@ rows
         ];


      Prepend[
         Table[
            Lookup[
               row,
               keys,
               Missing["NotAvailable"]
            ],
            {row, rows}
         ],
         keys
      ]
   ];


AssociationRowsToTable[
   rows_
] :=
   ValidationFailure[
      "InvalidAssociationRows",
      {
         "AssociationRowsToTable expects a list of Associations."
      }
   ];


ExportAssociationRowsCSV[
   rows_List,
   path_String
] :=
   Module[
      {table},

      table =
         AssociationRowsToTable[
            rows
         ];


      If[
         Head[table] === Failure,
         Return[table]
      ];


      EnsureDirectory[
         DirectoryName[path]
      ];


      Export[
         path,
         table,
         "CSV"
      ]
   ];


ExportAssociationRowsCSV[
   rows_,
   path_
] :=
   ValidationFailure[
      "InvalidCSVExportInput",
      {
         "ExportAssociationRowsCSV expects a list of Associations and a string path."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* JSON helpers                                                           *)
(* ---------------------------------------------------------------------- *)

CleanForJSON[
   x_Association
] :=
   Association @
   KeyValueMap[
      ToString[#1] ->
         CleanForJSON[#2] &,
      x
   ];


CleanForJSON[
   x_List
] :=
   CleanForJSON /@ x;


CleanForJSON[
   x_Rule
] :=
   ToString[
      First[x]
   ] ->
   CleanForJSON[
      Last[x]
   ];


CleanForJSON[
   x_Graph
] :=
   "<Graph>";


CleanForJSON[
   x_Missing
] :=
   ToString[x];


CleanForJSON[
   x_String
] :=
   x;


CleanForJSON[
   x_Integer
] :=
   x;


CleanForJSON[
   x_Real
] :=
   x;


CleanForJSON[
   x_Rational
] :=
   N[x];


CleanForJSON[
   x_Complex
] :=
   ToString[x];


CleanForJSON[
   x : (True | False)
] :=
   x;


CleanForJSON[
   x_Symbol
] :=
   ToString[x];


CleanForJSON[
   x_
] :=
   ToString[x];


ExportAssociationJSON[
   assoc_Association,
   path_String
] :=
   Module[
      {
         clean,
         exported
      },

      EnsureDirectory[
         DirectoryName[path]
      ];


      clean =
         CleanForJSON[
            assoc
         ];


      exported =
         Export[
            path,
            clean,
            "JSON"
         ];


      exported
   ];


ExportAssociationJSON[
   assoc_,
   path_
] :=
   ValidationFailure[
      "InvalidJSONExportInput",
      {
         "ExportAssociationJSON expects an Association and a string path."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Flattening nested summaries for CSV                                    *)
(* ---------------------------------------------------------------------- *)

FlattenAssociationOneLevel[
   assoc_Association
] :=
   Association @
   Flatten[
      KeyValueMap[
         Function[
            {
               key,
               value
            },

            If[
               AssociationQ[value],

               KeyValueMap[
                  Function[
                     {
                        subkey,
                        subvalue
                     },

                     ToString[key] <>
                     "_" <>
                     ToString[subkey] ->
                     subvalue
                  ],
                  value
               ],

               {
                  ToString[key] ->
                  value
               }
            ]
         ],
         assoc
      ]
   ];


FlattenAssociationOneLevel[
   assoc_
] :=
   ValidationFailure[
      "InvalidAssociation",
      {
         "FlattenAssociationOneLevel expects an Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Network and initial-source tables                                      *)
(* ---------------------------------------------------------------------- *)

NodeTypeTable[
   networkData_Association
] :=
   Module[
      {
         graph,
         nodeTypes,
         degreeTable,
         vertices,
         nodeType
      },

      graph =
         networkData["Graph"];

      nodeTypes =
         networkData["NodeTypes"];

      degreeTable =
         DegreeTable[
            graph
         ];

      vertices =
         VertexList[
            graph
         ];


      Table[
         nodeType =
            CanonicalNodeType[
               nodeTypes[v]
            ];

         <|
            "nodeID" ->
               v,

            "nodeType" ->
               nodeType,

            "degree" ->
               degreeTable[v],

            "isMediaNode" ->
               (nodeType === "B")
         |>,
         {v, vertices}
      ]
   ];


NodeTypeTable[
   networkData_
] :=
   ValidationFailure[
      "InvalidNetworkData",
      {
         "NodeTypeTable expects networkData Association."
      }
   ];


(* Only SM is an initial source set.
   Media nodes B are identified in node_types.csv and all of them
   become permanent official-information spreaders at campaign activation. *)

SourceTable[
   initialCondition_Association
] :=
   Table[
      <|
         "nodeID" ->
            v,

         "sourceType" ->
            "SM"
      |>,
      {
         v,
         Lookup[
            initialCondition,
            "SM",
            {}
         ]
      }
   ];


SourceTable[
   initialCondition_
] :=
   ValidationFailure[
      "InvalidInitialCondition",
      {
         "SourceTable expects initialCondition Association."
      }
   ];


MetadataAssociation[
   scenarioID_,
   networkData_Association,
   initialCondition_Association,
   params_Association
] :=
   Module[
      {
         validation,
         networkSummary,
         initialSummary,
         sourceSpec
      },


      validation =
         ValidateParameters[
            params
         ];

      If[
         ! ValidationSucceededQ[validation],
         Return[validation]
      ];


      networkSummary =
         NetworkSummary[
            networkData
         ];

      If[
         Head[networkSummary] === Failure,
         Return[networkSummary]
      ];


      initialSummary =
         InitialConditionSummary[
            initialCondition
         ];

      If[
         Head[initialSummary] === Failure,
         Return[initialSummary]
      ];


      sourceSpec =
         Lookup[
            initialCondition,
            "SourceSpec",
            <||>
         ];


      <|
         "ScenarioID" ->
            scenarioID,

         "ExportDateString" ->
            DateString[
               {
                  "Year",
                  "-",
                  "Month",
                  "-",
                  "Day",
                  " ",
                  "Hour",
                  ":",
                  "Minute",
                  ":",
                  "Second"
               }
            ],

         "Parameters" ->
            params,

         "NetworkSpec" ->
            Lookup[
               networkData,
               "NetworkSpec",
               <||>
            ],

         "SourceSpec" ->
            sourceSpec,

         "NetworkSummary" ->
            networkSummary,

         "InitialConditionSummary" ->
            initialSummary
      |>
   ];


MetadataAssociation[
   scenarioID_,
   networkData_,
   initialCondition_,
   params_
] :=
   ValidationFailure[
      "InvalidMetadataInput",
      {
         "MetadataAssociation expects scenarioID, networkData, initialCondition, and params."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Specific exports                                                       *)
(* ---------------------------------------------------------------------- *)

ExportHistoryCSV[
   history_List,
   path_String
] :=
   ExportAssociationRowsCSV[
      history,
      path
   ];


ExportHistoryCSV[
   history_,
   path_
] :=
   ValidationFailure[
      "InvalidHistoryExportInput",
      {
         "ExportHistoryCSV expects history list and path string."
      }
   ];


ExportTimeSeriesCSV[
   timeSeries_List,
   path_String
] :=
   ExportAssociationRowsCSV[
      timeSeries,
      path
   ];


ExportTimeSeriesCSV[
   timeSeries_,
   path_
] :=
   ValidationFailure[
      "InvalidTimeSeriesExportInput",
      {
         "ExportTimeSeriesCSV expects timeSeries list and path string."
      }
   ];


ExportRunMetricsCSV[
   runMetrics_Association,
   path_String
] :=
   ExportAssociationRowsCSV[
      MetricLongTable[
         {runMetrics}
      ],
      path
   ];


ExportRunMetricsCSV[
   runMetrics_,
   path_
] :=
   ValidationFailure[
      "InvalidRunMetricsExportInput",
      {
         "ExportRunMetricsCSV expects runMetrics Association and path string."
      }
   ];


ExportRunMetricsJSON[
   runMetrics_Association,
   path_String
] :=
   ExportAssociationJSON[
      KeyDrop[
         runMetrics,
         "TimeSeries"
      ],
      path
   ];


ExportRunMetricsJSON[
   runMetrics_,
   path_
] :=
   ValidationFailure[
      "InvalidRunMetricsExportInput",
      {
         "ExportRunMetricsJSON expects runMetrics Association and path string."
      }
   ];


ExportNetworkFiles[
   networkData_Association,
   dir_String
] :=
   Module[
      {
         edgePath,
         nodeTypePath
      },

      EnsureDirectory[
         dir
      ];


      edgePath =
         FileNameJoin[
            {
               dir,
               "edges.csv"
            }
         ];


      nodeTypePath =
         FileNameJoin[
            {
               dir,
               "node_types.csv"
            }
         ];


      ExportEdgeList[
         networkData["Graph"],
         edgePath
      ];


      ExportAssociationRowsCSV[
         NodeTypeTable[
            networkData
         ],
         nodeTypePath
      ];


      <|
         "EdgeListPath" ->
            edgePath,

         "NodeTypesPath" ->
            nodeTypePath
      |>
   ];


ExportNetworkFiles[
   networkData_,
   dir_
] :=
   ValidationFailure[
      "InvalidNetworkExportInput",
      {
         "ExportNetworkFiles expects networkData Association and directory path."
      }
   ];


ExportInitialConditionFiles[
   initialCondition_Association,
   dir_String
] :=
   Module[
      {
         sourcePath
      },

      EnsureDirectory[
         dir
      ];


      sourcePath =
         FileNameJoin[
            {
               dir,
               "sources.csv"
            }
         ];


      ExportAssociationRowsCSV[
         SourceTable[
            initialCondition
         ],
         sourcePath
      ];


      <|
         "SourcesPath" ->
            sourcePath
      |>
   ];


ExportInitialConditionFiles[
   initialCondition_,
   dir_
] :=
   ValidationFailure[
      "InvalidInitialConditionExportInput",
      {
         "ExportInitialConditionFiles expects initialCondition Association and directory path."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Save one run                                                           *)
(* ---------------------------------------------------------------------- *)

SaveRunOutput[
   result_Association,
   outDir_String,
   scenarioID_ : "scenario"
] :=
   Module[
      {
         scenarioDir,
         runDir,
         networkDir,
         metadataDir,
         runID,
         runMetrics,
         historyPath,
         timeSeriesPath,
         runMetricsCSVPath,
         runMetricsJSONPath,
         metadataPath,
         networkFiles,
         sourceFiles,
         metadata,
         jsonExport1,
         jsonExport2
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
                  "SaveRunOutput expects simulation result with key \"History\"."
               }
            ]
         ]
      ];


      runID =
         Lookup[
            result,
            "RunID",
            1
         ];


      scenarioDir =
         FileNameJoin[
            {
               outDir,
               ScenarioDirectoryName[
                  scenarioID
               ]
            }
         ];


      runDir =
         FileNameJoin[
            {
               scenarioDir,
               RunDirectoryName[
                  runID
               ]
            }
         ];


      networkDir =
         FileNameJoin[
            {
               scenarioDir,
               "network"
            }
         ];


      metadataDir =
         FileNameJoin[
            {
               scenarioDir,
               "metadata"
            }
         ];


      EnsureDirectory[
         runDir
      ];

      EnsureDirectory[
         networkDir
      ];

      EnsureDirectory[
         metadataDir
      ];


      runMetrics =
         ComputeRunMetrics[
            result
         ];

      If[
         Head[runMetrics] === Failure,
         Return[runMetrics]
      ];


      historyPath =
         FileNameJoin[
            {
               runDir,
               "history.csv"
            }
         ];


      timeSeriesPath =
         FileNameJoin[
            {
               runDir,
               "time_series.csv"
            }
         ];


      runMetricsCSVPath =
         FileNameJoin[
            {
               runDir,
               "run_metrics.csv"
            }
         ];


      runMetricsJSONPath =
         FileNameJoin[
            {
               runDir,
               "run_metrics.json"
            }
         ];


      metadataPath =
         FileNameJoin[
            {
               metadataDir,
               "metadata.json"
            }
         ];


      ExportHistoryCSV[
         result["History"],
         historyPath
      ];


      ExportTimeSeriesCSV[
         runMetrics["TimeSeries"],
         timeSeriesPath
      ];


      ExportRunMetricsCSV[
         runMetrics,
         runMetricsCSVPath
      ];


      jsonExport1 =
         ExportRunMetricsJSON[
            runMetrics,
            runMetricsJSONPath
         ];


      networkFiles =
         ExportNetworkFiles[
            result["NetworkData"],
            networkDir
         ];


      sourceFiles =
         ExportInitialConditionFiles[
            result["InitialCondition"],
            metadataDir
         ];


      metadata =
         MetadataAssociation[
            scenarioID,
            result["NetworkData"],
            result["InitialCondition"],
            result["Parameters"]
         ];


      If[
         Head[metadata] === Failure,
         Return[metadata]
      ];


      jsonExport2 =
         ExportAssociationJSON[
            metadata,
            metadataPath
         ];


      <|
         "ScenarioDirectory" ->
            scenarioDir,

         "RunDirectory" ->
            runDir,

         "HistoryPath" ->
            historyPath,

         "TimeSeriesPath" ->
            timeSeriesPath,

         "RunMetricsCSVPath" ->
            runMetricsCSVPath,

         "RunMetricsJSONPath" ->
            runMetricsJSONPath,

         "MetadataPath" ->
            metadataPath,

         "NetworkFiles" ->
            networkFiles,

         "SourceFiles" ->
            sourceFiles,

         "RunMetricsJSONExportResult" ->
            jsonExport1,

         "MetadataJSONExportResult" ->
            jsonExport2
      |>
   ];


SaveRunOutput[
   result_,
   outDir_,
   scenarioID_ : "scenario"
] :=
   ValidationFailure[
      "InvalidSaveRunInput",
      {
         "SaveRunOutput expects simulation result, output directory, and optional scenarioID."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Save replications                                                      *)
(* ---------------------------------------------------------------------- *)

SaveReplicationOutput[
   replicationResult_Association,
   outDir_String,
   scenarioID_ : "scenario"
] :=
   Module[
      {
         scenarioDir,
         runsDir,
         summaryDir,
         metadataDir,
         networkDir,
         results,
         runMetrics,
         metricRows,
         aggregate,
         runExports,
         runMetricsAllPath,
         aggregatePath,
         aggregateJSONPath,
         metadataPath,
         networkFiles,
         sourceFiles,
         metadata,
         jsonExport1,
         jsonExport2
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
                  "SaveReplicationOutput expects replicationResult with key \"Replications\"."
               }
            ]
         ]
      ];


      results =
         replicationResult[
            "Replications"
         ];


      If[
         results === {},

         Return[
            ValidationFailure[
               "InvalidReplicationResult",
               {
                  "Replication result contains an empty Replications list."
               }
            ]
         ]
      ];


      scenarioDir =
         FileNameJoin[
            {
               outDir,
               ScenarioDirectoryName[
                  scenarioID
               ]
            }
         ];


      runsDir =
         FileNameJoin[
            {
               scenarioDir,
               "runs"
            }
         ];


      summaryDir =
         FileNameJoin[
            {
               scenarioDir,
               "summaries"
            }
         ];


      metadataDir =
         FileNameJoin[
            {
               scenarioDir,
               "metadata"
            }
         ];


      networkDir =
         FileNameJoin[
            {
               scenarioDir,
               "network"
            }
         ];


      EnsureDirectory[
         runsDir
      ];

      EnsureDirectory[
         summaryDir
      ];

      EnsureDirectory[
         metadataDir
      ];

      EnsureDirectory[
         networkDir
      ];


      runMetrics =
         ComputeReplicationMetrics[
            replicationResult
         ];


      If[
         Head[runMetrics] === Failure,
         Return[runMetrics]
      ];


      metricRows =
         MetricLongTable[
            runMetrics
         ];


      If[
         Head[metricRows] === Failure,
         Return[metricRows]
      ];


      aggregate =
         AggregateReplicationMetrics[
            runMetrics
         ];


      If[
         Head[aggregate] === Failure,
         Return[aggregate]
      ];


      runExports =
         Table[
            Module[
               {
                  runResult,
                  runID,
                  runDir,
                  oneMetrics,
                  historyPath,
                  timeSeriesPath,
                  runMetricsPath
               },

               runResult =
                  results[[i]];


               runID =
                  runResult["RunID"];


               runDir =
                  FileNameJoin[
                     {
                        runsDir,
                        RunDirectoryName[
                           runID
                        ]
                     }
                  ];


               EnsureDirectory[
                  runDir
               ];


               oneMetrics =
                  runMetrics[[i]];


               historyPath =
                  FileNameJoin[
                     {
                        runDir,
                        "history.csv"
                     }
                  ];


               timeSeriesPath =
                  FileNameJoin[
                     {
                        runDir,
                        "time_series.csv"
                     }
                  ];


               runMetricsPath =
                  FileNameJoin[
                     {
                        runDir,
                        "run_metrics.csv"
                     }
                  ];


               ExportHistoryCSV[
                  runResult["History"],
                  historyPath
               ];


               ExportTimeSeriesCSV[
                  oneMetrics["TimeSeries"],
                  timeSeriesPath
               ];


               ExportRunMetricsCSV[
                  oneMetrics,
                  runMetricsPath
               ];


               <|
                  "RunID" ->
                     runID,

                  "RunDirectory" ->
                     runDir,

                  "HistoryPath" ->
                     historyPath,

                  "TimeSeriesPath" ->
                     timeSeriesPath,

                  "RunMetricsPath" ->
                     runMetricsPath
               |>
            ],
            {
               i,
               Length[results]
            }
         ];


      runMetricsAllPath =
         FileNameJoin[
            {
               summaryDir,
               "run_metrics_all.csv"
            }
         ];


      aggregatePath =
         FileNameJoin[
            {
               summaryDir,
               "aggregate_metrics.csv"
            }
         ];


      aggregateJSONPath =
         FileNameJoin[
            {
               summaryDir,
               "aggregate_metrics.json"
            }
         ];


      metadataPath =
         FileNameJoin[
            {
               metadataDir,
               "metadata.json"
            }
         ];


      ExportAssociationRowsCSV[
         metricRows,
         runMetricsAllPath
      ];


      ExportAssociationRowsCSV[
         {
            FlattenAssociationOneLevel[
               aggregate
            ]
         },
         aggregatePath
      ];


      jsonExport1 =
         ExportAssociationJSON[
            aggregate,
            aggregateJSONPath
         ];


      networkFiles =
         ExportNetworkFiles[
            replicationResult["NetworkData"],
            networkDir
         ];


      sourceFiles =
         ExportInitialConditionFiles[
            results[[1]][
               "InitialCondition"
            ],
            metadataDir
         ];


      metadata =
         MetadataAssociation[
            scenarioID,
            replicationResult[
               "NetworkData"
            ],
            results[[1]][
               "InitialCondition"
            ],
            replicationResult[
               "Parameters"
            ]
         ];


      If[
         Head[metadata] === Failure,
         Return[metadata]
      ];


      jsonExport2 =
         ExportAssociationJSON[
            metadata,
            metadataPath
         ];


      <|
         "ScenarioDirectory" ->
            scenarioDir,

         "RunsDirectory" ->
            runsDir,

         "SummaryDirectory" ->
            summaryDir,

         "MetadataDirectory" ->
            metadataDir,

         "RunExports" ->
            runExports,

         "RunMetricsAllPath" ->
            runMetricsAllPath,

         "AggregateMetricsCSVPath" ->
            aggregatePath,

         "AggregateMetricsJSONPath" ->
            aggregateJSONPath,

         "MetadataPath" ->
            metadataPath,

         "NetworkFiles" ->
            networkFiles,

         "SourceFiles" ->
            sourceFiles,

         "AggregateMetricsJSONExportResult" ->
            jsonExport1,

         "MetadataJSONExportResult" ->
            jsonExport2
      |>
   ];


SaveReplicationOutput[
   replicationResult_,
   outDir_,
   scenarioID_ : "scenario"
] :=
   ValidationFailure[
      "InvalidSaveReplicationInput",
      {
         "SaveReplicationOutput expects replication result, output directory, and optional scenarioID."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Experiment manifest                                                    *)
(* ---------------------------------------------------------------------- *)

SaveExperimentManifest[
   scenarioRows_List,
   path_String
] :=
   ExportAssociationRowsCSV[
      scenarioRows,
      path
   ];


SaveExperimentManifest[
   scenarioRows_,
   path_
] :=
   ValidationFailure[
      "InvalidManifestInput",
      {
         "SaveExperimentManifest expects a list of scenario rows and a path."
      }
   ];


End[];

EndPackage[];