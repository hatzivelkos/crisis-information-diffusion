(* ::Package:: *)

If[
   ! MemberQ[$Packages, "Dezinformacije`TypesAndValidation`"],
   Get[FileNameJoin[{$SrcDir, "01_TypesAndValidation.wl"}]]
];

BeginPackage[
   "Dezinformacije`Networks`",
   {"Dezinformacije`TypesAndValidation`"}
];


GenerateNetwork::usage =
   "GenerateNetwork[netSpec] generates a base graph from a network specification Association.";

BuildNetworkData::usage =
   "BuildNetworkData[netSpec] generates a graph, assigns ordinary-agent and media-node types, and optionally boosts media-node connectivity.";

AssignNodeTypes::usage =
   "AssignNodeTypes[graph, nMedia, method] assigns ordinary-agent nodes A and media/institutional nodes B.";

NormalizeNodeTypes::usage =
   "NormalizeNodeTypes[nodeTypes] converts node-type labels to canonical labels A and B.";


GenerateRandomNetwork::usage =
   "GenerateRandomNetwork[n, p] generates a random undirected graph.";

GenerateSmallWorldNetwork::usage =
   "GenerateSmallWorldNetwork[n, k, beta] generates a simple small-world graph.";

GenerateScaleFreeNetwork::usage =
   "GenerateScaleFreeNetwork[n, m] generates a simple preferential-attachment graph.";

GenerateModularNetwork::usage =
   "GenerateModularNetwork[n, communities, pIn, pOut] generates a modular random graph.";


BoostMediaConnectivity::usage =
   "BoostMediaConnectivity[graph, mediaNodes, agentNodes, targetDegree] adds edges so that media nodes have increased connectivity.";

NetworkSummary::usage =
   "NetworkSummary[networkData] returns basic network and node-type summary statistics.";

DegreeTable::usage =
   "DegreeTable[graph] returns an Association node -> degree.";

EdgeListTable::usage =
   "EdgeListTable[graph] returns an edge list table with columns source and target.";

ExportEdgeList::usage =
   "ExportEdgeList[graph, path] exports the graph edge list as CSV.";


Begin["`Private`"];


(* ---------------------------------------------------------------------- *)
(* Utility functions                                                      *)
(* ---------------------------------------------------------------------- *)

SortedPair[
   u_,
   v_
] :=
   Sort[
      {u, v}
   ];


PairToEdge[
   {u_, v_}
] :=
   UndirectedEdge[
      u,
      v
   ];


EdgesFromPairs[
   pairs_List
] :=
   PairToEdge /@
      DeleteDuplicates[
         Sort /@ pairs
      ];


SimpleGraphFromPairs[
   n_Integer,
   pairs_List
] :=
   Graph[
      Range[n],
      EdgesFromPairs[pairs],
      VertexLabels -> None
   ];


DegreeTable[
   graph_Graph
] :=
   AssociationThread[
      VertexList[graph] ->
         (
            VertexDegree[
               graph,
               #
            ] & /@
            VertexList[graph]
         )
   ];


NormalizeNodeTypes[
   nodeTypes_Association
] :=
   AssociationMap[
      CanonicalNodeType[
         nodeTypes[#]
      ] &,
      Keys[nodeTypes]
   ];


TakeLargestByDegree[
   graph_Graph,
   k_Integer
] :=
   Module[
      {
         vertices,
         degrees
      },

      vertices =
         VertexList[
            graph
         ];

      degrees =
         DegreeTable[
            graph
         ];


      Take[
         SortBy[
            vertices,
            {
               -degrees[#],
               #
            } &
         ],
         UpTo[k]
      ]
   ];


NeighbourNodesFromEdges[
   graph_Graph,
   v_
] :=
   Module[
      {
         pairs
      },

      pairs =
         List @@@
         EdgeList[
            graph
         ];


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
         ]
   ];


(* ---------------------------------------------------------------------- *)
(* Network generators                                                     *)
(* ---------------------------------------------------------------------- *)

GenerateRandomNetwork[
   n_Integer,
   p_?ValidProbabilityQ
] :=
   Module[
      {
         pairs
      },

      pairs =
         Select[
            Subsets[
               Range[n],
               {2}
            ],
            RandomReal[] <= p &
         ];


      SimpleGraphFromPairs[
         n,
         pairs
      ]
   ];


GenerateSmallWorldNetwork[
   n_Integer,
   k_Integer,
   beta_?ValidProbabilityQ
] :=
   Module[
      {
         kk,
         initialPairs,
         currentPairs,
         pair,
         u,
         v,
         candidates,
         newV,
         i,
         offset
      },

      kk =
         If[
            OddQ[k],
            k - 1,
            k
         ];

      kk =
         Max[
            2,
            Min[
               kk,
               n - 1
            ]
         ];


      initialPairs =
         DeleteDuplicates @
         Flatten[
            Table[
               SortedPair[
                  i,
                  1 + Mod[
                     i - 1 + offset,
                     n
                  ]
               ],
               {i, 1, n},
               {
                  offset,
                  1,
                  Floor[kk/2]
               }
            ],
            1
         ];


      currentPairs =
         initialPairs;


      Do[
         pair =
            initialPairs[[i]];

         {
            u,
            v
         } =
            pair;


         If[
            RandomReal[] <= beta,

            currentPairs =
               DeleteCases[
                  currentPairs,
                  pair
               ];


            candidates =
               Complement[
                  Range[n],
                  {u},
                  Cases[
                     currentPairs,
                     {u, x_} :> x
                  ],
                  Cases[
                     currentPairs,
                     {x_, u} :> x
                  ]
               ];


            If[
               candidates =!= {},

               newV =
                  RandomChoice[
                     candidates
                  ];

               currentPairs =
                  Append[
                     currentPairs,
                     SortedPair[
                        u,
                        newV
                     ]
                  ],

               currentPairs =
                  Append[
                     currentPairs,
                     pair
                  ]
            ];
         ],

         {
            i,
            Length[
               initialPairs
            ]
         }
      ];


      SimpleGraphFromPairs[
         n,
         currentPairs
      ]
   ];


GenerateScaleFreeNetwork[
   n_Integer,
   m_Integer
] :=
   Module[
      {
         mm,
         m0,
         pairs,
         graph,
         newNode,
         candidates,
         targets,
         chosen,
         degrees,
         weights
      },

      mm =
         Max[
            1,
            Min[
               m,
               n - 1
            ]
         ];


      m0 =
         Min[
            n,
            mm + 1
         ];


      pairs =
         Subsets[
            Range[m0],
            {2}
         ];


      Do[
         targets = {};


         While[
            Length[targets] <
            Min[
               mm,
               newNode - 1
            ],

            graph =
               SimpleGraphFromPairs[
                  newNode - 1,
                  pairs
               ];


            candidates =
               Complement[
                  Range[
                     newNode - 1
                  ],
                  targets
               ];


            degrees =
               AssociationThread[
                  Range[
                     newNode - 1
                  ] ->
                     (
                        VertexDegree[
                           graph,
                           #
                        ] & /@
                        Range[
                           newNode - 1
                        ]
                     )
               ];


            weights =
               Lookup[
                  degrees,
                  candidates
               ];


            chosen =
               If[
                  Total[weights] == 0,

                  RandomChoice[
                     candidates
                  ],

                  RandomChoice[
                     weights ->
                        candidates
                  ]
               ];


            targets =
               Append[
                  targets,
                  chosen
               ];
         ];


         pairs =
            Join[
               pairs,
               SortedPair[
                  newNode,
                  #
               ] & /@
               targets
            ],

         {
            newNode,
            m0 + 1,
            n
         }
      ];


      SimpleGraphFromPairs[
         n,
         pairs
      ]
   ];


GenerateModularNetwork[
   n_Integer,
   communities_Integer,
   pIn_?ValidProbabilityQ,
   pOut_?ValidProbabilityQ
] :=
   Module[
      {
         communityOf,
         pairs
      },

      communityOf =
         AssociationThread[
            Range[n] ->
               Table[
                  1 +
                  Mod[
                     i - 1,
                     communities
                  ],
                  {i, 1, n}
               ]
         ];


      pairs =
         Select[
            Subsets[
               Range[n],
               {2}
            ],

            If[
               communityOf[#[[1]]] ===
               communityOf[#[[2]]],

               RandomReal[] <= pIn,

               RandomReal[] <= pOut
            ] &
         ];


      SimpleGraphFromPairs[
         n,
         pairs
      ]
   ];


(* ---------------------------------------------------------------------- *)
(* Base network interface                                                 *)
(* ---------------------------------------------------------------------- *)

GenerateNetwork[
   netSpec_Association
] :=
   Module[
      {
         validation,
         networkType,
         n,
         p,
         k,
         beta,
         m,
         communities,
         pIn,
         pOut,
         graph
      },

      validation =
         ValidateNetworkSpec[
            netSpec
         ];


      If[
         ! ValidationSucceededQ[
            validation
         ],
         Return[validation]
      ];


      If[
         KeyExistsQ[
            netSpec,
            "Seed"
         ],

         SeedRandom[
            netSpec["Seed"]
         ]
      ];


      networkType =
         netSpec[
            "NetworkType"
         ];

      n =
         netSpec["n"];


      graph =
         Switch[

            networkType,


            "Random",

               p =
                  Lookup[
                     netSpec,
                     "p",
                     Lookup[
                        netSpec,
                        "averageDegree",
                        6
                     ]/
                     Max[
                        n - 1,
                        1
                     ]
                  ];

               p =
                  Min[
                     Max[
                        N[p],
                        0
                     ],
                     1
                  ];

               GenerateRandomNetwork[
                  n,
                  p
               ],


            "SmallWorld",

               k =
                  Lookup[
                     netSpec,
                     "k",
                     6
                  ];

               beta =
                  Lookup[
                     netSpec,
                     "rewiringProbability",
                     0.1
                  ];

               GenerateSmallWorldNetwork[
                  n,
                  k,
                  beta
               ],


            "ScaleFree",

               m =
                  Lookup[
                     netSpec,
                     "m",
                     Lookup[
                        netSpec,
                        "attachmentNumber",
                        2
                     ]
                  ];

               GenerateScaleFreeNetwork[
                  n,
                  m
               ],


            "Modular",

               communities =
                  Lookup[
                     netSpec,
                     "communities",
                     4
                  ];

               pIn =
                  Lookup[
                     netSpec,
                     "pIn",
                     0.08
                  ];

               pOut =
                  Lookup[
                     netSpec,
                     "pOut",
                     0.005
                  ];

               GenerateModularNetwork[
                  n,
                  communities,
                  pIn,
                  pOut
               ],


            "Imported",

               If[
                  KeyExistsQ[
                     netSpec,
                     "Graph"
                  ] &&
                  Head[
                     netSpec["Graph"]
                  ] === Graph,

                  netSpec["Graph"],

                  Return[
                     ValidationFailure[
                        "InvalidImportedNetwork",
                        {
                           "For NetworkType -> \"Imported\", netSpec must contain key \"Graph\" with a Graph value."
                        }
                     ]
                  ]
               ],


            _,

               Return[
                  ValidationFailure[
                     "InvalidNetworkType",
                     {
                        "Unsupported network type: " <>
                        ToString[
                           networkType
                        ]
                     }
                  ]
               ]
         ];


      graph
   ];


GenerateNetwork[
   netSpec_
] :=
   ValidationFailure[
      "InvalidNetworkSpec",
      {
         "GenerateNetwork expects a network specification Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Node-type assignment                                                   *)
(* ---------------------------------------------------------------------- *)

AssignNodeTypes[
   graph_Graph,
   nMedia_Integer,
   method_ : "TopDegree"
] :=
   Module[
      {
         vertices,
         mediaNodes
      },

      vertices =
         VertexList[
            graph
         ];


      (* The model requires at least one ordinary agent A
         and at least one media/institutional node B. *)

      If[
         nMedia < 1 ||
         nMedia >= Length[vertices],

         Return[
            ValidationFailure[
               "InvalidMediaNodeCount",
               {
                  "nMedia must be at least 1 and smaller than the total number of vertices."
               }
            ]
         ]
      ];


      mediaNodes =
         Switch[

            method,


            "TopDegree",

               TakeLargestByDegree[
                  graph,
                  nMedia
               ],


            "Random",

               RandomSample[
                  vertices,
                  nMedia
               ],


            "First",

               Take[
                  vertices,
                  nMedia
               ],


            _,

               Return[
                  ValidationFailure[
                     "InvalidNodeTypeAssignmentMethod",
                     {
                        "Unsupported node-type assignment method: " <>
                        ToString[
                           method
                        ]
                     }
                  ]
               ]
         ];


      AssociationThread[
         vertices,
         If[
            MemberQ[
               mediaNodes,
               #
            ],
            "B",
            "A"
         ] & /@
         vertices
      ]
   ];


AssignNodeTypes[
   graph_Graph,
   nodeTypes_Association
] :=
   Module[
      {
         normalized,
         validation,
         agents,
         mediaNodes
      },

      normalized =
         NormalizeNodeTypes[
            nodeTypes
         ];


      validation =
         ValidateNodeTypes[
            graph,
            normalized
         ];


      If[
         ! ValidationSucceededQ[
            validation
         ],
         Return[validation]
      ];


      agents =
         AgentNodes[
            normalized
         ];

      mediaNodes =
         MediaNodes[
            normalized
         ];


      If[
         agents === {},

         Return[
            ValidationFailure[
               "InvalidNodeTypes",
               {
                  "The network must contain at least one ordinary-agent node A."
               }
            ]
         ]
      ];


      If[
         mediaNodes === {},

         Return[
            ValidationFailure[
               "InvalidNodeTypes",
               {
                  "The network must contain at least one media or institutional node B."
               }
            ]
         ]
      ];


      normalized
   ];


AssignNodeTypes[
   graph_,
   nMedia_,
   method_ : "TopDegree"
] :=
   ValidationFailure[
      "InvalidNodeTypeAssignment",
      {
         "AssignNodeTypes expects a Graph and either nMedia or a node-type Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Media-connectivity boost                                               *)
(* ---------------------------------------------------------------------- *)

BoostMediaConnectivity[
   graph_Graph,
   mediaNodes_List,
   agentNodes_List,
   targetDegree_Integer
] :=
   Module[
      {
         boostedGraph,
         vertices,
         currentDegree,
         needed,
         neighbours,
         candidates,
         chosen,
         newEdges
      },

      boostedGraph =
         graph;

      vertices =
         VertexList[
            graph
         ];


      Do[
         currentDegree =
            VertexDegree[
               boostedGraph,
               b
            ];


         needed =
            Max[
               0,
               targetDegree -
               currentDegree
            ];


         neighbours =
            NeighbourNodesFromEdges[
               boostedGraph,
               b
            ];


         candidates =
            Complement[
               agentNodes,
               {b},
               neighbours
            ];


         chosen =
            If[
               needed > 0 &&
               candidates =!= {},

               RandomSample[
                  candidates,
                  Min[
                     needed,
                     Length[candidates]
                  ]
               ],

               {}
            ];


         newEdges =
            UndirectedEdge[
               b,
               #
            ] & /@
            chosen;


         boostedGraph =
            Graph[
               vertices,

               DeleteDuplicates @
               Join[
                  EdgeList[
                     boostedGraph
                  ],
                  newEdges
               ],

               VertexLabels ->
                  None
            ],

         {
            b,
            mediaNodes
         }
      ];


      boostedGraph
   ];


BoostMediaConnectivity[
   graph_,
   mediaNodes_,
   agentNodes_,
   targetDegree_
] :=
   ValidationFailure[
      "InvalidMediaConnectivityBoost",
      {
         "BoostMediaConnectivity expects a Graph, media node list, agent node list, and integer target degree."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Standard network-data object                                           *)
(* ---------------------------------------------------------------------- *)

BuildNetworkData[
   netSpec_Association
] :=
   Module[
      {
         graph,
         nodeTypes,
         validation,
         nMedia,
         method,
         agents,
         mediaNodes,
         boostQ,
         meanDegree,
         targetDegree
      },


      validation =
         ValidateNetworkSpec[
            netSpec
         ];


      If[
         ! ValidationSucceededQ[
            validation
         ],
         Return[validation]
      ];


      graph =
         GenerateNetwork[
            netSpec
         ];


      If[
         Head[graph] === Failure,
         Return[graph]
      ];


      (* Assign ordinary-agent and media-node types *)

      If[
         KeyExistsQ[
            netSpec,
            "NodeTypes"
         ],

         nodeTypes =
            AssignNodeTypes[
               graph,
               netSpec["NodeTypes"]
            ],

         nMedia =
            netSpec["nMedia"];

         method =
            Lookup[
               netSpec,
               "MediaAssignmentMethod",
               "TopDegree"
            ];

         nodeTypes =
            AssignNodeTypes[
               graph,
               nMedia,
               method
            ]
      ];


      If[
         Head[nodeTypes] === Failure,
         Return[nodeTypes]
      ];


      validation =
         ValidateNodeTypes[
            graph,
            nodeTypes
         ];


      If[
         ! ValidationSucceededQ[
            validation
         ],
         Return[validation]
      ];


      agents =
         AgentNodes[
            nodeTypes
         ];

      mediaNodes =
         MediaNodes[
            nodeTypes
         ];


      If[
         agents === {},

         Return[
            ValidationFailure[
               "InvalidNetworkData",
               {
                  "The generated network contains no ordinary-agent nodes A."
               }
            ]
         ]
      ];


      If[
         mediaNodes === {},

         Return[
            ValidationFailure[
               "InvalidNetworkData",
               {
                  "The generated network contains no media or institutional nodes B."
               }
            ]
         ]
      ];


      (* Optionally increase media-node connectivity.
         This changes only network structure; it does not assign
         any information state or source status. *)

      boostQ =
         Lookup[
            netSpec,
            "BoostMediaConnectivity",
            True
         ];


      If[
         TrueQ[boostQ],

         If[
            KeyExistsQ[
               netSpec,
               "MediaBoostSeed"
            ],

            SeedRandom[
               netSpec[
                  "MediaBoostSeed"
               ]
            ]
         ];


         meanDegree =
            Mean[
               VertexDegree[
                  graph
               ]
            ];


         targetDegree =
            Lookup[
               netSpec,
               "MediaTargetDegree",

               Ceiling[
                  Lookup[
                     netSpec,
                     "MediaDegreeMultiplier",
                     3
                  ] *
                  meanDegree
               ]
            ];


         targetDegree =
            Min[
               Max[
                  targetDegree,
                  0
               ],
               VertexCount[
                  graph
               ] - 1
            ];


         graph =
            BoostMediaConnectivity[
               graph,
               mediaNodes,
               agents,
               targetDegree
            ];
      ];


      (* Node assignments must remain valid after structural modification *)

      validation =
         ValidateNodeTypes[
            graph,
            nodeTypes
         ];


      If[
         ! ValidationSucceededQ[
            validation
         ],
         Return[validation]
      ];


      agents =
         AgentNodes[
            nodeTypes
         ];

      mediaNodes =
         MediaNodes[
            nodeTypes
         ];


      <|
         "Graph" ->
            graph,

         "NodeTypes" ->
            nodeTypes,

         "Agents" ->
            agents,

         "MediaNodes" ->
            mediaNodes,

         "NetworkSpec" ->
            netSpec
      |>
   ];


BuildNetworkData[
   netSpec_
] :=
   ValidationFailure[
      "InvalidNetworkSpec",
      {
         "BuildNetworkData expects a network specification Association."
      }
   ];


(* ---------------------------------------------------------------------- *)
(* Summary and export helpers                                             *)
(* ---------------------------------------------------------------------- *)

NetworkSummary[
   networkData_Association
] :=
   Module[
      {
         graph,
         nodeTypes,
         agents,
         mediaNodes,
         n,
         e,
         degrees,
         agentDegrees,
         mediaDegrees
      },

      graph =
         networkData["Graph"];

      nodeTypes =
         networkData["NodeTypes"];


      agents =
         AgentNodes[
            nodeTypes
         ];

      mediaNodes =
         MediaNodes[
            nodeTypes
         ];


      n =
         VertexCount[
            graph
         ];

      e =
         EdgeCount[
            graph
         ];

      degrees =
         VertexDegree[
            graph
         ];


      agentDegrees =
         VertexDegree[
            graph,
            #
         ] & /@
         agents;


      mediaDegrees =
         VertexDegree[
            graph,
            #
         ] & /@
         mediaNodes;


      <|
         "VertexCount" ->
            n,

         "EdgeCount" ->
            e,

         "Density" ->
            N[
               GraphDensity[
                  graph
               ]
            ],

         "MeanDegree" ->
            N[
               Mean[
                  degrees
               ]
            ],

         "MaxDegree" ->
            Max[
               degrees
            ],

         "AgentCount" ->
            Length[
               agents
            ],

         "MediaNodeCount" ->
            Length[
               mediaNodes
            ],

         "MeanAgentDegree" ->
            N[
               Mean[
                  agentDegrees
               ]
            ],

         "MeanMediaDegree" ->
            N[
               Mean[
                  mediaDegrees
               ]
            ],

         "MinMediaDegree" ->
            Min[
               mediaDegrees
            ],

         "MaxMediaDegree" ->
            Max[
               mediaDegrees
            ],

         "ConnectedGraphQ" ->
            ConnectedGraphQ[
               graph
            ]
      |>
   ];


NetworkSummary[
   networkData_
] :=
   ValidationFailure[
      "InvalidNetworkData",
      {
         "NetworkSummary expects networkData Association."
      }
   ];


EdgeListTable[
   graph_Graph
] :=
   Module[
      {
         edges
      },

      edges =
         List @@@
         EdgeList[
            graph
         ];


      Prepend[
         edges,
         {
            "source",
            "target"
         }
      ]
   ];


ExportEdgeList[
   graph_Graph,
   path_String
] :=
   Export[
      path,
      EdgeListTable[
         graph
      ],
      "CSV"
   ];


End[];

EndPackage[];