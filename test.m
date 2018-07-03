BeginPackage["Tablet`"]

SwapPaths::usage =
    "Giving the length of sequence, provide all possible paths."

FullPathShorten::usage = 
    "Giving a path returned by path finding, give back each pair
    of swaps"

PathsToTablete::usage =
    "Giving a pair represented full path returned 
    by pathfinding, create corresponding tablet"

ZosAndTabPairs::usage =
    "Giving tabs, show zeros and ones table of all sequences"

Begin["`Private`"]

SwapPairEdge::usage =
    "Giving a list of numbers, respond all possible mutations"

SwapPairEdgeRule::usage =
    "Listing the rule obeyed when swapping {{before},{after}}"

SwapPairEdge[l_List] := Module[
    {avail, edgeRule, edge},
    avail = Pick[Partition[l, 2, 1], Positive /@ Differences[l]];
    edge[{x_, y_}] := l -> (l /. {x -> y, y -> x});
    edge /@ avail
    ];

SwapPairEdgeRule[l_List] := Module[
    {avail, edgeRule, edge},
    avail = Pick[Partition[l, 2, 1], Positive /@ Differences[l]];
    edge[{x_, y_}] := {l, (l /. {x -> y, y -> x})};
    Thread[edge /@ avail -> avail]
    ];

SwapPaths[i_Integer] := Module[
    {permutations, rule, graph},
    permutations = Permutations@Range@i;
    rule = Join @@ (SwapPairEdgeRule /@ permutations);
    swapPathGraph = Graph@(Join @@ (SwapPairEdge /@ permutations));
    FindPath[swapPathGraph, Range@i, Reverse@Range@i, Infinity, All]
]

BeforeSwap[l_] := l[[All, ;; -2, All]];
AfterSwap[l_] := l[[All, 2 ;;, All]];
Mapify[x_] := Map[x, #]&

FullPathShorten[path_] := Module[
    {d, b, p, n},
    d = Differences[path, {0, 1, 0}];
    b = BeforeSwap[path];
    p = Pick[b, Positive@d];
    n = Pick[b, Negative@d];
    Join[p, n, 3]
];

PathToTablete[pair_] := Normal@ Transpose@ SparseArray@ 
    MapThread[ (#1 -> #2 &), {pair, Range@Length@pair} ] ;

PathsToTablete = Mapify@PathToTablete;

ZosAndTabPairs[tab_] := Differences[tab, {0, 1, 0}] // Positive // Boole

End[ ]

EndPackage[ ]
