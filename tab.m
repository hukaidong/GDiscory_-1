BeginPackage["Tablet2`"]

ValidTabQ::usage = 
    "giving a tab show if it obeys mutation rule and gives reversed seq as result"

TabSwaps6::usage =
    "given a tab, give all swap candidates"

BfsTabScan::usage = 
    "find all tabs followed by GenNodes"

Begin["`Private`"]

(* ValidTabQ *)
Idx2Pos[tab_] := Delete[
    Merge[
        MapIndexed[
            (#1 -> #2) &,
            tab, {2}
        ], Last
    ], Key[0]
]

PartPattern[p_] := Join[{x___}, Reverse@p, {y___}];
PartRule[p_] := Join[{x___}, Reverse@p, {y___}] -> Join[{x}, p, {y}];
ChkPos[False, _] := False;
ChkPos[l_List, p_] := If[
    MatchQ[l, PartPattern@ p],
    Replace[l, PartRule@ p],
    False
]
ChkOrd[False] := False;
ChkOrd[l_List] := OrderedQ@ Reverse@ l;

ValidTabQ[tab_] := Module[
    {idx, val, o},
    idx = Idx2Pos[tab];
    val = Values@ KeySort@ idx;
    out = Fold[ChkPos, Range@6, val];
    ChkOrd@ out
];

(* ~ValidTabQ *)

(* ValidTab2Q, Not correct *)

D0 = DeleteCases[#, 0]& ;

PosBetween[tab_, c_, {x_, y_} ] := Module[
    {m, n},
    m =  D0@ tab[[ ;; x-1, y ]];
    n =  D0@ tab[[ x, y+1 ;; ]];
    Inner[ Between[c, {##}]& , m, n, And ]
]


ValidTab2Q[tab_, p_] := Module[
    {idx, f, l},
    idx = Idx2Pos[tab];
    {f, l} = p;
    PosBetween[tab, f, idx@f] ~And~
        PosBetween[tab, l, idx@l]
]

(* ~ValidTab2Q *)

(* TabSwaps *)

TabSwap[tab_, {x1_, y1_}, {x2_, y2_}] := Module[
    {t=tab, p},
    p = {t[[ x2, y2]], t[[x1, y1]]};
    {t[[ x1, y1 ]], t[[ x2, y2 ]]} = p;
    If[ValidTabQ@t, t, Nothing]
]

NIT [x_, y_] := Nothing /; IntersectingQ[x, y];
NIT [x_, y_] := {x, y};

swapPos6 = Module[ 
    (* In order to accelerate swapping *)
    {l=6, a, t, p, x},
    a = Array[1&, {l, l}];
    l = LowerTriangularize[a, -1];
    p = Position[l, 1];
    x = Outer[NIT, p, p, 1];
    Flatten[x, {1, 2}]
];

TabSwaps6[tab_] := SparseArray/@Apply[TabSwap[tab, ##]&, swapPos6, {1}]

(* ~TabSwaps *)

BfsTabScan[tab_, GetNode_] := Module[
    {bfsS},
    DistributeDefinitions[GetNode];
    bfsS[{},close_]:=close;
    bfsS[open_, close_]:=bfsS[
        Complement[
            Union@@ParallelMap[GetNode,open],
            open,close
            ],
        Union[open,close]
    ];
    bfsS[{tab}, {}]
]
End[ ]

EndPackage[ ]
