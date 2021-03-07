(* ::Package:: *)

BeginPackage["Perikov`Utils`"];

Unprotect[Evaluate [Context[] <> "*"]]; (* \:0432\:043d\:0438\:0437\:0443 \:044d\:0442\:043e \:0432 \:0432\:0438\:0434\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 *)
Quiet @ Remove[Evaluate [Context[] <> "*"]];


(* ::Section:: *)
(*\:0418\:043d\:0442\:0435\:0440\:0444\:0435\:0439\:0441*)


(* ::Subsection::Closed:: *)
(*\:041d\:0435\:0439\:0440\:043e\:0441\:0435\:0442\:0438*)


softDiceLossLayer::usage = "softDiceLossLayer[] subj, \:043d\:043e \:043c\:043e\:0434\:0438\:0444\:0438\:0446\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:044b\:0439 \:0434\:043b\:044f \
\:0441\:043b\:0443\:0447\:0430\:044f, \:043a\:043e\:0433\:0434\:0430 Target \:0432\:0441\:044e\:0434\:0443 0, \:0441\:043c. \:0438\:0441\:0445\:043e\:0434\:043d\:0438\:043a\:0438. \:041f\:043e \:044d\:0442\:043e\:0439 \:043f\:0440\:0438\:0447\:0438\:043d\:0435 \:0441\:0432\:0435\:0440\:0445\:0443 \:043d\:0435 \:043e\:0433\:0440\:0430\:043d\:0438\:0447\:0435\:043d 1,\
\:043d\:043e \:0432 \:043d\:043e\:0440\:043c\:0435 \:0434\:043e\:043b\:0436\:0435\:043d \:0447\:0443\:0442\:043e\:043a \:0442\:043e\:043b\:044c\:043a\:043e \:043f\:0440\:0435\:0432\:044b\:0448\:0430\:0442\:044c.
https://arxiv.org/pdf/1606.04797.pdf
";


swishLayer::usage = "swishLayer[]  \:0444\:0443\:043d\:043a\:0446\:0438\:044f \:0430\:043a\:0442\:0438\:0432\:0430\:0446\:0438\:0438 \:043e\:0442 Google";


(* ::Subsection:: *)
(*\:041f\:0440\:043e\:0447\:0435\:0435*)


randomArrayFragment::usage = "randomArrayFragment[arr, len] \:0434\:043e\:0441\:0442\:0430\:0435\:0442 \:0441\:043b\:0443\:0447\:0430\:0439\:043d\:044b\:0439 \
\:043f\:043e\:0441\:043b\:0435\:0434\:043e\:0432\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0439 \:043a\:0443\:0441\:043e\:043a \:0434\:043b\:0438\:043d\:043e\:0439 len \:0438\:0437 arr";
addUtilityDock::usage = "addUtilityDock[] docks a cell with clock and other useful information";
clearDock::usage ="clearDock[] undocks all docked cells in EvaluationNotebook";
clearContext::usage = "clearContext[\"ctx`\"] Unprotects and removes all symbols in ctx
clearContext[] does the same with the current context";
protectContext::usage = "protectContext[\"ctx`\"] protects all symbols in ctx";
padArrayToMultiplyOf::usage = "padArrayToMultiplyOf[arr,n] \:0434\:043e\:0431\:0430\:0432\:043b\:044f\:0435\:0442 \:0441\:043f\:0440\:0430\:0432\:0430 \:043d\:0443\:043b\:0435\:0439, \:0447\:0442\:043e\:0431\:044b \:0434\:043b\:0438\:043d\:0430 \:0431\:044b\:043b\:0430 \:043a\:0440\:0430\:0442\:043d\:0430 n
padArrayToMultiplyOf[arr,n, pad] \:0434\:043e\:0431\:0430\:0432\:043b\:044f\:0435\:0442 \:0441\:043f\:0440\:0430\:0432\:0430 pad

\:0418\:043c\:0435\:0435\:0442 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:043d\:0443\:044e \:0444\:043e\:0440\:043c\:0443
"; 
utilsRunTests::usage = "utilsRunTests[] \:0437\:0430\:043f\:0443\:0441\:043a\:0430\:0435\:0442 \:0442\:0435\:0441\:0442\:044b.
\:041f\:043e\:0442\:043e\:043c \:043f\:0435\:0440\:0435\:0442\:0430\:0449\:0438\:0442\:044c \:0432 \:0444\:0430\:0439\:043b";

NonNegativeIntegerQ = Internal`NonNegativeIntegerQ;
PositiveIntegerQ = Internal`PositiveIntegerQ;
randomSplitInRate::usage = "randomSplitInRate[data, r] \:043f\:0435\:0440\:0435\:043c\:0435\:0448\:0438\:0432\:0430\:0435\:0442 data \:0438 \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0434\:0432\:0430 \:043a\:0443\:0441\:043a\:0430 \:0432 \:043f\:0440\:043e\:043f\:043e\:0440\:0446\:0438\:0438 r";

limitOutputTo::usage = "limitOutputTo[expr,byteLimit] \:043e\:0433\:0440\:0430\:043d\:0438\:0447\:0438\:0432\:0430\:0435\:0442 \:043a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:0431\:0430\:0439\:0442, \:043a\:043e\:0442\:043e\:0440\:044b\:0435 
expr \:043c\:043e\:0436\:0435\:0442 \:0437\:0430\:043f\:0438\:0441\:0430\:0442\:044c \:0432 First@$Output \:0434\:043e byteLimit";

referencesTo::usage = "hasReferencesTo[ctxStringPattern, pattern] \:043d\:0430\:0445\:043e\:0434\:0438\:0442 \:0441\:0438\:043c\:0432\:043e\:043b\:044b, \:043b\:0435\:0436\:0430\:0449\:0438\:0435 \:0432 \:043a\:043e\:043d\:0442\:0435\:043a\:0441\:0442\:0430\:0445,
\:043f\:043e\:0434\:0445\:043e\:0434\:044f\:0449\:0438\:0445 \:043f\:043e\:0434 ctxStringPattern, \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f \:043a\:043e\:0442\:043e\:0440\:044b\:0445 \:0432\:0445\:043e\:0434\:0438\:0442 pattern."


(* ::Section:: *)
(*\:0420\:0435\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f*)


Begin["`Private`"];
Quiet @ Remove[Evaluate [Context[] <> "*"]];


(* ::Subsection::Closed:: *)
(*\:041d\:0435\:0439\:0440\:043e\:0441\:0435\:0442\:0438*)


softDiceLossLayer[] = FunctionLayer[<|
	"Loss" ->1 -2 Total[#Input #Target ]/Total[#Input+#Target] +Mean [ Abs[#Input-#Target]Clip[1-#Target,{.01,1}]]
	|>&,
	"Input" -> "Varying", "Target"->"Varying"
];


swishLayer[] := ElementwiseLayer[# LogisticSigmoid[#]&];


(* ::Subsection:: *)
(*\:041c\:0430\:043d\:0438\:043f\:0443\:043b\:044f\:0446\:0438\:0438 \:0441 \:043a\:043e\:043d\:0442\:0435\:043a\:0441\:0442\:0430\:043c\:0438*)


Generic::expectingCtxArg = "Expecting context name argument in form of a string \"ctx`\"";



clearContext[ctx_String] := With[{pat = ctx <> "*"},
	Unprotect@pat; 
	Quiet @ Remove@pat;
];
clearContext[] := clearContext[Context[]];
clearContext[args__]/;Message[General::expectingCtxArg] = Null;


protectContext[ctx_String] := Protect @ Evaluate[ctx <> "*"];
protectContext[] := protectContext[Context[]];
protectContext[___]/; Message[General::expectingCtxArg] = Null;


(* ::Subsection:: *)
(*\:041f\:0440\:043e\:0447\:0435\:0435*)


hasReferencesTo[symDef:(_Symbol|_String), pat_] :=  MemberQ[Language`ExtendedDefinition[symDef], pat, \[Infinity], Heads->True];
hasReferencesTo[___] := False;
namesInCtxPattern[pat_String] := Flatten @ Map[Names[#<>"*"]&,Contexts[pat]];
referencesTo[ctxPat_String, pat_] :=  Select[namesInCtxPattern[ctxPat],hasReferencesTo[#,pat]&];


(* ::Subsubsection:: *)
(*\:041e\:0433\:0440\:0430\:043d\:0438\:0447\:0435\:043d\:0438\:0435 \:0432\:044b\:0432\:043e\:0434\:0430 \:043d\:0430 \:044d\:043a\:0440\:0430\:043d*)


ClearAll[outputLimiter, writeOutputLimiter, limitOutputTo];
Options[outputLimiter] = Join[{"OutputLimit" -> 100000, "Target" :> First
   @ $Output}, Options[OpenWrite]];

outputLimiter[_, append_, caller_, OptionsPattern[]] := {True, <|"BytesWritten"
   -> 0, "OutputLimit" -> OptionValue["OutputLimit"], "Target" -> OptionValue
  
["Target"]|>};

writeOutputLimiter[s_, bytes_] := Module[{len = Length @ bytes, written
   = s["BytesWritten"], newWritten},
  If[(newWritten = written + len) > s["OutputLimit"],
    {0, s}
    ,
    Write[s["Target"], FromCharacterCode[bytes, "Unicode"]]; {Length 
      @ bytes, <|s, "BytesWritten" -> newWritten|>}
  ]
];
SetAttributes[limitOutputTo, HoldFirst];

limitOutputTo[expr_, n_:1000] := Block[{$Output = Prepend[OpenWrite["Limited output"
  , Method -> {"countBytes", "OutputLimit" -> n}]] @ Rest @ $Output},
  With[{res = expr},
    Close[First @ $Output]; res
  ]
];

If[!MemberQ[$OutputStreamMethods, "countBytes"],
  DefineOutputStreamMethod["countBytes", 
  {"ConstructorFunction" -> outputLimiter
    , "WriteFunction" -> writeOutputLimiter
   }]
]


(* ::Subsubsection:: *)
(*\:0421\:043b\:0443\:0447\:0430\:0439\:043d\:044b\:0439 \:043a\:0443\:0441\:043e\:043a \:043c\:0430\:0441\:0441\:0438\:0432\:0430*)


randomArrayFragment::wrongLen = "Fragment of length `` can't be extracted from array of length ``";
randomArrayFragment[arr_?ArrayQ, len_Integer] /; 0 <= len <= Length[arr] := 
	Module[{arrLen = Length @ arr, pos}, 
		pos = RandomInteger [{1, arrLen - len + 1}];
		arr[[pos;;pos+len-1]]
	];
randomArrayFragment[arr_,len_] /; Message[randomArrayFragment::wrongLen, len,Length @ arr] = Null;


(* ::Subsubsection:: *)
(*\:0412\:0441\:044f\:043a\:0438\:0439 GUI*)


setDockedCells[cells_] := SetOptions[EvaluationNotebook[], DockedCells -> cells];



clearDock[] := setDockedCells[{}];


addUtilityDock[] := setDockedCells @ {Cell@ BoxData @ ToBoxes [ 
Dynamic[
	Row[{
		Now,
		Row@{"Memory:",Dynamic@MemoryInUse[]},
		Row@{"Session time:", TimeUsed[]}},
	"|"],UpdateInterval->1] 
]};


padArrayToMultiplyOf[arr_?VectorQ, n_Integer, padding_:0] := ArrayPad[arr,{0,Ceiling[Length @ arr, n] - Length @ arr}, padding];
padArrayToMultiplyOf[n_Integer,padding_:0] := padArrayToMultiplyOf[#,n,padding]&;


randomSplitInRate[data_List, r_/;0 < r < 1] := TakeDrop[RandomSample[data],Round[r Length[data]]];


(* ::Subsubsection:: *)
(*\:041f\:043e\:043a\:0430 \:043d\:0435 \:0440\:0430\:0431\:043e\:0442\:0430\:0435\:0442. \:041f\:043e\:0442\:0443\:0433\:0438 \:043f\:043e\:043a\:0430\:0437\:0430\:0442\:044c, \:043a\:0443\:0434\:0430 \:043f\:0430\:043c\:044f\:0442\:044c \:0434\:0435\:0451\:0442\:0441\:044f*)


SetAttributes[buildSizeTable,HoldFirst];
buildSizeTable[sym_Symbol]:=(sym =Grid[Prepend[{"Symbol","Bytes","Remove"}]@ReverseSortBy[#[[2]]&]@Table[With[{name = name},{name,ReleaseHold[Evaluate[Hold[ByteCount@ Symbol@ \[FormalA]] /. \[FormalA] -> name]] , Button["Remove",Function[Remove[name]; buildSizeTable[sym]]]}], {name, Names["Global`*"]}] , Alignment-> Right, Frame->True]);

showSizeTable[] := DynamicModule[{tbl},
	Dynamic @ tbl, Initialization:>buildSizeTable[tbl]
];


(* ::Subsubsection:: *)
(*\:0422\:0435\:0441\:0442\:044b*)


(* ::Text:: *)
(*\:0414\:043e\:043b\:0436\:043d\:044b \:0443\:0435\:0445\:0430\:0442\:044c \:0432 \:043e\:0442\:0434\:0435\:043b\:044c\:043d\:044b\:0439 \:0444\:0430\:0439\:043b*)


utilsRunTests[] := TestReport @ {
	VerificationTest[padArrayToMultiplyOf[{1,2,3,4},3,0],{1,2,3,4,0,0}, TestID->"padArray"],
	VerificationTest[padArrayToMultiplyOf[{1,2,3,4},3], {1,2,3,4,0,0}, TestID->"padArrayDefaultPad"],
	VerificationTest[padArrayToMultiplyOf[3]@{1,2,3,4}, {1,2,3,4,0,0}, TestID->"padArrayOperatorForm"],
	VerificationTest[Length @ randomArrayFragment[{1,2,3,4},2],2, TestID->"randomArrayFragment"],
	VerificationTest[Length /@ randomSplitInRate[Range[12],.75], {9,3}, TestID->"randomSplitInRate"]
	};


(* ::Text:: *)
(**)


End[(*Private*)];

protectContext[];
EndPackage[];
