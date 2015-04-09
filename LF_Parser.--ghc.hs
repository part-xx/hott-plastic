{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module LF_Parser (
		parseThenCall
		, parseTermThenCall
		, SimpleTerm
		, SyntaxInfo(..)
		) where



import IOExts_(trace)


import Data.Array
import Control.Monad (ap)
import Data.Char(isSpace)

import BaseClasses(OkF(..), Embeddable(..), Fallible(..))
import BaseTypes(isOk, fromFail, fromOk, elimOk)
import BaseFunctions(fst3, snd3, thd3)

import ParsingAux
import CommandTypes
import SimpleTerm
import SharedSyntax
import UserSyntax
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (TopCommand)
	| HappyAbsSyn8 ([Def_Decl] -> SimpleTerm -> TopCommand)
	| HappyAbsSyn9 ([Def_Decl])
	| HappyAbsSyn10 (Def_Decl)
	| HappyAbsSyn11 ([Declaration])
	| HappyAbsSyn12 (Declaration)
	| HappyAbsSyn13 ([Definition])
	| HappyAbsSyn14 (Definition)
	| HappyAbsSyn15 ([Def_PreBinding])
	| HappyAbsSyn16 (Def_PreBinding)
	| HappyAbsSyn17 ([Name])
	| HappyAbsSyn18 ([Cut_Expression])
	| HappyAbsSyn19 (Cut_Expression)
	| HappyAbsSyn24 ([InductiveParameter])
	| HappyAbsSyn25 (InductiveParameter)
	| HappyAbsSyn27 ([CompRule])
	| HappyAbsSyn28 ([ CompRuleFunction ])
	| HappyAbsSyn30 (FunctionDefinition)
	| HappyAbsSyn31 ([ FunctionClause ])
	| HappyAbsSyn33 (LocalFunctionDefs)
	| HappyAbsSyn34 ([FunctionDefinition])
	| HappyAbsSyn35 (FunctionClause)
	| HappyAbsSyn36 ([SimpleTerm])
	| HappyAbsSyn37 (SimpleTerm)
	| HappyAbsSyn38 (TacticCommand)
	| HappyAbsSyn39 (BasicCommand)
	| HappyAbsSyn40 (Maybe IdOrNum)
	| HappyAbsSyn41 (IdOrNum)
	| HappyAbsSyn42 (Maybe Name)
	| HappyAbsSyn43 ([BoundName])
	| HappyAbsSyn46 ([UnivOpt])
	| HappyAbsSyn48 (((String,SimpleTerm), SimpleTerm))
	| HappyAbsSyn49 (Name)
	| HappyAbsSyn50 (FiniteUniverse)
	| HappyAbsSyn51 ([Either UnivDecl FiniteUniverse])
	| HappyAbsSyn52 (UnivDecl)
	| HappyAbsSyn55 (SimpleTerm -> SimpleTerm -> SimpleTerm)
	| HappyAbsSyn61 ((Name,SimpleTerm))
	| HappyAbsSyn62 ((Name,SimpleTerm,SimpleTerm))
	| HappyAbsSyn63 ((Name, OpaqueOrManifest SimpleTerm))
	| HappyAbsSyn64 ([(Name,SimpleTerm,SimpleTerm)])
	| HappyAbsSyn65 ([(Name,OpaqueOrManifest SimpleTerm)])
	| HappyAbsSyn70 (SimpleTerm -> SimpleTerm)
	| HappyAbsSyn75 ([SimpleTerm] -> SimpleTerm)
	| HappyAbsSyn76 (([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm)
	| HappyAbsSyn78 (BoundName)
	| HappyAbsSyn79 (([BoundName],SimpleTerm))
	| HappyAbsSyn81 (Maybe SimpleTerm)
	| HappyAbsSyn84 ((SimpleTerm -> SimpleTerm, BindVariety))
	| HappyAbsSyn86 ([ ([SimpleTerm], SimpleTerm) ])
	| HappyAbsSyn87 (([SimpleTerm], SimpleTerm))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366 :: () => Happy_GHC_Exts.Int# -> ({-HappyReduction (P) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189 :: () => ({-HappyReduction (P) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

action_0 (88#) = happyShift action_29
action_0 (89#) = happyShift action_30
action_0 (90#) = happyShift action_31
action_0 (92#) = happyShift action_32
action_0 (93#) = happyShift action_33
action_0 (103#) = happyShift action_34
action_0 (111#) = happyShift action_35
action_0 (112#) = happyShift action_36
action_0 (113#) = happyShift action_37
action_0 (114#) = happyShift action_38
action_0 (115#) = happyShift action_39
action_0 (118#) = happyShift action_40
action_0 (119#) = happyShift action_41
action_0 (120#) = happyShift action_42
action_0 (121#) = happyReduce_102
action_0 (122#) = happyReduce_102
action_0 (123#) = happyShift action_43
action_0 (124#) = happyReduce_102
action_0 (125#) = happyReduce_102
action_0 (126#) = happyShift action_44
action_0 (127#) = happyShift action_45
action_0 (128#) = happyShift action_46
action_0 (129#) = happyShift action_47
action_0 (132#) = happyShift action_48
action_0 (133#) = happyShift action_49
action_0 (143#) = happyShift action_50
action_0 (145#) = happyShift action_51
action_0 (146#) = happyShift action_52
action_0 (147#) = happyShift action_53
action_0 (149#) = happyShift action_54
action_0 (151#) = happyShift action_55
action_0 (153#) = happyShift action_56
action_0 (154#) = happyShift action_57
action_0 (155#) = happyShift action_58
action_0 (156#) = happyShift action_59
action_0 (4#) = happyGoto action_60
action_0 (5#) = happyGoto action_2
action_0 (6#) = happyGoto action_3
action_0 (7#) = happyGoto action_4
action_0 (20#) = happyGoto action_5
action_0 (23#) = happyGoto action_6
action_0 (26#) = happyGoto action_7
action_0 (29#) = happyGoto action_8
action_0 (30#) = happyGoto action_9
action_0 (38#) = happyGoto action_10
action_0 (39#) = happyGoto action_11
action_0 (40#) = happyGoto action_12
action_0 (41#) = happyGoto action_13
action_0 (44#) = happyGoto action_14
action_0 (45#) = happyGoto action_15
action_0 (46#) = happyGoto action_16
action_0 (47#) = happyGoto action_17
action_0 (50#) = happyGoto action_18
action_0 (53#) = happyGoto action_19
action_0 (56#) = happyGoto action_20
action_0 (59#) = happyGoto action_21
action_0 (60#) = happyGoto action_22
action_0 (66#) = happyGoto action_23
action_0 (67#) = happyGoto action_24
action_0 (68#) = happyGoto action_25
action_0 (82#) = happyGoto action_26
action_0 (84#) = happyGoto action_27
action_0 (85#) = happyGoto action_28
action_0 x = happyTcHack x happyReduce_88

action_1 (88#) = happyShift action_29
action_1 (89#) = happyShift action_30
action_1 (90#) = happyShift action_31
action_1 (92#) = happyShift action_32
action_1 (93#) = happyShift action_33
action_1 (103#) = happyShift action_34
action_1 (111#) = happyShift action_35
action_1 (112#) = happyShift action_36
action_1 (113#) = happyShift action_37
action_1 (114#) = happyShift action_38
action_1 (115#) = happyShift action_39
action_1 (118#) = happyShift action_40
action_1 (119#) = happyShift action_41
action_1 (120#) = happyShift action_42
action_1 (123#) = happyShift action_43
action_1 (126#) = happyShift action_44
action_1 (127#) = happyShift action_45
action_1 (128#) = happyShift action_46
action_1 (129#) = happyShift action_47
action_1 (132#) = happyShift action_48
action_1 (133#) = happyShift action_49
action_1 (143#) = happyShift action_50
action_1 (145#) = happyShift action_51
action_1 (146#) = happyShift action_52
action_1 (147#) = happyShift action_53
action_1 (149#) = happyShift action_54
action_1 (151#) = happyShift action_55
action_1 (153#) = happyShift action_56
action_1 (154#) = happyShift action_57
action_1 (155#) = happyShift action_58
action_1 (156#) = happyShift action_59
action_1 (5#) = happyGoto action_2
action_1 (6#) = happyGoto action_3
action_1 (7#) = happyGoto action_4
action_1 (20#) = happyGoto action_5
action_1 (23#) = happyGoto action_6
action_1 (26#) = happyGoto action_7
action_1 (29#) = happyGoto action_8
action_1 (30#) = happyGoto action_9
action_1 (38#) = happyGoto action_10
action_1 (39#) = happyGoto action_11
action_1 (40#) = happyGoto action_12
action_1 (41#) = happyGoto action_13
action_1 (44#) = happyGoto action_14
action_1 (45#) = happyGoto action_15
action_1 (46#) = happyGoto action_16
action_1 (47#) = happyGoto action_17
action_1 (50#) = happyGoto action_18
action_1 (53#) = happyGoto action_19
action_1 (56#) = happyGoto action_20
action_1 (59#) = happyGoto action_21
action_1 (60#) = happyGoto action_22
action_1 (66#) = happyGoto action_23
action_1 (67#) = happyGoto action_24
action_1 (68#) = happyGoto action_25
action_1 (82#) = happyGoto action_26
action_1 (84#) = happyGoto action_27
action_1 (85#) = happyGoto action_28
action_1 x = happyTcHack x happyFail

action_2 x = happyTcHack x happyReduce_1

action_3 x = happyTcHack x happyReduce_15

action_4 x = happyTcHack x happyReduce_16

action_5 x = happyTcHack x happyReduce_10

action_6 x = happyTcHack x happyReduce_13

action_7 x = happyTcHack x happyReduce_12

action_8 x = happyTcHack x happyReduce_14

action_9 x = happyTcHack x happyReduce_63

action_10 (116#) = happyShift action_138
action_10 (117#) = happyShift action_139
action_10 x = happyTcHack x happyReduce_3

action_11 x = happyTcHack x happyReduce_76

action_12 (97#) = happyShift action_132
action_12 (98#) = happyShift action_133
action_12 (99#) = happyShift action_134
action_12 (100#) = happyShift action_135
action_12 (101#) = happyShift action_136
action_12 (102#) = happyShift action_137
action_12 x = happyTcHack x happyFail

action_13 x = happyTcHack x happyReduce_87

action_14 x = happyTcHack x happyReduce_11

action_15 x = happyTcHack x happyReduce_97

action_16 (121#) = happyShift action_128
action_16 (122#) = happyShift action_129
action_16 (124#) = happyShift action_130
action_16 (125#) = happyShift action_131
action_16 x = happyTcHack x happyFail

action_17 x = happyTcHack x happyReduce_96

action_18 x = happyTcHack x happyReduce_95

action_19 x = happyTcHack x happyReduce_89

action_20 (138#) = happyShift action_124
action_20 (141#) = happyShift action_125
action_20 (142#) = happyShift action_126
action_20 (157#) = happyShift action_127
action_20 (8#) = happyGoto action_122
action_20 (57#) = happyGoto action_123
action_20 x = happyTcHack x happyReduce_18

action_21 (131#) = happyShift action_121
action_21 x = happyTcHack x happyReduce_118

action_22 x = happyTcHack x happyReduce_129

action_23 (126#) = happyShift action_44
action_23 (127#) = happyShift action_45
action_23 (128#) = happyShift action_46
action_23 (143#) = happyShift action_91
action_23 (145#) = happyShift action_120
action_23 (147#) = happyShift action_84
action_23 (149#) = happyShift action_54
action_23 (151#) = happyShift action_55
action_23 (153#) = happyShift action_92
action_23 (154#) = happyShift action_57
action_23 (155#) = happyShift action_58
action_23 (156#) = happyShift action_59
action_23 (67#) = happyGoto action_119
action_23 (68#) = happyGoto action_25
action_23 (82#) = happyGoto action_26
action_23 (84#) = happyGoto action_27
action_23 x = happyTcHack x happyReduce_126

action_24 x = happyTcHack x happyReduce_142

action_25 x = happyTcHack x happyReduce_153

action_26 (126#) = happyShift action_44
action_26 (127#) = happyShift action_45
action_26 (128#) = happyShift action_46
action_26 (129#) = happyShift action_47
action_26 (132#) = happyShift action_48
action_26 (133#) = happyShift action_49
action_26 (143#) = happyShift action_91
action_26 (145#) = happyShift action_51
action_26 (147#) = happyShift action_84
action_26 (149#) = happyShift action_54
action_26 (151#) = happyShift action_55
action_26 (153#) = happyShift action_92
action_26 (154#) = happyShift action_57
action_26 (155#) = happyShift action_58
action_26 (156#) = happyShift action_59
action_26 (56#) = happyGoto action_118
action_26 (59#) = happyGoto action_21
action_26 (60#) = happyGoto action_22
action_26 (66#) = happyGoto action_23
action_26 (67#) = happyGoto action_24
action_26 (68#) = happyGoto action_25
action_26 (82#) = happyGoto action_26
action_26 (84#) = happyGoto action_27
action_26 (85#) = happyGoto action_28
action_26 x = happyTcHack x happyFail

action_27 (83#) = happyGoto action_117
action_27 x = happyTcHack x happyReduce_181

action_28 x = happyTcHack x happyReduce_128

action_29 x = happyTcHack x happyReduce_90

action_30 (145#) = happyShift action_73
action_30 (9#) = happyGoto action_116
action_30 (10#) = happyGoto action_69
action_30 (12#) = happyGoto action_70
action_30 (14#) = happyGoto action_71
action_30 x = happyTcHack x happyFail

action_31 (143#) = happyShift action_77
action_31 (153#) = happyShift action_101
action_31 (31#) = happyGoto action_113
action_31 (35#) = happyGoto action_114
action_31 (53#) = happyGoto action_115
action_31 x = happyTcHack x happyFail

action_32 (153#) = happyShift action_112
action_32 x = happyTcHack x happyFail

action_33 (94#) = happyShift action_111
action_33 (21#) = happyGoto action_110
action_33 x = happyTcHack x happyReduce_45

action_34 (11#) = happyGoto action_109
action_34 x = happyTcHack x happyReduce_29

action_35 (143#) = happyShift action_77
action_35 (153#) = happyShift action_101
action_35 (53#) = happyGoto action_108
action_35 x = happyTcHack x happyFail

action_36 (143#) = happyShift action_77
action_36 (153#) = happyShift action_101
action_36 (53#) = happyGoto action_107
action_36 x = happyTcHack x happyFail

action_37 (18#) = happyGoto action_106
action_37 x = happyTcHack x happyReduce_41

action_38 (143#) = happyShift action_77
action_38 (153#) = happyShift action_101
action_38 (42#) = happyGoto action_105
action_38 (53#) = happyGoto action_104
action_38 x = happyTcHack x happyReduce_92

action_39 (143#) = happyShift action_77
action_39 (153#) = happyShift action_101
action_39 (42#) = happyGoto action_103
action_39 (53#) = happyGoto action_104
action_39 x = happyTcHack x happyReduce_92

action_40 (126#) = happyShift action_44
action_40 (127#) = happyShift action_45
action_40 (128#) = happyShift action_46
action_40 (129#) = happyShift action_47
action_40 (132#) = happyShift action_48
action_40 (133#) = happyShift action_49
action_40 (143#) = happyShift action_91
action_40 (145#) = happyShift action_51
action_40 (147#) = happyShift action_84
action_40 (149#) = happyShift action_54
action_40 (151#) = happyShift action_55
action_40 (153#) = happyShift action_92
action_40 (154#) = happyShift action_57
action_40 (155#) = happyShift action_58
action_40 (156#) = happyShift action_59
action_40 (56#) = happyGoto action_102
action_40 (59#) = happyGoto action_21
action_40 (60#) = happyGoto action_22
action_40 (66#) = happyGoto action_23
action_40 (67#) = happyGoto action_24
action_40 (68#) = happyGoto action_25
action_40 (82#) = happyGoto action_26
action_40 (84#) = happyGoto action_27
action_40 (85#) = happyGoto action_28
action_40 x = happyTcHack x happyFail

action_41 (88#) = happyShift action_29
action_41 (143#) = happyShift action_77
action_41 (153#) = happyShift action_101
action_41 (40#) = happyGoto action_100
action_41 (41#) = happyGoto action_13
action_41 (53#) = happyGoto action_19
action_41 x = happyTcHack x happyReduce_88

action_42 (88#) = happyShift action_29
action_42 (89#) = happyShift action_30
action_42 (90#) = happyShift action_31
action_42 (92#) = happyShift action_32
action_42 (93#) = happyShift action_33
action_42 (103#) = happyShift action_34
action_42 (111#) = happyShift action_35
action_42 (112#) = happyShift action_36
action_42 (113#) = happyShift action_37
action_42 (114#) = happyShift action_38
action_42 (115#) = happyShift action_39
action_42 (118#) = happyShift action_40
action_42 (119#) = happyShift action_41
action_42 (120#) = happyShift action_42
action_42 (121#) = happyReduce_102
action_42 (122#) = happyReduce_102
action_42 (123#) = happyShift action_43
action_42 (124#) = happyReduce_102
action_42 (125#) = happyReduce_102
action_42 (126#) = happyShift action_44
action_42 (127#) = happyShift action_45
action_42 (128#) = happyShift action_46
action_42 (129#) = happyShift action_47
action_42 (132#) = happyShift action_48
action_42 (133#) = happyShift action_49
action_42 (143#) = happyShift action_50
action_42 (145#) = happyShift action_51
action_42 (146#) = happyShift action_52
action_42 (147#) = happyShift action_53
action_42 (149#) = happyShift action_54
action_42 (151#) = happyShift action_55
action_42 (153#) = happyShift action_56
action_42 (154#) = happyShift action_57
action_42 (155#) = happyShift action_58
action_42 (156#) = happyShift action_59
action_42 (5#) = happyGoto action_99
action_42 (6#) = happyGoto action_3
action_42 (7#) = happyGoto action_4
action_42 (20#) = happyGoto action_5
action_42 (23#) = happyGoto action_6
action_42 (26#) = happyGoto action_7
action_42 (29#) = happyGoto action_8
action_42 (30#) = happyGoto action_9
action_42 (38#) = happyGoto action_10
action_42 (39#) = happyGoto action_11
action_42 (40#) = happyGoto action_12
action_42 (41#) = happyGoto action_13
action_42 (44#) = happyGoto action_14
action_42 (45#) = happyGoto action_15
action_42 (46#) = happyGoto action_16
action_42 (47#) = happyGoto action_17
action_42 (50#) = happyGoto action_18
action_42 (53#) = happyGoto action_19
action_42 (56#) = happyGoto action_20
action_42 (59#) = happyGoto action_21
action_42 (60#) = happyGoto action_22
action_42 (66#) = happyGoto action_23
action_42 (67#) = happyGoto action_24
action_42 (68#) = happyGoto action_25
action_42 (82#) = happyGoto action_26
action_42 (84#) = happyGoto action_27
action_42 (85#) = happyGoto action_28
action_42 x = happyTcHack x happyReduce_88

action_43 (137#) = happyShift action_97
action_43 (156#) = happyShift action_98
action_43 x = happyTcHack x happyFail

action_44 x = happyTcHack x happyReduce_144

action_45 x = happyTcHack x happyReduce_143

action_46 (143#) = happyShift action_91
action_46 (153#) = happyShift action_94
action_46 (155#) = happyShift action_95
action_46 (156#) = happyShift action_96
action_46 (68#) = happyGoto action_93
action_46 x = happyTcHack x happyFail

action_47 (126#) = happyShift action_44
action_47 (127#) = happyShift action_45
action_47 (128#) = happyShift action_46
action_47 (129#) = happyShift action_47
action_47 (132#) = happyShift action_48
action_47 (133#) = happyShift action_49
action_47 (143#) = happyShift action_91
action_47 (145#) = happyShift action_51
action_47 (147#) = happyShift action_84
action_47 (149#) = happyShift action_54
action_47 (151#) = happyShift action_55
action_47 (153#) = happyShift action_92
action_47 (154#) = happyShift action_57
action_47 (155#) = happyShift action_58
action_47 (156#) = happyShift action_59
action_47 (36#) = happyGoto action_88
action_47 (37#) = happyGoto action_89
action_47 (54#) = happyGoto action_90
action_47 (56#) = happyGoto action_81
action_47 (59#) = happyGoto action_21
action_47 (60#) = happyGoto action_22
action_47 (66#) = happyGoto action_23
action_47 (67#) = happyGoto action_24
action_47 (68#) = happyGoto action_25
action_47 (82#) = happyGoto action_26
action_47 (84#) = happyGoto action_27
action_47 (85#) = happyGoto action_28
action_47 x = happyTcHack x happyFail

action_48 (147#) = happyShift action_87
action_48 x = happyTcHack x happyFail

action_49 (147#) = happyShift action_86
action_49 x = happyTcHack x happyFail

action_50 (88#) = happyShift action_29
action_50 (126#) = happyShift action_44
action_50 (127#) = happyShift action_45
action_50 (128#) = happyShift action_46
action_50 (129#) = happyShift action_47
action_50 (132#) = happyShift action_48
action_50 (133#) = happyShift action_49
action_50 (136#) = happyShift action_83
action_50 (143#) = happyShift action_50
action_50 (145#) = happyShift action_51
action_50 (147#) = happyShift action_84
action_50 (149#) = happyShift action_54
action_50 (151#) = happyShift action_55
action_50 (153#) = happyShift action_56
action_50 (154#) = happyShift action_57
action_50 (155#) = happyShift action_58
action_50 (156#) = happyShift action_59
action_50 (157#) = happyShift action_85
action_50 (38#) = happyGoto action_79
action_50 (39#) = happyGoto action_11
action_50 (40#) = happyGoto action_12
action_50 (41#) = happyGoto action_13
action_50 (53#) = happyGoto action_19
action_50 (54#) = happyGoto action_80
action_50 (56#) = happyGoto action_81
action_50 (59#) = happyGoto action_21
action_50 (60#) = happyGoto action_22
action_50 (66#) = happyGoto action_23
action_50 (67#) = happyGoto action_24
action_50 (68#) = happyGoto action_25
action_50 (69#) = happyGoto action_82
action_50 (82#) = happyGoto action_26
action_50 (84#) = happyGoto action_27
action_50 (85#) = happyGoto action_28
action_50 x = happyTcHack x happyReduce_88

action_51 (136#) = happyShift action_64
action_51 (143#) = happyShift action_77
action_51 (153#) = happyShift action_78
action_51 (53#) = happyGoto action_75
action_51 (77#) = happyGoto action_61
action_51 (78#) = happyGoto action_62
action_51 (79#) = happyGoto action_76
action_51 x = happyTcHack x happyFail

action_52 (13#) = happyGoto action_74
action_52 x = happyTcHack x happyReduce_32

action_53 (136#) = happyShift action_64
action_53 (145#) = happyShift action_73
action_53 (153#) = happyShift action_65
action_53 (9#) = happyGoto action_68
action_53 (10#) = happyGoto action_69
action_53 (12#) = happyGoto action_70
action_53 (14#) = happyGoto action_71
action_53 (77#) = happyGoto action_61
action_53 (78#) = happyGoto action_62
action_53 (79#) = happyGoto action_72
action_53 x = happyTcHack x happyFail

action_54 (136#) = happyShift action_64
action_54 (153#) = happyShift action_65
action_54 (77#) = happyGoto action_61
action_54 (78#) = happyGoto action_62
action_54 (79#) = happyGoto action_67
action_54 x = happyTcHack x happyFail

action_55 (136#) = happyShift action_64
action_55 (153#) = happyShift action_65
action_55 (77#) = happyGoto action_61
action_55 (78#) = happyGoto action_62
action_55 (79#) = happyGoto action_66
action_55 x = happyTcHack x happyFail

action_56 (126#) = happyReduce_149
action_56 (127#) = happyReduce_149
action_56 (128#) = happyReduce_149
action_56 (131#) = happyReduce_149
action_56 (134#) = happyReduce_149
action_56 (135#) = happyReduce_149
action_56 (137#) = happyReduce_149
action_56 (138#) = happyReduce_149
action_56 (139#) = happyReduce_149
action_56 (140#) = happyReduce_149
action_56 (141#) = happyReduce_149
action_56 (142#) = happyReduce_149
action_56 (143#) = happyReduce_149
action_56 (144#) = happyReduce_149
action_56 (145#) = happyReduce_149
action_56 (147#) = happyReduce_149
action_56 (149#) = happyReduce_149
action_56 (151#) = happyReduce_149
action_56 (153#) = happyReduce_149
action_56 (154#) = happyReduce_149
action_56 (155#) = happyReduce_149
action_56 (156#) = happyReduce_149
action_56 (157#) = happyReduce_149
action_56 (158#) = happyReduce_149
action_56 x = happyTcHack x happyReduce_112

action_57 (136#) = happyShift action_64
action_57 (153#) = happyShift action_65
action_57 (77#) = happyGoto action_61
action_57 (78#) = happyGoto action_62
action_57 (79#) = happyGoto action_63
action_57 x = happyTcHack x happyFail

action_58 x = happyTcHack x happyReduce_150

action_59 x = happyTcHack x happyReduce_151

action_60 (158#) = happyAccept
action_60 x = happyTcHack x happyFail

action_61 (137#) = happyShift action_175
action_61 (139#) = happyShift action_208
action_61 (80#) = happyGoto action_207
action_61 x = happyTcHack x happyFail

action_62 x = happyTcHack x happyReduce_171

action_63 (146#) = happyShift action_206
action_63 x = happyTcHack x happyFail

action_64 x = happyTcHack x happyReduce_173

action_65 x = happyTcHack x happyReduce_172

action_66 (152#) = happyShift action_205
action_66 x = happyTcHack x happyFail

action_67 (150#) = happyShift action_204
action_67 x = happyTcHack x happyFail

action_68 (145#) = happyShift action_73
action_68 (148#) = happyShift action_203
action_68 (10#) = happyGoto action_158
action_68 (12#) = happyGoto action_70
action_68 (14#) = happyGoto action_71
action_68 x = happyTcHack x happyFail

action_69 x = happyTcHack x happyReduce_25

action_70 x = happyTcHack x happyReduce_26

action_71 x = happyTcHack x happyReduce_27

action_72 (148#) = happyShift action_202
action_72 x = happyTcHack x happyFail

action_73 (143#) = happyShift action_77
action_73 (153#) = happyShift action_101
action_73 (17#) = happyGoto action_200
action_73 (53#) = happyGoto action_201
action_73 x = happyTcHack x happyFail

action_74 (145#) = happyShift action_199
action_74 (14#) = happyGoto action_198
action_74 x = happyTcHack x happyReduce_5

action_75 (138#) = happyShift action_197
action_75 x = happyTcHack x happyFail

action_76 (146#) = happyShift action_196
action_76 x = happyTcHack x happyFail

action_77 (157#) = happyShift action_195
action_77 x = happyTcHack x happyFail

action_78 (138#) = happyReduce_112
action_78 x = happyTcHack x happyReduce_172

action_79 (116#) = happyShift action_138
action_79 (117#) = happyShift action_139
action_79 (144#) = happyShift action_194
action_79 x = happyTcHack x happyFail

action_80 x = happyTcHack x happyReduce_156

action_81 (134#) = happyShift action_192
action_81 (135#) = happyShift action_193
action_81 (142#) = happyShift action_126
action_81 (157#) = happyShift action_127
action_81 (55#) = happyGoto action_191
action_81 (57#) = happyGoto action_123
action_81 x = happyTcHack x happyReduce_114

action_82 (139#) = happyShift action_189
action_82 (140#) = happyShift action_190
action_82 (70#) = happyGoto action_185
action_82 (71#) = happyGoto action_186
action_82 (73#) = happyGoto action_187
action_82 (74#) = happyGoto action_188
action_82 x = happyTcHack x happyReduce_166

action_83 x = happyTcHack x happyReduce_157

action_84 (136#) = happyShift action_64
action_84 (153#) = happyShift action_65
action_84 (77#) = happyGoto action_61
action_84 (78#) = happyGoto action_62
action_84 (79#) = happyGoto action_72
action_84 x = happyTcHack x happyFail

action_85 (144#) = happyShift action_184
action_85 x = happyTcHack x happyFail

action_86 (126#) = happyShift action_44
action_86 (127#) = happyShift action_45
action_86 (128#) = happyShift action_46
action_86 (129#) = happyShift action_47
action_86 (132#) = happyShift action_48
action_86 (133#) = happyShift action_49
action_86 (143#) = happyShift action_91
action_86 (145#) = happyShift action_51
action_86 (147#) = happyShift action_84
action_86 (149#) = happyShift action_54
action_86 (151#) = happyShift action_55
action_86 (153#) = happyShift action_92
action_86 (154#) = happyShift action_57
action_86 (155#) = happyShift action_58
action_86 (156#) = happyShift action_59
action_86 (54#) = happyGoto action_183
action_86 (56#) = happyGoto action_81
action_86 (59#) = happyGoto action_21
action_86 (60#) = happyGoto action_22
action_86 (66#) = happyGoto action_23
action_86 (67#) = happyGoto action_24
action_86 (68#) = happyGoto action_25
action_86 (82#) = happyGoto action_26
action_86 (84#) = happyGoto action_27
action_86 (85#) = happyGoto action_28
action_86 x = happyTcHack x happyFail

action_87 (126#) = happyShift action_44
action_87 (127#) = happyShift action_45
action_87 (128#) = happyShift action_46
action_87 (129#) = happyShift action_47
action_87 (132#) = happyShift action_48
action_87 (133#) = happyShift action_49
action_87 (143#) = happyShift action_91
action_87 (145#) = happyShift action_51
action_87 (147#) = happyShift action_84
action_87 (149#) = happyShift action_54
action_87 (151#) = happyShift action_55
action_87 (153#) = happyShift action_92
action_87 (154#) = happyShift action_57
action_87 (155#) = happyShift action_58
action_87 (156#) = happyShift action_59
action_87 (54#) = happyGoto action_182
action_87 (56#) = happyGoto action_81
action_87 (59#) = happyGoto action_21
action_87 (60#) = happyGoto action_22
action_87 (66#) = happyGoto action_23
action_87 (67#) = happyGoto action_24
action_87 (68#) = happyGoto action_25
action_87 (82#) = happyGoto action_26
action_87 (84#) = happyGoto action_27
action_87 (85#) = happyGoto action_28
action_87 x = happyTcHack x happyFail

action_88 (130#) = happyShift action_181
action_88 x = happyTcHack x happyFail

action_89 (126#) = happyShift action_44
action_89 (127#) = happyShift action_45
action_89 (128#) = happyShift action_46
action_89 (129#) = happyShift action_47
action_89 (132#) = happyShift action_48
action_89 (133#) = happyShift action_49
action_89 (143#) = happyShift action_91
action_89 (145#) = happyShift action_51
action_89 (147#) = happyShift action_84
action_89 (149#) = happyShift action_54
action_89 (151#) = happyShift action_55
action_89 (153#) = happyShift action_92
action_89 (154#) = happyShift action_57
action_89 (155#) = happyShift action_58
action_89 (156#) = happyShift action_59
action_89 (36#) = happyGoto action_180
action_89 (37#) = happyGoto action_89
action_89 (54#) = happyGoto action_90
action_89 (56#) = happyGoto action_81
action_89 (59#) = happyGoto action_21
action_89 (60#) = happyGoto action_22
action_89 (66#) = happyGoto action_23
action_89 (67#) = happyGoto action_24
action_89 (68#) = happyGoto action_25
action_89 (82#) = happyGoto action_26
action_89 (84#) = happyGoto action_27
action_89 (85#) = happyGoto action_28
action_89 x = happyTcHack x happyReduce_74

action_90 x = happyTcHack x happyReduce_75

action_91 (126#) = happyShift action_44
action_91 (127#) = happyShift action_45
action_91 (128#) = happyShift action_46
action_91 (129#) = happyShift action_47
action_91 (132#) = happyShift action_48
action_91 (133#) = happyShift action_49
action_91 (136#) = happyShift action_83
action_91 (143#) = happyShift action_91
action_91 (145#) = happyShift action_51
action_91 (147#) = happyShift action_84
action_91 (149#) = happyShift action_54
action_91 (151#) = happyShift action_55
action_91 (153#) = happyShift action_92
action_91 (154#) = happyShift action_57
action_91 (155#) = happyShift action_58
action_91 (156#) = happyShift action_59
action_91 (157#) = happyShift action_179
action_91 (54#) = happyGoto action_80
action_91 (56#) = happyGoto action_81
action_91 (59#) = happyGoto action_21
action_91 (60#) = happyGoto action_22
action_91 (66#) = happyGoto action_23
action_91 (67#) = happyGoto action_24
action_91 (68#) = happyGoto action_25
action_91 (69#) = happyGoto action_82
action_91 (82#) = happyGoto action_26
action_91 (84#) = happyGoto action_27
action_91 (85#) = happyGoto action_28
action_91 x = happyTcHack x happyFail

action_92 x = happyTcHack x happyReduce_149

action_93 x = happyTcHack x happyReduce_145

action_94 x = happyTcHack x happyReduce_146

action_95 x = happyTcHack x happyReduce_148

action_96 x = happyTcHack x happyReduce_147

action_97 (126#) = happyShift action_44
action_97 (127#) = happyShift action_45
action_97 (128#) = happyShift action_46
action_97 (129#) = happyShift action_47
action_97 (132#) = happyShift action_48
action_97 (133#) = happyShift action_49
action_97 (143#) = happyShift action_91
action_97 (145#) = happyShift action_51
action_97 (147#) = happyShift action_84
action_97 (149#) = happyShift action_54
action_97 (151#) = happyShift action_55
action_97 (153#) = happyShift action_92
action_97 (154#) = happyShift action_57
action_97 (155#) = happyShift action_58
action_97 (156#) = happyShift action_59
action_97 (54#) = happyGoto action_178
action_97 (56#) = happyGoto action_81
action_97 (59#) = happyGoto action_21
action_97 (60#) = happyGoto action_22
action_97 (66#) = happyGoto action_23
action_97 (67#) = happyGoto action_24
action_97 (68#) = happyGoto action_25
action_97 (82#) = happyGoto action_26
action_97 (84#) = happyGoto action_27
action_97 (85#) = happyGoto action_28
action_97 x = happyTcHack x happyFail

action_98 (143#) = happyShift action_77
action_98 (153#) = happyShift action_101
action_98 (53#) = happyGoto action_177
action_98 x = happyTcHack x happyFail

action_99 x = happyTcHack x happyReduce_2

action_100 x = happyTcHack x happyReduce_17

action_101 x = happyTcHack x happyReduce_112

action_102 (142#) = happyShift action_126
action_102 (157#) = happyShift action_127
action_102 (57#) = happyGoto action_123
action_102 x = happyTcHack x happyReduce_19

action_103 (137#) = happyShift action_175
action_103 (80#) = happyGoto action_176
action_103 x = happyTcHack x happyFail

action_104 x = happyTcHack x happyReduce_91

action_105 (137#) = happyShift action_175
action_105 (80#) = happyGoto action_174
action_105 x = happyTcHack x happyFail

action_106 (145#) = happyShift action_173
action_106 (19#) = happyGoto action_172
action_106 x = happyTcHack x happyReduce_6

action_107 (143#) = happyShift action_77
action_107 (153#) = happyShift action_101
action_107 (53#) = happyGoto action_171
action_107 x = happyTcHack x happyFail

action_108 (27#) = happyGoto action_170
action_108 x = happyTcHack x happyReduce_60

action_109 (145#) = happyShift action_169
action_109 (12#) = happyGoto action_168
action_109 x = happyTcHack x happyFail

action_110 (95#) = happyShift action_167
action_110 (22#) = happyGoto action_166
action_110 x = happyTcHack x happyReduce_47

action_111 (11#) = happyGoto action_165
action_111 x = happyTcHack x happyReduce_29

action_112 (96#) = happyShift action_164
action_112 x = happyTcHack x happyFail

action_113 (96#) = happyShift action_163
action_113 (33#) = happyGoto action_162
action_113 x = happyTcHack x happyReduce_69

action_114 (139#) = happyShift action_161
action_114 (32#) = happyGoto action_160
action_114 x = happyTcHack x happyReduce_67

action_115 (126#) = happyShift action_44
action_115 (127#) = happyShift action_45
action_115 (128#) = happyShift action_46
action_115 (129#) = happyShift action_47
action_115 (132#) = happyShift action_48
action_115 (133#) = happyShift action_49
action_115 (143#) = happyShift action_91
action_115 (145#) = happyShift action_51
action_115 (147#) = happyShift action_84
action_115 (149#) = happyShift action_54
action_115 (151#) = happyShift action_55
action_115 (153#) = happyShift action_92
action_115 (154#) = happyShift action_57
action_115 (155#) = happyShift action_58
action_115 (156#) = happyShift action_59
action_115 (36#) = happyGoto action_159
action_115 (37#) = happyGoto action_89
action_115 (54#) = happyGoto action_90
action_115 (56#) = happyGoto action_81
action_115 (59#) = happyGoto action_21
action_115 (60#) = happyGoto action_22
action_115 (66#) = happyGoto action_23
action_115 (67#) = happyGoto action_24
action_115 (68#) = happyGoto action_25
action_115 (82#) = happyGoto action_26
action_115 (84#) = happyGoto action_27
action_115 (85#) = happyGoto action_28
action_115 x = happyTcHack x happyFail

action_116 (145#) = happyShift action_73
action_116 (10#) = happyGoto action_158
action_116 (12#) = happyGoto action_70
action_116 (14#) = happyGoto action_71
action_116 x = happyTcHack x happyReduce_4

action_117 x = happyTcHack x happyReduce_180

action_118 (142#) = happyShift action_126
action_118 (157#) = happyShift action_127
action_118 (57#) = happyGoto action_123
action_118 x = happyTcHack x happyReduce_152

action_119 x = happyTcHack x happyReduce_141

action_120 (136#) = happyShift action_64
action_120 (153#) = happyShift action_65
action_120 (77#) = happyGoto action_61
action_120 (78#) = happyGoto action_62
action_120 (79#) = happyGoto action_76
action_120 x = happyTcHack x happyFail

action_121 (143#) = happyShift action_77
action_121 (153#) = happyShift action_101
action_121 (53#) = happyGoto action_157
action_121 x = happyTcHack x happyFail

action_122 x = happyTcHack x happyReduce_20

action_123 (126#) = happyShift action_44
action_123 (127#) = happyShift action_45
action_123 (128#) = happyShift action_46
action_123 (129#) = happyShift action_47
action_123 (132#) = happyShift action_48
action_123 (133#) = happyShift action_49
action_123 (143#) = happyShift action_91
action_123 (145#) = happyShift action_51
action_123 (147#) = happyShift action_84
action_123 (149#) = happyShift action_54
action_123 (151#) = happyShift action_55
action_123 (153#) = happyShift action_92
action_123 (154#) = happyShift action_57
action_123 (155#) = happyShift action_58
action_123 (156#) = happyShift action_59
action_123 (59#) = happyGoto action_156
action_123 (60#) = happyGoto action_22
action_123 (66#) = happyGoto action_23
action_123 (67#) = happyGoto action_24
action_123 (68#) = happyGoto action_25
action_123 (82#) = happyGoto action_26
action_123 (84#) = happyGoto action_27
action_123 (85#) = happyGoto action_28
action_123 x = happyTcHack x happyFail

action_124 (126#) = happyShift action_44
action_124 (127#) = happyShift action_45
action_124 (128#) = happyShift action_46
action_124 (129#) = happyShift action_47
action_124 (132#) = happyShift action_48
action_124 (133#) = happyShift action_49
action_124 (143#) = happyShift action_91
action_124 (145#) = happyShift action_51
action_124 (147#) = happyShift action_84
action_124 (149#) = happyShift action_54
action_124 (151#) = happyShift action_55
action_124 (153#) = happyShift action_92
action_124 (154#) = happyShift action_57
action_124 (155#) = happyShift action_58
action_124 (156#) = happyShift action_59
action_124 (56#) = happyGoto action_155
action_124 (59#) = happyGoto action_21
action_124 (60#) = happyGoto action_22
action_124 (66#) = happyGoto action_23
action_124 (67#) = happyGoto action_24
action_124 (68#) = happyGoto action_25
action_124 (82#) = happyGoto action_26
action_124 (84#) = happyGoto action_27
action_124 (85#) = happyGoto action_28
action_124 x = happyTcHack x happyFail

action_125 (126#) = happyShift action_44
action_125 (127#) = happyShift action_45
action_125 (128#) = happyShift action_46
action_125 (129#) = happyShift action_47
action_125 (132#) = happyShift action_48
action_125 (133#) = happyShift action_49
action_125 (143#) = happyShift action_91
action_125 (145#) = happyShift action_51
action_125 (147#) = happyShift action_84
action_125 (149#) = happyShift action_54
action_125 (151#) = happyShift action_55
action_125 (153#) = happyShift action_92
action_125 (154#) = happyShift action_57
action_125 (155#) = happyShift action_58
action_125 (156#) = happyShift action_59
action_125 (56#) = happyGoto action_154
action_125 (59#) = happyGoto action_21
action_125 (60#) = happyGoto action_22
action_125 (66#) = happyGoto action_23
action_125 (67#) = happyGoto action_24
action_125 (68#) = happyGoto action_25
action_125 (82#) = happyGoto action_26
action_125 (84#) = happyGoto action_27
action_125 (85#) = happyGoto action_28
action_125 x = happyTcHack x happyFail

action_126 (153#) = happyShift action_153
action_126 x = happyTcHack x happyFail

action_127 x = happyTcHack x happyReduce_120

action_128 (153#) = happyShift action_151
action_128 (156#) = happyShift action_152
action_128 (49#) = happyGoto action_150
action_128 x = happyTcHack x happyFail

action_129 (149#) = happyShift action_149
action_129 (48#) = happyGoto action_148
action_129 x = happyTcHack x happyFail

action_130 x = happyTcHack x happyReduce_100

action_131 x = happyTcHack x happyReduce_101

action_132 (126#) = happyShift action_44
action_132 (127#) = happyShift action_45
action_132 (128#) = happyShift action_46
action_132 (129#) = happyShift action_47
action_132 (132#) = happyShift action_48
action_132 (133#) = happyShift action_49
action_132 (143#) = happyShift action_91
action_132 (145#) = happyShift action_51
action_132 (147#) = happyShift action_84
action_132 (149#) = happyShift action_54
action_132 (151#) = happyShift action_55
action_132 (153#) = happyShift action_92
action_132 (154#) = happyShift action_57
action_132 (155#) = happyShift action_58
action_132 (156#) = happyShift action_59
action_132 (56#) = happyGoto action_147
action_132 (59#) = happyGoto action_21
action_132 (60#) = happyGoto action_22
action_132 (66#) = happyGoto action_23
action_132 (67#) = happyGoto action_24
action_132 (68#) = happyGoto action_25
action_132 (82#) = happyGoto action_26
action_132 (84#) = happyGoto action_27
action_132 (85#) = happyGoto action_28
action_132 x = happyTcHack x happyFail

action_133 (149#) = happyShift action_146
action_133 (43#) = happyGoto action_145
action_133 x = happyTcHack x happyReduce_94

action_134 (88#) = happyShift action_29
action_134 (143#) = happyShift action_77
action_134 (153#) = happyShift action_101
action_134 (41#) = happyGoto action_144
action_134 (53#) = happyGoto action_19
action_134 x = happyTcHack x happyFail

action_135 x = happyTcHack x happyReduce_86

action_136 x = happyTcHack x happyReduce_85

action_137 (126#) = happyShift action_44
action_137 (127#) = happyShift action_45
action_137 (128#) = happyShift action_46
action_137 (129#) = happyShift action_47
action_137 (132#) = happyShift action_48
action_137 (133#) = happyShift action_49
action_137 (143#) = happyShift action_91
action_137 (145#) = happyShift action_51
action_137 (147#) = happyShift action_84
action_137 (149#) = happyShift action_54
action_137 (151#) = happyShift action_55
action_137 (153#) = happyShift action_92
action_137 (154#) = happyShift action_57
action_137 (155#) = happyShift action_58
action_137 (156#) = happyShift action_59
action_137 (56#) = happyGoto action_143
action_137 (59#) = happyGoto action_21
action_137 (60#) = happyGoto action_22
action_137 (66#) = happyGoto action_23
action_137 (67#) = happyGoto action_24
action_137 (68#) = happyGoto action_25
action_137 (82#) = happyGoto action_26
action_137 (84#) = happyGoto action_27
action_137 (85#) = happyGoto action_28
action_137 x = happyTcHack x happyFail

action_138 (88#) = happyShift action_29
action_138 (143#) = happyShift action_141
action_138 (153#) = happyShift action_101
action_138 (38#) = happyGoto action_142
action_138 (39#) = happyGoto action_11
action_138 (40#) = happyGoto action_12
action_138 (41#) = happyGoto action_13
action_138 (53#) = happyGoto action_19
action_138 x = happyTcHack x happyReduce_88

action_139 (88#) = happyShift action_29
action_139 (143#) = happyShift action_141
action_139 (153#) = happyShift action_101
action_139 (38#) = happyGoto action_140
action_139 (39#) = happyGoto action_11
action_139 (40#) = happyGoto action_12
action_139 (41#) = happyGoto action_13
action_139 (53#) = happyGoto action_19
action_139 x = happyTcHack x happyReduce_88

action_140 (116#) = happyShift action_138
action_140 (117#) = happyShift action_139
action_140 x = happyTcHack x happyReduce_77

action_141 (88#) = happyShift action_29
action_141 (143#) = happyShift action_141
action_141 (153#) = happyShift action_101
action_141 (157#) = happyShift action_195
action_141 (38#) = happyGoto action_79
action_141 (39#) = happyGoto action_11
action_141 (40#) = happyGoto action_12
action_141 (41#) = happyGoto action_13
action_141 (53#) = happyGoto action_19
action_141 x = happyTcHack x happyReduce_88

action_142 (116#) = happyShift action_138
action_142 (117#) = happyShift action_139
action_142 x = happyTcHack x happyReduce_78

action_143 (142#) = happyShift action_126
action_143 (157#) = happyShift action_127
action_143 (57#) = happyGoto action_123
action_143 x = happyTcHack x happyReduce_84

action_144 x = happyTcHack x happyReduce_83

action_145 (136#) = happyShift action_64
action_145 (153#) = happyShift action_65
action_145 (78#) = happyGoto action_256
action_145 x = happyTcHack x happyReduce_81

action_146 (126#) = happyShift action_44
action_146 (127#) = happyShift action_45
action_146 (128#) = happyShift action_46
action_146 (129#) = happyShift action_47
action_146 (132#) = happyShift action_48
action_146 (133#) = happyShift action_49
action_146 (143#) = happyShift action_91
action_146 (145#) = happyShift action_51
action_146 (147#) = happyShift action_84
action_146 (149#) = happyShift action_54
action_146 (151#) = happyShift action_55
action_146 (153#) = happyShift action_92
action_146 (154#) = happyShift action_57
action_146 (155#) = happyShift action_58
action_146 (156#) = happyShift action_59
action_146 (54#) = happyGoto action_255
action_146 (56#) = happyGoto action_81
action_146 (59#) = happyGoto action_21
action_146 (60#) = happyGoto action_22
action_146 (66#) = happyGoto action_23
action_146 (67#) = happyGoto action_24
action_146 (68#) = happyGoto action_25
action_146 (82#) = happyGoto action_26
action_146 (84#) = happyGoto action_27
action_146 (85#) = happyGoto action_28
action_146 x = happyTcHack x happyFail

action_147 (142#) = happyShift action_126
action_147 (157#) = happyShift action_127
action_147 (57#) = happyGoto action_123
action_147 x = happyTcHack x happyReduce_80

action_148 (153#) = happyShift action_151
action_148 (156#) = happyShift action_152
action_148 (49#) = happyGoto action_254
action_148 x = happyTcHack x happyFail

action_149 (153#) = happyShift action_253
action_149 x = happyTcHack x happyFail

action_150 (149#) = happyShift action_252
action_150 x = happyTcHack x happyFail

action_151 x = happyTcHack x happyReduce_106

action_152 x = happyTcHack x happyReduce_105

action_153 (142#) = happyShift action_250
action_153 (157#) = happyShift action_251
action_153 x = happyTcHack x happyFail

action_154 (142#) = happyShift action_126
action_154 (157#) = happyShift action_127
action_154 (57#) = happyGoto action_123
action_154 x = happyTcHack x happyReduce_23

action_155 (142#) = happyShift action_126
action_155 (157#) = happyShift action_127
action_155 (57#) = happyGoto action_123
action_155 x = happyTcHack x happyReduce_22

action_156 (131#) = happyShift action_121
action_156 x = happyTcHack x happyReduce_119

action_157 (138#) = happyShift action_249
action_157 x = happyTcHack x happyFail

action_158 x = happyTcHack x happyReduce_24

action_159 (138#) = happyShift action_248
action_159 x = happyTcHack x happyFail

action_160 x = happyTcHack x happyReduce_65

action_161 (143#) = happyShift action_77
action_161 (153#) = happyShift action_101
action_161 (35#) = happyGoto action_247
action_161 (53#) = happyGoto action_115
action_161 x = happyTcHack x happyFail

action_162 (91#) = happyShift action_246
action_162 x = happyTcHack x happyFail

action_163 (34#) = happyGoto action_245
action_163 x = happyTcHack x happyReduce_71

action_164 x = happyTcHack x happyReduce_9

action_165 (145#) = happyShift action_169
action_165 (12#) = happyGoto action_244
action_165 x = happyTcHack x happyReduce_44

action_166 (138#) = happyShift action_243
action_166 x = happyTcHack x happyFail

action_167 (143#) = happyShift action_77
action_167 (153#) = happyShift action_101
action_167 (17#) = happyGoto action_242
action_167 (53#) = happyGoto action_201
action_167 x = happyTcHack x happyFail

action_168 (145#) = happyReduce_28
action_168 (24#) = happyGoto action_241
action_168 x = happyTcHack x happyReduce_50

action_169 (143#) = happyShift action_77
action_169 (153#) = happyShift action_101
action_169 (17#) = happyGoto action_240
action_169 (53#) = happyGoto action_201
action_169 x = happyTcHack x happyFail

action_170 (145#) = happyShift action_239
action_170 x = happyTcHack x happyReduce_57

action_171 (88#) = happyShift action_238
action_171 x = happyTcHack x happyFail

action_172 x = happyTcHack x happyReduce_40

action_173 (143#) = happyShift action_77
action_173 (153#) = happyShift action_101
action_173 (53#) = happyGoto action_237
action_173 x = happyTcHack x happyFail

action_174 x = happyTcHack x happyReduce_7

action_175 (126#) = happyShift action_44
action_175 (127#) = happyShift action_45
action_175 (128#) = happyShift action_236
action_175 (129#) = happyShift action_47
action_175 (132#) = happyShift action_48
action_175 (133#) = happyShift action_49
action_175 (143#) = happyShift action_91
action_175 (145#) = happyShift action_51
action_175 (147#) = happyShift action_84
action_175 (149#) = happyShift action_54
action_175 (151#) = happyShift action_55
action_175 (153#) = happyShift action_92
action_175 (154#) = happyShift action_57
action_175 (155#) = happyShift action_58
action_175 (156#) = happyShift action_59
action_175 (56#) = happyGoto action_235
action_175 (59#) = happyGoto action_21
action_175 (60#) = happyGoto action_22
action_175 (66#) = happyGoto action_23
action_175 (67#) = happyGoto action_24
action_175 (68#) = happyGoto action_25
action_175 (82#) = happyGoto action_26
action_175 (84#) = happyGoto action_27
action_175 (85#) = happyGoto action_28
action_175 x = happyTcHack x happyFail

action_176 x = happyTcHack x happyReduce_8

action_177 x = happyTcHack x happyReduce_98

action_178 (148#) = happyShift action_234
action_178 x = happyTcHack x happyFail

action_179 (144#) = happyShift action_233
action_179 x = happyTcHack x happyFail

action_180 x = happyTcHack x happyReduce_73

action_181 (149#) = happyShift action_232
action_181 x = happyTcHack x happyFail

action_182 (139#) = happyShift action_231
action_182 (61#) = happyGoto action_227
action_182 (62#) = happyGoto action_228
action_182 (63#) = happyGoto action_229
action_182 (65#) = happyGoto action_230
action_182 x = happyTcHack x happyFail

action_183 (139#) = happyShift action_226
action_183 (62#) = happyGoto action_224
action_183 (64#) = happyGoto action_225
action_183 x = happyTcHack x happyFail

action_184 (126#) = happyReduce_155
action_184 (127#) = happyReduce_155
action_184 (128#) = happyReduce_155
action_184 (131#) = happyReduce_155
action_184 (134#) = happyReduce_155
action_184 (135#) = happyReduce_155
action_184 (137#) = happyReduce_155
action_184 (138#) = happyReduce_155
action_184 (139#) = happyReduce_155
action_184 (140#) = happyReduce_155
action_184 (141#) = happyReduce_155
action_184 (142#) = happyReduce_155
action_184 (143#) = happyReduce_155
action_184 (144#) = happyReduce_155
action_184 (145#) = happyReduce_155
action_184 (147#) = happyReduce_155
action_184 (149#) = happyReduce_155
action_184 (151#) = happyReduce_155
action_184 (153#) = happyReduce_155
action_184 (154#) = happyReduce_155
action_184 (155#) = happyReduce_155
action_184 (156#) = happyReduce_155
action_184 (157#) = happyReduce_155
action_184 (158#) = happyReduce_155
action_184 x = happyTcHack x happyReduce_113

action_185 x = happyTcHack x happyReduce_154

action_186 (144#) = happyShift action_223
action_186 x = happyTcHack x happyFail

action_187 x = happyTcHack x happyReduce_159

action_188 (137#) = happyShift action_175
action_188 (144#) = happyShift action_222
action_188 (75#) = happyGoto action_220
action_188 (80#) = happyGoto action_221
action_188 x = happyTcHack x happyFail

action_189 (126#) = happyShift action_44
action_189 (127#) = happyShift action_45
action_189 (128#) = happyShift action_46
action_189 (129#) = happyShift action_47
action_189 (132#) = happyShift action_48
action_189 (133#) = happyShift action_49
action_189 (136#) = happyShift action_83
action_189 (143#) = happyShift action_91
action_189 (145#) = happyShift action_51
action_189 (147#) = happyShift action_84
action_189 (149#) = happyShift action_54
action_189 (151#) = happyShift action_55
action_189 (153#) = happyShift action_92
action_189 (154#) = happyShift action_57
action_189 (155#) = happyShift action_58
action_189 (156#) = happyShift action_59
action_189 (54#) = happyGoto action_80
action_189 (56#) = happyGoto action_81
action_189 (59#) = happyGoto action_21
action_189 (60#) = happyGoto action_22
action_189 (66#) = happyGoto action_23
action_189 (67#) = happyGoto action_24
action_189 (68#) = happyGoto action_25
action_189 (69#) = happyGoto action_219
action_189 (82#) = happyGoto action_26
action_189 (84#) = happyGoto action_27
action_189 (85#) = happyGoto action_28
action_189 x = happyTcHack x happyFail

action_190 (126#) = happyShift action_44
action_190 (127#) = happyShift action_45
action_190 (128#) = happyShift action_46
action_190 (129#) = happyShift action_47
action_190 (132#) = happyShift action_48
action_190 (133#) = happyShift action_49
action_190 (143#) = happyShift action_91
action_190 (145#) = happyShift action_51
action_190 (147#) = happyShift action_84
action_190 (149#) = happyShift action_54
action_190 (151#) = happyShift action_55
action_190 (153#) = happyShift action_92
action_190 (154#) = happyShift action_57
action_190 (155#) = happyShift action_58
action_190 (156#) = happyShift action_59
action_190 (56#) = happyGoto action_218
action_190 (59#) = happyGoto action_21
action_190 (60#) = happyGoto action_22
action_190 (66#) = happyGoto action_23
action_190 (67#) = happyGoto action_24
action_190 (68#) = happyGoto action_25
action_190 (82#) = happyGoto action_26
action_190 (84#) = happyGoto action_27
action_190 (85#) = happyGoto action_28
action_190 x = happyTcHack x happyFail

action_191 (126#) = happyShift action_44
action_191 (127#) = happyShift action_45
action_191 (128#) = happyShift action_46
action_191 (129#) = happyShift action_47
action_191 (132#) = happyShift action_48
action_191 (133#) = happyShift action_49
action_191 (143#) = happyShift action_91
action_191 (145#) = happyShift action_51
action_191 (147#) = happyShift action_84
action_191 (149#) = happyShift action_54
action_191 (151#) = happyShift action_55
action_191 (153#) = happyShift action_92
action_191 (154#) = happyShift action_57
action_191 (155#) = happyShift action_58
action_191 (156#) = happyShift action_59
action_191 (56#) = happyGoto action_217
action_191 (59#) = happyGoto action_21
action_191 (60#) = happyGoto action_22
action_191 (66#) = happyGoto action_23
action_191 (67#) = happyGoto action_24
action_191 (68#) = happyGoto action_25
action_191 (82#) = happyGoto action_26
action_191 (84#) = happyGoto action_27
action_191 (85#) = happyGoto action_28
action_191 x = happyTcHack x happyFail

action_192 x = happyTcHack x happyReduce_116

action_193 x = happyTcHack x happyReduce_117

action_194 x = happyTcHack x happyReduce_79

action_195 (144#) = happyShift action_216
action_195 x = happyTcHack x happyFail

action_196 x = happyTcHack x happyReduce_182

action_197 (126#) = happyShift action_44
action_197 (127#) = happyShift action_45
action_197 (128#) = happyShift action_46
action_197 (129#) = happyShift action_47
action_197 (132#) = happyShift action_48
action_197 (133#) = happyShift action_49
action_197 (143#) = happyShift action_91
action_197 (145#) = happyShift action_51
action_197 (147#) = happyShift action_84
action_197 (149#) = happyShift action_54
action_197 (151#) = happyShift action_55
action_197 (153#) = happyShift action_92
action_197 (154#) = happyShift action_57
action_197 (155#) = happyShift action_58
action_197 (156#) = happyShift action_59
action_197 (54#) = happyGoto action_215
action_197 (56#) = happyGoto action_81
action_197 (59#) = happyGoto action_21
action_197 (60#) = happyGoto action_22
action_197 (66#) = happyGoto action_23
action_197 (67#) = happyGoto action_24
action_197 (68#) = happyGoto action_25
action_197 (82#) = happyGoto action_26
action_197 (84#) = happyGoto action_27
action_197 (85#) = happyGoto action_28
action_197 x = happyTcHack x happyFail

action_198 x = happyTcHack x happyReduce_31

action_199 (143#) = happyShift action_77
action_199 (153#) = happyShift action_101
action_199 (17#) = happyGoto action_214
action_199 (53#) = happyGoto action_201
action_199 x = happyTcHack x happyFail

action_200 (137#) = happyShift action_175
action_200 (139#) = happyShift action_213
action_200 (15#) = happyGoto action_211
action_200 (80#) = happyGoto action_212
action_200 x = happyTcHack x happyReduce_36

action_201 x = happyTcHack x happyReduce_39

action_202 x = happyTcHack x happyReduce_184

action_203 (126#) = happyShift action_44
action_203 (127#) = happyShift action_45
action_203 (128#) = happyShift action_46
action_203 (129#) = happyShift action_47
action_203 (132#) = happyShift action_48
action_203 (133#) = happyShift action_49
action_203 (143#) = happyShift action_91
action_203 (145#) = happyShift action_51
action_203 (147#) = happyShift action_84
action_203 (149#) = happyShift action_54
action_203 (151#) = happyShift action_55
action_203 (153#) = happyShift action_92
action_203 (154#) = happyShift action_57
action_203 (155#) = happyShift action_58
action_203 (156#) = happyShift action_59
action_203 (56#) = happyGoto action_210
action_203 (59#) = happyGoto action_21
action_203 (60#) = happyGoto action_22
action_203 (66#) = happyGoto action_23
action_203 (67#) = happyGoto action_24
action_203 (68#) = happyGoto action_25
action_203 (82#) = happyGoto action_26
action_203 (84#) = happyGoto action_27
action_203 (85#) = happyGoto action_28
action_203 x = happyTcHack x happyFail

action_204 x = happyTcHack x happyReduce_183

action_205 x = happyTcHack x happyReduce_185

action_206 x = happyTcHack x happyReduce_179

action_207 x = happyTcHack x happyReduce_174

action_208 (136#) = happyShift action_64
action_208 (153#) = happyShift action_65
action_208 (78#) = happyGoto action_209
action_208 x = happyTcHack x happyFail

action_209 x = happyTcHack x happyReduce_170

action_210 (138#) = happyShift action_124
action_210 (141#) = happyShift action_125
action_210 (142#) = happyShift action_126
action_210 (157#) = happyShift action_127
action_210 (8#) = happyGoto action_304
action_210 (57#) = happyGoto action_123
action_210 x = happyTcHack x happyFail

action_211 (138#) = happyShift action_302
action_211 (145#) = happyShift action_303
action_211 (16#) = happyGoto action_301
action_211 x = happyTcHack x happyFail

action_212 (138#) = happyShift action_299
action_212 (146#) = happyShift action_300
action_212 x = happyTcHack x happyFail

action_213 (143#) = happyShift action_77
action_213 (153#) = happyShift action_101
action_213 (53#) = happyGoto action_298
action_213 x = happyTcHack x happyFail

action_214 (137#) = happyShift action_175
action_214 (139#) = happyShift action_213
action_214 (15#) = happyGoto action_211
action_214 (80#) = happyGoto action_297
action_214 x = happyTcHack x happyReduce_36

action_215 (146#) = happyShift action_296
action_215 x = happyTcHack x happyFail

action_216 x = happyTcHack x happyReduce_113

action_217 (142#) = happyShift action_126
action_217 (157#) = happyShift action_127
action_217 (57#) = happyGoto action_123
action_217 x = happyTcHack x happyReduce_115

action_218 (140#) = happyShift action_295
action_218 (142#) = happyShift action_126
action_218 (157#) = happyShift action_127
action_218 (57#) = happyGoto action_123
action_218 (72#) = happyGoto action_294
action_218 x = happyTcHack x happyReduce_162

action_219 (139#) = happyShift action_189
action_219 (74#) = happyGoto action_293
action_219 x = happyTcHack x happyReduce_166

action_220 x = happyTcHack x happyReduce_163

action_221 (144#) = happyShift action_292
action_221 x = happyTcHack x happyFail

action_222 x = happyTcHack x happyReduce_164

action_223 x = happyTcHack x happyReduce_158

action_224 (139#) = happyShift action_226
action_224 (62#) = happyGoto action_224
action_224 (64#) = happyGoto action_291
action_224 x = happyTcHack x happyReduce_138

action_225 (148#) = happyShift action_290
action_225 x = happyTcHack x happyFail

action_226 (143#) = happyShift action_77
action_226 (153#) = happyShift action_101
action_226 (53#) = happyGoto action_289
action_226 x = happyTcHack x happyFail

action_227 x = happyTcHack x happyReduce_135

action_228 x = happyTcHack x happyReduce_136

action_229 (139#) = happyShift action_231
action_229 (61#) = happyGoto action_227
action_229 (62#) = happyGoto action_228
action_229 (63#) = happyGoto action_229
action_229 (65#) = happyGoto action_288
action_229 x = happyTcHack x happyReduce_140

action_230 (148#) = happyShift action_287
action_230 x = happyTcHack x happyFail

action_231 (143#) = happyShift action_77
action_231 (153#) = happyShift action_101
action_231 (53#) = happyGoto action_286
action_231 x = happyTcHack x happyFail

action_232 (126#) = happyShift action_44
action_232 (127#) = happyShift action_45
action_232 (128#) = happyShift action_46
action_232 (129#) = happyShift action_47
action_232 (132#) = happyShift action_48
action_232 (133#) = happyShift action_49
action_232 (143#) = happyShift action_91
action_232 (145#) = happyShift action_51
action_232 (147#) = happyShift action_84
action_232 (149#) = happyShift action_54
action_232 (151#) = happyShift action_55
action_232 (153#) = happyShift action_92
action_232 (154#) = happyShift action_57
action_232 (155#) = happyShift action_58
action_232 (156#) = happyShift action_59
action_232 (36#) = happyGoto action_283
action_232 (37#) = happyGoto action_89
action_232 (54#) = happyGoto action_90
action_232 (56#) = happyGoto action_81
action_232 (59#) = happyGoto action_21
action_232 (60#) = happyGoto action_22
action_232 (66#) = happyGoto action_23
action_232 (67#) = happyGoto action_24
action_232 (68#) = happyGoto action_25
action_232 (82#) = happyGoto action_26
action_232 (84#) = happyGoto action_27
action_232 (85#) = happyGoto action_28
action_232 (86#) = happyGoto action_284
action_232 (87#) = happyGoto action_285
action_232 x = happyTcHack x happyFail

action_233 x = happyTcHack x happyReduce_155

action_234 (126#) = happyShift action_44
action_234 (127#) = happyShift action_45
action_234 (128#) = happyShift action_46
action_234 (129#) = happyShift action_47
action_234 (132#) = happyShift action_48
action_234 (133#) = happyShift action_49
action_234 (143#) = happyShift action_91
action_234 (145#) = happyShift action_51
action_234 (147#) = happyShift action_84
action_234 (149#) = happyShift action_54
action_234 (151#) = happyShift action_55
action_234 (153#) = happyShift action_92
action_234 (154#) = happyShift action_57
action_234 (155#) = happyShift action_58
action_234 (156#) = happyShift action_59
action_234 (54#) = happyGoto action_282
action_234 (56#) = happyGoto action_81
action_234 (59#) = happyGoto action_21
action_234 (60#) = happyGoto action_22
action_234 (66#) = happyGoto action_23
action_234 (67#) = happyGoto action_24
action_234 (68#) = happyGoto action_25
action_234 (82#) = happyGoto action_26
action_234 (84#) = happyGoto action_27
action_234 (85#) = happyGoto action_28
action_234 x = happyTcHack x happyFail

action_235 (142#) = happyShift action_126
action_235 (157#) = happyShift action_127
action_235 (57#) = happyGoto action_123
action_235 x = happyTcHack x happyReduce_175

action_236 (140#) = happyShift action_281
action_236 (143#) = happyShift action_91
action_236 (153#) = happyShift action_94
action_236 (155#) = happyShift action_95
action_236 (156#) = happyShift action_96
action_236 (68#) = happyGoto action_93
action_236 x = happyTcHack x happyFail

action_237 (138#) = happyShift action_280
action_237 x = happyTcHack x happyFail

action_238 (28#) = happyGoto action_279
action_238 x = happyTcHack x happyReduce_62

action_239 (126#) = happyShift action_44
action_239 (127#) = happyShift action_45
action_239 (128#) = happyShift action_46
action_239 (129#) = happyShift action_47
action_239 (132#) = happyShift action_48
action_239 (133#) = happyShift action_49
action_239 (143#) = happyShift action_91
action_239 (145#) = happyShift action_51
action_239 (147#) = happyShift action_84
action_239 (149#) = happyShift action_54
action_239 (151#) = happyShift action_55
action_239 (153#) = happyShift action_92
action_239 (154#) = happyShift action_57
action_239 (155#) = happyShift action_58
action_239 (156#) = happyShift action_59
action_239 (56#) = happyGoto action_278
action_239 (59#) = happyGoto action_21
action_239 (60#) = happyGoto action_22
action_239 (66#) = happyGoto action_23
action_239 (67#) = happyGoto action_24
action_239 (68#) = happyGoto action_25
action_239 (82#) = happyGoto action_26
action_239 (84#) = happyGoto action_27
action_239 (85#) = happyGoto action_28
action_239 x = happyTcHack x happyFail

action_240 (137#) = happyShift action_175
action_240 (139#) = happyShift action_213
action_240 (80#) = happyGoto action_277
action_240 x = happyTcHack x happyFail

action_241 (104#) = happyShift action_270
action_241 (105#) = happyShift action_271
action_241 (106#) = happyShift action_272
action_241 (107#) = happyShift action_273
action_241 (108#) = happyShift action_274
action_241 (109#) = happyShift action_275
action_241 (110#) = happyShift action_276
action_241 (25#) = happyGoto action_269
action_241 x = happyTcHack x happyFail

action_242 (139#) = happyShift action_213
action_242 x = happyTcHack x happyReduce_46

action_243 (126#) = happyShift action_44
action_243 (127#) = happyShift action_45
action_243 (128#) = happyShift action_46
action_243 (129#) = happyShift action_47
action_243 (132#) = happyShift action_48
action_243 (133#) = happyShift action_49
action_243 (143#) = happyShift action_91
action_243 (145#) = happyShift action_51
action_243 (147#) = happyShift action_84
action_243 (149#) = happyShift action_54
action_243 (151#) = happyShift action_55
action_243 (153#) = happyShift action_92
action_243 (154#) = happyShift action_57
action_243 (155#) = happyShift action_58
action_243 (156#) = happyShift action_59
action_243 (56#) = happyGoto action_268
action_243 (59#) = happyGoto action_21
action_243 (60#) = happyGoto action_22
action_243 (66#) = happyGoto action_23
action_243 (67#) = happyGoto action_24
action_243 (68#) = happyGoto action_25
action_243 (82#) = happyGoto action_26
action_243 (84#) = happyGoto action_27
action_243 (85#) = happyGoto action_28
action_243 x = happyTcHack x happyFail

action_244 x = happyTcHack x happyReduce_28

action_245 (90#) = happyShift action_31
action_245 (30#) = happyGoto action_267
action_245 x = happyTcHack x happyReduce_68

action_246 x = happyTcHack x happyReduce_64

action_247 (139#) = happyShift action_161
action_247 (32#) = happyGoto action_266
action_247 x = happyTcHack x happyReduce_67

action_248 (126#) = happyShift action_44
action_248 (127#) = happyShift action_45
action_248 (128#) = happyShift action_46
action_248 (129#) = happyShift action_47
action_248 (132#) = happyShift action_48
action_248 (133#) = happyShift action_49
action_248 (143#) = happyShift action_91
action_248 (145#) = happyShift action_51
action_248 (147#) = happyShift action_84
action_248 (149#) = happyShift action_54
action_248 (151#) = happyShift action_55
action_248 (153#) = happyShift action_92
action_248 (154#) = happyShift action_57
action_248 (155#) = happyShift action_58
action_248 (156#) = happyShift action_59
action_248 (56#) = happyGoto action_265
action_248 (59#) = happyGoto action_21
action_248 (60#) = happyGoto action_22
action_248 (66#) = happyGoto action_23
action_248 (67#) = happyGoto action_24
action_248 (68#) = happyGoto action_25
action_248 (82#) = happyGoto action_26
action_248 (84#) = happyGoto action_27
action_248 (85#) = happyGoto action_28
action_248 x = happyTcHack x happyFail

action_249 (126#) = happyShift action_44
action_249 (127#) = happyShift action_45
action_249 (128#) = happyShift action_46
action_249 (143#) = happyShift action_91
action_249 (145#) = happyShift action_120
action_249 (147#) = happyShift action_84
action_249 (149#) = happyShift action_54
action_249 (151#) = happyShift action_55
action_249 (153#) = happyShift action_92
action_249 (154#) = happyShift action_57
action_249 (155#) = happyShift action_58
action_249 (156#) = happyShift action_59
action_249 (67#) = happyGoto action_264
action_249 (68#) = happyGoto action_25
action_249 (82#) = happyGoto action_26
action_249 (84#) = happyGoto action_27
action_249 x = happyTcHack x happyFail

action_250 x = happyTcHack x happyReduce_121

action_251 (153#) = happyShift action_262
action_251 (157#) = happyShift action_263
action_251 (58#) = happyGoto action_261
action_251 x = happyTcHack x happyFail

action_252 (51#) = happyGoto action_260
action_252 x = happyTcHack x happyReduce_110

action_253 (138#) = happyShift action_259
action_253 x = happyTcHack x happyFail

action_254 (153#) = happyShift action_151
action_254 (156#) = happyShift action_152
action_254 (49#) = happyGoto action_258
action_254 x = happyTcHack x happyFail

action_255 (150#) = happyShift action_257
action_255 x = happyTcHack x happyFail

action_256 x = happyTcHack x happyReduce_93

action_257 (43#) = happyGoto action_330
action_257 x = happyTcHack x happyReduce_94

action_258 x = happyTcHack x happyReduce_103

action_259 (126#) = happyShift action_44
action_259 (127#) = happyShift action_45
action_259 (128#) = happyShift action_46
action_259 (129#) = happyShift action_47
action_259 (132#) = happyShift action_48
action_259 (133#) = happyShift action_49
action_259 (143#) = happyShift action_91
action_259 (145#) = happyShift action_51
action_259 (147#) = happyShift action_84
action_259 (149#) = happyShift action_54
action_259 (151#) = happyShift action_55
action_259 (153#) = happyShift action_92
action_259 (154#) = happyShift action_57
action_259 (155#) = happyShift action_58
action_259 (156#) = happyShift action_59
action_259 (56#) = happyGoto action_329
action_259 (59#) = happyGoto action_21
action_259 (60#) = happyGoto action_22
action_259 (66#) = happyGoto action_23
action_259 (67#) = happyGoto action_24
action_259 (68#) = happyGoto action_25
action_259 (82#) = happyGoto action_26
action_259 (84#) = happyGoto action_27
action_259 (85#) = happyGoto action_28
action_259 x = happyTcHack x happyFail

action_260 (139#) = happyShift action_327
action_260 (150#) = happyShift action_328
action_260 x = happyTcHack x happyFail

action_261 (142#) = happyShift action_326
action_261 x = happyTcHack x happyFail

action_262 (157#) = happyShift action_325
action_262 x = happyTcHack x happyReduce_124

action_263 x = happyTcHack x happyReduce_123

action_264 x = happyTcHack x happyReduce_130

action_265 (142#) = happyShift action_126
action_265 (157#) = happyShift action_127
action_265 (57#) = happyGoto action_123
action_265 x = happyTcHack x happyReduce_72

action_266 x = happyTcHack x happyReduce_66

action_267 x = happyTcHack x happyReduce_70

action_268 (137#) = happyShift action_175
action_268 (142#) = happyShift action_126
action_268 (157#) = happyShift action_127
action_268 (57#) = happyGoto action_123
action_268 (80#) = happyGoto action_323
action_268 (81#) = happyGoto action_324
action_268 x = happyTcHack x happyReduce_178

action_269 x = happyTcHack x happyReduce_49

action_270 (11#) = happyGoto action_322
action_270 x = happyTcHack x happyReduce_29

action_271 x = happyTcHack x happyReduce_51

action_272 x = happyTcHack x happyReduce_52

action_273 x = happyTcHack x happyReduce_53

action_274 x = happyTcHack x happyReduce_54

action_275 x = happyTcHack x happyReduce_55

action_276 x = happyTcHack x happyReduce_56

action_277 (146#) = happyShift action_300
action_277 x = happyTcHack x happyFail

action_278 (138#) = happyShift action_321
action_278 (142#) = happyShift action_126
action_278 (157#) = happyShift action_127
action_278 (57#) = happyGoto action_123
action_278 x = happyTcHack x happyFail

action_279 (145#) = happyShift action_320
action_279 x = happyTcHack x happyReduce_58

action_280 (126#) = happyShift action_44
action_280 (127#) = happyShift action_45
action_280 (128#) = happyShift action_46
action_280 (129#) = happyShift action_47
action_280 (132#) = happyShift action_48
action_280 (133#) = happyShift action_49
action_280 (143#) = happyShift action_91
action_280 (145#) = happyShift action_51
action_280 (147#) = happyShift action_84
action_280 (149#) = happyShift action_54
action_280 (151#) = happyShift action_55
action_280 (153#) = happyShift action_92
action_280 (154#) = happyShift action_57
action_280 (155#) = happyShift action_58
action_280 (156#) = happyShift action_59
action_280 (54#) = happyGoto action_319
action_280 (56#) = happyGoto action_81
action_280 (59#) = happyGoto action_21
action_280 (60#) = happyGoto action_22
action_280 (66#) = happyGoto action_23
action_280 (67#) = happyGoto action_24
action_280 (68#) = happyGoto action_25
action_280 (82#) = happyGoto action_26
action_280 (84#) = happyGoto action_27
action_280 (85#) = happyGoto action_28
action_280 x = happyTcHack x happyFail

action_281 (126#) = happyShift action_44
action_281 (127#) = happyShift action_45
action_281 (128#) = happyShift action_46
action_281 (129#) = happyShift action_47
action_281 (132#) = happyShift action_48
action_281 (133#) = happyShift action_49
action_281 (143#) = happyShift action_91
action_281 (145#) = happyShift action_51
action_281 (147#) = happyShift action_84
action_281 (149#) = happyShift action_54
action_281 (151#) = happyShift action_55
action_281 (153#) = happyShift action_92
action_281 (154#) = happyShift action_57
action_281 (155#) = happyShift action_58
action_281 (156#) = happyShift action_59
action_281 (56#) = happyGoto action_318
action_281 (59#) = happyGoto action_21
action_281 (60#) = happyGoto action_22
action_281 (66#) = happyGoto action_23
action_281 (67#) = happyGoto action_24
action_281 (68#) = happyGoto action_25
action_281 (82#) = happyGoto action_26
action_281 (84#) = happyGoto action_27
action_281 (85#) = happyGoto action_28
action_281 x = happyTcHack x happyFail

action_282 (148#) = happyShift action_317
action_282 x = happyTcHack x happyFail

action_283 (138#) = happyShift action_316
action_283 x = happyTcHack x happyFail

action_284 (150#) = happyShift action_315
action_284 x = happyTcHack x happyFail

action_285 (139#) = happyShift action_314
action_285 x = happyTcHack x happyReduce_188

action_286 (137#) = happyShift action_313
action_286 (138#) = happyShift action_312
action_286 x = happyTcHack x happyFail

action_287 x = happyTcHack x happyReduce_132

action_288 x = happyTcHack x happyReduce_139

action_289 (138#) = happyShift action_312
action_289 x = happyTcHack x happyFail

action_290 x = happyTcHack x happyReduce_131

action_291 x = happyTcHack x happyReduce_137

action_292 (157#) = happyShift action_311
action_292 (76#) = happyGoto action_310
action_292 x = happyTcHack x happyReduce_169

action_293 x = happyTcHack x happyReduce_165

action_294 x = happyTcHack x happyReduce_160

action_295 (126#) = happyShift action_44
action_295 (127#) = happyShift action_45
action_295 (128#) = happyShift action_46
action_295 (129#) = happyShift action_47
action_295 (132#) = happyShift action_48
action_295 (133#) = happyShift action_49
action_295 (143#) = happyShift action_91
action_295 (145#) = happyShift action_51
action_295 (147#) = happyShift action_84
action_295 (149#) = happyShift action_54
action_295 (151#) = happyShift action_55
action_295 (153#) = happyShift action_92
action_295 (154#) = happyShift action_57
action_295 (155#) = happyShift action_58
action_295 (156#) = happyShift action_59
action_295 (56#) = happyGoto action_309
action_295 (59#) = happyGoto action_21
action_295 (60#) = happyGoto action_22
action_295 (66#) = happyGoto action_23
action_295 (67#) = happyGoto action_24
action_295 (68#) = happyGoto action_25
action_295 (82#) = happyGoto action_26
action_295 (84#) = happyGoto action_27
action_295 (85#) = happyGoto action_28
action_295 x = happyTcHack x happyFail

action_296 (126#) = happyShift action_44
action_296 (127#) = happyShift action_45
action_296 (128#) = happyShift action_46
action_296 (129#) = happyShift action_47
action_296 (132#) = happyShift action_48
action_296 (133#) = happyShift action_49
action_296 (143#) = happyShift action_91
action_296 (145#) = happyShift action_51
action_296 (147#) = happyShift action_84
action_296 (149#) = happyShift action_54
action_296 (151#) = happyShift action_55
action_296 (153#) = happyShift action_92
action_296 (154#) = happyShift action_57
action_296 (155#) = happyShift action_58
action_296 (156#) = happyShift action_59
action_296 (56#) = happyGoto action_308
action_296 (59#) = happyGoto action_21
action_296 (60#) = happyGoto action_22
action_296 (66#) = happyGoto action_23
action_296 (67#) = happyGoto action_24
action_296 (68#) = happyGoto action_25
action_296 (82#) = happyGoto action_26
action_296 (84#) = happyGoto action_27
action_296 (85#) = happyGoto action_28
action_296 x = happyTcHack x happyFail

action_297 (138#) = happyShift action_299
action_297 x = happyTcHack x happyFail

action_298 x = happyTcHack x happyReduce_38

action_299 (126#) = happyShift action_44
action_299 (127#) = happyShift action_45
action_299 (128#) = happyShift action_46
action_299 (129#) = happyShift action_47
action_299 (132#) = happyShift action_48
action_299 (133#) = happyShift action_49
action_299 (143#) = happyShift action_91
action_299 (145#) = happyShift action_51
action_299 (147#) = happyShift action_84
action_299 (149#) = happyShift action_54
action_299 (151#) = happyShift action_55
action_299 (153#) = happyShift action_92
action_299 (154#) = happyShift action_57
action_299 (155#) = happyShift action_58
action_299 (156#) = happyShift action_59
action_299 (56#) = happyGoto action_307
action_299 (59#) = happyGoto action_21
action_299 (60#) = happyGoto action_22
action_299 (66#) = happyGoto action_23
action_299 (67#) = happyGoto action_24
action_299 (68#) = happyGoto action_25
action_299 (82#) = happyGoto action_26
action_299 (84#) = happyGoto action_27
action_299 (85#) = happyGoto action_28
action_299 x = happyTcHack x happyFail

action_300 x = happyTcHack x happyReduce_30

action_301 x = happyTcHack x happyReduce_35

action_302 (126#) = happyShift action_44
action_302 (127#) = happyShift action_45
action_302 (128#) = happyShift action_46
action_302 (129#) = happyShift action_47
action_302 (132#) = happyShift action_48
action_302 (133#) = happyShift action_49
action_302 (143#) = happyShift action_91
action_302 (145#) = happyShift action_51
action_302 (147#) = happyShift action_84
action_302 (149#) = happyShift action_54
action_302 (151#) = happyShift action_55
action_302 (153#) = happyShift action_92
action_302 (154#) = happyShift action_57
action_302 (155#) = happyShift action_58
action_302 (156#) = happyShift action_59
action_302 (56#) = happyGoto action_306
action_302 (59#) = happyGoto action_21
action_302 (60#) = happyGoto action_22
action_302 (66#) = happyGoto action_23
action_302 (67#) = happyGoto action_24
action_302 (68#) = happyGoto action_25
action_302 (82#) = happyGoto action_26
action_302 (84#) = happyGoto action_27
action_302 (85#) = happyGoto action_28
action_302 x = happyTcHack x happyFail

action_303 (136#) = happyShift action_64
action_303 (153#) = happyShift action_65
action_303 (77#) = happyGoto action_305
action_303 (78#) = happyGoto action_62
action_303 x = happyTcHack x happyFail

action_304 x = happyTcHack x happyReduce_21

action_305 (137#) = happyShift action_175
action_305 (139#) = happyShift action_208
action_305 (80#) = happyGoto action_349
action_305 x = happyTcHack x happyFail

action_306 (137#) = happyShift action_175
action_306 (142#) = happyShift action_126
action_306 (157#) = happyShift action_127
action_306 (57#) = happyGoto action_123
action_306 (80#) = happyGoto action_323
action_306 (81#) = happyGoto action_348
action_306 x = happyTcHack x happyReduce_178

action_307 (142#) = happyShift action_126
action_307 (146#) = happyShift action_347
action_307 (157#) = happyShift action_127
action_307 (57#) = happyGoto action_123
action_307 x = happyTcHack x happyFail

action_308 (142#) = happyShift action_126
action_308 (157#) = happyShift action_127
action_308 (57#) = happyGoto action_123
action_308 x = happyTcHack x happyReduce_127

action_309 (140#) = happyShift action_295
action_309 (142#) = happyShift action_126
action_309 (157#) = happyShift action_127
action_309 (57#) = happyGoto action_123
action_309 (72#) = happyGoto action_346
action_309 x = happyTcHack x happyReduce_162

action_310 (126#) = happyShift action_44
action_310 (127#) = happyShift action_45
action_310 (128#) = happyShift action_46
action_310 (129#) = happyShift action_47
action_310 (132#) = happyShift action_48
action_310 (133#) = happyShift action_49
action_310 (143#) = happyShift action_91
action_310 (145#) = happyShift action_51
action_310 (147#) = happyShift action_84
action_310 (149#) = happyShift action_54
action_310 (151#) = happyShift action_55
action_310 (153#) = happyShift action_92
action_310 (154#) = happyShift action_57
action_310 (155#) = happyShift action_58
action_310 (156#) = happyShift action_59
action_310 (56#) = happyGoto action_345
action_310 (59#) = happyGoto action_21
action_310 (60#) = happyGoto action_22
action_310 (66#) = happyGoto action_23
action_310 (67#) = happyGoto action_24
action_310 (68#) = happyGoto action_25
action_310 (82#) = happyGoto action_26
action_310 (84#) = happyGoto action_27
action_310 (85#) = happyGoto action_28
action_310 x = happyTcHack x happyFail

action_311 x = happyTcHack x happyReduce_168

action_312 (126#) = happyShift action_44
action_312 (127#) = happyShift action_45
action_312 (128#) = happyShift action_46
action_312 (129#) = happyShift action_47
action_312 (132#) = happyShift action_48
action_312 (133#) = happyShift action_49
action_312 (143#) = happyShift action_91
action_312 (145#) = happyShift action_51
action_312 (147#) = happyShift action_84
action_312 (149#) = happyShift action_54
action_312 (151#) = happyShift action_55
action_312 (153#) = happyShift action_92
action_312 (154#) = happyShift action_57
action_312 (155#) = happyShift action_58
action_312 (156#) = happyShift action_59
action_312 (54#) = happyGoto action_344
action_312 (56#) = happyGoto action_81
action_312 (59#) = happyGoto action_21
action_312 (60#) = happyGoto action_22
action_312 (66#) = happyGoto action_23
action_312 (67#) = happyGoto action_24
action_312 (68#) = happyGoto action_25
action_312 (82#) = happyGoto action_26
action_312 (84#) = happyGoto action_27
action_312 (85#) = happyGoto action_28
action_312 x = happyTcHack x happyFail

action_313 (126#) = happyShift action_44
action_313 (127#) = happyShift action_45
action_313 (128#) = happyShift action_46
action_313 (129#) = happyShift action_47
action_313 (132#) = happyShift action_48
action_313 (133#) = happyShift action_49
action_313 (143#) = happyShift action_91
action_313 (145#) = happyShift action_51
action_313 (147#) = happyShift action_84
action_313 (149#) = happyShift action_54
action_313 (151#) = happyShift action_55
action_313 (153#) = happyShift action_92
action_313 (154#) = happyShift action_57
action_313 (155#) = happyShift action_58
action_313 (156#) = happyShift action_59
action_313 (54#) = happyGoto action_343
action_313 (56#) = happyGoto action_81
action_313 (59#) = happyGoto action_21
action_313 (60#) = happyGoto action_22
action_313 (66#) = happyGoto action_23
action_313 (67#) = happyGoto action_24
action_313 (68#) = happyGoto action_25
action_313 (82#) = happyGoto action_26
action_313 (84#) = happyGoto action_27
action_313 (85#) = happyGoto action_28
action_313 x = happyTcHack x happyFail

action_314 (126#) = happyShift action_44
action_314 (127#) = happyShift action_45
action_314 (128#) = happyShift action_46
action_314 (129#) = happyShift action_47
action_314 (132#) = happyShift action_48
action_314 (133#) = happyShift action_49
action_314 (143#) = happyShift action_91
action_314 (145#) = happyShift action_51
action_314 (147#) = happyShift action_84
action_314 (149#) = happyShift action_54
action_314 (151#) = happyShift action_55
action_314 (153#) = happyShift action_92
action_314 (154#) = happyShift action_57
action_314 (155#) = happyShift action_58
action_314 (156#) = happyShift action_59
action_314 (36#) = happyGoto action_283
action_314 (37#) = happyGoto action_89
action_314 (54#) = happyGoto action_90
action_314 (56#) = happyGoto action_81
action_314 (59#) = happyGoto action_21
action_314 (60#) = happyGoto action_22
action_314 (66#) = happyGoto action_23
action_314 (67#) = happyGoto action_24
action_314 (68#) = happyGoto action_25
action_314 (82#) = happyGoto action_26
action_314 (84#) = happyGoto action_27
action_314 (85#) = happyGoto action_28
action_314 (86#) = happyGoto action_342
action_314 (87#) = happyGoto action_285
action_314 x = happyTcHack x happyFail

action_315 x = happyTcHack x happyReduce_186

action_316 (126#) = happyShift action_44
action_316 (127#) = happyShift action_45
action_316 (128#) = happyShift action_46
action_316 (129#) = happyShift action_47
action_316 (132#) = happyShift action_48
action_316 (133#) = happyShift action_49
action_316 (143#) = happyShift action_91
action_316 (145#) = happyShift action_51
action_316 (147#) = happyShift action_84
action_316 (149#) = happyShift action_54
action_316 (151#) = happyShift action_55
action_316 (153#) = happyShift action_92
action_316 (154#) = happyShift action_57
action_316 (155#) = happyShift action_58
action_316 (156#) = happyShift action_59
action_316 (54#) = happyGoto action_341
action_316 (56#) = happyGoto action_81
action_316 (59#) = happyGoto action_21
action_316 (60#) = happyGoto action_22
action_316 (66#) = happyGoto action_23
action_316 (67#) = happyGoto action_24
action_316 (68#) = happyGoto action_25
action_316 (82#) = happyGoto action_26
action_316 (84#) = happyGoto action_27
action_316 (85#) = happyGoto action_28
action_316 x = happyTcHack x happyFail

action_317 (143#) = happyShift action_77
action_317 (153#) = happyShift action_101
action_317 (53#) = happyGoto action_340
action_317 x = happyTcHack x happyFail

action_318 (142#) = happyShift action_126
action_318 (157#) = happyShift action_127
action_318 (57#) = happyGoto action_123
action_318 x = happyTcHack x happyReduce_176

action_319 (146#) = happyShift action_339
action_319 x = happyTcHack x happyFail

action_320 (143#) = happyShift action_77
action_320 (153#) = happyShift action_101
action_320 (53#) = happyGoto action_338
action_320 x = happyTcHack x happyFail

action_321 (126#) = happyShift action_44
action_321 (127#) = happyShift action_45
action_321 (128#) = happyShift action_46
action_321 (129#) = happyShift action_47
action_321 (132#) = happyShift action_48
action_321 (133#) = happyShift action_49
action_321 (143#) = happyShift action_91
action_321 (145#) = happyShift action_51
action_321 (147#) = happyShift action_84
action_321 (149#) = happyShift action_54
action_321 (151#) = happyShift action_55
action_321 (153#) = happyShift action_92
action_321 (154#) = happyShift action_57
action_321 (155#) = happyShift action_58
action_321 (156#) = happyShift action_59
action_321 (56#) = happyGoto action_337
action_321 (59#) = happyGoto action_21
action_321 (60#) = happyGoto action_22
action_321 (66#) = happyGoto action_23
action_321 (67#) = happyGoto action_24
action_321 (68#) = happyGoto action_25
action_321 (82#) = happyGoto action_26
action_321 (84#) = happyGoto action_27
action_321 (85#) = happyGoto action_28
action_321 x = happyTcHack x happyFail

action_322 (145#) = happyShift action_169
action_322 (12#) = happyGoto action_244
action_322 x = happyTcHack x happyReduce_48

action_323 x = happyTcHack x happyReduce_177

action_324 x = happyTcHack x happyReduce_43

action_325 (153#) = happyShift action_262
action_325 (157#) = happyShift action_263
action_325 (58#) = happyGoto action_336
action_325 x = happyTcHack x happyFail

action_326 x = happyTcHack x happyReduce_122

action_327 (145#) = happyShift action_335
action_327 (46#) = happyGoto action_332
action_327 (50#) = happyGoto action_333
action_327 (52#) = happyGoto action_334
action_327 x = happyTcHack x happyReduce_102

action_328 x = happyTcHack x happyReduce_107

action_329 (139#) = happyShift action_331
action_329 (142#) = happyShift action_126
action_329 (157#) = happyShift action_127
action_329 (57#) = happyGoto action_123
action_329 x = happyTcHack x happyFail

action_330 (136#) = happyShift action_64
action_330 (153#) = happyShift action_65
action_330 (78#) = happyGoto action_256
action_330 x = happyTcHack x happyReduce_82

action_331 (126#) = happyShift action_44
action_331 (127#) = happyShift action_45
action_331 (128#) = happyShift action_46
action_331 (129#) = happyShift action_47
action_331 (132#) = happyShift action_48
action_331 (133#) = happyShift action_49
action_331 (143#) = happyShift action_91
action_331 (145#) = happyShift action_51
action_331 (147#) = happyShift action_84
action_331 (149#) = happyShift action_54
action_331 (151#) = happyShift action_55
action_331 (153#) = happyShift action_92
action_331 (154#) = happyShift action_57
action_331 (155#) = happyShift action_58
action_331 (156#) = happyShift action_59
action_331 (56#) = happyGoto action_356
action_331 (59#) = happyGoto action_21
action_331 (60#) = happyGoto action_22
action_331 (66#) = happyGoto action_23
action_331 (67#) = happyGoto action_24
action_331 (68#) = happyGoto action_25
action_331 (82#) = happyGoto action_26
action_331 (84#) = happyGoto action_27
action_331 (85#) = happyGoto action_28
action_331 x = happyTcHack x happyFail

action_332 (121#) = happyShift action_128
action_332 (124#) = happyShift action_130
action_332 (125#) = happyShift action_131
action_332 x = happyTcHack x happyFail

action_333 x = happyTcHack x happyReduce_108

action_334 x = happyTcHack x happyReduce_109

action_335 (153#) = happyShift action_355
action_335 x = happyTcHack x happyFail

action_336 x = happyTcHack x happyReduce_125

action_337 (137#) = happyShift action_175
action_337 (142#) = happyShift action_126
action_337 (157#) = happyShift action_127
action_337 (57#) = happyGoto action_123
action_337 (80#) = happyGoto action_354
action_337 x = happyTcHack x happyFail

action_338 (88#) = happyShift action_353
action_338 x = happyTcHack x happyFail

action_339 x = happyTcHack x happyReduce_42

action_340 x = happyTcHack x happyReduce_99

action_341 x = happyTcHack x happyReduce_189

action_342 x = happyTcHack x happyReduce_187

action_343 x = happyTcHack x happyReduce_133

action_344 (137#) = happyShift action_352
action_344 x = happyTcHack x happyFail

action_345 (142#) = happyShift action_126
action_345 (157#) = happyShift action_127
action_345 (57#) = happyGoto action_123
action_345 x = happyTcHack x happyReduce_167

action_346 x = happyTcHack x happyReduce_161

action_347 x = happyTcHack x happyReduce_33

action_348 (146#) = happyShift action_351
action_348 x = happyTcHack x happyFail

action_349 (146#) = happyShift action_350
action_349 x = happyTcHack x happyFail

action_350 x = happyTcHack x happyReduce_37

action_351 x = happyTcHack x happyReduce_34

action_352 (126#) = happyShift action_44
action_352 (127#) = happyShift action_45
action_352 (128#) = happyShift action_46
action_352 (129#) = happyShift action_47
action_352 (132#) = happyShift action_48
action_352 (133#) = happyShift action_49
action_352 (143#) = happyShift action_91
action_352 (145#) = happyShift action_51
action_352 (147#) = happyShift action_84
action_352 (149#) = happyShift action_54
action_352 (151#) = happyShift action_55
action_352 (153#) = happyShift action_92
action_352 (154#) = happyShift action_57
action_352 (155#) = happyShift action_58
action_352 (156#) = happyShift action_59
action_352 (54#) = happyGoto action_361
action_352 (56#) = happyGoto action_81
action_352 (59#) = happyGoto action_21
action_352 (60#) = happyGoto action_22
action_352 (66#) = happyGoto action_23
action_352 (67#) = happyGoto action_24
action_352 (68#) = happyGoto action_25
action_352 (82#) = happyGoto action_26
action_352 (84#) = happyGoto action_27
action_352 (85#) = happyGoto action_28
action_352 x = happyTcHack x happyFail

action_353 (138#) = happyShift action_360
action_353 x = happyTcHack x happyFail

action_354 (146#) = happyShift action_359
action_354 x = happyTcHack x happyFail

action_355 (138#) = happyShift action_358
action_355 x = happyTcHack x happyFail

action_356 (142#) = happyShift action_126
action_356 (150#) = happyShift action_357
action_356 (157#) = happyShift action_127
action_356 (57#) = happyGoto action_123
action_356 x = happyTcHack x happyFail

action_357 x = happyTcHack x happyReduce_104

action_358 (126#) = happyShift action_44
action_358 (127#) = happyShift action_45
action_358 (128#) = happyShift action_46
action_358 (129#) = happyShift action_47
action_358 (132#) = happyShift action_48
action_358 (133#) = happyShift action_49
action_358 (143#) = happyShift action_91
action_358 (145#) = happyShift action_51
action_358 (147#) = happyShift action_84
action_358 (149#) = happyShift action_54
action_358 (151#) = happyShift action_55
action_358 (153#) = happyShift action_92
action_358 (154#) = happyShift action_57
action_358 (155#) = happyShift action_58
action_358 (156#) = happyShift action_59
action_358 (56#) = happyGoto action_363
action_358 (59#) = happyGoto action_21
action_358 (60#) = happyGoto action_22
action_358 (66#) = happyGoto action_23
action_358 (67#) = happyGoto action_24
action_358 (68#) = happyGoto action_25
action_358 (82#) = happyGoto action_26
action_358 (84#) = happyGoto action_27
action_358 (85#) = happyGoto action_28
action_358 x = happyTcHack x happyFail

action_359 x = happyTcHack x happyReduce_59

action_360 (126#) = happyShift action_44
action_360 (127#) = happyShift action_45
action_360 (128#) = happyShift action_46
action_360 (129#) = happyShift action_47
action_360 (132#) = happyShift action_48
action_360 (133#) = happyShift action_49
action_360 (143#) = happyShift action_91
action_360 (145#) = happyShift action_51
action_360 (147#) = happyShift action_84
action_360 (149#) = happyShift action_54
action_360 (151#) = happyShift action_55
action_360 (153#) = happyShift action_92
action_360 (154#) = happyShift action_57
action_360 (155#) = happyShift action_58
action_360 (156#) = happyShift action_59
action_360 (56#) = happyGoto action_362
action_360 (59#) = happyGoto action_21
action_360 (60#) = happyGoto action_22
action_360 (66#) = happyGoto action_23
action_360 (67#) = happyGoto action_24
action_360 (68#) = happyGoto action_25
action_360 (82#) = happyGoto action_26
action_360 (84#) = happyGoto action_27
action_360 (85#) = happyGoto action_28
action_360 x = happyTcHack x happyFail

action_361 x = happyTcHack x happyReduce_134

action_362 (137#) = happyShift action_175
action_362 (142#) = happyShift action_126
action_362 (157#) = happyShift action_127
action_362 (57#) = happyGoto action_123
action_362 (80#) = happyGoto action_365
action_362 x = happyTcHack x happyFail

action_363 (142#) = happyShift action_126
action_363 (146#) = happyShift action_364
action_363 (157#) = happyShift action_127
action_363 (57#) = happyGoto action_123
action_363 x = happyTcHack x happyFail

action_364 x = happyTcHack x happyReduce_111

action_365 (146#) = happyShift action_366
action_365 x = happyTcHack x happyFail

action_366 x = happyTcHack x happyReduce_61

happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5# happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (JustParse happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5# happyReduction_3
happyReduction_3 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn4
		 (Tactic happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5# happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Defs_Decls happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5# happyReduction_5
happyReduction_5 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Local_Defs happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5# happyReduction_6
happyReduction_6 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Cut happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Claim PlainClaim happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5# happyReduction_8
happyReduction_8 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Claim GoalClaim happy_var_2 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5# happyReduction_9
happyReduction_9 _
	(HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn4
		 (ModuleHeader happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5# happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5# happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  5# happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  5# happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  5# happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  6# happyReduction_17
happyReduction_17 (HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Ctxt_op happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7# happyReduction_18
happyReduction_18 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn4
		 (Term happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7# happyReduction_19
happyReduction_19 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Normal happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  7# happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 [] happy_var_1
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 5# 7# happyReduction_21
happyReduction_21 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_5 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  8# happyReduction_22
happyReduction_22 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (\dds t1 -> Conv StrictConv dds t1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  8# happyReduction_23
happyReduction_23 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (\dds t1 -> Conv UnifyConv  dds t1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  9# happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9# happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10# happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (Def_Decl (Right happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  10# happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn10
		 (Def_Decl (Left happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  11# happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  11# happyReduction_29
happyReduction_29  =  HappyAbsSyn11
		 ([]
	)

happyReduce_30 = happyReduce 4# 12# happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Declaration happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_2  13# happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  13# happyReduction_32
happyReduction_32  =  HappyAbsSyn13
		 ([]
	)

happyReduce_33 = happyReduce 6# 14# happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Definition happy_var_2 [] happy_var_5 (Just happy_var_3)
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 7# 14# happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_6) `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Definition happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_2  15# happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  15# happyReduction_36
happyReduction_36  =  HappyAbsSyn15
		 ([]
	)

happyReduce_37 = happyReduce 4# 16# happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	(HappyAbsSyn43  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((happy_var_2,happy_var_3)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  17# happyReduction_38
happyReduction_38 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  17# happyReduction_39
happyReduction_39 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  18# happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  18# happyReduction_41
happyReduction_41  =  HappyAbsSyn18
		 ([]
	)

happyReduce_42 = happyReduce 5# 19# happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Cut_Expression happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 6# 20# happyReduction_43
happyReduction_43 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (CoercionDecl happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  21# happyReduction_44
happyReduction_44 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  21# happyReduction_45
happyReduction_45  =  HappyAbsSyn11
		 ([]
	)

happyReduce_46 = happySpecReduce_2  22# happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  22# happyReduction_47
happyReduction_47  =  HappyAbsSyn17
		 ([]
	)

happyReduce_48 = happyReduce 6# 23# happyReduction_48
happyReduction_48 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Inductive happy_var_3 happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_2  24# happyReduction_49
happyReduction_49 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  24# happyReduction_50
happyReduction_50  =  HappyAbsSyn24
		 ([]
	)

happyReduce_51 = happySpecReduce_1  25# happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn25
		 (WantInd
	)

happyReduce_52 = happySpecReduce_1  25# happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn25
		 (Theorems
	)

happyReduce_53 = happySpecReduce_1  25# happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn25
		 (Relation
	)

happyReduce_54 = happySpecReduce_1  25# happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn25
		 (Relation_LE
	)

happyReduce_55 = happySpecReduce_1  25# happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn25
		 (AlsoLift
	)

happyReduce_56 = happySpecReduce_1  25# happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn25
		 (NoDischarge
	)

happyReduce_57 = happySpecReduce_3  26# happyReduction_57
happyReduction_57 (HappyAbsSyn27  happy_var_3)
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (ElimRule happy_var_2 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 5# 26# happyReduction_58
happyReduction_58 ((HappyAbsSyn28  happy_var_5) `HappyStk`
	(HappyTerminal (TokenNum happy_var_4)) `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SimpleElimRule happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 7# 27# happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_6) `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (happy_var_1 ++ [CompRule happy_var_3 happy_var_5 happy_var_6]
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_0  27# happyReduction_60
happyReduction_60  =  HappyAbsSyn27
		 ([]
	)

happyReduce_61 = happyReduce 8# 28# happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_7) `HappyStk`
	(HappyAbsSyn37  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_4)) `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (happy_var_1 ++ [ CRF happy_var_3 happy_var_4 happy_var_6 happy_var_7 ]
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_0  28# happyReduction_62
happyReduction_62  =  HappyAbsSyn28
		 ([]
	)

happyReduce_63 = happySpecReduce_1  29# happyReduction_63
happyReduction_63 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn4
		 (FunctionDef happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happyReduce 4# 30# happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn31  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FnDefinition happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_2  31# happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  32# happyReduction_66
happyReduction_66 (HappyAbsSyn31  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2 : happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_0  32# happyReduction_67
happyReduction_67  =  HappyAbsSyn31
		 ([]
	)

happyReduce_68 = happySpecReduce_2  33# happyReduction_68
happyReduction_68 (HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (Just happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  33# happyReduction_69
happyReduction_69  =  HappyAbsSyn33
		 (Nothing
	)

happyReduce_70 = happySpecReduce_2  34# happyReduction_70
happyReduction_70 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_0  34# happyReduction_71
happyReduction_71  =  HappyAbsSyn34
		 ([]
	)

happyReduce_72 = happyReduce 4# 35# happyReduction_72
happyReduction_72 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	(HappyAbsSyn49  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (FnClause happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_73 = happySpecReduce_2  36# happyReduction_73
happyReduction_73 (HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 : happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  36# happyReduction_74
happyReduction_74 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  37# happyReduction_75
happyReduction_75 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  38# happyReduction_76
happyReduction_76 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 (Basic happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  38# happyReduction_77
happyReduction_77 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Then_T happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  38# happyReduction_78
happyReduction_78 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Else_T happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  38# happyReduction_79
happyReduction_79 _
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  39# happyReduction_80
happyReduction_80 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Refine     happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  39# happyReduction_81
happyReduction_81 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Intros     happy_var_1 Nothing happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happyReduce 6# 39# happyReduction_82
happyReduction_82 ((HappyAbsSyn43  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (Intros     happy_var_1 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_83 = happySpecReduce_3  39# happyReduction_83
happyReduction_83 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Induction  happy_var_1 happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  39# happyReduction_84
happyReduction_84 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Equiv      happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  39# happyReduction_85
happyReduction_85 _
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Immed      happy_var_1
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_2  39# happyReduction_86
happyReduction_86 _
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (Assumption happy_var_1
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  40# happyReduction_87
happyReduction_87 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn40
		 (Just happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_0  40# happyReduction_88
happyReduction_88  =  HappyAbsSyn40
		 (Nothing
	)

happyReduce_89 = happySpecReduce_1  41# happyReduction_89
happyReduction_89 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn41
		 (Left happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  41# happyReduction_90
happyReduction_90 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn41
		 (Right happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  42# happyReduction_91
happyReduction_91 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn42
		 (Just happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_0  42# happyReduction_92
happyReduction_92  =  HappyAbsSyn42
		 (Nothing
	)

happyReduce_93 = happySpecReduce_2  43# happyReduction_93
happyReduction_93 (HappyAbsSyn78  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_0  43# happyReduction_94
happyReduction_94  =  HappyAbsSyn43
		 ([]
	)

happyReduce_95 = happySpecReduce_1  44# happyReduction_95
happyReduction_95 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn4
		 (FiniteUniv happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  44# happyReduction_96
happyReduction_96 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  44# happyReduction_97
happyReduction_97 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  45# happyReduction_98
happyReduction_98 (HappyAbsSyn49  happy_var_3)
	(HappyTerminal (TokenUniv happy_var_2))
	_
	 =  HappyAbsSyn4
		 (AddToUniverse happy_var_2 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happyReduce 7# 45# happyReduction_99
happyReduction_99 ((HappyAbsSyn49  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AddToPseudoUniverse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_2  46# happyReduction_100
happyReduction_100 _
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 ++ [U_Cumulative]
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  46# happyReduction_101
happyReduction_101 _
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 ++ [U_Closed]
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_0  46# happyReduction_102
happyReduction_102  =  HappyAbsSyn46
		 ([]
	)

happyReduce_103 = happyReduce 5# 47# happyReduction_103
happyReduction_103 ((HappyAbsSyn49  happy_var_5) `HappyStk`
	(HappyAbsSyn49  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (InfiniteUniv happy_var_1 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_104 = happyReduce 7# 48# happyReduction_104
happyReduction_104 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (((happy_var_2, happy_var_4), happy_var_6)
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_1  49# happyReduction_105
happyReduction_105 (HappyTerminal (TokenUniv happy_var_1))
	 =  HappyAbsSyn49
		 (Plain happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  49# happyReduction_106
happyReduction_106 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn49
		 (Plain happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happyReduce 6# 50# happyReduction_107
happyReduction_107 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn50
		 (FiniteUniverse happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_3  51# happyReduction_108
happyReduction_108 (HappyAbsSyn50  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 ++ [Right happy_var_3]
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  51# happyReduction_109
happyReduction_109 (HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 ++ [Left happy_var_3]
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_0  51# happyReduction_110
happyReduction_110  =  HappyAbsSyn51
		 ([]
	)

happyReduce_111 = happyReduce 5# 52# happyReduction_111
happyReduction_111 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn52
		 ((Plain happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_112 = happySpecReduce_1  53# happyReduction_112
happyReduction_112 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn49
		 (Plain happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  53# happyReduction_113
happyReduction_113 _
	(HappyTerminal (TokenSymbol happy_var_2))
	_
	 =  HappyAbsSyn49
		 (Operator happy_var_2
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  54# happyReduction_114
happyReduction_114 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  54# happyReduction_115
happyReduction_115 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  55# happyReduction_116
happyReduction_116 _
	 =  HappyAbsSyn55
		 (\a b -> Ext_S (SoftCast a b)
	)

happyReduce_117 = happySpecReduce_1  55# happyReduction_117
happyReduction_117 _
	 =  HappyAbsSyn55
		 (\a b -> Ext_S (HardCast a b)
	)

happyReduce_118 = happySpecReduce_1  56# happyReduction_118
happyReduction_118 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  56# happyReduction_119
happyReduction_119 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  57# happyReduction_120
happyReduction_120 (HappyTerminal (TokenSymbol happy_var_1))
	 =  HappyAbsSyn55
		 (\l -> Op_S l (mk_op happy_var_1)
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_3  57# happyReduction_121
happyReduction_121 _
	(HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn55
		 (\l -> Op_S l (Name_S (Backticks happy_var_2))
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happyReduce 5# 57# happyReduction_122
happyReduction_122 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	(HappyTerminal (TokenSymbol happy_var_3)) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn55
		 (\l -> Op_S l (Op_S (mk_plain happy_var_2) (mk_op happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_123 = happySpecReduce_1  58# happyReduction_123
happyReduction_123 (HappyTerminal (TokenSymbol happy_var_1))
	 =  HappyAbsSyn37
		 (mk_op happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  58# happyReduction_124
happyReduction_124 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn37
		 (mk_plain happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  58# happyReduction_125
happyReduction_125 (HappyAbsSyn37  happy_var_3)
	(HappyTerminal (TokenSymbol happy_var_2))
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn37
		 (Op_S (mk_plain happy_var_1) (mk_op happy_var_2) happy_var_3
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  59# happyReduction_126
happyReduction_126 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happyReduce 6# 59# happyReduction_127
happyReduction_127 ((HappyAbsSyn37  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (Let_S happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_128 = happySpecReduce_1  59# happyReduction_128
happyReduction_128 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  59# happyReduction_129
happyReduction_129 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happyReduce 5# 59# happyReduction_130
happyReduction_130 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (Ext_S (With happy_var_1 happy_var_3 happy_var_5 Nothing)
	) `HappyStk` happyRest

happyReduce_131 = happyReduce 5# 60# happyReduction_131
happyReduction_131 (_ `HappyStk`
	(HappyAbsSyn64  happy_var_4) `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (mk_rec happy_var_3 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_132 = happyReduce 5# 60# happyReduction_132
happyReduction_132 (_ `HappyStk`
	(HappyAbsSyn65  happy_var_4) `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (mk_sig happy_var_3 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_133 = happyReduce 4# 61# happyReduction_133
happyReduction_133 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn61
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_134 = happyReduce 6# 62# happyReduction_134
happyReduction_134 ((HappyAbsSyn37  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn62
		 ((happy_var_2,happy_var_4,happy_var_6)
	) `HappyStk` happyRest

happyReduce_135 = happySpecReduce_1  63# happyReduction_135
happyReduction_135 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn63
		 ((fst happy_var_1, Opaque (snd happy_var_1))
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  63# happyReduction_136
happyReduction_136 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn63
		 ((fst3 happy_var_1, Manifest (snd3 happy_var_1) (thd3 happy_var_1))
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_2  64# happyReduction_137
happyReduction_137 (HappyAbsSyn64  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn64
		 (happy_var_1 : happy_var_2
	)
happyReduction_137 _ _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_1  64# happyReduction_138
happyReduction_138 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn64
		 ([happy_var_1]
	)
happyReduction_138 _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_2  65# happyReduction_139
happyReduction_139 (HappyAbsSyn65  happy_var_2)
	(HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1 : happy_var_2
	)
happyReduction_139 _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  65# happyReduction_140
happyReduction_140 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn65
		 ([happy_var_1]
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_2  66# happyReduction_141
happyReduction_141 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (Ap_S happy_var_1 happy_var_2
	)
happyReduction_141 _ _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_1  66# happyReduction_142
happyReduction_142 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_142 _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  67# happyReduction_143
happyReduction_143 _
	 =  HappyAbsSyn37
		 (mk_plain "Type"
	)

happyReduce_144 = happyMonadReduce 1# 67# happyReduction_144
happyReduction_144 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( failP "kind")
	) (\r -> happyReturn (HappyAbsSyn37 r))

happyReduce_145 = happySpecReduce_2  67# happyReduction_145
happyReduction_145 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (mk_El happy_var_2
	)
happyReduction_145 _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_2  67# happyReduction_146
happyReduction_146 (HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn37
		 (mk_El (mk_plain happy_var_2)
	)
happyReduction_146 _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_2  67# happyReduction_147
happyReduction_147 (HappyTerminal (TokenUniv happy_var_2))
	_
	 =  HappyAbsSyn37
		 (mk_El (mk_plain happy_var_2)
	)
happyReduction_147 _ _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_2  67# happyReduction_148
happyReduction_148 (HappyTerminal (TokenMeta happy_var_2))
	_
	 =  HappyAbsSyn37
		 (mk_El (mk_plain happy_var_2)
	)
happyReduction_148 _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_1  67# happyReduction_149
happyReduction_149 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn37
		 (mk_plain happy_var_1
	)
happyReduction_149 _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_1  67# happyReduction_150
happyReduction_150 (HappyTerminal (TokenMeta happy_var_1))
	 =  HappyAbsSyn37
		 (mk_plain happy_var_1
	)
happyReduction_150 _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  67# happyReduction_151
happyReduction_151 (HappyTerminal (TokenUniv happy_var_1))
	 =  HappyAbsSyn37
		 (mk_plain happy_var_1
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_2  67# happyReduction_152
happyReduction_152 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 happy_var_2
	)
happyReduction_152 _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_1  67# happyReduction_153
happyReduction_153 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_153 _  = notHappyAtAll 

happyReduce_154 = happyMonadReduce 3# 68# happyReduction_154
happyReduction_154 ((HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn37  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( elimOk failP returnP (is_dp_fail (happy_var_3 happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn37 r))

happyReduce_155 = happySpecReduce_3  68# happyReduction_155
happyReduction_155 _
	(HappyTerminal (TokenSymbol happy_var_2))
	_
	 =  HappyAbsSyn37
		 (Name_S (Operator happy_var_2)
	)
happyReduction_155 _ _ _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_1  69# happyReduction_156
happyReduction_156 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  69# happyReduction_157
happyReduction_157 _
	 =  HappyAbsSyn37
		 (underscore_name
	)

happyReduce_158 = happySpecReduce_2  70# happyReduction_158
happyReduction_158 _
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn70
		 (\t -> mk_ra_ap t happy_var_1
	)
happyReduction_158 _ _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_1  70# happyReduction_159
happyReduction_159 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_3  71# happyReduction_160
happyReduction_160 (HappyAbsSyn36  happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (happy_var_2 : happy_var_3
	)
happyReduction_160 _ _ _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_3  72# happyReduction_161
happyReduction_161 (HappyAbsSyn36  happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (happy_var_2 : happy_var_3
	)
happyReduction_161 _ _ _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_0  72# happyReduction_162
happyReduction_162  =  HappyAbsSyn36
		 ([]
	)

happyReduce_163 = happySpecReduce_2  73# happyReduction_163
happyReduction_163 (HappyAbsSyn75  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn70
		 (\t -> happy_var_2 (t : happy_var_1)
	)
happyReduction_163 _ _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_2  73# happyReduction_164
happyReduction_164 _
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn70
		 (\t -> case happy_var_1 of
											[] -> Pars_S t
											ts -> foldr1 (ap2 ",") (t:ts)
	)
happyReduction_164 _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_3  74# happyReduction_165
happyReduction_165 (HappyAbsSyn36  happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (happy_var_2 : happy_var_3
	)
happyReduction_165 _ _ _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_0  74# happyReduction_166
happyReduction_166  =  HappyAbsSyn36
		 ([]
	)

happyReduce_167 = happyReduce 4# 75# happyReduction_167
happyReduction_167 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	(HappyAbsSyn76  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (\ts -> check_all_ids_and_bind happy_var_3 ts happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_168 = happyMonadReduce 1# 76# happyReduction_168
happyReduction_168 ((HappyTerminal (TokenSymbol happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( check_dependent_arrow ("(",")") happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn76 r))

happyReduce_169 = happySpecReduce_0  76# happyReduction_169
happyReduction_169  =  HappyAbsSyn76
		 (mkBinding By_DP
	)

happyReduce_170 = happySpecReduce_3  77# happyReduction_170
happyReduction_170 (HappyAbsSyn78  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_170 _ _ _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_1  77# happyReduction_171
happyReduction_171 (HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn43
		 ([happy_var_1]
	)
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_1  78# happyReduction_172
happyReduction_172 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn78
		 (Bound (Plain happy_var_1)
	)
happyReduction_172 _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_1  78# happyReduction_173
happyReduction_173 _
	 =  HappyAbsSyn78
		 (Underscore
	)

happyReduce_174 = happySpecReduce_2  79# happyReduction_174
happyReduction_174 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn79
		 ((happy_var_1,happy_var_2)
	)
happyReduction_174 _ _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_2  80# happyReduction_175
happyReduction_175 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_175 _ _  = notHappyAtAll 

happyReduce_176 = happyReduce 4# 80# happyReduction_176
happyReduction_176 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (mk_El happy_var_4
	) `HappyStk` happyRest

happyReduce_177 = happySpecReduce_1  81# happyReduction_177
happyReduction_177 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn81
		 (Just happy_var_1
	)
happyReduction_177 _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_0  81# happyReduction_178
happyReduction_178  =  HappyAbsSyn81
		 (Nothing
	)

happyReduce_179 = happySpecReduce_3  82# happyReduction_179
happyReduction_179 _
	(HappyAbsSyn79  happy_var_2)
	(HappyTerminal (TokenBindId happy_var_1))
	 =  HappyAbsSyn70
		 (mkBinding (By_Constr happy_var_1) happy_var_2
	)
happyReduction_179 _ _ _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_2  82# happyReduction_180
happyReduction_180 _
	(HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn70
		 (fst happy_var_1
	)
happyReduction_180 _ _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_0  83# happyReduction_181
happyReduction_181  =  HappyAbsSyn42
		 (Nothing
	)

happyReduce_182 = happySpecReduce_3  84# happyReduction_182
happyReduction_182 _
	(HappyAbsSyn79  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (mk_binding By_FO happy_var_2
	)
happyReduction_182 _ _ _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_3  84# happyReduction_183
happyReduction_183 _
	(HappyAbsSyn79  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (mk_binding (By_Brackets "{" "}") happy_var_2
	)
happyReduction_183 _ _ _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_3  84# happyReduction_184
happyReduction_184 _
	(HappyAbsSyn79  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (mk_binding (By_Brackets "<" ">") happy_var_2
	)
happyReduction_184 _ _ _  = notHappyAtAll 

happyReduce_185 = happyMonadReduce 3# 84# happyReduction_185
happyReduction_185 ((HappyTerminal (TokenCloseBracket happy_var_3)) `HappyStk`
	(HappyAbsSyn79  happy_var_2) `HappyStk`
	(HappyTerminal (TokenOpenBracket happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( mk_brackets happy_var_1 happy_var_3 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn84 r))

happyReduce_186 = happyReduce 6# 85# happyReduction_186
happyReduction_186 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (Ext_S (Case happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyReduce_187 = happySpecReduce_3  86# happyReduction_187
happyReduction_187 (HappyAbsSyn86  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 (happy_var_1 : happy_var_3
	)
happyReduction_187 _ _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_1  86# happyReduction_188
happyReduction_188 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 ([happy_var_1]
	)
happyReduction_188 _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_3  87# happyReduction_189
happyReduction_189 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn87
		 ((happy_var_1, happy_var_3)
	)
happyReduction_189 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 158# 158# tk (HappyState action) sts stk;
	TokenNum happy_dollar_dollar -> cont 88#;
	TokenDefsDecls -> cont 89#;
	TokenBeginFunction -> cont 90#;
	TokenEndFunction -> cont 91#;
	TokenModule -> cont 92#;
	TokenCoercion -> cont 93#;
	TokenParameters -> cont 94#;
	TokenPrerequisites -> cont 95#;
	TokenWhere -> cont 96#;
	TokenRefine -> cont 97#;
	TokenIntros -> cont 98#;
	TokenInduction -> cont 99#;
	TokenAssumption -> cont 100#;
	TokenImmed -> cont 101#;
	TokenEquiv -> cont 102#;
	TokenInductive -> cont 103#;
	TokenConstructors -> cont 104#;
	TokenWantInd -> cont 105#;
	TokenTheorems -> cont 106#;
	TokenRelation -> cont 107#;
	TokenRelation_LE -> cont 108#;
	TokenAlsoLift -> cont 109#;
	TokenNoDischarge -> cont 110#;
	TokenElimRule -> cont 111#;
	TokenSimpleElimRule -> cont 112#;
	TokenKeyword "Cut" -> cont 113#;
	TokenKeyword "Claim" -> cont 114#;
	TokenKeyword "Goal" -> cont 115#;
	TokenElse_T -> cont 116#;
	TokenThen_T -> cont 117#;
	TokenNormal -> cont 118#;
	TokenCtxt -> cont 119#;
	TokenJustParse -> cont 120#;
	TokenFiniteUniverse -> cont 121#;
	TokenInfiniteUniverse -> cont 122#;
	TokenAddToUniverse -> cont 123#;
	TokenCumulative -> cont 124#;
	TokenClosed -> cont 125#;
	TokenKind -> cont 126#;
	TokenType -> cont 127#;
	TokenEl -> cont 128#;
	TokenCase -> cont 129#;
	TokenOf -> cont 130#;
	TokenWith -> cont 131#;
	TokenSig -> cont 132#;
	TokenStr -> cont 133#;
	TokenColonColon -> cont 134#;
	TokenColonExclColon -> cont 135#;
	TokenUnderSc -> cont 136#;
	TokenColon -> cont 137#;
	TokenEq -> cont 138#;
	TokenComma -> cont 139#;
	TokenDollar -> cont 140#;
	TokenSymbol "==" -> cont 141#;
	TokenBacktick -> cont 142#;
	TokenOP -> cont 143#;
	TokenCP -> cont 144#;
	TokenOB -> cont 145#;
	TokenCB -> cont 146#;
	TokenOA -> cont 147#;
	TokenCA -> cont 148#;
	TokenOpenBracket  ("{","}") -> cont 149#;
	TokenCloseBracket ("{","}") -> cont 150#;
	TokenOpenBracket happy_dollar_dollar -> cont 151#;
	TokenCloseBracket happy_dollar_dollar -> cont 152#;
	TokenId happy_dollar_dollar -> cont 153#;
	TokenBindId happy_dollar_dollar -> cont 154#;
	TokenMeta happy_dollar_dollar -> cont 155#;
	TokenUniv happy_dollar_dollar -> cont 156#;
	TokenSymbol happy_dollar_dollar -> cont 157#;
	_ -> happyError' tk
	})

happyError_ 158# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

lf_parser = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Parse = P TopCommand
lf_parser :: Parse


parseThenCall 
 :: (Fallible m, Monad m) => 
       (TopCommand -> m a) 
    -> (SimpleTerm -> m a) 
    -> BracketTable
    -> String 
    -> m a
parseThenCall cf tf ts s
 = do
		let i_tables = (compileBrackets $ map fst ts, 
		                compileArrows   ts)
		c <- embed (run_parser lf_parser i_tables s)
		case c of
			Term t -> tf t
			cmd    -> cf cmd
   where
		compileArrows ts = [ (a,se) | (se, Just a) <- ts ]





parseTermThenCall 
 :: (Fallible m, Monad m) => 
				(SimpleTerm -> m a) -> BracketTable -> String -> m a
parseTermThenCall 
 = parseThenCall (\cmd -> fail_with_msg $ "Got cmd: " ++ show cmd)








type SyntaxInfo = ( BracketTable, ArrowInfo )
type InternalSyntaxInfo = ( BracketInfo, ArrowInfo )

type P a = String -> InternalSyntaxInfo -> Int -> OkF a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ts l -> m s ts l >>= \a -> k a s ts l

returnP :: a -> P a
returnP a = \s ts l -> Ok a

failP :: String -> P a
failP m = \s ts l -> Fail m

syntaxInfo :: P InternalSyntaxInfo
syntaxInfo s i l = Ok i




lexer :: (Token -> P a) -> P a
lexer cont s tables
 = let (rest,token) = basic_lex (fst tables) s in
   case token of
     TokenNL  -> \line -> lexer cont rest tables (line+1)
     t        -> cont t rest tables






run_parser :: P a -> InternalSyntaxInfo -> String -> OkF a
run_parser parser tables s
 | take 1 (dropWhile isSpace s) == "["
    = case (run_parser_ s) of
			ok@(Ok _) -> ok
			Fail m    -> change_msg (\_ -> m) $ 
						 run_parser_ (prefix_as_defs_decls s)
 | otherwise
    = run_parser_ s
 where
	run_parser_ s = parser s tables 1
	prefix_as_defs_decls s = defs_decls_string ++ " " ++ s





happyError :: P a		-- any P.
happyError = \s _ i -> Fail (
   "Parse error in line " ++ show (i::Int) ++ ": \n|" ++ take 100 s ++ "|\n")

happyErrorToken :: Token -> P a
happyErrorToken tk = \s _ i -> Fail (
   "Parse error in line " ++ show i ++ ", at token " ++ show tk 
	   ++ ":\n|" ++ take 100 s ++ "|\n")






check :: String -> String -> a -> String -> P a
check inp exp r e 
 | inp == exp = returnP r
 | otherwise  = failP $ "Expected " ++ exp ++ " in " ++ e






case_rhs_separator = "->" :: String

case_clause_separator = "|" :: String 

fn_clause_separator = "|" :: String 





mk_El = Ap_S (mk_plain "El") 

mk_op = Name_S . Operator

mk_plain = Name_S . Plain





mk_dot q e = q ++ "." ++ e 



mk_binding :: BindVariety -> ([BoundName],SimpleTerm)
                              -> (SimpleTerm -> SimpleTerm, BindVariety)
mk_binding k (vs,ty) = (mkBinding k (vs,ty), k)





ap2 f x y = Ap_S (Ap_S (mk_plain f) x) y





mk_ra_ap f []      = f
mk_ra_ap f (r1:rs) = Ap_S f (mk_ra_ap r1 rs)




mk_rec l []     = l
mk_rec l (f:fs) = Ext_S (Rec (mk_rec l fs) (fst3 f) (snd3 f) (thd3 f))
mk_sig l []     = l
mk_sig l (f:fs) = Ext_S (Sig (mk_sig l fs) (fst f) (snd f))







fail_dp_creation_string = "Failed DP"
mk_fail_dp m = Name_S $ Plain $ fail_dp_creation_string ++ m
is_dp_fail :: SimpleTerm -> OkF SimpleTerm
is_dp_fail t@(Name_S (Plain n))
 | prefix == fail_dp_creation_string = Fail suffix
 | otherwise                         = Ok t
 where
		(prefix, suffix) = splitAt (length fail_dp_creation_string) n
is_dp_fail t = Ok t



mk_brackets 
 :: BracketPair -> BracketPair -> ([BoundName],SimpleTerm) 
				-> P (SimpleTerm -> SimpleTerm, BindVariety)
mk_brackets open close r
 | open == close = returnP $ mk_binding (By_Brackets (fst open) (snd open)) r 
 | otherwise = failP $ "Mismatch in bracket symbols: " ++ show (open,close)





check_all_ids_and_bind 
 :: (([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm) 
		-> [SimpleTerm] -> SimpleTerm -> SimpleTerm -> SimpleTerm

check_all_ids_and_bind bf ns ty t
 | null bad_ids = bf (map fromOk ok_ids, ty) t
 | otherwise    = mk_fail_dp $ fromFail $ head bad_ids
 where
		is_ok_id (Name_S (Plain "_")) = Ok $ Underscore
		is_ok_id (Name_S n)           = Ok $ Bound n
		is_ok_id t                    = Fail $ "bad id term: " ++ show t
		(ok_ids, bad_ids) = span isOk $ map is_ok_id ns






type TEMP_Operator = String

check_dependent_arrow :: BracketPair -> TEMP_Operator 
					-> P (([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm)
check_dependent_arrow se op 
 = trace ("CDA " ++ show se ++ "," ++ show op ++ "\n") $ 
   syntaxInfo `thenP` \i -> 
   let pair = lookup op (snd i) in
   if pair == Just se
     then returnP (trace "OK\n" $ mkBinding $ uncurry By_Brackets se)
     else failP (trace "NOT OK\n" $ "Optional arrow error: "++op++" is used with "++show pair)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}






# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 1# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
