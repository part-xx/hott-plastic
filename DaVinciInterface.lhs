> module DaVinciInterface (module DaVinciInterface, OkF) where

DaVinciInterface:
	types and utils for interacting with daVinci.

	includes:
		* the basic types, translated in to Haskell
		* some util functions/values to ease use.
		* some functions that are locally used (eg CIprog ones)


> import List((\\))
> import Char(toUpper)

> import BaseClasses(OkF(..))

> infixr 5 `sp`
> sp a b = a ++ ' ' : b
> capital_first []     = []
> capital_first (c:cs) = toUpper c : cs

> sevenTupleArg n (a,b,c,d,e,f,g) = showString $ n ++ "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e ++ "," ++ show f ++ "," ++ show g ++ ")"
> sixTupleArg n (a,b,c,d,e,f) = showString $ n ++ "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e ++ "," ++ show f ++ ")"


********************************************************************************
Convenience functions.

> message = Window . Show_message
> status = Window . Show_status

---

> isOkAnswer (Ok OkAnswer) = True
> isOkAnswer _            = False

---
`parseAnswerOk' categorises the result of `parseAnswer', returning single 
alternatives as Ok, anything else as Fail.

> parseAnswerOk :: String -> OkF Answer 
> parseAnswerOk s
>  = case (parseAnswer s) of
>	[]  -> Fail ("Invalid Answer: " ++ s)
>	[r] -> Ok $ fst r
>	rs  -> Fail ("Ambiguous Answer:" `sp` s `sp` "=>" `sp` show rs)

---
`parseAnswer' "reads" a string into an Answer value. An exception is needed for
"ok" since we must use OkAnswer (because Ok is already in use in OkF). The first
letter of the "constructor" is capitalised before parsing, to make it look like
a legal Haskell constructor.

NOTE: this done to avoid having to write my own readsPrec for Answer...

> parseAnswer :: String -> [(Answer,String)]
> parseAnswer "ok" = reads $ show OkAnswer
> parseAnswer cs   = reads $ capital_first cs


********************************************************************************
Graphterm utils.

`pruneByHiding' hides the nodes at a given depth in the graph. `throughEdge'
propagates a (Graphterm->Graphterm) function over edges (with labels).
`hideGraph' is a convenience function.

> pruneByHiding :: Int -> Graphterm -> Graphterm
> pruneByHiding _ g@(R _)     = g	-- INSUFFICIENT - should prune target
> pruneByHiding d (NL x g)    = NL x $ pruneByHiding d g
> pruneByHiding 0 (N c as es) = N c (hidden:(as\\[notHidden])) es
> pruneByHiding d (N c as es) = N c as $
>				map (throughEdge $ pruneByHiding (d-1)) es

> throughEdge :: (Graphterm -> Graphterm) -> (Edge -> Edge)
> throughEdge f (EL x y)   = EL x $ throughEdge f y
> throughEdge f (E c as g) = E c as $ f g

> hideGraph :: Graphterm -> Graphterm
> hideGraph = pruneByHiding 0

---
`labelGraph' adds a node containing the given text to a graph, which has the
graph as its child.

> labelGraph :: String -> Graphterm -> Graphterm
> labelGraph txt g
>  = N "" [bold_italic_s, helvetica, text txt, box_t] [E "" [] g]


--------------------------------------------------------------------------------
Aux fns for showsPrec _ definitions.

`zeroArg', `singleArg', and `tupleArg' are convenience functions.

> zeroArg :: String -> ShowS
> zeroArg = showString

> singleArg :: Show a => String -> a -> ShowS
> singleArg n v = showString $ n ++ '(' : show v ++ ")"

> tupleArg :: Show a => String -> a -> ShowS
> tupleArg n v = showString $ n ++ show v



********************************************************************************
********************************************************************************

This goes at the end of the DV-I module. (Ie, none of the above should change.)

********************************************************************************
********************************************************************************

IMPORTANT:
	 In daVinci V2.0, all nodes and edges of a graph must have a unique
	 node/edge-ID in the term representation.


- Graph terms ------------------------------------------------------------------

> data Graphterm
>  = N  Node_id [Attribute] [Edge]
>  | NL Type Graphterm
>  | R  Node_id

> type Node_id = String

> type Type = String		-- node display type - unused?


> instance Eq Graphterm where		-- QQ: is this needed?
>	NL la _ == NL lb _ = error "Eq on Graphterm" -- la == lb
>	_       == _       = False

> instance Show Graphterm where
>	showsPrec _ (N n as es) = tupleArg "n" (n,as,es)
>	showsPrec _ (NL l g)    = tupleArg "l" (l,g)
>	showsPrec _ (R n)       = singleArg "r" n


> data Edge
>  = E  Edge_id [Attribute] Graphterm
>  | EL Type Edge

> type Edge_id = String


> instance Show Edge where
>	showsPrec _ (E n as g) = tupleArg "e" (n,as,g)
>	showsPrec _ (EL l e)   = tupleArg "l" (l,e)


- Attributes, and utility functions --------------------------------------------

Note: these are shared between edges & nodes. No attempt made to distinguish!

> data Attribute = A String String deriving (Eq)

> instance Show Attribute where
>	showsPrec _ (A k v) = tupleArg "a" (k,v)

---
`text' creates an attribute for a node's text.

> text :: String -> Attribute
> text = A "OBJECT"

---
`colour' creates an attribute for a node's colour.
NB: could extend with consts for the (16?) allowed colours...

> colour :: String -> Attribute
> colour = A "COLOR"

---
Text Font & Style settings - for a node's text.

> fontFamily, fontStyle :: String -> Attribute
> fontFamily = A "FONTFAMILY"
> fontStyle = A "FONTSTYLE"

> lucida, times, helvetica, courier :: Attribute
> lucida = fontFamily "lucida"
> times = fontFamily "times"
> helvetica = fontFamily "helvetica"
> courier = fontFamily "courier"

> normal_s, bold_s, italic_s, bold_italic_s :: Attribute
> normal_s = fontStyle "normal"
> bold_s = fontStyle "bold"
> italic_s = fontStyle "italic"
> bold_italic_s = fontStyle "bold_italic"

---
Node visualisation style functions

> nodeStyle = A "_GO"

> box_t, circle_t, ellipse_t, rhombus_t, text_t, icon_t :: Attribute
> box_t = nodeStyle "box"
> circle_t = nodeStyle "circle"
> ellipse_t = nodeStyle "ellipse"
> rhombus_t = nodeStyle "rhombus"
> text_t = nodeStyle "text"
> icon_t = nodeStyle "icon"

---
`iconFile' creates an ICONFILE attribute for a node. NB - need to set env var
DAVINCI_ICONDIR.

> iconFile = A "ICONFILE"

---
`hidden' creates an attribute for a node which hides it, and `notHidden' does
the reverse.

> hidden, notHidden :: Attribute
> hidden = A "HIDDEN" "true"
> notHidden = A "HIDDEN" "false"

---
`dbl_bordered' creates an attribute for a node which has a double-bordered
shape.

> dbl_bordered :: Attribute
> dbl_bordered = A "BORDER" "double"

---
`edgeColour' specifies the colour of an edge.

> edgeColour :: String -> Attribute
> edgeColour = colour

---
<these> create attributes for esge styles.
NB: solid edges are default, but fn given for readbility in complex exprs.

> solid_e, dotted_e, dashed_e, double_e, thick_e :: Attribute
> solid_e = edgePattern "solid"
> dotted_e = edgePattern "dotted"
> dashed_e = edgePattern "dashed"
> double_e = edgePattern "double"
> thick_e = edgePattern "thick"
> edgePattern = A "EDGEPATTERN"

---
`normal_a', `inverse_a', `double_a', and `no_a' create attributes for arrow
styles.

> normal_a, inverse_a, double_a, no_a :: Attribute
> normal_a = arrows "normal"
> inverse_a = arrows "inverse"
> double_a = arrows "both"
> no_a = arrows "none"
> arrows = A "_DIR"

- Menu types -------------------------------------------------------------------

> data Menu
>  = Menu_entry Menu_id Menu_label
>  | Submenu_entry Menu_id Menu_label [Menu]
>  | Menu_entry_mne Menu_id Menu_label Menu_mne Menu_mod Menu_acc
>  | Submenu_entry_mne Menu_id Menu_label [Menu] Menu_mne
>  | Blank

> type Menu_id = String
> type Menu_label = String

> instance Show Menu where
>	showsPrec _ Blank = zeroArg "blank"
>	showsPrec _ (Menu_entry id nm) = tupleArg "menu_entry" (id,nm)
>	showsPrec _ (Submenu_entry id nm ms) = tupleArg "submenu_entry" (id,nm,ms)
>	showsPrec _ (Menu_entry_mne id nm mne mod acc)
>				= tupleArg "menu_entry_mne" (id,nm,mne,mod,acc)
>	showsPrec _ (Submenu_entry_mne id nm ms mne)
>				= tupleArg "submenu_entry" (id,nm,ms,mne)

> type Menu_mne = String		-- motif mnemonic
> type Menu_acc = String		-- motif acc = keysym WITHOUT XK_
>					-- see <X>/include/keysymdef.h
> data Menu_mod = Alt | Control | Meta | Shift | None
>					-- modifier for acc.
>					-- WARNING: don't steal daVinci accs!

> instance Show Menu_mod where
>	showsPrec _ Alt     = showString "alt"
>	showsPrec _ Control = showString "control"
>	showsPrec _ Meta    = showString "meta"
>	showsPrec _ Shift   = showString "shift"
>	showsPrec _ None    = showString "none"


--- Graph Updates: -------------------------------------------------------------

> data Node_update
>	= Delete_node Node_id
>	| New_node Node_id Type [Attribute]

> instance Show Node_update where
>	showsPrec _ (Delete_node id) = singleArg "delete_node" id
>	showsPrec _ (New_node id ty as) = tupleArg "new_node" (id,ty,as)


> data Edge_update
>	= Delete_edge Edge_id
>	| New_edge                Edge_id Type [Attribute] Node_id Node_id
>	| New_edge_behind Edge_id Edge_id Type [Attribute] Node_id Node_id
>		-- New_edge_behind not useful in v2.0.1

> instance Show Edge_update where
>	showsPrec _ (Delete_edge id) = singleArg "delete_edge" id
>	showsPrec _ (New_edge id ty as n1 n2)
>				= tupleArg "new_edge" (id,ty,as,n1,n2)
>	showsPrec _ (New_edge_behind ln id ty as n1 n2)
>				= sixTupleArg "new_edge_behind" (ln,id,ty,as,n1,n2)
>--				= tupleArg "new_edge_behind" (ln,id,ty,as,n1,n2)


> data Attr_change
>	= Chg_node Node_id [Attribute]
>	| Chg_edge Node_id [Attribute]

> instance Show Attr_change where
>	showsPrec _ (Chg_node id as) = tupleArg "node" (id,as)
>	showsPrec _ (Chg_edge id as) = tupleArg "edge" (id,as)

> data Icon_entry
>	= Icon_entry Icon_id Filename String
>	| Icon_blank
> type Icon = String
> type Icon_id = String

> instance Show Icon_entry where
>	showsPrec _ (Icon_entry id f s) = tupleArg "icon_entry" (id,f,s)
>	showsPrec _ Icon_blank = zeroArg "icon_blank"


- Simple alg types -------------------------------------------------------------

Bool_ = type isomorphic to Bool, but has lower case constructors - used for
convenience when showing cmds or reading replies.

> data Bool_ = False_ | True_ deriving (Eq)
> instance Show Bool_ where
>	showsPrec _ False_ = showString "false"
>	showsPrec _ True_  = showString "true"


---
view orientation. (nb - some unimplemented in v2.0.1?)

> data Orientation = Top_down | Bottom_up | Left_right | Right_left
> instance Show Orientation where
>	showsPrec _ Top_down   = showString "top_down"
>	showsPrec _ Bottom_up  = showString "bottom_up"
>	showsPrec _ Left_right = showString "left_right"
>	showsPrec _ Right_left = showString "right_left"

---
Navigator controls

> data Direction = Up_ | Down_ | Left_ | Right_
> instance Show Direction where
>	showsPrec _ Up_    = showString "up"
>	showsPrec _ Down_  = showString "down"
>	showsPrec _ Left_  = showString "left"
>	showsPrec _ Right_ = showString "right"

---
For controlling a browser?

> data Btype = Bt String String String
> instance Show Btype where
>	showsPrec _ (Bt s1 s2 s3) = tupleArg "bt" (s1,s2,s3)

> type Filename = String



********************************************************************************

> data Answer
>	= OkAnswer
>	| Communication_error String
>	| Node_selections_labels [Node_id]
>	| Node_double_click
>	| Edge_selection_label Edge_id
>	| Edge_selection_labels (Node_id,Node_id)	-- for easy readsPrec
>	| Edge_double_click
>	| Menu_selection Menu_id
>	| Icon_selection Icon_id
>	| Context Context_id
>	| Tcl_answer String
>	| Browser_answer (String,String)         	-- for easy readsPrec
>	| Disconnect
>	| Close
>	| Quit
>	deriving (Eq, Show, Read)

NOTE:	leave tuples as this allows auto-derived readsPrec. Could do own
	version if required.


********************************************************************************

> data Command
>	= Graph Graph_cmd
>	| Multi Multi_cmd
>	| Menu Menu_cmd
>	| App_menu App_menu_cmd
>	| Set Set_cmd
>	| Window Window_cmd
>	| Tcl Tcl_cmd
>	| Special Special_cmd
>	| Nothing_

> instance Show Command where
>	showsPrec _ (Graph c) = singleArg "graph" c
>	showsPrec _ (Multi c) = singleArg "multi" c
>	showsPrec _ (Menu c) = singleArg "menu" c
>	showsPrec _ (App_menu c) = singleArg "app_menu" c
>	showsPrec _ (Set c) = singleArg "set" c
>	showsPrec _ (Window c) = singleArg "window" c
>	showsPrec _ (Tcl c) = singleArg "tcl" c
>	showsPrec _ (Special c) = singleArg "special" c
>	showsPrec _ Nothing_ = zeroArg "nothing"

> data Graph_cmd
>	= New [Graphterm]
>	| New_placed [Graphterm]
>	| Update [Node_update] [Edge_update]
>	| Change_attr [Attr_change]

> instance Show Graph_cmd where
>	showsPrec _ (New g) = singleArg "new" g
>	showsPrec _ (New_placed g) = singleArg "new_placed" g
>	showsPrec _ (Update nus eus) = tupleArg "update" (nus,eus)
>	showsPrec _ (Change_attr as) = singleArg "change_attr" as

> data Multi_cmd
>	= New_context
>	| Open_context Context_id
>	| Set_context Context_id
> type Context_id = String

> instance Show Multi_cmd where
>	showsPrec _ New_context  = zeroArg "new_context"
>	showsPrec _ (Open_context c) = singleArg "open_context" c
>	showsPrec _ (Set_context c) = singleArg "set_context" c

> data Menu_cmd
>	= File File_menu_cmd
>	| View View_menu_cmd
>	| Navigation Navigation_menu_cmd
>	| Abstraction Abstraction_menu_cmd
>	| Layout Layout_menu_cmd

> instance Show Menu_cmd where
>	showsPrec _ (File c) = singleArg "file" c
>	showsPrec _ (View c) = singleArg "view" c
>	showsPrec _ (Navigation c) = singleArg "navigation" c
>	showsPrec _ (Abstraction c) = singleArg "abstraction" c
>	showsPrec _ (Layout c) = singleArg "layout" c

> data File_menu_cmd
>	= NewFile
>	| Open_graph Filename
>	| Open_graph_placed Filename
>	| Open_status Filename
>	| Save_graph Filename
>	| Save_status Filename
>	| Print Filename
>	| PrintDialogue 			-- here, Print was overloaded.
>	| CloseFile
>	| Exit

> instance Show File_menu_cmd where
>	showsPrec _ NewFile = zeroArg "new"
>	showsPrec _ (Open_graph f) = singleArg "open_graph" f
>	showsPrec _ (Open_graph_placed f) = singleArg "open_graph_placed" f
>	showsPrec _ (Open_status f) = singleArg "open_status" f
>	showsPrec _ (Save_graph f) = singleArg "save_graph" f
>	showsPrec _ (Save_status f) = singleArg "save_status" f
>	showsPrec _ (Print f) = singleArg "print" f
>	showsPrec _ PrintDialogue = zeroArg "print"
>	showsPrec _ CloseFile = zeroArg "close"
>	showsPrec _ Exit = zeroArg "exit"


> data View_menu_cmd
>	= Open_new_view
>	| Open_survey_view
>	| Full_scale
>	| Fit_scale_to_window
>	| Scale Int
>	| ScaleDialogue 			-- overloaded.
>	| Graph_info
>	| DaVinci_info

> instance Show View_menu_cmd where
>	showsPrec _ Open_new_view = zeroArg "open_new_view"
>	showsPrec _ Open_survey_view = zeroArg "open_survey_view"
>	showsPrec _ Full_scale = zeroArg "full_scale"
>	showsPrec _ Fit_scale_to_window = zeroArg "fit_scale_to_window"
>	showsPrec _ (Scale i) = singleArg "scale" i
>	showsPrec _ ScaleDialogue = zeroArg "scale"
>	showsPrec _ Graph_info = zeroArg "graph_info"
>	showsPrec _ DaVinci_info = zeroArg "daVinci_info"

> data Navigation_menu_cmd
>	= Select_parents [Node_id]
>	| Select_siblings [Node_id]
>	| Select_childs [Node_id]
>	| Navigator Node_id Direction Bool_
>	| NavigatorDialogue 					-- overloaded
>	| Find String Bool_ Bool_
>	| FindDialogue							-- overloaded

> instance Show Navigation_menu_cmd where
>	showsPrec _ (Select_parents ids) = singleArg "select_parents" ids
>	showsPrec _ (Select_siblings ids) = singleArg "select_siblings" ids
>	showsPrec _ (Select_childs ids) = singleArg "select_childs" ids
>	showsPrec _ (Navigator id d b) = tupleArg "navigator" (id,d,b)
>	showsPrec _ NavigatorDialogue = zeroArg "navigator"
>	showsPrec _ (Find s b1 b2) = tupleArg "find" (s,b1,b2)
>	showsPrec _ FindDialogue = zeroArg "find"


> data Abstraction_menu_cmd
>	= Hide_subgraph [Node_id]
>	| Show_subgraph [Node_id]
>	| Restore_all_subgraphs
>	| Hide_edges [Node_id]
>	| Show_edges [Node_id]
>	| Restore_all_edges

> instance Show Abstraction_menu_cmd where
>	showsPrec _ (Hide_subgraph ids) = singleArg "hide_subgraph" ids
>	showsPrec _ (Show_subgraph ids) = singleArg "show_subgraph" ids
>	showsPrec _ Restore_all_subgraphs = zeroArg "restore_all_subgraphs"
>	showsPrec _ (Hide_edges ids) = singleArg "hide_edges" ids
>	showsPrec _ (Show_edges ids) = singleArg "show_edges" ids
>	showsPrec _ Restore_all_edges = zeroArg "restore_all_edges"



> data Layout_menu_cmd
>	= Improve_all
>	| Improve_visible
>	| Compact_all
>	| Orientation Orientation

> instance Show Layout_menu_cmd where
>	showsPrec _ Improve_all = zeroArg "improve_all"
>	showsPrec _ Improve_visible = zeroArg "improve_visible"
>	showsPrec _ Compact_all = zeroArg "compact_all"
>	showsPrec _ (Orientation o) = singleArg "orientation" o



> data App_menu_cmd
>	= Create_menus [Menu]
>	| Create_icons [Icon_id]
>	| Activate_menus [Menu_id]
>	| Activate_icons [Icon_id]
>	| Control_file_events

> instance Show App_menu_cmd where
>	showsPrec _ (Create_menus ms) = singleArg "create_menus" ms
>	showsPrec _ (Create_icons is) = singleArg "create_icons" is
>	showsPrec _ (Activate_menus ms) = singleArg "activate_menus" ms
>	showsPrec _ (Activate_icons is) = singleArg "activate_icons" is
>	showsPrec _ Control_file_events = zeroArg "control_file_events"



> data Set_cmd
>	= Layout_accuracy Int
>	| Font_size Int
>	| Gap_width Int
>	| Gap_height Int
>	| Multi_edge_gap Int
>	| Self_edge_radius Int
>	| Animation_speed Int
>	| No_cache Bool_

> instance Show Set_cmd where
>	showsPrec _ (Layout_accuracy i) = singleArg "layout_accuracy" i
>	showsPrec _ (Font_size i) = singleArg "font_size" i
>	showsPrec _ (Gap_width i) = singleArg "gap_width" i
>	showsPrec _ (Gap_height i) = singleArg "gap_height" i
>	showsPrec _ (Multi_edge_gap i) = singleArg "multi_edge_gap" i
>	showsPrec _ (Self_edge_radius i) = singleArg "self_edge_radius" i
>	showsPrec _ (Animation_speed i) = singleArg "animation_speed" i
>	showsPrec _ (No_cache b) = singleArg "no_cache" b



> data Window_cmd
>	= Title String
>	| Show_message String
>	| Show_status String
>	| Position Int Int
>	| Size Int Int
>	| Raise
>	| Iconify
>	| Deiconify
>	| Activate
>	| Deactivate
>	| File_browser Bool_ String String String String [Btype] Bool_


> instance Show Window_cmd where
>	showsPrec _ (Title t) = singleArg "title" t
>	showsPrec _ (Show_message s) = singleArg "show_message" s
>	showsPrec _ (Show_status s) = singleArg "show_status" s
>	showsPrec _ (Position i j) = tupleArg "position" (i,j)
>	showsPrec _ (Size i j) = tupleArg "size" (i,j)
>	showsPrec _ Raise = zeroArg "raise"
>	showsPrec _ Iconify = zeroArg "iconify"
>	showsPrec _ Deiconify = zeroArg "deiconify"
>	showsPrec _ Activate = zeroArg "activate"
>	showsPrec _ Deactivate = zeroArg "deactivate"
>	showsPrec _ (File_browser opening_file title action i_dir i_file types show_hidden)
>	 = sevenTupleArg "file_browser"
>		(opening_file,title,action,i_dir,i_file,types,show_hidden)
>--	 = tupleArg "file_browser"
>--		(opening_file,title,action,i_dir,i_file,types,show_hidden)


> data Tcl_cmd
>	= Eval String
>	| Eval_file Filename

> instance Show Tcl_cmd where
>	showsPrec _ (Eval s) = singleArg "eval" s
>	showsPrec _ (Eval_file f) = singleArg "eval_file" f


> data Special_cmd
>	= Select_nodes [Node_id]
>	| Select_edge Edge_id
>	| Focus_node Node_id
>	| Focus_node_animated Node_id
>	| Show_url String

> instance Show Special_cmd where
>	showsPrec _ (Select_nodes ids) = singleArg "select_nodes" ids
>	showsPrec _ (Select_edge id) = singleArg "select_edge" id
>	showsPrec _ (Focus_node id) = singleArg "focus_node" id
>	showsPrec _ (Focus_node_animated id) = singleArg "focus_node_animated" id
>	showsPrec _ (Show_url url) = singleArg "show_url" url
