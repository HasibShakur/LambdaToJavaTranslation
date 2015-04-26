module Lam = struct
  type gratr2_nt = Ws_plus_5
		  | Ws_bar_4
		  | Ws_bar_3
		  | Ws
		  | Term
		  | Start
		  | Ows_opt_6
		  | Ows
		  | Id_range_1
		  | Id_plus_2
		  | Id

  type _Term = App of _Term * _Term | Lam of _Id * _Term | Paren of _Term | Var of _Id
   and _Start = Strt of _Term
   and _Id_range_1 = string
   and _Id_plus_2 = string
   and _Id = string


  type gratr2_parse_tree = Ows
			| Ows_opt_6
			| Ws
			| Ws_bar_3
			| Ws_bar_4
			| Ws_plus_5
			| Id of _Id
			| Id_plus_2 of _Id_plus_2
			| Id_range_1 of _Id_range_1
			| Start of _Start
			| Term of _Term

  type gratr2_rtn_input  = IC of string | NT of gratr2_nt
  type gratr2_parse_rule = gratr2_rtn_input list
  type gratr2_parse_call = string option * string option * gratr2_nt option * gratr2_parse_rule
  type gratr2_run_element = InputChar of string | Id of string | ParseTree of gratr2_parse_tree
  type gratr2_run = gratr2_run_element list


  let gratr2_nt_to_string : gratr2_nt -> string = function
     | Ws_plus_5 -> "ws-plus-5"
     | Ws_bar_4 -> "ws-bar-4"
     | Ws_bar_3 -> "ws-bar-3"
     | Ws -> "ws"
     | Term -> "term"
     | Start -> "start"
     | Ows_opt_6 -> "ows-opt-6"
     | Ows -> "ows"
     | Id_range_1 -> "id-range-1"
     | Id_plus_2 -> "id-plus-2"
     | Id -> "id"
  ;;

  let gratr2_start : gratr2_nt -> gratr2_parse_call list = function
    | Ws_plus_5 -> (Some "P34", None, Some Ws_plus_5, NT Ws_bar_4 :: NT Ws_plus_5 :: []) :: (Some "P33", None, Some Ws_plus_5, NT Ws_bar_4 :: []) :: []
    | Ws_bar_4 -> (Some "P32", None, Some Ws_bar_4, NT Ws_bar_3 :: []) :: (Some "P31", None, Some Ws_bar_4, IC "\t" :: []) :: []
    | Ws_bar_3 -> (Some "P30", None, Some Ws_bar_3, IC " " :: []) :: (Some "P29", None, Some Ws_bar_3, IC "\n" :: []) :: []
    | Ws -> (Some "P35", None, Some Ws, NT Ws_plus_5 :: []) :: []
    | Term -> (Some "Var", None, Some Term, NT Id :: []) :: (Some "Paren", None, Some Term, IC "(" :: NT Ows :: NT Term :: NT Ows :: IC ")" :: []) :: (Some "Lam", None, Some Term, IC "\\" :: NT Ows :: NT Id :: NT Ows :: IC "." :: NT Ows :: NT Term :: []) :: []
    | Start -> (Some "Strt", Some "Strt_end", Some Start, NT Ows :: NT Term :: NT Ows :: []) :: []
    | Ows_opt_6 -> (Some "P37", None, Some Ows_opt_6, []) :: (Some "P36", None, Some Ows_opt_6, NT Ws :: []) :: []
    | Ows -> (Some "P38", None, Some Ows, NT Ows_opt_6 :: []) :: []
    | Id_range_1 -> (Some "P9", None, Some Id_range_1, IC "j" :: []) :: (Some "P8", None, Some Id_range_1, IC "i" :: []) :: (Some "P7", None, Some Id_range_1, IC "h" :: []) :: (Some "P6", None, Some Id_range_1, IC "g" :: []) :: (Some "P5", None, Some Id_range_1, IC "f" :: []) :: (Some "P4", None, Some Id_range_1, IC "e" :: []) :: (Some "P3", None, Some Id_range_1, IC "d" :: []) :: (Some "P25", None, Some Id_range_1, IC "z" :: []) :: (Some "P24", None, Some Id_range_1, IC "y" :: []) :: (Some "P23", None, Some Id_range_1, IC "x" :: []) :: (Some "P22", None, Some Id_range_1, IC "w" :: []) :: (Some "P21", None, Some Id_range_1, IC "v" :: []) :: (Some "P20", None, Some Id_range_1, IC "u" :: []) :: (Some "P2", None, Some Id_range_1, IC "c" :: []) :: (Some "P19", None, Some Id_range_1, IC "t" :: []) :: (Some "P18", None, Some Id_range_1, IC "s" :: []) :: (Some "P17", None, Some Id_range_1, IC "r" :: []) :: (Some "P16", None, Some Id_range_1, IC "q" :: []) :: (Some "P15", None, Some Id_range_1, IC "p" :: []) :: (Some "P14", None, Some Id_range_1, IC "o" :: []) :: (Some "P13", None, Some Id_range_1, IC "n" :: []) :: (Some "P12", None, Some Id_range_1, IC "m" :: []) :: (Some "P11", None, Some Id_range_1, IC "l" :: []) :: (Some "P10", None, Some Id_range_1, IC "k" :: []) :: (Some "P1", None, Some Id_range_1, IC "b" :: []) :: (Some "P0", None, Some Id_range_1, IC "a" :: []) :: []
    | Id_plus_2 -> (Some "P27", None, Some Id_plus_2, NT Id_range_1 :: NT Id_plus_2 :: []) :: (Some "P26", None, Some Id_plus_2, NT Id_range_1 :: []) :: []
    | Id -> (Some "P28", None, Some Id, NT Id_plus_2 :: []) :: []
  ;;

  let start : gratr2_nt = Start

  let gratr2_return : gratr2_nt option -> gratr2_parse_call list = function
    | Some Term -> (None, None, Some Term, NT Ws :: NT Term :: []) :: []
    | _ -> []
  ;;

  let rec norm_Term : _Term -> _Term = function
    | (App((App(x1, (Lam(x2, x3)))), x4)) -> norm_Term(App(x1, norm_Term(Lam(x2, norm_Term(App(x3, x4))))))
    | (App((Lam(x1, x2)), x3)) -> norm_Term(Lam(x1, norm_Term(App(x2, x3))))
    | (App(x1, (App(x2, x3)))) -> norm_Term(App(norm_Term(App(x1, x2)), x3))
    | x -> x
;;

  let rewrite_rules : gratr2_run -> bool * gratr2_run = function
    | ParseTree(Term pt0) :: ParseTree(Ws) :: ParseTree(Term pt1) :: rest -> (true, ParseTree(Term(norm_Term (App(pt0,pt1)))) :: rest)
    | Id "Lam" :: InputChar "\\" :: ParseTree(Ows) :: ParseTree(Id pt0) :: ParseTree(Ows) :: InputChar "." :: ParseTree(Ows) :: ParseTree(Term pt1) :: rest -> (true, ParseTree(Term(norm_Term (Lam(pt0,pt1)))) :: rest)
    | Id "P0" :: InputChar "a" :: rest -> (true, ParseTree(Id_range_1("a")) :: rest)
    | Id "P1" :: InputChar "b" :: rest -> (true, ParseTree(Id_range_1("b")) :: rest)
    | Id "P10" :: InputChar "k" :: rest -> (true, ParseTree(Id_range_1("k")) :: rest)
    | Id "P11" :: InputChar "l" :: rest -> (true, ParseTree(Id_range_1("l")) :: rest)
    | Id "P12" :: InputChar "m" :: rest -> (true, ParseTree(Id_range_1("m")) :: rest)
    | Id "P13" :: InputChar "n" :: rest -> (true, ParseTree(Id_range_1("n")) :: rest)
    | Id "P14" :: InputChar "o" :: rest -> (true, ParseTree(Id_range_1("o")) :: rest)
    | Id "P15" :: InputChar "p" :: rest -> (true, ParseTree(Id_range_1("p")) :: rest)
    | Id "P16" :: InputChar "q" :: rest -> (true, ParseTree(Id_range_1("q")) :: rest)
    | Id "P17" :: InputChar "r" :: rest -> (true, ParseTree(Id_range_1("r")) :: rest)
    | Id "P18" :: InputChar "s" :: rest -> (true, ParseTree(Id_range_1("s")) :: rest)
    | Id "P19" :: InputChar "t" :: rest -> (true, ParseTree(Id_range_1("t")) :: rest)
    | Id "P2" :: InputChar "c" :: rest -> (true, ParseTree(Id_range_1("c")) :: rest)
    | Id "P20" :: InputChar "u" :: rest -> (true, ParseTree(Id_range_1("u")) :: rest)
    | Id "P21" :: InputChar "v" :: rest -> (true, ParseTree(Id_range_1("v")) :: rest)
    | Id "P22" :: InputChar "w" :: rest -> (true, ParseTree(Id_range_1("w")) :: rest)
    | Id "P23" :: InputChar "x" :: rest -> (true, ParseTree(Id_range_1("x")) :: rest)
    | Id "P24" :: InputChar "y" :: rest -> (true, ParseTree(Id_range_1("y")) :: rest)
    | Id "P25" :: InputChar "z" :: rest -> (true, ParseTree(Id_range_1("z")) :: rest)
    | Id "P26" :: ParseTree(Id_range_1 pt0) :: rest -> (true, ParseTree(Id_plus_2(pt0)) :: rest)
    | Id "P27" :: ParseTree(Id_range_1 pt0) :: ParseTree(Id_plus_2 pt1) :: rest -> (true, ParseTree(Id_plus_2(pt0^pt1)) :: rest)
    | Id "P28" :: ParseTree(Id_plus_2 pt0) :: rest -> (true, ParseTree(Id(pt0)) :: rest)
    | Id "P29" :: InputChar "\n" :: rest -> (true, ParseTree(Ws_bar_3) :: rest)
    | Id "P3" :: InputChar "d" :: rest -> (true, ParseTree(Id_range_1("d")) :: rest)
    | Id "P30" :: InputChar " " :: rest -> (true, ParseTree(Ws_bar_3) :: rest)
    | Id "P31" :: InputChar "\t" :: rest -> (true, ParseTree(Ws_bar_4) :: rest)
    | Id "P32" :: ParseTree(Ws_bar_3) :: rest -> (true, ParseTree(Ws_bar_4) :: rest)
    | Id "P33" :: ParseTree(Ws_bar_4) :: rest -> (true, ParseTree(Ws_plus_5) :: rest)
    | Id "P34" :: ParseTree(Ws_bar_4) :: ParseTree(Ws_plus_5) :: rest -> (true, ParseTree(Ws_plus_5) :: rest)
    | Id "P35" :: ParseTree(Ws_plus_5) :: rest -> (true, ParseTree(Ws) :: rest)
    | Id "P36" :: ParseTree(Ws) :: rest -> (true, ParseTree(Ows_opt_6) :: rest)
    | Id "P38" :: ParseTree(Ows_opt_6) :: rest -> (true, ParseTree(Ows) :: rest)
    | Id "P4" :: InputChar "e" :: rest -> (true, ParseTree(Id_range_1("e")) :: rest)
    | Id "P5" :: InputChar "f" :: rest -> (true, ParseTree(Id_range_1("f")) :: rest)
    | Id "P6" :: InputChar "g" :: rest -> (true, ParseTree(Id_range_1("g")) :: rest)
    | Id "P7" :: InputChar "h" :: rest -> (true, ParseTree(Id_range_1("h")) :: rest)
    | Id "P8" :: InputChar "i" :: rest -> (true, ParseTree(Id_range_1("i")) :: rest)
    | Id "P9" :: InputChar "j" :: rest -> (true, ParseTree(Id_range_1("j")) :: rest)
    | Id "Paren" :: InputChar "(" :: ParseTree(Ows) :: ParseTree(Term pt0) :: ParseTree(Ows) :: InputChar ")" :: rest -> (true, ParseTree(Term(norm_Term (Paren(pt0)))) :: rest)
    | Id "Strt" :: ParseTree(Ows) :: ParseTree(Term pt0) :: ParseTree(Ows) :: Id "Strt_end" :: rest -> (true, ParseTree(Start(Strt(pt0))) :: rest)
    | Id "Var" :: ParseTree(Id pt0) :: rest -> (true, ParseTree(Term(norm_Term (Var(pt0)))) :: rest)
    | Id "P37" :: rest -> (true, ParseTree(Ows_opt_6) :: rest)
    | x -> (false, x)
  ;;

end

