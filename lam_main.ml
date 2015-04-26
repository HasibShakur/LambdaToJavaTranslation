open Printf
open String
open Gratr2_ocaml;;
open Arg;;
open Lam;;

module Lam_main = Gratr2_main(Lam);;
open Lam_main;;
open Lam;;


(**"return \"\";"**)
let message = "public class sample{ 
		public interface Lambda{ 
			public Lambda app(Lambda arg);
			public String toString();		
		}\n"
let message_class1 = "public static class "
let class_name = "Lambda"
let message_class2 = " implements Lambda {\n"
let message_variable1 = " protected String str;\n"
let message_variable2 = " protected String buff;\n"
let message_constructor1 = "public Lambda" 
let message_constructor2 = "(String s,String b){\n"
let message_setter1 = "str = s;\n"
let message_setter2 = "buff = b;
		}\n"
let message_app =	"public Lambda app(Lambda arg){ 
				return arg;
			}\n"
let message_toString1=	"public String toString() {\n" 
let message_toStringBody = "if(buff == null)
			    {
			       return str;
			    }
			    else
			    {
				return str+buff;
			    }\n"
	
let message_toString2=  "               	}	
		}\n"
let message_main_declaration = "public static void main(String[] args){\n"
let object_name1 = "Lambda"
let object_new = " = new "

let message_main_function = "public static void main(String[] args)
			{ 
				Lambda1 l1 = new Lambda1(\"s\",null);
				Lambda2 l2 = new Lambda2(\"z\","
let message_end = " );
		}
	}"
let java_file = "sample.java"
let infile = ref "";;
let show_run = ref false;;
let show_parsed = ref false;;
let debug_parse = ref false;;
let debug_rewrite = ref false;;
let i = ref 0;;
let buf = ref "";;


let usage = "Usage: gratr <file>\n";;

let analysis_options = [ ]
;;

(*let rec print_term (os:string -> unit) (t:_Term) : unit =
  match t with
      App(t1,t2) -> os "("; print_term os t1; os " "; print_term os t2; os ")"
    | Lam(x,t) -> os "(\\ "; os x; os " . "; print_term os t; os ")"
    | Paren(t) -> os "("; print_term os t; os ")"
    | Var(x) -> os x
;;*)	

let rec print_java (oc:out_channel ) (t:_Term) : unit =
  match t with
      App(t1,t2) -> output_string oc "";print_java oc t1;print_java oc t2;output_string oc ""
    | Lam(x,t) -> i := !(i) + 1;
	          buf := !(buf) ^ x;		
		  output_string oc message_class1;
		  output_string oc class_name;
		  output_string oc (string_of_int !(i)); 
		  output_string oc " ";
		  output_string oc message_class2;
                  output_string oc message_variable1;
		  output_string oc message_variable2;
    		  output_string oc message_constructor1;
		  output_string oc (string_of_int !(i)); 
		  output_string oc message_constructor2;
                  output_string oc message_setter1;
		  output_string oc message_setter2;
		  output_string oc message_app;
     		  output_string oc message_toString1;
		  output_string oc message_toStringBody;
		  output_string oc message_toString2;
		  print_java oc t;
		  output_string oc ""
    | Paren(t) -> print_java oc t;output_string oc ""
    | Var(x) -> output_string oc ""
;;

let main (oc:out_channel) : unit =
  Arg.parse (align analysis_options) (fun x -> infile := x) usage;
  let read =
    let ic = open_in !infile in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)
   in
  

  match gratr2_parse read false with
  | Fail(n) ->
     prerr_string "Parsing error\n";
     prerr_string "Characters read before failure:\n";
     prerr_string (String.sub read 0 n);
     prerr_string "\n";
  | Run(parsed) ->
     let parsed = List.rev parsed in

     let run = rewrite parsed false in
	match run with
	   [ ParseTree(Start(Strt(t)))] ->
		print_java oc t;	     
		output_string oc "\n"
	 | _ -> prerr_string "Parse error\n"

;;

(*let writeMain (oc:out_channel) : unit = 
	output_string oc message_main_declaration;
	for j =1 to !(i) do output_string oc object_name1;
			    output_string oc (string_of_int j);
			    output_string oc " ";	
			    output_string oc "l";
			    output_string oc (string_of_int j);
			    output_string oc object_new;
			    output_string oc object_name1;
			    output_string oc (string_of_int j);
			    if j = 1 then output_string oc "(s,null);\n"
			    else output_string oc "(z,"	;  output_string oc !(buf); output_string oc ");\n";
			    if j = 1 then output_string oc "l1.toString()\n;"
			    else  output_string oc "l2.toString()\n;"
			done;
;;	*)
			    		

let () = 
  let oc = 
	open_out_gen
         [Open_wronly; Open_creat; Open_append; Open_text] 0o666 java_file in
  output_string oc message;
  main oc;
  output_string oc message_main_function;
  output_string oc "\"";
  output_string oc !(buf);
  output_string oc "\"";
  output_string oc message_end;	
  
  (*writeMain oc;*)


