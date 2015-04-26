open Char;;

module type Gratr2_types =
  sig
    type gratr2_nt
    type gratr2_parse_tree 

    type gratr2_rtn_input  = IC of string | NT of gratr2_nt
    type gratr2_parse_rule = gratr2_rtn_input list
    type gratr2_parse_call = string option * string option * gratr2_nt option * gratr2_parse_rule
    type gratr2_run_element = InputChar of string | Id of string | ParseTree of gratr2_parse_tree
    type gratr2_run = gratr2_run_element list

    val start : gratr2_nt
    val gratr2_start : gratr2_nt -> gratr2_parse_call list
    val gratr2_return : gratr2_nt option -> gratr2_parse_call list
    val rewrite_rules : gratr2_run -> (bool * gratr2_run)
    val gratr2_nt_to_string : gratr2_nt -> string
  end
;;

module Gratr2_main (X :Gratr2_types) = struct    
  open X
  let rec run_to_string : gratr2_run -> string = function
    | InputChar(c) :: xs -> 
       "#" ^ c ^ " " ^ run_to_string xs
    | Id(s) :: xs ->
       s ^ " " ^ run_to_string xs
    | ParseTree(pt) :: xs -> "ParseTree " ^ (* ^ (gratr2_nt_to_string nt) ^*) " " ^ run_to_string xs 
    | [] -> "\n"
  ;;

  type parsed = Run of gratr2_run | Fail of int

  let gratr2_parse s dbg =
    let len = String.length s in
    
    let insert_front_id run = function
      | Some(id),_,_,_ -> Id id ::run
      | _ -> run
    in

    let insert_back_id run = function
      | Some(id) -> Id id ::run
      | _ -> run
    in

    let rec parse frames n run long contin =
      let left = n < len in

      if dbg then (
	if left then (
	  print_string "'";
	  print_string (Char.escaped s.[n]);
	  print_string "'";
	) else (
	  print_string "end";
	);
	print_string "\t";
	List.iter (
	    fun (id,_,_,_) -> (
	      (match id with
	       | Some(x) -> print_string x;
	       | _ -> print_string "ret");
	      print_string "; ";
	    )
	  ) frames;
	print_string "\n";
	
      );

      match frames with

      | (id,id',call,[])::fs -> parse_filter fs n (insert_back_id run id') long (fun l -> parse fs n (insert_back_id run id') l contin) (gratr2_return call) 

      | (id,id',call,NT nt :: xs)::fs -> 
	 (* This prevents stack overflow. This logic is not proven to be complete: there may be other situations that would lead to a stack overflow *)
	 let frames = 
	   if xs = [] && (match call with Some(x) -> x = nt | _ -> false) then match id' with
									       | None -> fs
									       | Some(_) -> (id,id',None,xs)::fs
	   else
	     (id,id',call,xs)::fs
	 in
	 parse_filter frames n run long contin (gratr2_start nt)

      | (id,id',call,IC c :: xs)::fs -> (
	let rec aux i n = 
	  if i >= String.length c then
	    parse ((id,id',call,xs)::fs) n (InputChar c :: run) long contin
	  else if left && c.[i] = s.[n] then
	    aux (i+1) (n+1)
	  else
	    contin (if n > long then n else long)
	in
	aux 0 n
      )

      | [] -> if left then contin (if n > long then n else long) else Run(run) 

    and parse_filter frames n run long contin = function
      | [] -> contin (if n > long then n else long)
      | c::cs -> parse (c::frames) n (insert_front_id run c) long (fun l -> parse_filter frames n run l contin cs)
    in

    parse_filter [] 0 [] 0 (fun n -> Fail(n)) (gratr2_start start)
  ;;

  let rewrite (r : gratr2_run) (dbg : bool) : gratr2_run =
    let rec rewrite (n,r) contin = 
      if dbg then (
	print_string (string_of_int n);
	print_string (run_to_string r);
	print_string "\n";	
      );    

      match (n,r) with
      | 0, r -> contin (false, 1, r)
      | _, [] -> contin (false, 1, [])
      | n, x :: xs -> rewrite (n-1,xs) (fun (b,n,r) -> match rewrite_rules (x::r) with
						     | true, r -> rewrite (n,r) contin
						     | false, r -> contin (b , (if b then n+1 else 1) , r)
				       )
    in
    let _,_,r = rewrite ((List.length r), r) (fun x -> x) in
    r
  ;;

  let rewrite_old (r : gratr2_run) (dbg : bool) : gratr2_run =
    let rec rewrite contin = function
      | [] -> contin []
      | h::t -> rewrite (fun r -> match rewrite_rules (h::r) with
				    | true, r -> rewrite contin r
				    | false, r -> contin r) t
    in
    rewrite (fun x -> x) r
  ;;  

end



 
  
  



