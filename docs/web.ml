open Js_of_ocaml

class type editor = object
  method getValue : unit -> Js.js_string Js.t Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method getSession : < setMode : Js.js_string Js.t -> unit Js.meth > Js.t Js.meth
  method setTheme : Js.js_string Js.t -> unit Js.meth
end

let document = Dom_html.document
let editor_div = Js.Opt.get (document##getElementById(Js.string "editor")) (fun () -> assert false)
let output_div = Js.Opt.get (document##getElementById(Js.string "output")) (fun () -> assert false)
let run_button = Js.Opt.get (document##getElementById(Js.string "run")) (fun () -> assert false)
let clear_button = Js.Opt.get (document##getElementById(Js.string "clear")) (fun () -> assert false)
let load_example_button = Js.Opt.get (document##getElementById(Js.string "load_example")) (fun () -> assert false)
let load_stacknew_example_button = Js.Opt.get (document##getElementById(Js.string "load_stacknew_example")) (fun () -> assert false)
let toggle_help_button = Js.Opt.get (document##getElementById(Js.string "toggle_help")) (fun () -> assert false)
let close_help_button = Js.Opt.get (document##getElementById(Js.string "close_help")) (fun () -> assert false)
let syntax_help_div = Js.Opt.get (document##getElementById(Js.string "syntax_help")) (fun () -> assert false)

let editor = (Js.Unsafe.coerce (Js.Unsafe.global##.ace##edit(editor_div)) : editor Js.t)
let () = editor##setTheme(Js.string "ace/theme/monokai")
let () = (editor##getSession)##setMode(Js.string "ace/mode/ocaml")

let append_output str =
  let current = output_div##.innerHTML in
  output_div##.innerHTML := Js.string (Js.to_string current ^ str ^ "\n")

let clear_output () =
  output_div##.innerHTML := Js.string ""

let example_code = {|type STACK = {
  type t a;
  empty 'a : t a;
  push 'a : a -> t a -> t a;

  type pair a = (a, t a);
  pop 'a : t a -> opt (pair a);
};

ListStack :> STACK = {
  type t a = List.list a;

  empty = List.nil;
  push = fun x st => List.cons x st;

  type pair a = (a, t a);

  pop = fun st =>
    List.caselist st
      (fun () => Opt.none)
      (fun head tail => Opt.some (head, tail));
};

reverse =
  fun (S : STACK) => fun '(a : type) => fun (s : S.t a) =>
    (rec (aux : S.t a -> S.t a -> S.t a) => 
      (fun acc => fun st =>
        Opt.caseopt (S.pop st)
          (fun () => acc)
          (fun (x, rest) => aux (S.push x acc) rest)
      )
    ) 
    (S.empty) s
;

test = fun () => {
  st =
    ListStack.push 3 (
      ListStack.push 2 (
        ListStack.push 1 (ListStack.empty)
      )
    );

  printStack = rec self => fun s =>
      caseopt (ListStack.pop s)
        (fun () => ())
        (fun (x, sRest) => {
          do Int.print x;
          do self sRest;
        });

  do Text.print "Original stack (top to bottom): ";
  do printStack st; 

  stRev = reverse ListStack st;

  do Text.print "\nReversed stack (top to bottom): ";
  do printStack stRev;
};

do test ();|}

let stacknew_example_code = {|type STACK = {
    type container a;
    new 'a : list a -> container a;
    push 'a : container a -> a -> container a;
    pop 'a : container a -> opt a;
};

Stack = {
    new =
        fun (module : STACK) =>
        fun '(a : type) =>
        fun (list_of_element : list a) =>
            (module, module.new list_of_element);
            
    push =
        fun '(a : type) '(cont : type) =>
        fun (module : (STACK with (container a = cont))) =>
        fun (container : cont) =>
        fun (element : a) =>
            (module, module.push container element);
};

ListStack :> STACK = {
    type container a = list a;
    new = fun list_element => list_element;
    push = fun li => fun el => List.cons el li;
    pop = List.head;
};

do {
    li = List.cons 0 (List.nil);  (; define a list of integer ;)
    do (li : list int);           (; check it is indeed list int ;)
    st = Stack.new ListStack li;  (; generic Stack.new returns a pair ;)
    do (st._1 : STACK);                   (; first element is a STACK module ;)
    do (st._2 : ListStack.container int); (; 2nd element is a container int;)
};|}

(* Set up print function for primitives to use *)
let () = Prim.print := append_output

let env = ref Env.empty
let state = ref Lambda.Env.empty

let run_code ?(is_prelude=false) code =
  try
    let lexbuf = Lexing.from_string code in
    lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = if is_prelude then "prelude" else "web"};
    let prog = Parser.prog Lexer.token lexbuf in
    let sign, _, fprog = Elab.elab !env prog in
    let lambda = Compile.compile (Erase.erase_env !env) fprog in
    let value = Lambda.eval !state lambda in
    (* Update environment for next evaluation *)
    let Types.ExT(aks, Types.StrT(tr)) = sign in
    env := Env.add_row tr (Env.add_typs aks !env);
    let ls = List.sort compare (List.map fst tr) in
    let vs = match value with Lambda.TupV(vs) -> vs | _ -> assert false in
    state := List.fold_right2 Lambda.Env.add ls vs !state;
    if is_prelude then append_output "Prelude loaded successfully.\n"
  with
  | Source.Error (region, msg) ->
    append_output ("Error at " ^ Source.string_of_region region ^ ": " ^ msg)
  | e ->
    append_output ("Error: " ^ Printexc.to_string e)

let () =
  run_button##.onclick := Dom_html.handler (fun _ ->
    run_code (Js.to_string (editor##getValue()));
    Js._false
  );
  clear_button##.onclick := Dom_html.handler (fun _ ->
    clear_output ();
    Js._false
  );
  load_example_button##.onclick := Dom_html.handler (fun _ ->
    editor##setValue(Js.string example_code);
    run_code example_code;
    Js._false
  );
  load_stacknew_example_button##.onclick := Dom_html.handler (fun _ ->
    editor##setValue(Js.string stacknew_example_code);
    run_code stacknew_example_code;
    Js._false
  );
  toggle_help_button##.onclick := Dom_html.handler (fun _ ->
    let classList = syntax_help_div##.classList in
    ignore (classList##remove(Js.string "hidden"));
    Js._false
  );
  close_help_button##.onclick := Dom_html.handler (fun _ ->
    let classList = syntax_help_div##.classList in
    ignore (classList##add(Js.string "hidden"));
    Js._false
  );
  (* Add Ctrl+Enter shortcut *)
  let _ = Dom_html.addEventListener Dom_html.document 
    Dom_html.Event.keydown
    (Dom_html.handler (fun (e : Dom_html.keyboardEvent Js.t) ->
      if Js.to_bool e##.ctrlKey && e##.keyCode = 13 then begin
        run_code (Js.to_string (editor##getValue()));
        Js._false
      end else
        Js._true))
    Js._false in
  () 