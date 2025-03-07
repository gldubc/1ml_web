open Js_of_ocaml
module P = Proto_lib

class type editor = object
  method getValue : unit -> Js.js_string Js.t Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method getSession : < setMode : Js.js_string Js.t -> unit Js.meth > Js.t Js.meth
  method setTheme : Js.js_string Js.t -> unit Js.meth
  method resize : unit -> unit Js.meth
  method setReadOnly : bool Js.t -> unit Js.meth
end

let document = Dom_html.document
let elixir_editor_div = Js.Opt.get (document##getElementById(Js.string "elixir_editor")) (fun () -> assert false)
let editor_div = Js.Opt.get (document##getElementById(Js.string "editor")) (fun () -> assert false)
let output_div = Js.Opt.get (document##getElementById(Js.string "output")) (fun () -> assert false)
let systemf_editor_div = Js.Opt.get (document##getElementById(Js.string "systemf_editor")) (fun () -> assert false)
let run_button = Js.Opt.get (document##getElementById(Js.string "run")) (fun () -> assert false)
let clear_button = Js.Opt.get (document##getElementById(Js.string "clear")) (fun () -> assert false)
let load_example_button = Js.Opt.get (document##getElementById(Js.string "load_example")) (fun () -> assert false)
let load_stacknew_example_button = Js.Opt.get (document##getElementById(Js.string "load_stacknew_example")) (fun () -> assert false)
let load_elixir_example_button = Js.Opt.get (document##getElementById(Js.string "load_elixir_example")) (fun () -> assert false)
let edit_ml_button = Js.Opt.get (document##getElementById(Js.string "edit_ml")) (fun () -> assert false)
let toggle_help_button = Js.Opt.get (document##getElementById(Js.string "toggle_help")) (fun () -> assert false)
let close_help_button = Js.Opt.get (document##getElementById(Js.string "close_help")) (fun () -> assert false)
let syntax_help_div = Js.Opt.get (document##getElementById(Js.string "syntax_help")) (fun () -> assert false)
let load_prelude_checkbox = Js.Opt.get (document##getElementById(Js.string "load_prelude")) (fun () -> assert false)
let load_prelude_checkbox = (Js.Unsafe.coerce load_prelude_checkbox : < checked : bool Js.t Js.prop > Js.t)

(* Initialize Elixir editor with Ace *)
let elixir_editor = (Js.Unsafe.coerce (Js.Unsafe.global##.ace##edit(elixir_editor_div)) : editor Js.t)
let () = elixir_editor##setTheme(Js.string "ace/theme/monokai")
let () = (elixir_editor##getSession)##setMode(Js.string "ace/mode/elixir")

(* Initialize 1ML editor with Ace *)
let ml_editor = (Js.Unsafe.coerce (Js.Unsafe.global##.ace##edit(editor_div)) : editor Js.t)
let () = ml_editor##setTheme(Js.string "ace/theme/monokai")
let () = (ml_editor##getSession)##setMode(Js.string "ace/mode/ocaml")
let () = ml_editor##setReadOnly(Js._true)

(* Set initial content for the systemf div *)
let () = systemf_editor_div##.innerHTML := Js.string "<div style=\"color: #aaa; padding: 10px; font-style: italic;\">System F output will appear here when you run code</div>"

(* Get tab elements *)
let tab_buttons = document##getElementsByClassName(Js.string "tab-button")
let tab_panes = document##getElementsByClassName(Js.string "tab-pane")

(* Tab switching function *)
let switch_tab button =
  let target_id = Js.to_string (Js.Unsafe.get button "dataset")##.tab in
  
  (* Update button states *)
  for i = 0 to tab_buttons##.length - 1 do
    let btn = Js.Opt.get (tab_buttons##item(i)) (fun () -> assert false) in
    if btn == button then
      btn##.classList##add(Js.string "active")
    else
      btn##.classList##remove(Js.string "active")
  done;
  
  (* Update tab pane states *)
  for i = 0 to tab_panes##.length - 1 do
    let pane = Js.Opt.get (tab_panes##item(i)) (fun () -> assert false) in
    if Js.to_string pane##.id = target_id then
      pane##.classList##add(Js.string "active")
    else
      pane##.classList##remove(Js.string "active")
  done;
  
  (* Refresh the systemf editor if it's being shown *)
  if target_id = "systemf-term" then begin
    (* Just ensure the div is visible with good styles *)
    let style = systemf_editor_div##.style in
    style##.display := Js.string "block";
    style##.height := Js.string "100%";
    style##.width := Js.string "100%";
    style##.overflow := Js.string "auto";
  end

(* Add click handlers to tab buttons *)
let () =
  for i = 0 to tab_buttons##.length - 1 do
    let button = Js.Opt.get (tab_buttons##item(i)) (fun () -> assert false) in
    button##.onclick := Dom_html.handler (fun _ ->
      switch_tab button;
      Js._false
    )
  done

let append_output str =
  let current = output_div##.innerHTML in
  output_div##.innerHTML := Js.string (Js.to_string current ^ str ^ "\n")

let clear_output () =
  output_div##.innerHTML := Js.string ""

(* Variables to track editor state *)
let ml_editor_readonly = ref true

(* Toggle ML editor read-only state *)
let toggle_ml_editor_editable () =
  ml_editor_readonly := not !ml_editor_readonly;
  ml_editor##setReadOnly(Js.bool !ml_editor_readonly);
  
  (* Use DOM methods to set button text *)
  let button_text = if !ml_editor_readonly then "Edit" else "Lock" in
  Js.Opt.iter (edit_ml_button##.firstChild)
    (fun node -> Dom.removeChild edit_ml_button node);
  Dom.appendChild edit_ml_button
    (document##createTextNode(Js.string button_text))

(* Example code *)
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

  type pair a = (a, t a);
  pop = fun (n, xs) => 
    case xs
      (fun () => None)
      (fun (h, t) => Some (h, (n-1, t)));
};

Stack = {
  type STACK = STACK;
  
  stack : STACK = ListStack;

  push 'a (x : a) (st : stack.t a) : stack.t a = stack.push x st;
  
  pop 'a (st : stack.t a) = stack.pop st;
};

Stack.push 42 (Stack.push 17 Stack.empty);
|} 

(* Example Elixir code *)
let elixir_example_code = {|defmodtype StackT do
  $param item
  $opaque s
  callback new : list(item) -> s
end

defmodule StackInt do
  @behaviour StackT
  $param item = int
  $type s = list(int)
  def new (l : list(item)) : s = l
end

defmodule StackGeneric do
  $param a
  @behaviour StackT
  $param item = a
  $type s = list(item)
  def new (l : list(item)) : s = l
end
|}

(* New function to translate Elixir to 1ML *)
let translate_elixir_to_ml elixir_code =
  (* Use the read_string function from Formal1to2 *)
  P.Formal1to2.read_string elixir_code

(* Example Stacknew code *)
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
    li = List.cons 42 (List.nil);  (; define a list of integer ;)
    do (li : list int);           (; check it is indeed list int ;)
    st = Stack.new ListStack li;  (; generic Stack.new returns a pair ;)
    do (st._1 : STACK);                   (; first element is a STACK module ;)
    do (st._2 : ListStack.container int); (; 2nd element is a container int;)
    x = ListStack.pop (st._2);
    do caseopt x (fun () => ()) (fun y => do Int.print y);
};|}

(* Set up print function for primitives to use *)
let () = Oneml.Prim.print := append_output

let env = ref Oneml.Env.empty
let state = ref Oneml.Lambda.Env.empty

let run_code ?(is_prelude = false) src =
  try
    let lexbuf = Lexing.from_string src in
    lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = if is_prelude then "prelude" else "web"};
    let prog = Oneml.Parser.prog Oneml.Lexer.token lexbuf in
    let sign, _, fprog = Oneml.Elab.elab !env prog in
    let lambda = Oneml.Compile.compile (Oneml.Erase.erase_env !env) fprog in
    let value = Oneml.Lambda.eval !state lambda in
    
    (* Display System F term and type *)
    let buffer = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt "@[<v 0>";
    Format.fprintf fmt "Term:@.";
    Oneml.Fomega.pp_exp fmt fprog;
    Format.fprintf fmt "@.@.Type:@.";
    Oneml.Types.pp_of_norm_extyp fmt sign;
    Format.fprintf fmt "@]@.";
    Format.pp_print_flush fmt ();
    let buffer_contents = Buffer.contents buffer in
    
    (* COMPLETELY REPLACE the Ace editor approach with direct HTML *)
    (* Convert the raw buffer to HTML-safe text with proper formatting *)
    let html_safe_content = Js.string ("<pre style=\"color: white; font-family: monospace; white-space: pre-wrap; margin: 0; padding: 10px; height: 100%; width: 100%; overflow: auto; box-sizing: border-box; background-color: #272822;\">" ^ 
                                      buffer_contents ^ "</pre>") in
    
    (* Set the innerHTML directly instead of using Ace *)
    systemf_editor_div##.innerHTML := html_safe_content;
    
    (* Update environment for next evaluation *)
    let Oneml.Types.ExT(aks, Oneml.Types.StrT(tr)) = sign in
    env := Oneml.Env.add_row tr (Oneml.Env.add_typs aks !env);
    let ls = List.sort compare (List.map fst tr) in
    let vs = match value with Oneml.Lambda.TupV(vs) -> vs | _ -> assert false in
    state := List.fold_right2 Oneml.Lambda.Env.add ls vs !state;
  with
  | Oneml.Source.Error (region, msg) ->
    append_output ("Error at " ^ Oneml.Source.string_of_region region ^ ": " ^ msg)
  | e ->
    append_output ("Error: " ^ Printexc.to_string e)

let prelude_source = {|;; Fun

Fun =
{
  bot = rec bot => fun x => bot x;
  id x = x;
  const x y = x;
  (>>) (f, g) x = g (f x);
  (<<) (f, g) x = f (g x);
  curry f x y = f (x, y);
  uncurry f (x, y) = f x y;
};
include Fun;


;; Bool

Bool =
{
  type bool = primitive "bool";
  type t = bool;
  true = primitive "true" ();
  false = primitive "false" ();
  not b = if b then false else true;
  print b = primitive "Text.print" (if b then "true" else "false");
};
type bool = Bool.t;
true = Bool.true;
false = Bool.false;
not = Bool.not;

(==) 'a (x : a, y : a) = primitive "==" a (x, y);
(<>) 'a (x : a, y : a) = not (x == y);


;; Int

Int =
{
  type int = primitive "int";
  type t = int;
  (+) = primitive "Int.+";
  (-) = primitive "Int.-";
  (*) = primitive "Int.*";
  (/) = primitive "Int./";
  (%) = primitive "Int.%";
  (<) = primitive "Int.<";
  (>) = primitive "Int.>";
  (<=) = primitive "Int.<=";
  (>=) = primitive "Int.>=";
  print = primitive "Int.print";
};
type int = Int.t;
(+) = Int.+;
(-) = Int.-;
(*) = Int.*;
(/) = Int./;
(%) = Int.%;
(<) = Int.<;
(>) = Int.>;
(<=) = Int.<=;
(>=) = Int.>=;


;; Char

Char =
{
  type char = primitive "char";
  type t = char;
  toInt = primitive "Char.toInt";
  fromInt = primitive "Char.fromInt";
  print = primitive "Char.print";
};
type char = Char.t;


;; Text

Text =
{
  type text = primitive "text";
  type t = text;
  (++) = primitive "Text.++";
  (<) = primitive "Text.<";
  (>) = primitive "Text.>";
  (<=) = primitive "Text.<=";
  (>=) = primitive "Text.>=";
  length t = primitive "Text.length" t;
  sub t i = primitive "Text.sub" (t, i);
  fromChar c = primitive "Text.fromChar" c;
  print = primitive "Text.print";
};
type text = Text.t;
(++) = Text.++;
print = Text.print;


;; Opt

type OPT =
{
  type opt a;
  none 'a : opt a;
  some 'a : a -> opt a;
  caseopt 'a 'b : opt a -> (() -> b) -> (a -> b) -> b;
};
Opt :> OPT =
{
  type opt a = wrap (b : type) => (() -> b) -> (a -> b) -> b;
  none 'a = wrap (fun (b : type) (n : () -> b) (s : a -> b) => n ()) : opt a;
  some 'a x = wrap (fun (b : type) (n : () -> b) (s : a -> b) => s x) : opt a;
  caseopt xo = (unwrap xo : opt _) _;
};
include Opt;


;; Alt

type ALT =
{
  type alt a b;
  left 'a 'b : a -> alt a b;
  right 'a 'b : b -> alt a b;
  casealt 'a 'b 'c : alt a b -> (a -> c) -> (b -> c) -> c;
};
Alt :> ALT =
{
  type alt a b = wrap (c : type) => (a -> c) -> (b -> c) -> c;
  left 'a 'b x =
    wrap (fun (c : type) (l : a -> c) (r : b -> c) => l x) : alt a b;
  right 'a 'b x =
    wrap (fun (c : type) (l : a -> c) (r : b -> c) => r x) : alt a b;
  casealt xy = (unwrap xy : alt _ _) _;
};
include Alt;


;; List

type LIST_CORE =
{
  type list a;
  nil 'a : list a;
  cons 'a : a -> list a -> list a;
  foldr 'a 'b : list a -> b -> (a -> b -> b) -> b;
};
type LIST =
{
  include LIST_CORE;
  caselist 'a 'b : list a -> (() -> b) -> (a -> list a -> b) -> b;
  isNil 'a : list a -> bool;
  head 'a : list a -> opt a;
  tail 'a : list a -> opt (list a);
  length 'a : list a -> int;
  cat 'a : list a -> list a -> list a;
  rev 'a : list a -> list a;
  nth 'a : list a -> int -> opt a;
  map 'a 'b : list a -> (a -> b) -> list b;
  filter 'a : list a -> (a -> bool) -> list a;
  foldl 'a 'b : list a -> b -> (b -> a -> b) -> b;
};
List :> LIST =
{
  include
  {
    type list a = wrap (b : type) => b -> (a -> b -> b) -> b;
    nil 'a = wrap (fun (b : type) (n : b) (c : a -> b -> b) => n) : list _;
    cons x xs =
      wrap (fun (b : type) (n : b) (c : _ -> b -> b) =>
        c x ((unwrap xs : list _) b n c)) : list _;
    foldr xs = (unwrap xs : list _) _;
  } :> LIST_CORE;
  isNil xs = foldr xs true (fun _ _ => false);
  head xs = foldr xs none (fun x _ => some x);
  tail xs =
    foldr xs (nil, none)
      (fun x (acc : (_, _)) => (cons x (acc.1), some (acc.1))) .2;
  caselist xs n c =
    caseopt (head xs) n (fun x => caseopt (tail xs) n (fun xs' => c x xs'));
  length xs = foldr xs 0 (fun _ n => n + 1);
  cat xs1 xs2 = foldr xs1 xs2 cons;
  rev xs = foldr xs nil (fun x xs => cat xs (cons x nil));
  map xs f = foldr xs nil (fun x => cons (f x));
  foldl xs x f = foldr (rev xs) x (fun x y => f y x);
  filter xs f = foldr xs nil (fun x ys => if f x then cons x ys else ys);
  nth xs n =
    foldr xs (length xs - 1, none) (fun x (p : (_, _)) =>
      (p.1 - 1, if p.1 == n then some x else p.2)
    ) .2;
};

include List;


;; Set

type ORD =
{
  type t;
  (<=) : (t, t) -> bool;
};

type SET =
{
  type set;
  type elem;
  type t = set;
  empty : set;
  add : elem -> set -> set;
  mem : elem -> set -> bool;
  card : set -> int;
};

Set (Elem : ORD) :> SET with (elem = Elem.t) =
{
  type elem = Elem.t;
  type set = (int, elem -> bool);
  type t = set;
  empty = (0, fun (x : elem) => false);
  card (s : set) = s._1;
  mem (x : elem) (s : set) = s._2 x;
  add (x : elem) (s : set) =
    if mem x s then s
    else (s._1 + 1, fun (y : elem) => x == y or mem y s) : set;
};


;; Map

type MAP =
{
  type map a;
  type key;
  type t a = map a;
  empty 'a : map a;
  add 'a : key -> a -> map a -> map a;
  lookup 'a : key -> map a -> opt a;
};

Map (Key : ORD) :> MAP with (key = Key.t) =
{
  type key = Key.t;
  type map a = key -> opt a;
  t = map;
  empty x = none;
  lookup x m = m x;
  add x y m z = if x == z then some y else m x;
};

|}

(* Load prelude on initialization if checkbox is checked *)
let () =
  if (Js.to_bool (load_prelude_checkbox##.checked)) then
    run_code ~is_prelude:true prelude_source

(* Handle run button *)
let () =
  run_button##.onclick := Dom_html.handler (fun _ ->
    clear_output ();
    
    (* Get source code from appropriate editor *)
    let elixir_code = Js.to_string (elixir_editor##getValue()) in
    
    (* First translate Elixir to 1ML if there's Elixir code *)
    let ml_code = 
      if elixir_code <> "" then begin
        (* Try to translate Elixir to 1ML *)
        try
          let translated = translate_elixir_to_ml elixir_code in
          ml_editor##setValue(Js.string translated);
          translated
        with e ->
          append_output ("Error translating Elixir: " ^ Printexc.to_string e);
          ""
      end else begin
        (* Directly use 1ML code if no Elixir code or if 1ML was edited directly *)
        Js.to_string (ml_editor##getValue())
      end
    in
    
    (* Process 1ML code if available *)
    if ml_code <> "" then begin
      try
        (* Run the 1ML code using existing run_code function *)
        let should_load_prelude = Js.to_bool (load_prelude_checkbox##.checked) in
        if should_load_prelude then (
          env := Oneml.Env.empty;
          state := Oneml.Lambda.Env.empty;
          run_code ~is_prelude:true prelude_source;
        ) else (
          env := Oneml.Env.empty;
          state := Oneml.Lambda.Env.empty;
        );
        run_code ml_code;
      with
      | Oneml.Source.Error (region, msg) ->
        append_output ("Error at " ^ Oneml.Source.string_of_region region ^ ": " ^ msg)
      | e ->
        append_output ("Error: " ^ Printexc.to_string e)
    end;
    
    Js._false
  )

(* Handle clear button *)
let () =
  clear_button##.onclick := Dom_html.handler (fun _ ->
    elixir_editor##setValue(Js.string "");
    ml_editor##setValue(Js.string "");
    clear_output ();
    Js._false
  )

(* Handle examples *)
let () =
  load_example_button##.onclick := Dom_html.handler (fun _ ->
    elixir_editor##setValue(Js.string "");
    ml_editor##setValue(Js.string example_code);
    clear_output ();
    Js._false
  );
  load_stacknew_example_button##.onclick := Dom_html.handler (fun _ ->
    elixir_editor##setValue(Js.string "");
    ml_editor##setValue(Js.string stacknew_example_code);
    clear_output ();
    Js._false
  );
  load_elixir_example_button##.onclick := Dom_html.handler (fun _ ->
    elixir_editor##setValue(Js.string elixir_example_code);
    ml_editor##setValue(Js.string "");
    clear_output ();
    Js._false
  )

(* Handle edit_ml button *)
let () =
  edit_ml_button##.onclick := Dom_html.handler (fun _ ->
    toggle_ml_editor_editable ();
    Js._false
  )

(* Handle toggle help button *)
let () =
  toggle_help_button##.onclick := Dom_html.handler (fun _ ->
    let classList = syntax_help_div##.classList in
    ignore (classList##remove(Js.string "hidden"));
    Js._false
  )

(* Handle close help button *)
let () =
  close_help_button##.onclick := Dom_html.handler (fun _ ->
    let classList = syntax_help_div##.classList in
    ignore (classList##add(Js.string "hidden"));
    Js._false
  )

(* Add Ctrl+Enter shortcut *)
let _ = Dom_html.addEventListener Dom_html.document 
  (Dom_html.Event.make "keydown")
  (Dom_html.handler (fun e ->
    if e##.ctrlKey = Js._true && e##.keyCode = 13 then begin
      (* Manually trigger the run button's onclick event instead of using click() *)
      let onclick_handler = Js.Unsafe.get run_button "onclick" in
      ignore (Js.Unsafe.fun_call onclick_handler [|Js.Unsafe.inject e|]);
      Js._false
    end else
      Js._true
  ))
  Js._true 