open Js_of_ocaml
(* open Oneml *)

let elixir_to_1ml s = Proto_lib.Formal1to2.read_string s

(* Editor interface *)
class type editor = object
  method getValue : unit -> Js.js_string Js.t Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method getSession : < setMode : Js.js_string Js.t -> unit Js.meth > Js.t Js.meth
  method setTheme : Js.js_string Js.t -> unit Js.meth
  method resize : unit -> unit Js.meth
end

(* Editor setup *)
let setup_editor div mode is_readonly =
  let ace = Js.Unsafe.get Js.Unsafe.global "ace" in
  let ed = (Js.Unsafe.coerce (Js.Unsafe.meth_call ace "edit" [|Js.Unsafe.inject div|]) : editor Js.t) in
  Js.Unsafe.meth_call ed "setTheme" [|Js.Unsafe.inject (Js.string "ace/theme/monokai")|];
  let session = Js.Unsafe.meth_call ed "getSession" [||] in
  Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string ("ace/mode/" ^ mode))|];
  if is_readonly then
    Js.Unsafe.meth_call ed "setReadOnly" [|Js.Unsafe.inject (Js.bool true)|];
  ed

(* Editor content manipulation *)
let get_editor_value editor =
  Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])

let set_editor_value editor value =
  Js.Unsafe.meth_call editor "setValue" [|Js.Unsafe.inject (Js.string value)|]

(* Set editor mode *)
let set_editor_mode editor mode =
  let session = Js.Unsafe.meth_call editor "getSession" [||] in
  Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string ("ace/mode/" ^ mode))|] 

(* HTML content manipulation *)
let set_inner_html elt content =
  Js.Unsafe.set elt "innerHTML" (Js.string content)

let append_html elt content =
  let current = Js.Unsafe.get elt "innerHTML" in
  Js.Unsafe.set elt "innerHTML" (Js.string (Js.to_string current ^ content))

(* Error handling *)
let handle_error f output_div =
  try f ()
  with
  | e -> 
      set_inner_html output_div ("Error: " ^ Printexc.to_string e)

      (* Use Fomega.s *)
let pp_exp fmt exp = 
  Format.fprintf fmt "%s" (Oneml.Fomega.string_of_exp exp)

let pp_extyp fmt exp = 
  Format.fprintf fmt "%s" (Oneml.Types.string_of_extyp exp)

(* Run 1ML code *)
let aux_run_1ml_code ?(is_prelude=false) code env state output_div systemf_editor =
  handle_error (fun () ->
    let lexbuf = Lexing.from_string code in
    lexbuf.Lexing.lex_curr_p <- {
      lexbuf.Lexing.lex_curr_p with 
      Lexing.pos_fname = if is_prelude then "prelude" else "web"
    };
    
    let prog = Oneml.Parser.prog Oneml.Lexer.token lexbuf in
    let sign, _, fprog = Oneml.Elab.elab !env prog in
    let lambda = Oneml.Compile.compile (Oneml.Erase.erase_env !env) fprog in
    let value = Oneml.Lambda.eval !state lambda in
    
    (* Display System F term and type *)
    let buffer = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt "@[<v 0>Term:@.";
    pp_exp fmt fprog;
    Format.fprintf fmt "@.@.Type:@.";
    pp_extyp fmt sign;
    Format.fprintf fmt "@]@.";
    Format.pp_print_flush fmt ();
    
    set_editor_value systemf_editor (Buffer.contents buffer);
    Js.Unsafe.set (Js.Unsafe.get systemf_editor "setReadOnly") 
      systemf_editor [|Js.Unsafe.inject (Js.bool true)|];
    
    (* Update environment for next evaluation *)
    let Oneml.Types.ExT(aks, Oneml.Types.StrT(tr)) = sign in
    env := Oneml.Env.add_row tr (Oneml.Env.add_typs aks !env);
    let ls = List.sort compare (List.map fst tr) in
    let vs = match value with Oneml.Lambda.TupV(vs) -> vs | _ -> assert false in
    state := List.fold_right2 Oneml.Lambda.Env.add ls vs !state;
    
    if is_prelude then 
      append_html output_div "Prelude loaded successfully.\n"
  ) output_div

(* Run Elixir code *)
let aux_run_elixir_code code env state output_div systemf_editor ml_editor =
  handle_error (fun () ->
    (* Translate Elixir code to 1ML *)
    let ml_code = elixir_to_1ml code in
    
    (* Set the 1ML editor with the translated code *)
    set_editor_value ml_editor ml_code;
    
    (* Run the translated 1ML code *)
    aux_run_1ml_code ml_code env state output_div systemf_editor;
    
    (* Show a success message *)
    append_html output_div "Elixir program translated and executed successfully.\n"
  ) output_div

(* Run code based on mode *)
let run_code ?(is_prelude=false) code current_mode env state output_div systemf_editor ml_editor =
  if !current_mode = "elixir" && not is_prelude then
    aux_run_elixir_code code env state output_div systemf_editor ml_editor
  else
    aux_run_1ml_code ~is_prelude code env state output_div systemf_editor 

(* DOM element access utilities *)
let get_element_by_id id =
  let doc = Dom_html.document in
  let element_opt = Js.Unsafe.meth_call doc "getElementById" [|Js.Unsafe.inject (Js.string id)|] in
  Js.Opt.get element_opt
    (fun () -> failwith ("Element with id " ^ id ^ " not found"))

(* Type coercion utilities *)
let coerce_to_checked elt =
  (Js.Unsafe.coerce elt : < checked : bool Js.t Js.prop > Js.t)

let coerce_to_element elt =
  (Js.Unsafe.coerce elt : Dom_html.element Js.t)

let get_parent elt =
  let parent_node = Js.Unsafe.get elt "parentNode" in
  Js.Opt.get parent_node (fun () -> failwith "Parent node not found")

(* DOM manipulation utilities *)
let add_click_handler elt handler =
  Js.Unsafe.set elt "onclick" (Dom_html.handler (fun _ -> 
    handler (); 
    Js._false
  ))

let add_keydown_handler key_code _ctrl handler =
  ignore (Dom_html.addEventListener Dom_html.document 
    Dom_html.Event.keydown
    (Dom_html.handler (fun (e : Dom_html.keyboardEvent Js.t) ->
      let ctrl_key = Js.Unsafe.get e "ctrlKey" in
      let key_code_val = Js.Unsafe.get e "keyCode" in
      if Js.to_bool ctrl_key && key_code_val = key_code then begin
        handler ();
        Js._false
      end else
        Js._true))
    Js._false)

(* Class list manipulation *)
let add_class elt class_name =
  let class_list = Js.Unsafe.get elt "classList" in
  Js.Unsafe.meth_call class_list "add" [|Js.Unsafe.inject (Js.string class_name)|]

let remove_class elt class_name =
  let class_list = Js.Unsafe.get elt "classList" in
  Js.Unsafe.meth_call class_list "remove" [|Js.Unsafe.inject (Js.string class_name)|]

let toggle_class elt class_name =
  let class_list = Js.Unsafe.get elt "classList" in
  let contains = Js.Unsafe.meth_call class_list "contains" [|Js.Unsafe.inject (Js.string class_name)|] in
  if Js.to_bool contains then
    remove_class elt class_name
  else
    add_class elt class_name

(* Get DOM elements *)
let document = Dom_html.document
let editor_div = get_element_by_id "editor"
let output_div = get_element_by_id "output"
let systemf_editor_div = get_element_by_id "systemf_editor"
let ml_editor_div = get_element_by_id "1ml_editor"
let run_button = get_element_by_id "run"
let clear_button = get_element_by_id "clear"
let load_example_button = get_element_by_id "load_example"
let load_stacknew_example_button = get_element_by_id "load_stacknew_example"
let load_elixir_example_button = get_element_by_id "load_elixir_example"
let toggle_help_button = get_element_by_id "toggle_help"
let close_help_button = get_element_by_id "close_help"
let syntax_help_div = get_element_by_id "syntax_help"
let load_prelude_checkbox = coerce_to_checked (get_element_by_id "load_prelude")

(* Mode selectors *)
let mode_1ml_radio = coerce_to_checked (get_element_by_id "mode_1ml")
let mode_elixir_radio = coerce_to_checked (get_element_by_id "mode_elixir")
let mode_1ml_radio_dom = coerce_to_element (get_element_by_id "mode_1ml")
let mode_elixir_radio_dom = coerce_to_element (get_element_by_id "mode_elixir")
let mode_1ml_label = get_parent mode_1ml_radio_dom
let mode_elixir_label = get_parent mode_elixir_radio_dom

(* Tab elements *)
let tab_buttons = Js.Unsafe.meth_call document "getElementsByClassName" [|Js.Unsafe.inject (Js.string "tab-button")|]
let tab_panes = Js.Unsafe.meth_call document "getElementsByClassName" [|Js.Unsafe.inject (Js.string "tab-pane")|]

(* Setup editors *)
let editor = setup_editor editor_div "ocaml" false
let systemf_editor = setup_editor systemf_editor_div "ocaml" false
let ml_editor = setup_editor ml_editor_div "ocaml" true

(* Environment state *)
let env = ref Oneml.Env.empty
let state = ref Oneml.Lambda.Env.empty
let current_mode = ref "1ml"

(* Output functions *)
let append_output str =
  append_html output_div (str ^ "\n")

let clear_output () =
  set_inner_html output_div ""

(* Set up print function for primitives to use *)
(* let () = Oneml.Prim.print := append_output *)

(* Setup tab handlers *)
let setup_tab_handlers tab_buttons tab_panes systemf_editor =
  let len = Js.Unsafe.get tab_buttons "length" in
  for i = 0 to len - 1 do
    let button = Js.Unsafe.meth_call tab_buttons "item" [|Js.Unsafe.inject i|] in
    let dataset = Js.Unsafe.get button "dataset" in
    let tab_id = Js.to_string (Js.Unsafe.get dataset "target") in
    let tab_pane = get_element_by_id tab_id in
    
    add_click_handler button (fun () ->
      (* Hide all panes *)
      let panes_len = Js.Unsafe.get tab_panes "length" in
      for j = 0 to panes_len - 1 do
        let pane = Js.Unsafe.meth_call tab_panes "item" [|Js.Unsafe.inject j|] in
        add_class pane "hidden"
      done;
      
      (* Show selected pane *)
      remove_class tab_pane "hidden";
      
      (* Remove active class from all buttons *)
      for j = 0 to len - 1 do
        let btn = Js.Unsafe.meth_call tab_buttons "item" [|Js.Unsafe.inject j|] in
        remove_class btn "active"
      done;
      
      (* Add active class to clicked button *)
      add_class button "active";
      
      (* Resize editor to fit new container *)
      ignore (Js.Unsafe.meth_call systemf_editor "resize" [||])
    )
  done

(* Setup mode handlers *)
let setup_mode_handlers current_mode editor mode_1ml_label mode_elixir_label =
  add_click_handler mode_1ml_label (fun () ->
    current_mode := "1ml";
    let session = Js.Unsafe.meth_call editor "getSession" [||] in
    Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string "ace/mode/ocaml")|]
  );
  
  add_click_handler mode_elixir_label (fun () ->
    current_mode := "elixir";
    let session = Js.Unsafe.meth_call editor "getSession" [||] in
    Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string "ace/mode/elixir")|]
  )

(* Example code *)
let prelude_source = "(* Minimal prelude *)"  (* Replace with actual prelude code *)
let stack_example = "(* Stack example *)"   (* Replace with actual stack example *)
let stacknew_example = "(* New stack example *)" (* Replace with actual stacknew example *)
let elixir_example = "(* Elixir example *)"  (* Replace with actual elixir example *)

(* Set mode for editor *)
let aux_set_mode mode editor mode_1ml_radio mode_elixir_radio =
  match mode with
  | "1ml" -> 
      Js.Unsafe.set mode_1ml_radio "checked" (Js.bool true);
      set_editor_mode editor "ocaml"
  | "elixir" -> 
      Js.Unsafe.set mode_elixir_radio "checked" (Js.bool true);
      set_editor_mode editor "elixir"
  | _ -> ()

(* Initialize the application *)
let initialize () =
  (* Setup tab handlers *)
  setup_tab_handlers tab_buttons tab_panes systemf_editor;
  
  (* Setup mode handlers *)
  setup_mode_handlers current_mode editor mode_1ml_label mode_elixir_label;
  
  (* Setup button handlers *)
  add_click_handler run_button (fun () ->
    let should_load_prelude = Js.to_bool (Js.Unsafe.get load_prelude_checkbox "checked") in
    if should_load_prelude then (
      env := Oneml.Env.empty;
      state := Oneml.Lambda.Env.empty;
      aux_run_1ml_code ~is_prelude:true prelude_source env state output_div systemf_editor;
    ) else (
      env := Oneml.Env.empty;
        state := Oneml.Lambda.Env.empty;
    );
    run_code (Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])) current_mode env state output_div systemf_editor ml_editor;
  );
  
  add_click_handler clear_button (fun () ->
    clear_output ()
  );
  
  add_click_handler load_example_button (fun () ->
    current_mode := "1ml";
    aux_set_mode "1ml" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor stack_example;
    run_code stack_example current_mode env state output_div systemf_editor ml_editor
  );
  
  add_click_handler load_stacknew_example_button (fun () ->
    current_mode := "1ml";
    aux_set_mode "1ml" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor stacknew_example;
    run_code stacknew_example current_mode env state output_div systemf_editor ml_editor
  );
  
  add_click_handler load_elixir_example_button (fun () ->
    current_mode := "elixir";
    aux_set_mode "elixir" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor elixir_example;
    run_code elixir_example current_mode env state output_div systemf_editor ml_editor
  );
  
  add_click_handler toggle_help_button (fun () ->
    remove_class syntax_help_div "hidden"
  );
  
  add_click_handler close_help_button (fun () ->
    add_class syntax_help_div "hidden"
  );
  
  (* Add Ctrl+Enter shortcut *)
  add_keydown_handler 13 true (fun () ->
    run_code (Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])) current_mode env state output_div systemf_editor ml_editor
  );
  
  (* Load prelude on initialization if checkbox is checked *)
  if (Js.to_bool (Js.Unsafe.get load_prelude_checkbox "checked")) then
    aux_run_1ml_code ~is_prelude:true prelude_source env state output_div systemf_editor

(* Start the application *)
let () = initialize ()

let rec run_elixir_code code =
  try
    (* Translate Elixir code to 1ML *)
    let ml_code = Proto_lib.Formal1to2.read_string code in
    
    (* Set the 1ML editor with the translated code *)
    Js.Unsafe.meth_call ml_editor "setValue" [|Js.Unsafe.inject (Js.string ml_code)|];
    
    (* Run the translated 1ML code *)
    run_1ml_code ml_code;
    
    (* Show a success message *)
    append_output "Elixir program translated and executed successfully.\n"
  with
  | e ->
    append_output ("Error in Elixir translation: " ^ Printexc.to_string e)

and run_1ml_code ?(is_prelude=false) code =
  try
    let lexbuf = Lexing.from_string code in
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
    pp_exp fmt fprog;
    Format.fprintf fmt "@.@.Type:@.";
    pp_extyp fmt sign;
    Format.fprintf fmt "@]@.";
    Format.pp_print_flush fmt ();
    Js.Unsafe.meth_call systemf_editor "setValue" [|Js.Unsafe.inject (Js.string (Buffer.contents buffer))|];
    
    (* Make System F editor read-only *)
    let () = Js.Unsafe.set (Js.Unsafe.get systemf_editor "setReadOnly") systemf_editor [|Js.Unsafe.inject (Js.bool true)|] in
    
    (* Update environment for next evaluation *)
    let Oneml.Types.ExT(aks, Oneml.Types.StrT(tr)) = sign in
    env := Oneml.Env.add_row tr (Oneml.Env.add_typs aks !env);
    let ls = List.sort compare (List.map fst tr) in
    let vs = match value with Oneml.Lambda.TupV(vs) -> vs | _ -> assert false in
    state := List.fold_right2 Oneml.Lambda.Env.add ls vs !state;
    if is_prelude then append_output "Prelude loaded successfully.\n"
  with
  | Oneml.Source.Error (region, msg) ->
    append_output ("Error at " ^ Oneml.Source.string_of_region region ^ ": " ^ msg)
  | e ->
    append_output ("Error: " ^ Printexc.to_string e)

let run_code ?(is_prelude=false) code current_mode env state output_div systemf_editor ml_editor =
  if !current_mode = "elixir" && not is_prelude then
    aux_run_elixir_code code env state output_div systemf_editor ml_editor
  else
    aux_run_1ml_code ~is_prelude code env state output_div systemf_editor

let set_mode mode =
  current_mode := mode;
  match mode with
  | "1ml" -> 
      Js.Unsafe.set mode_1ml_radio "checked" (Js.bool true);
      let session = Js.Unsafe.meth_call editor "getSession" [||] in
      Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string "ace/mode/ocaml")|]
  | "elixir" -> 
      Js.Unsafe.set mode_elixir_radio "checked" (Js.bool true);
      let session = Js.Unsafe.meth_call editor "getSession" [||] in
      Js.Unsafe.meth_call session "setMode" [|Js.Unsafe.inject (Js.string "ace/mode/elixir")|]
  | _ -> ()

let () =
  (* Set up mode selector click handlers on the label elements *)
  let mode_1ml_label_element = (Js.Unsafe.coerce mode_1ml_label : Dom_html.element Js.t) in
  let mode_elixir_label_element = (Js.Unsafe.coerce mode_elixir_label : Dom_html.element Js.t) in
  
  Js.Unsafe.set mode_1ml_label_element "onclick" (Dom_html.handler (fun _ -> 
    current_mode := "1ml"; 
    aux_set_mode "1ml" editor mode_1ml_radio mode_elixir_radio;
    Js._false
  ));
  
  Js.Unsafe.set mode_elixir_label_element "onclick" (Dom_html.handler (fun _ -> 
    current_mode := "elixir"; 
    aux_set_mode "elixir" editor mode_1ml_radio mode_elixir_radio;
    Js._false
  ));

  Js.Unsafe.set run_button "onclick" (Dom_html.handler (fun _ ->
    let should_load_prelude = Js.to_bool (Js.Unsafe.get load_prelude_checkbox "checked") in
    if should_load_prelude then (
      env := Oneml.Env.empty;
      state := Oneml.Lambda.Env.empty;
      aux_run_1ml_code ~is_prelude:true prelude_source env state output_div systemf_editor;
    ) else (
      env := Oneml.Env.empty;
      state := Oneml.Lambda.Env.empty;
    );
    run_code (Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])) current_mode env state output_div systemf_editor ml_editor;
    Js._false
  ));
  Js.Unsafe.set clear_button "onclick" (Dom_html.handler (fun _ ->
    clear_output ();
    Js._false
  ));
  Js.Unsafe.set load_example_button "onclick" (Dom_html.handler (fun _ ->
    current_mode := "1ml";
    aux_set_mode "1ml" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor stack_example;
    run_code stack_example current_mode env state output_div systemf_editor ml_editor;
    Js._false
  ));
  Js.Unsafe.set load_stacknew_example_button "onclick" (Dom_html.handler (fun _ ->
    current_mode := "1ml";
    aux_set_mode "1ml" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor stacknew_example;
    run_code stacknew_example current_mode env state output_div systemf_editor ml_editor;
    Js._false
  ));
  Js.Unsafe.set load_elixir_example_button "onclick" (Dom_html.handler (fun _ ->
    current_mode := "elixir";
    aux_set_mode "elixir" editor mode_1ml_radio mode_elixir_radio;
    set_editor_value editor elixir_example;
    run_code elixir_example current_mode env state output_div systemf_editor ml_editor;
    Js._false
  ));
  Js.Unsafe.set toggle_help_button "onclick" (Dom_html.handler (fun _ ->
    let classList = Js.Unsafe.get syntax_help_div "classList" in
    ignore (Js.Unsafe.meth_call classList "remove" [|Js.Unsafe.inject (Js.string "hidden")|]);
    Js._false
  ));
  Js.Unsafe.set close_help_button "onclick" (Dom_html.handler (fun _ ->
    let classList = Js.Unsafe.get syntax_help_div "classList" in
    ignore (Js.Unsafe.meth_call classList "add" [|Js.Unsafe.inject (Js.string "hidden")|]);
    Js._false
  ));
  (* Add Ctrl+Enter shortcut *)
  let _ = Dom_html.addEventListener Dom_html.document 
    Dom_html.Event.keydown
    (Dom_html.handler (fun (e : Dom_html.keyboardEvent Js.t) ->
      let ctrl_key = Js.Unsafe.get e "ctrlKey" in
      let key_code = Js.Unsafe.get e "keyCode" in
      if Js.to_bool ctrl_key && key_code = 13 then begin
        run_code (Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])) current_mode env state output_div systemf_editor ml_editor;
        Js._false
      end else
        Js._true))
    Js._false in
  () 