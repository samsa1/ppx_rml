module type S = sig end

[%%rml

[@@@sampling 0.01]

let () =  Random.self_init ()

type coord = { x: float; y: float; }

type number_state =
    { id: int;
      pos: coord;
      speed: coord;
      radius: float;
      color: Graphics.color;
      kill: (number_state, number_state option) event; }

type wall = { left: float; right: float;
	      bot: float; top: float }


(* Question 1 *)

let move state wall =
  let vx =
    if state.pos.x -. state.radius <= wall.left
	or state.pos.x +. state.radius >= wall.right then
      -. state.speed.x
    else
      state.speed.x
  in
  let vy =
    if state.pos.y -. state.radius <= wall.bot
	or state.pos.y +. state.radius >= wall.top then
      -. state.speed.y
    else
      state.speed.y
  in
  { id = state.id;
    pos = { x = state.pos.x +. vx; y = state.pos.y +. vy };
    speed = { x = vx; y = vy };
    radius = state.radius;
    color = state.color;
    kill = state.kill;
  }

let process moving_number (init_state : number_state) (wall : wall) (s : (number_state, 'a) event) : unit process =
  let current_state = ref init_state in
  while true do
    current_state := move !current_state wall;
    emit s !current_state;
    pause;
  done


(* Question 2 *)

let draw_wall wall =
  Graphics.set_color Graphics.black;
  Graphics.draw_rect
    (int_of_float wall.left)
    (int_of_float wall.bot)
    (int_of_float (wall.right -. wall.left))
    (int_of_float (wall.top -. wall.bot))

let draw_number n =
  Graphics.set_color n.color;
  Graphics.fill_circle
    (int_of_float n.pos.x)
    (int_of_float n.pos.y)
    (int_of_float n.radius);
  Graphics.set_color Graphics.black;
  Graphics.moveto
    (int_of_float (n.pos.x -. n.radius /. 2.))
    (int_of_float (n.pos.y -. n.radius /. 2.));
  Graphics.draw_string (string_of_int n.id)

let init_graphics wall =
  Graphics.open_graph (" "^(string_of_int (int_of_float wall.right))^
		       "x"^(string_of_int (int_of_float wall.right)))

let process window (wall : wall) (s : ('a, number_state list) event) : unit process =
  init_graphics wall;
  Graphics.auto_synchronize false;
  while true do
    let%await ALL = l = s in
    Graphics.clear_graph ();
    List.iter draw_number l;
    draw_wall wall;
    Graphics.synchronize ();
    pause;
  done


(* Question 3 *)

let random_pos wall =
  { x = Random.float (wall.right -. wall.left) +. wall.left;
    y = Random.float (wall.top -. wall.bot) +. wall.bot; }

let max_speed = 2.
let random_speed () =
  { x = Random.float (max_speed *. 2.) -. max_speed ;
    y = Random.float (max_speed *. 2.) -. max_speed ; }



let random_number_state n wall =
  let%signal kill_s = { default = None; gather = (fun x y -> Some x)} in
  {
    id = n;
    pos = random_pos wall;
    speed = random_speed ();
    radius = 12.0;
    color = Graphics.cyan;
    kill = kill_s;
  }


(* Question 4 *)

let%signal list_signal = {default = []; gather = (fun x y -> x :: y)};;


(* Question 6 *)

let collision me him =
  let dx = me.pos.x -. him.pos.x in
  let dy = me.pos.y -. him.pos.y in
  if dx *. dx +. dy *. dy <= 12. *. 12. && him.id <> me.id && him.id mod me.id = 0 then
    emit him.kill me
  

let process number (init_state : number_state) (wall : wall) (s : (number_state, 'a) event) : unit process =
  let state = ref init_state in
  let alive = ref true in
  while !alive do
    try%until
      while true do
        state := move !state wall;
        emit s !state;
        let%await All = l = s in
          List.iter (collision !state) l;
        pause;
      done;
    with [%event Some hunter = init_state.kill] -> (* until init_state.kill(Some hunter) -> *)
      begin
          print_int init_state.id;
          print_string " killed by ";
          print_int hunter.id;
          print_newline ();
          alive := false;
        end
  done;
  state := {
    id = !state.id;
    pos = !state.pos;
    speed = !state.speed;
    radius = !state.radius;
    color = Graphics.red;
    kill = !state.kill;
  };
  while !state.radius > 0. do
    state := {
      id = !state.id;
      pos = !state.pos;
      speed = !state.speed;
      radius = !state.radius -. 0.5;
      color = Graphics.red;
      kill = !state.kill;
    };
    emit s !state;
    pause;
  done

(* Question 8 *)

let rec process add new_number wall n s =
  let%await All = coord = new_number in (* (c1, c2) = s1 || (c2, c1) = s2) *)
  run (
    let nn = random_number_state n wall in
    let nn2 = {
      id = n;
      pos = coord;
      speed = nn.speed;
      radius = nn.radius;
      color = nn.color;
      kill = nn.kill;
    } in
    number nn2 wall s
  ) || 
  run (add new_number wall (n + 1) s)



(* Question 9 *)

let process click_of_button_down click =
  while true do
    if Graphics.button_down() then begin
      let x, y = Graphics.mouse_pos() in
      emit click { x = float_of_int x; y = float_of_int y}
    end;
    pause
  done

let process main : unit process =
  let wall = {
    left = 0.;
    bot = 0.;
    top = 512.;
    right = 512.;
  } in
  let%signal click = {default = {x = 0.; y = 0.; }; gather = (fun x y -> x)} in
  (for%par i = 2 to 100 do
      let state = random_number_state i wall in
      run (number state wall list_signal);
  done) || run (window wall list_signal)
  || run (click_of_button_down click)
  || run (add click wall 100 list_signal)

(* Main *)
let () =
  run main
]