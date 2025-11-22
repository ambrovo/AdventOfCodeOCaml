open Solvers.Signature
open Utils.Module_utils

(* Spell definitions *)
type spell_name = MagicMissile | Drain | Shield | Poison | Recharge

type spell = {
  name: spell_name;
  cost: int;
  damage: int;
  heal: int;
  armor: int;
  effect_turns: int;
  mana_gain: int;
  dot_damage: int;
}

let spells = [
  { name = MagicMissile; cost = 53;  damage = 4; heal = 0; armor = 0; effect_turns = 0; mana_gain = 0; dot_damage = 0 };
  { name = Drain;        cost = 73;  damage = 2; heal = 2; armor = 0; effect_turns = 0; mana_gain = 0; dot_damage = 0 };
  { name = Shield;       cost = 113; damage = 0; heal = 0; armor = 7; effect_turns = 6; mana_gain = 0; dot_damage = 0 };
  { name = Poison;       cost = 173; damage = 0; heal = 0; armor = 0; effect_turns = 6; mana_gain = 0; dot_damage = 3 };
  { name = Recharge;     cost = 229; damage = 0; heal = 0; armor = 0; effect_turns = 5; mana_gain = 101; dot_damage = 0 };
]

(* Game state with active effects *)
type effects = {
  shield_timer: int;
  poison_timer: int;
  recharge_timer: int;
}

type game_state = {
  player_hp: int;
  player_mana: int;
  boss_hp: int;
  boss_damage: int;
  effects: effects;
  mana_spent: int;
}

module type STATE = sig
  type t = game_state
  val create : int -> int -> int -> int -> t
  val apply_effects : t -> t
  val cast_spell : t -> spell -> t option
  val boss_attack : t -> t
  val is_player_alive : t -> bool
  val is_boss_alive : t -> bool
end

(* applying rounds of player and boss always in a pair*)
module State : STATE = struct
  type t = game_state
  
  let create player_hp player_mana boss_hp boss_dmg = {
    player_hp;
    player_mana;
    boss_hp;
    boss_damage = boss_dmg;
    effects = { shield_timer = 0; poison_timer = 0; recharge_timer = 0 };
    mana_spent = 0;
  }
  
  let apply_effects state =
    let eff = state.effects in
    let new_mana = if eff.recharge_timer > 0 then state.player_mana + 101 else state.player_mana in
    let new_boss_hp = if eff.poison_timer > 0 then state.boss_hp - 3 else state.boss_hp in
    let new_effects = {
      shield_timer = max 0 (eff.shield_timer - 1);
      poison_timer = max 0 (eff.poison_timer - 1);
      recharge_timer = max 0 (eff.recharge_timer - 1);
    } in
    { state with player_mana = new_mana; boss_hp = new_boss_hp; effects = new_effects }
  
  let can_cast state spell =
    state.player_mana >= spell.cost &&
    match spell.name with
    | Shield -> state.effects.shield_timer = 0
    | Poison -> state.effects.poison_timer = 0
    | Recharge -> state.effects.recharge_timer = 0
    | _ -> true
  
  let cast_spell state spell =
    if not (can_cast state spell) then None
    else
      let new_mana = state.player_mana - spell.cost in
      let new_spent = state.mana_spent + spell.cost in
      let new_hp = state.player_hp + spell.heal in
      let new_boss_hp = state.boss_hp - spell.damage in
      let new_effects = 
        match spell.name with
        | Shield -> { state.effects with shield_timer = spell.effect_turns }
        | Poison -> { state.effects with poison_timer = spell.effect_turns }
        | Recharge -> { state.effects with recharge_timer = spell.effect_turns }
        | _ -> state.effects
      in
      Some { state with 
        player_hp = new_hp;
        player_mana = new_mana;
        boss_hp = new_boss_hp;
        effects = new_effects;
        mana_spent = new_spent;
      }
  
  let boss_attack state =
    let armor = if state.effects.shield_timer > 0 then 7 else 0 in
    let damage = max 1 (state.boss_damage - armor) in
    { state with player_hp = state.player_hp - damage}
  
  let is_player_alive state = state.player_hp > 0
  let is_boss_alive state = state.boss_hp > 0
end


let run_simulations boss_hp boss_dmg hard_mode =
  let initial = State.create 50 500 boss_hp boss_dmg in
  let pq_0 = PriorityQ.empty in
  let pq_0 = PriorityQ.insert pq_0 0 initial in
  let visited = Hashtbl.create 10000 in

  let state_key state =
    (state.player_hp, state.player_mana, state.boss_hp,
     state.effects.shield_timer, state.effects.poison_timer,
     state.effects.recharge_timer)
  in

  let rec do_round pq =
    match PriorityQ.extract_min pq with
    | None -> None
    | Some ((_, state), pq') ->
      let key = state_key state in
      if Hashtbl.mem visited key then
        do_round pq'
      else begin
        Hashtbl.add visited key ();
        if not (State.is_boss_alive state) then Some state.mana_spent
        else
          (* Apply hard mode damage *)
          let state = if hard_mode then { state with player_hp = state.player_hp - 1 } else state in
          if not (State.is_player_alive state) then do_round pq'
          else
            (* Apply effects at start of player turn *)
            let state = State.apply_effects state in
            if not (State.is_boss_alive state) then Some state.mana_spent
            else
              (* Try each spell, then simulate full boss turn *)
              let new_pq = List.fold_left (fun pq_1 sp ->
                match State.cast_spell state sp with
                | None -> pq_1
                | Some state_after_cast ->
                    (* Check if boss died from spell damage *)
                    if not (State.is_boss_alive state_after_cast) then
                      PriorityQ.insert pq_1 state_after_cast.mana_spent state_after_cast
                    else
                      (* Boss turn: apply effects *)
                      let state_boss_turn = State.apply_effects state_after_cast in
                      if not (State.is_boss_alive state_boss_turn) then
                        (* Boss died from poison *)
                        PriorityQ.insert pq_1 state_boss_turn.mana_spent state_boss_turn
                      else
                        (* Boss attacks *)
                        let state_after_attack = State.boss_attack state_boss_turn in
                        if State.is_player_alive state_after_attack then
                          PriorityQ.insert pq_1 state_after_attack.mana_spent state_after_attack
                        else
                          pq_1  (* Player died, discard this branch *)
              ) pq' spells in
              do_round new_pq
      end
  in

  do_round pq_0



let parse_boss lines =
  let parse_single_line line =
    let parts = String.split_on_char ':' line in
    match parts with
    | [_; value] -> int_of_string (String.trim value)
    | _ -> 0
  in
  match lines with
  | hp_line :: dmg_line :: _ ->
      let hp = parse_single_line hp_line in
      let dmg = parse_single_line dmg_line in
      (hp, dmg)
  | _ -> (0, 0)

module Solver : Solver = struct

  let part1 lines =
    let boss_hp, boss_dmg = parse_boss lines in
    match run_simulations boss_hp boss_dmg false with
    | Some mana -> string_of_int mana
    | None -> "No solution"
  
  let part2 lines =
    let boss_hp, boss_dmg = parse_boss lines in
    match run_simulations boss_hp boss_dmg true with
    | Some mana -> string_of_int mana
    | None -> "No solution"

end