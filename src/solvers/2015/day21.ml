open Solvers.Signature

(* Shop items *)
type item = {
  name: string;
  cost: int;
  damage: int;
  armor: int;
}

let weapons = [
  { name = "Dagger";     cost = 8;   damage = 4; armor = 0 };
  { name = "Shortsword"; cost = 10;  damage = 5; armor = 0 };
  { name = "Warhammer";  cost = 25;  damage = 6; armor = 0 };
  { name = "Longsword";  cost = 40;  damage = 7; armor = 0 };
  { name = "Greataxe";   cost = 74;  damage = 8; armor = 0 };
]

let armors = [
  { name = "None";       cost = 0;   damage = 0; armor = 0 };
  { name = "Leather";    cost = 13;  damage = 0; armor = 1 };
  { name = "Chainmail";  cost = 31;  damage = 0; armor = 2 };
  { name = "Splintmail"; cost = 53;  damage = 0; armor = 3 };
  { name = "Bandedmail"; cost = 75;  damage = 0; armor = 4 };
  { name = "Platemail";  cost = 102; damage = 0; armor = 5 };
]

let rings = [
  { name = "None1";      cost = 0;   damage = 0; armor = 0 };
  { name = "None2";      cost = 0;   damage = 0; armor = 0 };
  { name = "Damage +1";  cost = 25;  damage = 1; armor = 0 };
  { name = "Damage +2";  cost = 50;  damage = 2; armor = 0 };
  { name = "Damage +3";  cost = 100; damage = 3; armor = 0 };
  { name = "Defense +1"; cost = 20;  damage = 0; armor = 1 };
  { name = "Defense +2"; cost = 40;  damage = 0; armor = 2 };
  { name = "Defense +3"; cost = 80;  damage = 0; armor = 3 };
]

(* Warrior module *)
module type WARRIOR = sig
  type t
  val create : int -> int -> int -> t
  val hit_points : t -> int
  val damage : t -> int
  val armor : t -> int
  val take_damage : t -> int -> t
  val is_alive : t -> bool
end

module Warrior : WARRIOR = struct
  type t = {
    hp: int;
    damage: int;
    armor: int;
  }
  
  let create hp dmg arm = { hp; damage = dmg; armor = arm }
  let hit_points w = w.hp
  let damage w = w.damage
  let armor w = w.armor
  let take_damage w dmg = { w with hp = w.hp - dmg }
  let is_alive w = w.hp > 0
end

(* Battleground module *)
module type BATTLEGROUND = sig
  val simulate : Warrior.t -> Warrior.t -> bool
end

module Battleground : BATTLEGROUND = struct
  let simulate player boss =
    let rec fight p b player_turn =
      if not (Warrior.is_alive p) then false
      else if not (Warrior.is_alive b) then true
      else
        if player_turn then
          let damage_dealt = max 1 (Warrior.damage p - Warrior.armor b) in
          fight p (Warrior.take_damage b damage_dealt) false
        else
          let damage_dealt = max 1 (Warrior.damage b - Warrior.armor p) in
          fight (Warrior.take_damage p damage_dealt) b true
    in
    fight player boss true
end

(* Generate all equipment combinations *)
let all_loadouts () =
  List.concat_map (fun weapon ->
    List.concat_map (fun armor ->
      List.mapi (fun i ring1 ->
        List.mapi (fun j ring2 ->
          if i < j then
            Some (weapon, armor, [ring1; ring2])
          else None
        ) rings |> List.filter_map (fun x -> x)
      ) rings |> List.flatten
    ) armors
  ) weapons

let calculate_stats weapon armor rings_list =
  let total_cost = weapon.cost + armor.cost + List.fold_left (fun acc r -> acc + r.cost) 0 rings_list in
  let total_damage = weapon.damage + armor.damage + List.fold_left (fun acc r -> acc + r.damage) 0 rings_list in
  let total_armor = weapon.armor + armor.armor + List.fold_left (fun acc r -> acc + r.armor) 0 rings_list in
  (total_cost, total_damage, total_armor)

let parse_boss lines =
  let parse_single_line line =
    let parts = String.split_on_char ':' line in
    match parts with
    | [_; value] -> int_of_string (String.trim value)
    | _ -> failwith "Invalid boss line format"
  in
  match lines with
  | hp_line :: dmg_line :: arm_line :: _ ->
      let hp = parse_single_line hp_line in
      let dmg = parse_single_line dmg_line in
      let arm = parse_single_line arm_line in
      hp, dmg, arm
  | _ -> failwith "Insufficient boss data"

module Solver : Solver = struct

  let part1 lines =
    let boss_hp, boss_dmg, boss_arm = parse_boss lines in
    let boss = Warrior.create boss_hp boss_dmg boss_arm in
    
    let loadouts = all_loadouts () in
    (* just generate all combinations are filter the winning ones*)
    let winning_costs = List.filter_map (fun (weapon, armor, rings_list) ->
      let cost, dmg, arm = calculate_stats weapon armor rings_list in
      let player = Warrior.create 100 dmg arm in
      if Battleground.simulate player boss then Some cost else None
    ) loadouts in
    
    List.fold_left min max_int winning_costs |> string_of_int
  
  let part2 lines =
    let boss_hp, boss_dmg, boss_arm = parse_boss lines in
    let boss = Warrior.create boss_hp boss_dmg boss_arm in
    
    let loadouts = all_loadouts () in
    let losing_costs = List.filter_map (fun (weapon, armor, rings_list) ->
      let cost, dmg, arm = calculate_stats weapon armor rings_list in
      let player = Warrior.create 100 dmg arm in
      if not (Battleground.simulate player boss) then Some cost else None
    ) loadouts in
    
    List.fold_left max 0 losing_costs |> string_of_int

end