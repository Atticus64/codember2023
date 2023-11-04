open Map;;

let lower = fun p: string -> String.lowercase_ascii p;;

module StringMap = Map.Make(String);;

let remove_first list =
  match list with
  | [] -> []  (* If the list is empty, return an empty list *)
  | _ :: rest -> rest

let rec last lst =
  match lst with
  | [] -> None  (* The list is empty, there is no last item *)
  | [x] -> Some x  (* There is only one item, and it's the last one *)
  | _ :: rest -> last rest  (* Discard the current item and continue with the rest of the list *)

let rec add_to_back item my_list =
  match my_list with
  | [] -> [item]  (* If the list is empty, create a new list with the item as the only element *)
  | head :: tail -> head :: add_to_back item tail

let first list =
  match list with
  | [] -> None  (* La lista está vacía, no hay primer elemento *)
  | f :: _ -> Some f;;


let rec count_coincidences = fun list map ->
  match list with 
  | [] -> map
  | _ -> 
     let pos_item = first list in
     let f = Option.get pos_item in
        let values = List.filter (fun x -> x = f) list in
        let l = List.length values in
        let map = StringMap.update f (fun _ -> Some l) map in
        let others = List.filter (fun x -> x <> f) list in
        if List.length others > 0 then
          count_coincidences others map
        else 
          map

let rec item_exists item lst =
  match lst with
  | [] -> false  (* The list is empty, the item doesn't exist *)
  | x :: rest -> if x = item then true else item_exists item rest

let rec unique_items = fun list uniques ->
  match list with
  | [] -> uniques
  | _ -> 
    let pos = first list in 
    let list = remove_first list in
    if Option.is_some pos then
      let item = Option.get pos in
      if item_exists item uniques  then
        unique_items list uniques  
      else
        let uniques = add_to_back item uniques in
        if List.length list < 0 then
          uniques
        else
           unique_items list uniques  
     else
       uniques

let split_and_join_with_spaces text =
  let lines = String.split_on_char '\n' text in
  let joined_text = String.concat "" lines in
  joined_text


let parse = fun i map str ->
  let m = StringMap.find i map in
  let str = str ^ i ^ string_of_int m in
  str;;


let decode = fun coded -> 
   let phrase = lower coded in
   let map = StringMap.empty in
   let s = split_and_join_with_spaces phrase in
   let s = s 
     |> String.split_on_char ' '
     |> List.filter (fun s -> s <> "") in
   let items = unique_items s [] in
   let map = count_coincidences s map in 
   let words = List.map (fun i -> 
     let r = parse i map "" in
   r) items in
   String.concat "" words

let coded = "murcielago leon jirafa cebra elefante rinoceronte hipopotamo ardilla mapache zorro lobo oso puma jaguar tigre leopardo gato perro caballo vaca toro cerdo oveja cabra gallina pato ganso pavo paloma halcon aguila buho colibri canario loro tucan pinguino flamenco tigre jaguar leopardo oso lobo zorro mapache ardilla elefante rinoceronte hipopotamo cebra jirafa leon murcielago cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago buho aguila halcon paloma pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago colibri buho aguila halcon paloma pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago tucan loro canario colibri buho aguila halcon paloma pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago flamenco pinguino tucan loro canario colibri buho aguila halcon paloma pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago jaguar oso lobo zorro mapache ardilla cebra elefante rinoceronte hipopotamo leon jirafa murcielago caballo vaca toro cerdo oveja cabra gallina pato ganso pavo paloma halcon aguila buho colibri canario loro tucan pinguino flamenco jaguar oso lobo zorro mapache ardilla cebra elefante rinoceronte hipopotamo leon jirafa murcielago caballo vaca toro cerdo oveja cabra gallina pato ganso pavo paloma halcon aguila buho colibri canario loro tucan pinguino flamenco murcielago leon jirafa cebra elefante rinoceronte hipopotamo ardilla mapache zorro lobo oso puma jaguar tigre leopardo gato perro caballo vaca toro cerdo oveja cabra gallina pato ganso pavo paloma halcon aguila buho colibri canario loro tucan pinguino flamenco oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago cabra oveja cerdo toro vaca caballo perro gato leopardo tigre jaguar oso lobo zorro mapache ardilla cebra elefante rinoceronte hipopotamo jirafa leon murcielago pavo ganso pato gallina cabra oveja cerdo toro vaca caballo perro gato buho aguila halcon paloma colibri canario loro tucan pinguino flamenco jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago cabra oveja cerdo toro vaca caballo perro gato buho aguila halcon paloma colibri canario loro tucan pinguino flamenco jaguar oso lobo zorro mapache ardilla hipopotamo rinoceronte elefante jirafa leon murcielago cabra oveja cerdo toro vaca caballo perro gato buho aguila halcon";;

let dec = decode coded;;
print_endline dec;;