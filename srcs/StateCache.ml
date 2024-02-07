open Types

let create = 
  let new_item = {
    entries = [];
    total_hits = 0;
    hit_limit = 100;
  } in
  new_item

let hash state tape blank = 
  let stage1 = "" in
  let stage2 = stage1 ^ state in 
  let stage3 = stage2 ^ Utils.remove_character blank tape.data in 
  stage3 ^ Printf.sprintf "%d" tape.head

let add_into cache state tape blank =
  let hashed = hash state tape blank in 
  match List.find_index (fun elem -> elem.hash = hashed) cache.entries with
  | Some index ->
    if (List.nth cache.entries index).hits >= cache.hit_limit then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: StateCache: No-Halt detected (hash: %s)\n" hashed; exit (-1) 
    end;
    {
      total_hits = cache.total_hits + 1;
      entries = Utils.replace_lst cache.entries index {(List.nth cache.entries index) with hits = (List.nth cache.entries index).hits + 1};
      hit_limit = cache.hit_limit;
    }
  | None -> 
    {
      total_hits = cache.total_hits + 1;
      entries = cache.entries @ [{
        hash = hashed;
        hits = 1;
        state = state;
        tape = tape;
      }];
      hit_limit = cache.hit_limit;
    }