  (* Grid.to_numbers_grid g; *)
  let gw = Grid.of_warped "mappa-nodes.dump" in
  Grid.to_geojson gw "warped.json";
