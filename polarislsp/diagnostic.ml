type t = {
  loc : Polaris.Loc.t;
  severity : [ `Error | `Warning | `Information | `Hint ];
  source : string;
  message : string;
}

let loc_to_json loc =
  let open Polaris.Loc in
  if Polaris.Loc.is_internal loc then
    `Assoc
      [
        ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
        ("end", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
      ]
  else
    `Assoc
      [
        ( "start",
          `Assoc
            [
              ("line", `Int (loc.start_line - 1));
              ("character", `Int (loc.start_col - 1));
            ] );
        ( "end",
          `Assoc
            [
              ("line", `Int (loc.end_line - 1));
              ("character", `Int (loc.end_col - 1));
            ] );
      ]

let to_json diagnostic =
  `Assoc
    [
      ("range", loc_to_json diagnostic.loc);
      ( "severity",
        `Int
          (match diagnostic.severity with
          | `Error -> 1
          | `Warning -> 2
          | `Information -> 3
          | `Hint -> 4) );
      ("source", `String diagnostic.source);
      ("message", `String diagnostic.message);
    ]
