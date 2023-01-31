
type t = {
  loc : Polaris.Loc.t;
  severity : [ `Error | `Warning | `Information | `Hint ];
  source : string;
  message : string
}

let to_json diagnostic = 
  `Assoc [
    ("range", `Assoc [
      ("start", `Assoc [
        ("line", `Int (diagnostic.loc.start_line - 1));
        ("character", `Int (diagnostic.loc.start_col - 1))
      ]);
      ("end", `Assoc [
        ("line", `Int (diagnostic.loc.end_line - 1));
        ("character",`Int (diagnostic.loc.end_col - 1))
      ])
    ]);
    ("severity", `Int (match diagnostic.severity with
      | `Error -> 1
      | `Warning -> 2
      | `Information -> 3
      | `Hint -> 4
    ));
    ("source", `String diagnostic.source);
    ("message", `String diagnostic.message)
  ]
