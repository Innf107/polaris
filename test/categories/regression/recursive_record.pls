
let infinite_map(f, seq) = f(seq.head) :: infinite_map(f, seq.tail)
