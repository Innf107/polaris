type t = {
  underlying : Buffer.t;
  size : int;
}

let create initial_capacity =
  { underlying = Buffer.create initial_capacity; size = 0 }

let length buffer = buffer.size
let contents buffer = Buffer.sub buffer.underlying 0 buffer.size

let as_cow_unchecked : ?size_hint:(int -> int) -> (Buffer.t -> unit) -> t -> t =
 fun ?(size_hint = fun x -> x + 4) transformation buffer ->
  if Buffer.length buffer.underlying > buffer.size then begin
    let new_buffer =
      Buffer.create (size_hint (Buffer.length buffer.underlying))
    in
    Buffer.add_string new_buffer (contents buffer);
    transformation new_buffer;
    { underlying = new_buffer; size = Buffer.length new_buffer }
  end
  else begin
    transformation buffer.underlying;
    { buffer with size = Buffer.length buffer.underlying }
  end

let add_char char = as_cow_unchecked (fun buffer -> Buffer.add_char buffer char)

let add_string string =
  as_cow_unchecked (fun buffer -> Buffer.add_string buffer string)

let add_substring string offset length =
  as_cow_unchecked (fun buffer ->
      Buffer.add_substring buffer string offset length)

let add_utf_8_uchar uchar =
  as_cow_unchecked (fun buffer -> Buffer.add_utf_8_uchar buffer uchar)

let nth buffer index =
  if index < length buffer then Buffer.nth buffer.underlying index
  else invalid_arg "Cowbuffer.nth"
