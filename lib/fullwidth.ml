(* TODO: implementation *)

exception Already_in_full_width
exception Already_in_half_width

module Ascii = struct
  let is_full_width cp = 0xFF00 < cp && cp < 0xFF5F
  let is_half_width cp = 0x0020 < cp && cp < 0x007F

  let to_full_width cp =
    if is_full_width cp then raise Already_in_full_width;
    cp + 0xFEE0

  let to_half_width cp =
    if is_half_width cp then raise Already_in_half_width;
    cp - 0xFEE0
end

module Bracket = struct
  let to_full_width cp =
    if cp = 0x2985 || cp = 0x2E28 then
      0xFF5F else
    if cp = 0x2986 then
      0xFF60
    else invalid_arg "Not half width bracket"
end

