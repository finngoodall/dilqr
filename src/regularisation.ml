open Owl

module AD = Algodiff.D

type t = float * float

let increase (delta, mu) =
  let delta = max 2. (2. *. delta) in
  let mu = max 1E-6 (mu *. delta) in
  delta, mu


let decrease (delta, mu) =
  let delta = min (1. /. 2.) (delta /. 2.) in
  let mu =
    let mu = mu *. delta in
    if mu > 1E-6 then mu else 0.
  in
  delta, mu

let min_eig mat =
  mat
  |> AD.unpack_arr
  |> Linalg.D.eigvals
  |> Dense.Matrix.Z.re
  |> Mat.min'

let regularize ?(thresh = 1e-3) mat =
  let n = AD.Mat.row_num mat in
  let w = min_eig mat in
  let out =
    if w < thresh
    then AD.Maths.(mat + ((F thresh + abs (F w)) * (AD.Mat.eye n)))
    else mat
  in
  out