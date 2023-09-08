module AD = Owl.Algodiff.D

type t = float * float

val increase : t -> t
val decrease : t -> t
val regularize : ?thresh:float -> AD.t -> AD.t
