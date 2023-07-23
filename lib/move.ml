type direction = UL | UR | DL | DR

type move =
  | Simple of (int * int * direction)
  | Jump of (int * int * direction list)
