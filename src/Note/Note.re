type letter =
  | A
  | B
  | C
  | D
  | E
  | F
  | G;

type accidental =
  | Sharp
  | Flat
  | Natural
  | None;

type t = (letter, accidental);

/* Number of semitones, offset with A = 0. */
let toSemitones = ((letter, accidental): t) => {
  let base =
    switch (letter) {
    | A => 0
    | B => 2
    | C => 3
    | D => 5
    | E => 7
    | F => 8
    | G => 10
    };
  let shift =
    switch (accidental) {
    | Sharp => 1
    | Flat => (-1)
    | Natural => 0
    | None => 0
    };
  base + shift;
};

let toCents = note => (note |> toSemitones) * 100;

/* "cs d a, cs d b a, cs d fs e d cs d, b d fs e d" */
let fromString = string => {};

let letterFromString = string =>
  switch (string) {
  | "a" => Some(A)
  | "b" => Some(B)
  | "c" => Some(C)
  | "d" => Some(D)
  | "e" => Some(E)
  | "f" => Some(F)
  | "g" => Some(G)
  | _ => None
  };

let letterToString = letter =>
  switch (letter) {
  | A => "a"
  | B => "b"
  | C => "c"
  | D => "d"
  | E => "e"
  | F => "f"
  | G => "g"
  };

let accidentalFromString = string =>
  switch (string) {
  | "s"
  | "#" => Sharp
  | "b"
  | "f" => Flat
  | "n" => Natural
  | "" => None
  | _ => None
  };

let accidentalToString = accidental =>
  switch (accidental) {
  | Sharp => "#"
  | Flat => "b"
  | Natural => "n"
  | None => ""
  };