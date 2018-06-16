type letter =
  | A
  | B
  | C
  | D
  | E
  | F
  | G;

type accidental =
  | Natural
  | Sharp
  | Flat;

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
    | Natural => 0
    | Sharp => 1
    | Flat => (-1)
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
  | Some(A) => "a"
  | Some(B) => "b"
  | Some(C) => "c"
  | Some(D) => "d"
  | Some(E) => "e"
  | Some(F) => "f"
  | Some(G) => "g"
  | None => ""
  };

let accidentalFromString = string =>
  switch (string) {
  | ""
  | "n" => Some(Natural)
  | "s"
  | "#" => Some(Sharp)
  | "b"
  | "f" => Some(Flat)
  | _ => None
  };

let accidentalToString = accidental =>
  switch (accidental) {
  | Some(Natural) => ""
  | Some(Sharp) => "#"
  | Some(Flat) => "b"
  | None => ""
  };