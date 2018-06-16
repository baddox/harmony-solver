type t = (Note.t, int);

let toCents = ((note, octave): t) => {
  let base = note |> Note.toCents;
  let octaveShift = octave * 1200;
  base + octaveShift;
};

let toHertz = ((note, octave): t) => {
  let semitones = Note.toSemitones(note) |> float_of_int;
  let octaveFactor = 2.0 ** float_of_int(octave);
  semitones *. 440.0 *. octaveFactor;
};

let fromString = string => {};