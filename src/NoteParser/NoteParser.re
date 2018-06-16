/*
 letter
 s, f, #, b, n, nothing
 +, -, +++, ---
 0, 1, 2, 10, -1, -2, -10
  */
type parsed = {
  letter: option(Note.letter),
  accidental: option(Note.accidental),
  octaveShift: int,
  octave: option(int),
};

let print = t =>
  switch (t) {
  | Some({letter, accidental, octaveShift, octave}) =>
    Js.log2("letter", letter);
    Js.log2("accidental", accidental);
    Js.log2("octaveShift", octaveShift);
    Js.log2("octave", octave);
  | None => Js.log("PARSE ERROR")
  };

let octaveShiftToString = octaveShift =>
  octaveShift > 0 ?
    String.make(octaveShift, '+') :
    octaveShift < 0 ?
      String.make(
        octaveShift |> Int32.of_int |> Int32.abs |> Int32.to_int,
        '-',
      ) :
      "";

let octaveShiftFromString = string =>
  switch (string.[0]) {
  | '+' => String.length(string)
  | '-' => - String.length(string)
  | _ => 0
  };

let regex = Js.Re.fromString({|^([a-g])([s#fbn])?([-\+]*)?([\d]+)?$|});

let fromString = string => {
  let exec = Js.Re.exec(string |> String.lowercase, regex);
  /* if no result throw exception */
  let parsed =
    switch (exec) {
    | Some(result) =>
      let [|_, letter, accidental, octaveShift, octave|] =
        Js.Re.captures(result) |> Array.map(Js.Nullable.toOption);
      let letter =
        switch (letter) {
        | Some(l) => Note.letterFromString(l)
        | None => None
        };
      let accidental =
        switch (accidental) {
        | Some(a) => Note.accidentalFromString(a)
        | None => None /* The regex should never allow this to happen */
        };
      let octaveShift' =
        switch (octaveShift) {
        | Some(o) => octaveShiftFromString(o)
        | None => 0
        };
      let octave' =
        switch (octave) {
        | Some(o) => Some(int_of_string(o))
        | None => None
        };
      let (octaveShift, octave) =
        switch (octaveShift', octave') {
        | ((-1), Some(i)) => (0, Some(- i))
        | _ => (octaveShift', octave')
        };
      Some({letter, accidental, octaveShift, octave});
    | None => None
    };
  parsed;
};

let toString = parsed =>
  switch (parsed) {
  | Some({letter, accidental, octaveShift, octave}) =>
    let letter = Note.letterToString(letter);
    let accidental = Note.accidentalToString(accidental);
    let octaveShift = octaveShiftToString(octaveShift);
    let octave =
      switch (octave) {
      | Some(o) => string_of_int(o)
      | None => ""
      };
    letter ++ accidental ++ octaveShift ++ octave;
  | None => ""
  };