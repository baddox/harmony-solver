open Jest;

open NoteParser;

let cases = [
  /* Invalid inputs */
  ("", ""),
  ("  ", ""),
  (" a ", ""),
  ("h", ""),
  ("abc", ""),
  ("#", ""),
  /* naturals */
  ("a", "a"),
  ("an", "a"),
  /* sharps */
  ("as", "a#"),
  ("a#", "a#"),
  /* flats */
  ("ab", "ab"),
  ("af", "ab"),
  /* octave shifts */
  ("a+", "a+"),
  ("a++", "a++"),
  ("a+++", "a+++"),
  ("a-", "a-"),
  ("a--", "a--"),
  ("a---", "a---"),
  /* octaves */
  ("a0", "a0"),
  ("a1", "a1"),
  ("a-1", "a-1"),
  ("a10", "a10"),
  /* everything together */
  ("as+", "a#+"),
  ("as-", "a#-"),
  ("af+++10", "ab+++10"),
];

let foo =
  describe(
    "NoteParser",
    ExpectJs.(
      () =>
        cases
        |> List.iter(((input, expected)) => {
             test(input, () =>
               expect(input |> fromString |> toString) |> toEqual(expected)
             );
             test(input |> Js.String.toUpperCase, () =>
               expect(
                 input |> Js.String.toUpperCase |> fromString |> toString,
               )
               |> toEqual(expected)
             );
           })
    ),
  );