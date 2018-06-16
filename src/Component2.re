/* State declaration */
type state = {value: string};

/* Action declaration */
type action =
  | Change(string);

let getValue = event => ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event),
                        )##value;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,
  initialState: () => {
    value: "cs d a, cs d b a, cs d fs e d cs d, b d fs e d",
  },
  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Change(value) =>
      NoteParser.fromString(value);
      ReasonReact.Update({...state, value});
    },
  render: self =>
    <div>
      <input
        _type="text"
        value=self.state.value
        onChange=(event => self.send(Change(getValue(event))))
      />
      <p> (ReasonReact.string(self.state.value)) </p>
      <ul>
        (
          NoteParser.fromText(self.state.value)
          |> Array.mapi((index, string) =>
               <li key=(index |> string_of_int)>
                 (ReasonReact.string(string))
               </li>
             )
          |> ReasonReact.array
        )
      </ul>
    </div>,
};