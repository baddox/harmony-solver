let build = (tokens: array(NoteParser.token)) => {
  let lastOctave = ref(0);
  Array.map(token => NoteParser.toNoteWithOctave(token));
};