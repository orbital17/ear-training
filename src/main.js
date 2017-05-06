

const ac = new AudioContext();
let instrument;

Soundfont.instrument(ac, '/assets/acoustic_grand_piano-mp3.js').then(function (i) {
  instrument = i;
})

const chordToNotes = function(chord, time) {
  return chord.map((note) => ({time, note}));
}

const playChords = function([chords, interval]) {
  if (!instrument) return;
  const join = (arr) => arr.reduce((acc, current) => acc.concat(current), []);
  const notes = join(chords.map((c, i) => chordToNotes(c, interval * i)));
  instrument.stop();
  instrument.schedule(ac.currentTime, notes);
}
