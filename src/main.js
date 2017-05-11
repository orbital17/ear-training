

const ac = new AudioContext();
let instrument;
let timerId;

Soundfont.instrument(ac, '/assets/acoustic_grand_piano-ogg.js').then(function (i) {
  instrument = i;
})

const chordToNotes = function(chord, time) {
  return chord.map((note) => ({time, note}));
}

const playChords = function([chords, interval]) {
  if (!instrument) return;
  const notes = chords.map((c) => chordToNotes(c, 0));

  const play = function(i) {
    if (i >= notes.length) return;
    instrument.stop();
    instrument.schedule(ac.currentTime, notes[i]);
    timerId = setTimeout(() => play(i + 1), 1000 * interval);
  }

  if (timerId) clearTimeout(timerId);
  play(0);
}
