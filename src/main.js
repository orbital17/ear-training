

const ac = new AudioContext();
let instrument;

Soundfont.instrument(ac, '/assets/acoustic_grand_piano-mp3.js').then(function (i) {
  instrument = i;
})

const playChord = function(arr) {
  if (!instrument) return;
  instrument.stop();
  instrument.schedule(ac.currentTime, arr.map((v) => ({ time: 0, note: v })));
}

const stop = function() {
  if (!instrument) return;
  instrument.stop();
}


