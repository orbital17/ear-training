

const ac = new AudioContext();
let instrument;

Soundfont.instrument(ac, '/assets/acoustic_grand_piano-mp3.js').then(function (i) {
  instrument = i;
})

const playChord = function(arr) {
	instrument.schedule(ac.currentTime, arr.map((v) => ({ time: 0, note: v })));
}

const stop = function() {
	instrument.stop();
}


