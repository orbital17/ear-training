const ac = new AudioContext();
let timerId;

let instrument = new SplendidGrandPiano(ac);

const playChords = function([chords, interval]) {
    if (!instrument) return;
    ac.resume();
    const play = function(i) {
        if (i >= chords.length) return;
        instrument.stop();
        for (let j = 0; j < chords[i].length; j++) {
            instrument.start({
                note: chords[i][j],
                time: ac.currentTime,
            });
        }
        timerId = setTimeout(() => play(i + 1), 1000 * interval);
    };

    if (timerId) clearTimeout(timerId);
    play(0);
};
