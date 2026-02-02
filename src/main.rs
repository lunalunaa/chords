use std::collections::HashSet;
use std::fmt::Display;
use std::ops::Deref;

use anyhow::Context;
use anyhow::{Ok, Result};
use dioxus::prelude::*;
use rand::prelude::*;
use rust_music_theory::{
    chord::{self, Chord, Number::*, Quality::*},
    note::{Notes, Pitch},
    scale::{Direction, Scale, ScaleType},
};
use serde::{Deserialize, Serialize};
use strum::{EnumIter, IntoEnumIterator};
use wasm_bindgen::prelude::*;
use web_sys::console;

static CSS: Asset = asset!("assets/main.css");

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumIter)]
enum ChordQuality {
    Major,
    Minor,
    Diminished,
    Augmented,
    HalfDiminished,
    Dominant,
    Suspended2,
    Suspended4,
}

impl ChordQuality {
    fn to_library_quality(self) -> chord::Quality {
        match self {
            ChordQuality::Major => chord::Quality::Major,
            ChordQuality::Minor => chord::Quality::Minor,
            ChordQuality::Diminished => chord::Quality::Diminished,
            ChordQuality::Augmented => chord::Quality::Augmented,
            ChordQuality::HalfDiminished => chord::Quality::HalfDiminished,
            ChordQuality::Dominant => chord::Quality::Dominant,
            ChordQuality::Suspended2 => chord::Quality::Suspended2,
            ChordQuality::Suspended4 => chord::Quality::Suspended4,
        }
    }

    fn display_name(self) -> &'static str {
        match self {
            ChordQuality::Major => "Major",
            ChordQuality::Minor => "Minor",
            ChordQuality::Diminished => "Dim",
            ChordQuality::Augmented => "Aug",
            ChordQuality::HalfDiminished => "Half-Dim",
            ChordQuality::Dominant => "Dom",
            ChordQuality::Suspended2 => "Sus2",
            ChordQuality::Suspended4 => "Sus4",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumIter)]
enum ChordNumber {
    Triad,
    Seventh,
    Ninth,
    Eleventh,
    Thirteenth,
}

impl ChordNumber {
    fn to_library_number(self, quality: chord::Quality) -> chord::Number {
        match self {
            ChordNumber::Triad => chord::Number::Triad,
            ChordNumber::Seventh => {
                // Major quality + 7th = Major Seventh
                if quality == chord::Quality::Major {
                    chord::Number::MajorSeventh
                } else {
                    chord::Number::Seventh
                }
            }
            ChordNumber::Ninth => chord::Number::Ninth,
            ChordNumber::Eleventh => chord::Number::Eleventh,
            ChordNumber::Thirteenth => chord::Number::Thirteenth,
        }
    }

    fn display_name(self) -> &'static str {
        match self {
            ChordNumber::Triad => "Triad",
            ChordNumber::Seventh => "7th",
            ChordNumber::Ninth => "9th",
            ChordNumber::Eleventh => "11th",
            ChordNumber::Thirteenth => "13th",
        }
    }
}

#[derive(Clone, Debug)]
struct ChordWrapper(pub Chord);

impl Deref for ChordWrapper {
    type Target = Chord;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ChordWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Chord {
            root,
            octave: _,
            intervals: _,
            quality,
            number,
            inversion,
        } = self.0;

        let slash = if inversion != 0 {
            let bass = self.notes().first().unwrap().clone();
            Some(bass)
        } else {
            None
        };

        let quality_str = match quality {
            Major if matches!(number, Triad | Seventh | MajorSeventh) => "",
            Major => "Δ",
            Minor => "-",
            Diminished => "o",
            Augmented => "+",
            HalfDiminished => "ø",
            Dominant => "",
            Suspended2 => "sus2",
            Suspended4 => "sus4",
        };

        let number_str = match number {
            Triad => "",
            Seventh => "7",
            MajorSeventh => "Δ",
            Ninth => "9",
            Eleventh => "11",
            Thirteenth => "13",
        };

        if let Some(bass) = slash {
            write!(f, "{root}{quality_str}{number_str}/{bass}")
        } else {
            write!(f, "{root}{quality_str}{number_str}")
        }
    }
}

fn generate_next_chord(
    s: &Scale,
    qualities: &[chord::Quality],
    numbers: &[ChordNumber],
    inversion: bool,
) -> Result<ChordWrapper> {
    let mut rng = rand::rng();
    let default_numbers: Vec<ChordNumber> = ChordNumber::iter().collect();
    let default_qualities: Vec<chord::Quality> = ChordQuality::iter()
        .map(|q| q.to_library_quality())
        .collect();

    let numbers = if numbers.is_empty() {
        &default_numbers
    } else {
        numbers
    };
    let qualities = if qualities.is_empty() {
        &default_qualities
    } else {
        qualities
    };

    loop {
        let root = s.notes().choose(&mut rng).context("Empty Scale!")?.pitch;
        let quality = *(qualities.choose(&mut rng).context("Empty Chord Quality!"))?;
        let number = numbers.choose(&mut rng).context("Empty Chord Number!")?;
        let library_number = number.to_library_number(quality);
        let inversion_range: u8 = if inversion {
            match library_number {
                chord::Number::Triad => 2,
                chord::Number::Seventh => 3,
                chord::Number::MajorSeventh => 4,
                chord::Number::Ninth => 5,
                chord::Number::Eleventh => 6,
                chord::Number::Thirteenth => 7,
            }
        } else {
            0
        };
        let inversion_number = rand::random_range(0..=inversion_range);
        let chord = Chord::with_inversion(root, quality, library_number, inversion_number);

        if chord.notes().len() == 3 && chord.number != Triad {
            continue; // if it's an impossible chord then generate again
        }

        return Ok(ChordWrapper(chord));
    }
}

#[derive(Clone, Debug)]
struct ChordSettings {
    qualities: HashSet<ChordQuality>,
    numbers: HashSet<ChordNumber>,
    inversions: bool,
}

impl Default for ChordSettings {
    fn default() -> Self {
        Self {
            qualities: ChordQuality::iter().collect(),
            numbers: ChordNumber::iter().collect(),
            inversions: true,
        }
    }
}

fn next_chord_with_settings(settings: &ChordSettings) -> ChordWrapper {
    let scale = Scale::new(
        ScaleType::Chromatic,
        Pitch::from_u8(0),
        4,
        None,
        Direction::Ascending,
    )
    .unwrap();

    let qualities: Vec<chord::Quality> = settings
        .qualities
        .iter()
        .map(|q| q.to_library_quality())
        .collect();
    let numbers: Vec<ChordNumber> = settings.numbers.iter().copied().collect();

    generate_next_chord(&scale, &qualities, &numbers, settings.inversions).unwrap()
}

static SETTINGS: GlobalSignal<ChordSettings> = Signal::global(ChordSettings::default);
static CHORD: GlobalSignal<Option<ChordWrapper>> = Signal::global(|| None);
static SETTINGS_OPEN: GlobalSignal<bool> = Signal::global(|| false);
static LISTENING: GlobalSignal<bool> = Signal::global(|| false);
static DETECTED_NOTES: GlobalSignal<Vec<u8>> = Signal::global(Vec::new);
static RECOGNITION_FEEDBACK: GlobalSignal<Option<String>> = Signal::global(|| None);
static KEYBOARD_VISIBLE: GlobalSignal<bool> = Signal::global(|| true);

fn generate_chord() {
    let settings = SETTINGS.read();
    *CHORD.write() = Some(next_chord_with_settings(&settings));
}

#[wasm_bindgen(inline_js = r#"
export function playChord(frequencies, duration) {
    const audioContext = new (window.AudioContext || window.webkitAudioContext)();
    const now = audioContext.currentTime;

    frequencies.forEach(freq => {
        // Create a more piano-like sound with multiple harmonics
        const fundamental = audioContext.createOscillator();
        const second = audioContext.createOscillator();
        const third = audioContext.createOscillator();

        const fundamentalGain = audioContext.createGain();
        const secondGain = audioContext.createGain();
        const thirdGain = audioContext.createGain();
        const masterGain = audioContext.createGain();

        // Connect oscillators to their gain nodes
        fundamental.connect(fundamentalGain);
        second.connect(secondGain);
        third.connect(thirdGain);

        fundamentalGain.connect(masterGain);
        secondGain.connect(masterGain);
        thirdGain.connect(masterGain);
        masterGain.connect(audioContext.destination);

        // Set frequencies for harmonics
        fundamental.frequency.value = freq;
        second.frequency.value = freq * 2;
        third.frequency.value = freq * 3;

        // Use triangle wave for warmer tone
        fundamental.type = 'triangle';
        second.type = 'triangle';
        third.type = 'sine';

        // Balance the harmonics
        fundamentalGain.gain.value = 1.0;
        secondGain.gain.value = 0.3;
        thirdGain.gain.value = 0.15;

        // Piano-like envelope: fast attack, exponential decay
        masterGain.gain.setValueAtTime(0, now);
        masterGain.gain.linearRampToValueAtTime(0.15, now + 0.005); // Very fast attack
        masterGain.gain.exponentialRampToValueAtTime(0.08, now + 0.2); // Quick decay
        masterGain.gain.exponentialRampToValueAtTime(0.01, now + duration);

        fundamental.start(now);
        second.start(now);
        third.start(now);

        fundamental.stop(now + duration);
        second.stop(now + duration);
        third.stop(now + duration);
    });
}
"#)]
extern "C" {
    fn playChord(frequencies: Vec<f64>, duration: f64);
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct RecognitionResult {
    notes: Vec<u8>,
    confidence: f64,
}

#[wasm_bindgen(inline_js = r#"
    // Store the stop function and previous notes globally
    window.audioStopFunction = null;
    window.previousNotes = [];
    window.noiseFloor = 100;

    export function startListening(callback) {
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
            console.error("Media devices not supported");
            return null;
        }

        const audioContext = new (window.AudioContext || window.webkitAudioContext)();
        let analyser = null;
        let timeDataArray = null;
        let animationId = null;
        let mediaStream = null;

        navigator.mediaDevices.getUserMedia({ audio: true })
            .then(stream => {
                mediaStream = stream;
                const source = audioContext.createMediaStreamSource(stream);
                analyser = audioContext.createAnalyser();
                analyser.fftSize = 8192; // Higher resolution for better accuracy
                analyser.smoothingTimeConstant = 0.85;
                source.connect(analyser);

                const bufferLength = analyser.frequencyBinCount;
                timeDataArray = new Float32Array(bufferLength);

                // Adaptive noise floor calculation
                function updateNoiseFloor(freqData) {
                    const sum = freqData.reduce((acc, val) => acc + val, 0);
                    const avg = sum / freqData.length;
                    window.noiseFloor = window.noiseFloor * 0.95 + avg * 0.05;
                }

                // Autocorrelation for fundamental frequency detection
                function autoCorrelate(buffer, sampleRate) {
                    const minFreq = 50;
                    const maxFreq = 2000;
                    const minSamples = Math.floor(sampleRate / maxFreq);
                    const maxSamples = Math.ceil(sampleRate / minFreq);

                    let bestOffset = -1;
                    let bestCorrelation = 0;
                    let foundGoodCorrelation = false;

                    // Calculate RMS to check if signal is strong enough
                    let rms = 0;
                    for (let i = 0; i < buffer.length; i++) {
                        rms += buffer[i] * buffer[i];
                    }
                    rms = Math.sqrt(rms / buffer.length);

                    if (rms < 0.01) return -1; // Signal too weak

                    // Normalize buffer
                    const normalized = new Float32Array(buffer.length);
                    for (let i = 0; i < buffer.length; i++) {
                        normalized[i] = buffer[i] / rms;
                    }

                    // Autocorrelation
                    for (let offset = minSamples; offset < maxSamples && offset < buffer.length / 2; offset++) {
                        let correlation = 0;
                        for (let i = 0; i < buffer.length - offset; i++) {
                            correlation += Math.abs(normalized[i] - normalized[i + offset]);
                        }
                        correlation = 1 - correlation / (buffer.length - offset);

                        if (correlation > 0.9 && correlation > bestCorrelation) {
                            bestCorrelation = correlation;
                            bestOffset = offset;
                            foundGoodCorrelation = true;
                        }
                    }

                    if (foundGoodCorrelation && bestOffset > 0) {
                        return sampleRate / bestOffset;
                    }
                    return -1;
                }

                // Harmonic Product Spectrum for better fundamental detection
                function harmonicProductSpectrum(freqData, sampleRate, fftSize) {
                    const hpsData = new Float32Array(freqData.length);
                    const numHarmonics = 5;

                    for (let i = 0; i < freqData.length; i++) {
                        hpsData[i] = freqData[i];
                    }

                    // Multiply by downsampled versions (harmonics)
                    for (let h = 2; h <= numHarmonics; h++) {
                        for (let i = 0; i < Math.floor(freqData.length / h); i++) {
                            hpsData[i] *= freqData[i * h];
                        }
                    }

                    return hpsData;
                }

                // Check if freq2 is a harmonic of freq1
                function isHarmonic(freq1, freq2, tolerance = 0.05) {
                    if (freq2 <= freq1) return false;

                    const ratio = freq2 / freq1;
                    const nearestHarmonic = Math.round(ratio);

                    // Check if close to integer harmonic (2x, 3x, 4x, etc.)
                    if (nearestHarmonic >= 2 && nearestHarmonic <= 8) {
                        const expectedFreq = freq1 * nearestHarmonic;
                        const error = Math.abs(freq2 - expectedFreq) / expectedFreq;
                        return error < tolerance;
                    }
                    return false;
                }

                // Filter out harmonics from peaks
                function filterHarmonics(peaks) {
                    if (peaks.length === 0) return [];

                    // Sort by magnitude (strongest first)
                    const sorted = [...peaks].sort((a, b) => b.magnitude - a.magnitude);
                    const fundamentals = [];

                    for (let i = 0; i < sorted.length; i++) {
                        const current = sorted[i];
                        let isHarmonicOfExisting = false;

                        // Check if this peak is a harmonic of any already-accepted fundamental
                        for (let j = 0; j < fundamentals.length; j++) {
                            if (isHarmonic(fundamentals[j].freq, current.freq)) {
                                isHarmonicOfExisting = true;
                                break;
                            }
                        }

                        if (!isHarmonicOfExisting) {
                            fundamentals.push(current);
                        }
                    }

                    return fundamentals;
                }

                function detectPitch() {
                    analyser.getFloatTimeDomainData(timeDataArray);
                    const freqData = new Uint8Array(analyser.frequencyBinCount);
                    analyser.getByteFrequencyData(freqData);

                    const sampleRate = audioContext.sampleRate;

                    // Update adaptive noise floor
                    updateNoiseFloor(freqData);

                    // Apply Harmonic Product Spectrum
                    const hpsData = harmonicProductSpectrum(Array.from(freqData), sampleRate, analyser.fftSize);

                    const minFreq = 50;
                    const maxFreq = 2000;
                    const minBin = Math.floor(minFreq * analyser.fftSize / sampleRate);
                    const maxBin = Math.floor(maxFreq * analyser.fftSize / sampleRate);

                    // Dynamic threshold based on noise floor
                    const minPeakHeight = Math.max(window.noiseFloor * 2.5, 100);
                    const minPeakDistance = 5;

                    // Find peaks using both FFT and HPS
                    const peaks = [];

                    for (let i = minBin; i < maxBin; i++) {
                        const fftMag = freqData[i];
                        const hpsMag = hpsData[i];

                        // Combined score: FFT magnitude + HPS boost
                        const score = fftMag + (hpsMag / 255) * 50;

                        if (fftMag > minPeakHeight) {
                            let isPeak = true;

                            // Check if it's a local maximum
                            for (let j = Math.max(minBin, i - minPeakDistance);
                                 j <= Math.min(maxBin - 1, i + minPeakDistance); j++) {
                                if (j !== i && freqData[j] >= fftMag) {
                                    isPeak = false;
                                    break;
                                }
                            }

                            if (isPeak) {
                                // Parabolic interpolation for sub-bin accuracy
                                const y1 = i > 0 ? freqData[i - 1] : freqData[i];
                                const y2 = freqData[i];
                                const y3 = i < freqData.length - 1 ? freqData[i + 1] : freqData[i];

                                const delta = 0.5 * (y3 - y1) / (2 * y2 - y1 - y3);
                                const interpolatedBin = i + (isFinite(delta) ? delta : 0);

                                const freq = interpolatedBin * sampleRate / analyser.fftSize;
                                peaks.push({ freq, magnitude: fftMag, score });
                            }
                        }
                    }

                    // Filter out harmonics
                    const filteredPeaks = filterHarmonics(peaks);

                    // Try autocorrelation for strongest fundamental
                    const autoCorrelatedFreq = autoCorrelate(timeDataArray, sampleRate);
                    if (autoCorrelatedFreq > 0) {
                        // Check if autocorrelation found a note not in peaks
                        const exists = filteredPeaks.some(p => Math.abs(p.freq - autoCorrelatedFreq) < 10);
                        if (!exists) {
                            filteredPeaks.unshift({
                                freq: autoCorrelatedFreq,
                                magnitude: 200,
                                score: 250
                            });
                        }
                    }

                    // Sort by score and take top peaks
                    filteredPeaks.sort((a, b) => b.score - a.score);
                    const topPeaks = filteredPeaks.slice(0, 6);

                    // Convert frequencies to MIDI notes
                    const detectedNotes = topPeaks
                        .map(peak => {
                            const midiNote = Math.round(12 * Math.log2(peak.freq / 440) + 69);
                            return { note: midiNote, magnitude: peak.magnitude };
                        })
                        .filter(item => item.note >= 21 && item.note <= 108);

                    if (detectedNotes.length > 0) {
                        const notes = [...new Set(detectedNotes.map(n => n.note))].sort((a, b) => a - b);

                        // Smoothing: only update if notes changed significantly
                        const notesChanged = notes.length !== window.previousNotes.length ||
                            notes.some((note, i) => note !== window.previousNotes[i]);

                        if (notesChanged) {
                            window.previousNotes = notes;
                            const maxMagnitude = Math.max(...detectedNotes.map(n => n.magnitude));
                            const result = {
                                notes: notes,
                                confidence: Math.min(maxMagnitude / 255, 1.0)
                            };
                            callback(result);
                        }
                    } else if (window.previousNotes.length > 0) {
                        // Clear notes if nothing detected
                        window.previousNotes = [];
                        callback({ notes: [], confidence: 0 });
                    }

                    animationId = requestAnimationFrame(detectPitch);
                }

                detectPitch();
            })
            .catch(err => {
                console.error("Error accessing microphone:", err);
            });

        // Store cleanup function globally
        window.audioStopFunction = function() {
            if (animationId) {
                cancelAnimationFrame(animationId);
            }
            if (mediaStream) {
                mediaStream.getTracks().forEach(track => track.stop());
            }
            if (audioContext.state !== 'closed') {
                audioContext.close();
            }
            window.previousNotes = [];
            window.noiseFloor = 100;
        };
    }

    export function stopListening() {
        if (window.audioStopFunction) {
            window.audioStopFunction();
            window.audioStopFunction = null;
        }
    }
    "#)]
extern "C" {
    fn startListening(callback: &Closure<dyn Fn(JsValue)>);
    fn stopListening();
}

fn play_current_chord() {
    if let Some(chord) = CHORD.read().as_ref() {
        let frequencies: Vec<f64> = chord
            .notes()
            .iter()
            .map(|note| {
                // Convert MIDI note to frequency: f = 440 * 2^((n-69)/12)
                let midi_note = note.pitch.into_u8() as f64 + (note.octave as f64 * 12.0);
                440.0 * 2.0_f64.powf((midi_note - 69.0) / 12.0)
            })
            .collect();

        playChord(frequencies, 2.0);
    }
}

fn start_recognition() {
    // If already listening, stop first
    if *LISTENING.read() {
        stop_recognition();
        return;
    }

    *LISTENING.write() = true;
    *DETECTED_NOTES.write() = Vec::new();
    *RECOGNITION_FEEDBACK.write() = None;

    let callback = Closure::wrap(Box::new(move |value: JsValue| {
        if let Result::Ok(result) = serde_wasm_bindgen::from_value::<RecognitionResult>(value) {
            console::log_1(&format!("Detected notes: {:?}", result.notes).into());
            *DETECTED_NOTES.write() = result.notes.clone();

            // Check if detected notes match the current chord
            if let Some(chord) = CHORD.read().as_ref() {
                let target_notes: Vec<u8> = chord
                    .notes()
                    .iter()
                    .map(|note| note.pitch.into_u8() + (note.octave as u8 * 12))
                    .collect();

                let detected = &result.notes;

                // Simple matching: check if detected notes contain all target notes
                let all_matched = target_notes.iter().all(|&note| {
                    detected
                        .iter()
                        .any(|&d| (d as i16 - note as i16).abs() <= 1)
                });

                if all_matched && detected.len() > 0 {
                    *RECOGNITION_FEEDBACK.write() = Some("✓ Correct!".to_string());
                } else if detected.len() > 0 {
                    *RECOGNITION_FEEDBACK.write() = Some("Keep trying...".to_string());
                }
            }
        }
    }) as Box<dyn Fn(JsValue)>);

    startListening(&callback);
    callback.forget(); // Keep callback alive
}

fn stop_recognition() {
    stopListening(); // Call JavaScript cleanup function
    *LISTENING.write() = false;
    *DETECTED_NOTES.write() = Vec::new();
    *RECOGNITION_FEEDBACK.write() = None;
}

#[component]
fn PianoVisualization() -> Element {
    let chord = CHORD.read();
    let detected_notes = DETECTED_NOTES.read();

    // Piano range: C3 (48) to C6 (84) - 3 octaves
    let start_note = 48u8;
    let end_note = 84u8;

    let active_notes: HashSet<u8> = if let Some(ref c) = *chord {
        c.notes()
            .iter()
            .map(|note| note.pitch.into_u8() + (note.octave as u8 * 12))
            .collect()
    } else {
        HashSet::new()
    };

    let detected_set: HashSet<u8> = detected_notes
        .iter()
        .filter(|&&n| n >= start_note && n <= end_note)
        .copied()
        .collect();

    // Generate white keys and black keys separately
    let mut white_keys = Vec::new();
    let mut black_keys = Vec::new();

    for note in start_note..=end_note {
        let pitch = note % 12;
        let is_black = matches!(pitch, 1 | 3 | 6 | 8 | 10); // C#, D#, F#, G#, A#
        let is_active = active_notes.contains(&note);
        let is_detected = detected_set.contains(&note);

        let state_class = if is_detected && is_active {
            "detected-correct"
        } else if is_detected {
            "detected"
        } else if is_active {
            "active"
        } else {
            ""
        };

        if is_black {
            // Black keys positioned between white keys
            // Calculate position based on the pattern within an octave
            let octave_position = pitch % 12;
            let white_keys_before = match octave_position {
                1 => 1,  // C# comes after C
                3 => 2,  // D# comes after D
                6 => 4,  // F# comes after F
                8 => 5,  // G# comes after G
                10 => 6, // A# comes after A
                _ => 0,
            };

            let octave = (note - start_note) / 12;
            let white_keys_in_octave = 7;
            let total_white_keys_before = octave as u32 * white_keys_in_octave + white_keys_before;

            // Use CSS calc() for responsive positioning
            let left_style = format!(
                "left: calc({} * var(--white-key-width) - var(--black-key-offset));",
                total_white_keys_before
            );

            black_keys.push(rsx! {
                div {
                    key: "{note}",
                    class: "piano-key black {state_class}",
                    style: "{left_style}",
                }
            });
        } else {
            // White keys
            white_keys.push(rsx! {
                div {
                    key: "{note}",
                    class: "piano-key white {state_class}",
                }
            });
        }
    }

    rsx! {
        div { class: "piano-container",
            div { class: "piano-keyboard",
                // White keys layer
                {white_keys.into_iter()}
                // Black keys layer (on top)
                {black_keys.into_iter()}
            }
        }
    }
}

#[component]
fn CurrentChord() -> Element {
    let chord = CHORD.read();
    rsx! {
        div { id: "chord",
            match chord.as_ref() {
                Some(c) => rsx! { "{c}" },
                None => rsx! {
                    span { class: "chord-placeholder", "Configure & Start" }
                },
            }
        }
    }
}

#[component]
fn ToggleButton(label: String, active: bool, on_toggle: EventHandler<()>) -> Element {
    let class = if active {
        "toggle-btn active"
    } else {
        "toggle-btn"
    };
    rsx! {
        button {
            class: "{class}",
            onclick: move |_| on_toggle.call(()),
            "{label}"
        }
    }
}

#[component]
fn SettingsPanel() -> Element {
    let settings_open = SETTINGS_OPEN.read();

    if !*settings_open {
        return rsx! {};
    }

    let settings = SETTINGS.read();

    rsx! {
        div { class: "settings-overlay",
            onclick: move |_| *SETTINGS_OPEN.write() = false,
        }
        div { class: "settings-panel",
            div { class: "settings-header",
                h2 { "Chord Options" }
                button {
                    class: "close-btn",
                    onclick: move |_| *SETTINGS_OPEN.write() = false,
                    "✕"
                }
            }

            div { class: "settings-content",
                // Chord Qualities Section
                div { class: "settings-section",
                    h3 { "Chord Qualities" }
                    div { class: "toggle-grid",
                        for quality in ChordQuality::iter() {
                            ToggleButton {
                                key: "{quality:?}",
                                label: quality.display_name().to_string(),
                                active: settings.qualities.contains(&quality),
                                on_toggle: move |_| {
                                    let mut settings = SETTINGS.write();
                                    if settings.qualities.contains(&quality) {
                                        if settings.qualities.len() > 1 {
                                            settings.qualities.remove(&quality);
                                        }
                                    } else {
                                        settings.qualities.insert(quality);
                                    }
                                },
                            }
                        }
                    }
                    div { class: "quick-actions",
                        button {
                            class: "quick-btn",
                            onclick: move |_| {
                                SETTINGS.write().qualities = ChordQuality::iter().collect();
                            },
                            "All"
                        }
                        button {
                            class: "quick-btn",
                            onclick: move |_| {
                                let mut set = HashSet::new();
                                set.insert(ChordQuality::Major);
                                set.insert(ChordQuality::Minor);
                                SETTINGS.write().qualities = set;
                            },
                            "Basic"
                        }
                    }
                }

                // Chord Numbers Section
                div { class: "settings-section",
                    h3 { "Extensions" }
                    div { class: "toggle-grid",
                        for number in ChordNumber::iter() {
                            ToggleButton {
                                key: "{number:?}",
                                label: number.display_name().to_string(),
                                active: settings.numbers.contains(&number),
                                on_toggle: move |_| {
                                    let mut settings = SETTINGS.write();
                                    if settings.numbers.contains(&number) {
                                        if settings.numbers.len() > 1 {
                                            settings.numbers.remove(&number);
                                        }
                                    } else {
                                        settings.numbers.insert(number);
                                    }
                                },
                            }
                        }
                    }
                    div { class: "quick-actions",
                        button {
                            class: "quick-btn",
                            onclick: move |_| {
                                SETTINGS.write().numbers = ChordNumber::iter().collect();
                            },
                            "All"
                        }
                        button {
                            class: "quick-btn",
                            onclick: move |_| {
                                let mut set = HashSet::new();
                                set.insert(ChordNumber::Triad);
                                SETTINGS.write().numbers = set;
                            },
                            "Triads Only"
                        }
                    }
                }

                // Inversions Section
                div { class: "settings-section",
                    h3 { "Inversions" }
                    div { class: "toggle-grid",
                        ToggleButton {
                            label: if settings.inversions { "Enabled".to_string() } else { "Disabled".to_string() },
                            active: settings.inversions,
                            on_toggle: move |_| {
                                let mut settings = SETTINGS.write();
                                settings.inversions = !settings.inversions;
                            },
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn App() -> Element {
    let listening = LISTENING.read();
    let feedback = RECOGNITION_FEEDBACK.read();
    let has_chord = CHORD.read().is_some();
    let keyboard_visible = KEYBOARD_VISIBLE.read();

    rsx! {
        document::Stylesheet { href: CSS }
        document::Title { "Chords Practice" }

        div { id: "main",
            div { id: "title",
                h1 { "Play This Chord!" }
            }

            CurrentChord {}

            if has_chord && *keyboard_visible {
                PianoVisualization {}
            }

            if let Some(ref msg) = *feedback {
                div { class: "recognition-feedback",
                    "{msg}"
                }
            }

            div { id: "buttons",
                button {
                    class: "primary-btn",
                    onclick: move |_| generate_chord(),
                    "Next Chord"
                }
                button {
                    class: "play-btn",
                    onclick: move |_| play_current_chord(),
                    disabled: !has_chord,
                    "▶ Play"
                }
                if !*listening {
                    button {
                        class: "listen-btn",
                        onclick: move |_| start_recognition(),
                        disabled: !has_chord,
                        "🎤 Listen"
                    }
                } else {
                    button {
                        class: "listen-btn active",
                        onclick: move |_| stop_recognition(),
                        "Stop"
                    }
                }
                button {
                    class: "settings-btn",
                    onclick: move |_| *SETTINGS_OPEN.write() = true,
                    "Options"
                }
                button {
                    class: "keyboard-toggle-btn",
                    onclick: move |_| {
                        let mut visible = KEYBOARD_VISIBLE.write();
                        *visible = !*visible;
                    },
                    if *keyboard_visible { "Hide Piano" } else { "Show Piano" }
                }
            }

            SettingsPanel {}
        }
    }
}

fn main() {
    dioxus::launch(App);
}
