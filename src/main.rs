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
use strum::{EnumIter, IntoEnumIterator};
use wasm_bindgen::prelude::*;

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
    rsx! {
        document::Stylesheet { href: CSS }
        document::Title { "Chords Practice" }

        div { id: "main",
            div { id: "title",
                h1 { "Play This Chord!" }
            }

            CurrentChord {}

            div { id: "buttons",
                button {
                    class: "primary-btn",
                    onclick: move |_| generate_chord(),
                    "Next Chord"
                }
                button {
                    class: "play-btn",
                    onclick: move |_| play_current_chord(),
                    disabled: CHORD.read().is_none(),
                    "▶ Play"
                }
                button {
                    class: "settings-btn",
                    onclick: move |_| *SETTINGS_OPEN.write() = true,
                    "Options"
                }
            }

            SettingsPanel {}
        }
    }
}

fn main() {
    dioxus::launch(App);
}
