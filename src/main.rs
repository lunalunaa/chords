use std::fmt::Display;
use std::ops::Deref;

use anyhow::anyhow;
use anyhow::{Ok, Result};
use dioxus::prelude::*;
use rand::prelude::*;
use rust_music_theory::{
    chord::{self, Chord, Number::*, Quality::*},
    note::{Notes, Pitch},
    scale::{Direction, Scale, ScaleType},
};

static CSS: Asset = asset!(
    "/assets/main.css",
    CssAssetOptions::new().with_preload(true)
);

const ALL_CHORD_QUALITIES: [chord::Quality; 8] = [
    Major,
    Minor,
    Diminished,
    Augmented,
    HalfDiminished,
    Dominant,
    Suspended2,
    Suspended4,
];

const ALL_NUMBERS: [chord::Number; 6] = [Triad, Seventh, MajorSeventh, Ninth, Eleventh, Thirteenth];

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
            Major if matches!(number, Triad | Seventh) => "",
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
    numbers: &[chord::Number],
    inversion: bool,
) -> Result<ChordWrapper> {
    let mut rng = rand::rng();
    let numbers = if numbers.is_empty() {
        &ALL_NUMBERS
    } else {
        numbers
    };
    let qualities = if qualities.is_empty() {
        &ALL_CHORD_QUALITIES
    } else {
        qualities
    };

    loop {
        let root = s
            .notes()
            .choose(&mut rng)
            .ok_or(anyhow!("Empty Scale!"))?
            .pitch;
        let quality = *(qualities
            .choose(&mut rng)
            .ok_or(anyhow!("Empty Chord Quality!"))?);
        let number = numbers
            .choose(&mut rng)
            .ok_or(anyhow!("Empty Chord Number!"))?;
        let inversion_range: u8 = if inversion {
            match number {
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
        let chord = Chord::with_inversion(root, quality, *number, inversion_number);

        if chord.notes().len() == 3 && chord.number != Triad {
            continue; // if it's an impossible chord then generate again
        }

        return Ok(ChordWrapper(chord));
    }
}

fn next_chord() -> ChordWrapper {
    let scale = Scale::new(
        ScaleType::Chromatic,
        Pitch::from_u8(0),
        4,
        None,
        Direction::Ascending,
    )
    .unwrap();
    generate_next_chord(&scale, &[], &[], true).unwrap()
}

static CHORD: GlobalSignal<ChordWrapper> = Signal::global(|| next_chord());

#[component]
fn CurrentChord() -> Element {
    rsx! {
        div { id: "chord", "{CHORD}"}
    }
}

#[component]
fn App() -> Element {
    rsx! {
        document::Stylesheet { href: CSS }
        document::Title { "Chords Practice" }
        div { id: "title", h1 { "Play this Chord!" } }
        CurrentChord {  }
        div {id: "buttons",  button { onclick: move |_| *CHORD.write() = next_chord(), "Next" } }
    }
}

fn main() {
    dioxus::launch(App);
}
