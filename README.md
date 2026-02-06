# Chords

A web-based chord learning and ear training application built with Rust and Dioxus.

## Features

- Random chord generation with customizable settings
- Interactive piano visualization
- Audio playback of chords
- Chord recognition and ear training
- Support for multiple chord qualities (Major, Minor, Diminished, Augmented, etc.)
- Support for various chord extensions (Triads, 7ths, 9ths, 11ths, 13ths)
- Chord inversions

## Technologies

- **Rust** - Core language
- **Dioxus** - Web framework
- **rust-music-theory** - Music theory library
- **WebAssembly** - Browser runtime

## Building

### Prerequisites

- Rust toolchain
- Dioxus CLI

### Development

```sh
dx serve
```

### Production

```sh
dx build --release
```

## Usage

1. Click "Generate Chord" to create a random chord
2. Use the settings panel to customize chord types and inversions
3. Play the chord using the "Play" button
4. Toggle the piano visualization to see the chord on a keyboard
5. Use "Start Listening" for ear training exercises

## License

MIT License - see LICENSE file for details

## Author

Luna Xin