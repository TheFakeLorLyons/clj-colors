# 🎨 Clj-Colors (AKA "Swatch")

Swatch is a color palette management and query library for Clojure.

It provides a curated collection of color-swatches, a browser-based studio for creating and editing them, 
and an API for access and generation of color combinations and gradients by visual characteristics, 
theme, and semantic tags.

Swatches are named sets of colors/gradients, stored as hex or rgba and enriched on load with 
computed metadata so you can query them by 'family', 'tag', 'temperature', 'brightness', and more. 
Includes SVG export for using a gradient as a layer in other applications (sky, water, terrain).

While it might be hard to keep track of so many palettes for us as humans, the tagging system is intended (albeit still in early development) for use
in LLM workflows, where textual input from user requests may map onto a number of these gradients
which might be accessed as a tool call via the api.

Swatch can also generate SVG gradients, transparency fades, and other color assets directly from 
palette definitions; allowing palettes to move seamlessly from data to design.

## Metadata

Each loaded swatch carries derived fields alongside its colors:
```clojure
{:family            :green ; Derived based off of the rbga values when the palette is generated.
 :brightness        0.39   ; Mean values across the palette...
 :temperature       0.491  ; Using the mean provides a general idea of which palettes
 :saturation        0.604  ;   are 'mildly' vs 'highly' saturated, hot, tinted, etc.
 :contrast          0.937
 :mean-lightness    0.46
 :hue-concentration 0.97
 :count             5      ; The number of 'stops'/hues in the gradient. Currently this has a max of 12 (arbitrarily).
 :name              :alien-jungle
 :category          :forest
 :tags              #{"green" "vivid" "neon" "high-contrast" "alien" "organic"}}
```

## Data
A swatch is stored as its hex & rgba colors plus optional tags:
```clojure
; Examples are drawn from resources/palettes.edn
; Example 1:
 :forest/jungle
 {:hex ["#0C1A12" "#183A26" "#2A623E" "#4C9660" "#C2ECBC"]
  :rgba [[12 26 18 255] [24 58 38 255] [42 98 62 255] [76 150 96 255] [194 236 188 255]]
  :count 5
  :name :jungle
  :category :forest
  :family :green
  :brightness 0.226
  :temperature 0.479
  :saturation 0.414
  :contrast 0.742
  :mean-lightness 0.357
  :hue-concentration 0.972
  :tags #{"earth-tone" "green" "monochrome"}}

; Example 2:
 :ocean/abyss
 {:hex ["#05101F" "#0A2038" "#123A5C" "#1E6088" "#4C9CC0" "#A8DCEC"]
  :rgba [[5 16 31 255] [10 32 56 255] [18 58 92 255] [30 96 136 255] [76 156 192 255] [168 220 236 255]]
  :count 6
  :name :abyss
  :category :ocean
  :family :blue
  :brightness 0.185
  :temperature 0.358
  :saturation 0.642
  :contrast 0.651
  :mean-lightness 0.343
  :hue-concentration 0.992
  :tags #{"blue" "dark" "deep" "low-contrast" "monochrome" "vivid"}}
```

##  Examples of useage are included in scratch.clj!
### Below is a small snippet as a quick snapshot:

```clojure
(require '[clj-colors.main :as main]
         '[clj-colors.access :as access])

(main/get-palette :forest/jungle)   ;; by full key
(main/get-palette :jungle)          ;; or by bare name

(main/palette-keys)
(main/categories)
(keys (main/palettes-in-category :ocean))

(keys (main/palettes-with-tag "vivid"))
(keys (main/palettes-with-tags "retro" "80s"))
(keys (main/palettes-by #(> (:brightness %) 0.45)))

(main/random-palette)               ;; [key palette]
(main/random-palette "water")       ;; restricted to a tag
(main/random-color :synthwave/synthwave)

; Also works  
(access/get-orange-palettes)
(keys (access/get-orange-palettes))
(access/get-category-palettes :sunset)
(keys (access/get-category-palettes :sunset))
```

### Families & Categories

- `:family` is the dominant perceived _color family_ derived from the palette's hues.

- `:category` is the thematic collection a palette belongs to and forms the namespace prefix of its id.
  - _Categories are primarily organizational and curated by the palette author._

```clojure
 :green
 :blue
 :teal
 :orange
 :purple
 :red
; ^^^
; color-families

 :forest/jungle
; ^^^^^^  ^^^^
; catgry  name

; Examples of some default categories (contained in `resources/palettes.edn`):

:forest
:ocean
:space
:synthwave
:sunset
:volcanic
; ... and more!

```

### Tags

Tags are descriptive labels used for discovery, filtering, and semantic search.

Some tags are generated automatically from palette statistics:
```
"dark"
"light",
"vivid",
"muted",
"pastel",
"neon",
"monochrome",
"grayscale",
"earth-tone",
"high-contrast",
"low-contrast"
```

These built-in tags are intended as a useful baseline, not a complete vocabulary.

*Palette authors are encouraged to add their own tags to capture themes, moods, use-cases, aesthetics, cultural references, or any other concepts that might help identify a palette later.*

Examples:
```
#{"organic" "alien" "jungle" "bioluminescent"}
#{"retro" "80s" "arcade" "synthwave"}
#{"stormy" "deep" "oceanic" "cold"}
#{"warm" "cozy" "autumn" "harvest"}
```
There is no fixed tag taxonomy; the library deliberately allows arbitrary descriptors because semantic richness makes palettes easier to discover and reuse.

A palette with twenty meaningful tags is often more useful than one with only a handful of color-derived labels.

This becomes particularly valuable in generative and AI-assisted workflows, where natural language requests can be mapped onto palette metadata:

- "Give me something dark, organic, and slightly alien."
- "Find a warm retro palette with high contrast."
- "Pick a dreamy pastel palette for a spring scene."

The more descriptive metadata attached to a palette, the more effectively it can participate in search, recommendation, procedural generation, and LLM-driven selection.

## Gallery & studio

![Swatch Palette Gallery](resources/swatch_capture.png)
A live, browser-based gallery served straight from the library, no build step:

```clojure
(require '[clj-colors.gallery :as gallery])
(gallery/serve!)   ;; http://localhost:8350, opens a browser
```

Filter by category, family, tag, or brightness; click a palette to open the
studio, where you can edit colors, weights, alphas, and fades, then save the
design back to the registry and the palettes file (surgically) or delete it.
The page chrome themes itself from the palette you're editing. Works from any
project that depends on clj-colors.

## SVG export

```clojure
(require '[clj-colors.svg :as svg])

;; smooth gradient on a rectangle as an svg
(svg/spit-svg "out.svg"
  (main/palette-gradient-svg :ocean/abyss {:width 400 :height 600}))

;; discrete blocks
(main/palette-main-svg :forest/fern {:width 600 :height 120})

;; straight from arbitrary colors
(svg/gradient-svg ["#102030" "#4080C0" "#E0F0FF"] {:orientation :horizontal})
```

Output is a plain SVG string with no dependencies.

## Layout

```
deps.edn                   - dependencies
resources/palettes.edn     - canonical data, hex only, grouped by category
src/clj_colors/color.clj   - hex <-> rgba <-> hsl, relative luminance
src/clj_colors/meta.clj    - metadata derivation from colors
src/clj_colors/svg.clj     - gradient and block SVG rendering
src/clj_colors/core.clj    - load, query, mutate, persist
src/clj_colors/scratch.clj - REPL usage examples
test/main_test.clj         - tests
samples/                   - example exported SVGs
```

## Adding palettes

At runtime:

```clojure
(main/register-palette! :forest/swamp
  ["#0A140C" "#1C3220" "#3E5E38" "#6E9A52" "#C4E29A"]
  {:tags ["murky"]})

(main/unregister-palette! :forest/swamp)
```

Colors may be hex strings or `[r g b]` / `[r g b a]` vectors. Write the current
registry back to canonical EDN (computed fields stripped, sorted by key):

```clojure
(main/save-registry! "resources/palettes.edn")
```

Or add to `resources/palettes.edn` directly and call `(main/reset-registry!)`.

## A note on persistence:

This program assumes that users want to develop their own libraries of color palettes that span far beyond the defaults provided here. This note contained in `main.clj` describes the key role palettes.edn serves as a simple clojure map which contains all available palettes to this particular instance of the program. Perhaps in the future it would be interesting to be able to manage multiple sets of categories; but for now all palettes live inside the one map.

```clojure
; Persistence
;
; The registry file is the user's document: it carries an index block,
; category comments, and hand-kept formatting that the library must never
; destroy. save-registry! therefore only rewrites a file wholesale when it
; does not exist yet. Otherwise the file's text is scanned for its
; top-level entries (everything between them is opaque bytes), new
; palettes are inserted after the last entry of their category or in a
; fresh category section before the closing brace, and entries whose
; colors or tags changed are replaced in place. Map ordering never comes
; into it because the file is never round-tripped through a map; the
; file's own text supplies the order.

;; To see this data structure, go check out resources/palettes.edn
```

## Tests

The project currently includes 58 passing tests covering the core library functionality:

- Color parsing and conversion (hex, rgba)
- Metadata derivation and palette enrichment
- Palette lookup by key, category, family, and tags
- Tag query semantics (all vs any)
- Random palette and color selection
- Runtime palette registration and removal
- Registry loading and persistence
- Surgical updates to palettes.edn without disturbing unrelated content
- Bundled palette data invariants

The focus so far has been on correctness of the palette registry, metadata system, and persistence layer, as these contain most of the library's core logic.

Run the test suite with:

```
clojure -M:test
```
```clojure
; or, in tests/main_test.clj run
(run-tests)
```

## Contributing

Contributions are welcome in many forms!

### Palette bundles

One of the goals of Swatch is to build a large, searchable collection of high-quality palettes spanning different aesthetics, themes, moods, and domains.

If you've curated a set of palettes you think others would find useful, feel free to publish them yourself, open a PR, or send them my way - I'm especially interested in:

* Nature-inspired collections
* Game development palettes
* Pixel art palettes
* Fantasy and sci-fi themes
* Historical and cultural color studies
* UI/UX design systems
* Generative art palettes
* Experimental or unusual color schemes

A palette bundle doesn't need to follow any particular style. If it's interesting, coherent, and useful, I'd love to see it.

### Features and improvements

Bug fixes, documentation improvements, performance work, new query capabilities, additional exports, and studio enhancements are all welcome.

If there's a workflow you wish existed, open an issue and let's talk about it.

### Generative art and AI

Swatch was built with procedural and AI-assisted workflows in mind. The metadata and tagging system exists so palettes can be discovered semantically rather than only by name.

If you're building:

* Generative art tools
* Creative coding projects
* Agentic workflows
* Design assistants
* Worldbuilding tools
* Image generation pipelines
* Color recommendation systems

I'd be interested to hear about it and see how we might be able to collaborate.

The long-term vision is a shared semantic palette library that creative software, generative systems, and humans can all use together.

If you'd like to collaborate on Swatch itself—or on adjacent projects involving generative art, procedural content, visualization, or AI—please reach out. I'm always interested in conversation and collaboration with other people working in these spaces. Feel free to reach out with any questions as well.

If you would like to learn more about me or my other work or interests, check out [brainfloj](https://example.com), my current life's ambition; or my [somewhat out-of-date personal website](https://lorelailyons.me/).