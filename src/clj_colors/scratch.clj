(ns scratch
  "Interactive examples. Evaluate forms in the comment block at a REPL."
  (:require [clj-colors.main :as main]
            [clj-colors.access :as access]
            [clj-colors.fade :as fade]
            [clj-colors.api :as api]
            [clj-colors.svg :as svg]))

(comment
  ; --- base lookups ---
  (main/get-palette :forest/jungle)
  (main/get-palette :jungle) ; bare name works too
  (main/palette-keys)
  (main/categories)
  (keys (main/palettes-in-category :ocean))
  (keys (main/palettes-with-tags "retro" "80s"))

  ; --- thematic getters (access) ---
  (keys (access/get-orange-palettes))
  (keys (access/get-blue-palettes))
  (keys (access/get-category-palettes :sunset))

  ; --- tag-based getters (access) ---
  ; Returns nil because no palette is tagged with both "forest" and "earth-tone"
  (keys (access/get-tagged-palettes {:match :all}
                                    "forest"
                                    "earth-tone"))
  ; Returns palettes tagged with either "forest" or "earth-tone"
  (keys (access/get-tagged-palettes "forest" "earth-tone"))
  ; Returns the whole registry, as no tags are specified
  (keys (access/get-tagged-palettes))

  ; pull colors straight out
  (access/palette-hex :ocean/abyss)
  (access/palette-rgb :sunset/golden-hour)
  (access/family-hex :green)
  ; random selection for generative callers
  (access/random-hex {:family :blue :min-count 5})
  (access/random-hex {:category :sunset})
  (access/random-palette {:tags ["neon"]})

  ; --- fades / transparency ramps (the cream) ---
  ; default linear fade-out (opaque -> transparent), RGB untouched
  (fade/fade (access/palette-hex :ocean/oasis))
  ; reverse it
  (fade/fade-in (access/palette-hex :ocean/oasis))
  ; algorithmic curves
  (fade/fade-out (access/palette-hex :spring/cherry-sky) {:curve :logarithmic})
  (fade/fade-in  (access/palette-hex :sunset/golden-hour) {:curve :exponential})
  ; custom curve: any fn t->[0,1]
  (fade/fade (access/palette-hex :forest/fern) {:curve (fn [t] (* t t t))})
  ; clamp the alpha range, e.g. never fully transparent
  (fade/fade (access/palette-hex :ocean/abyss) {:min-alpha 0.25})
  ; 8-digit hex out, ready to drop into CSS or SVG
  (fade/fade-hex (access/palette-hex :ocean/oasis) {:curve :ease-in})
  ; flat wash
  (fade/with-alpha (access/palette-hex :pastel/peachy) 0.5)

  ; --- render a faded gradient (honors alpha) ---
  (svg/spit-svg "/tmp/oasis-fade.svg"
                (svg/alpha-gradient-svg
                 (fade/fade-out (access/palette-hex :ocean/oasis) {:curve :logarithmic})
                 {:width 400 :height 600}))

  ; --- the public interface ---
  (api/print-api)        ; list every public function with arglists + docs
  (count (api/catalog))  ; data view of all palettes for a tool/model to scan
  (first (api/catalog))

  ; --- runtime mutation + persistence ---
  (main/register-palette! :forest/swamp
                          ["#0A140C" "#1C3220" "#3E5E38" "#6E9A52" "#C4E29A"]
                          {:tags ["murky"]})
  (main/unregister-palette! :forest/swamp)
  (main/save-registry! "/tmp/palettes-out.edn")
  )