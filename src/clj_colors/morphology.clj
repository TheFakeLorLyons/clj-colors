(ns clj-colors.morphology
  "Morphological analysis for tag canonicalization. Two words are
   morphological variants of the same root when they share a stem
   AND the transformation between them uses recognized English
   derivational suffixes. Used at two points:

   1. During synonym map authoring (hand or LLM): detect when a
      proposed synonym is actually just a morphological variant of
      the canonical and should be excluded rather than added.

   2. During tag canonicalization (apply pipeline): collapse
      morphological variants into their canonical descriptor so
      :joyfully, :joyfulness, :joy all become :joyful before
      contributing to the hex's tag map.

   The suffix list is intentionally conservative: better to miss a
   variant than to conflate distinct words. Edge cases get handled
   explicitly in the irregulars map and false-positive blacklist."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def descriptor-suffixes
  "Recognized English derivational suffixes, sorted longest-first so
   greedy stripping picks the longest match (fulness wins over ness).
   Conservative list: only suffixes that genuinely turn a base word
   into a descriptor or noun form of the same concept. -ic and -ism
   are deliberately excluded because they tend to indicate distinct
   concepts (sadism is not a form of sad; periodic is not a form of
   period). Add new suffixes only when you've checked they don't
   introduce false positives."
  (sort-by (comp - count)
           ["fulness" "ousness" "ableness" "iveness" "lessness"
            "fully"   "ously"   "ingly"    "edly"    "ably"    "lessly"
            "ical"    "hood"
            "ful"     "ous"     "ing"      "ed"      "able" "ible" "less"
            "ness"    "ment"    "tion"     "sion"   "al"
            "ly"      "y"]))

(defn- strip-one-suffix
  "Remove the longest matching suffix if doing so leaves a stem of at
   least 3 characters. Returns the stem or the original word. The
   length floor prevents pathological matches like 'be' having 'e'
   stripped from it. Operates on strings; callers handle keyword
   conversion."
  [s]
  (or (some (fn [suf]
              (when (and (str/ends-with? s suf)
                         (>= (- (count s) (count suf)) 3))
                (subs s 0 (- (count s) (count suf)))))
            descriptor-suffixes)
      s))

(defn stem
  "Reduce a word to its morphological stem by stripping derivational
   suffixes iteratively until no more match. Handles compound
   suffixes (joyfully -> joyful -> joy) by repeated application.
   Takes and returns a keyword."
  [word]
  (let [s (name word)
        stemmed (loop [current s]
                  (let [stripped (strip-one-suffix current)]
                    (if (= stripped current)
                      current
                      (recur stripped))))]
    (keyword stemmed)))

(def irregular-canonical
  "Manually-mapped irregular forms where suffix-stripping doesn't
   recover the canonical (proud is not pride minus a recognized
   suffix). Each key maps to the canonical descriptor it should
   collapse into. Add entries as you encounter irregulars."
  {:pride :proud})

(def false-positive-pairs
  #{#{:sad :sadistic}
    #{:sad :sadism}
    #{:hate :hat}
    #{:relief :relic}
    #{:man :mane}
    #{:car :caring}
    #{:legal :leg}      ; -al false positive
    #{:metal :met}      ; -al false positive
    #{:final :fin}})

(defn descriptor-by-stem-index
  "Build a lookup map from stem to canonical descriptor. Called
   once per canonical-descriptor-map and cached by the caller.
   Use canonicalize-with-index to consume it."
  [canonical-descriptor-map]
  (into {}
        (for [[_ desc] canonical-descriptor-map]
          [(stem desc) desc])))

(defn canonicalize-with-index
  "Like canonicalize but takes a precomputed stem-to-descriptor
   index. Use when canonicalizing many tags against the same
   canonical map: build the index once, pass it in repeatedly."
  [word descriptor-by-stem]
  (or (get irregular-canonical word)
      (let [s (stem word)]
        (get descriptor-by-stem s word))))

(defn canonicalize
  "Reduce a word to its canonical descriptor form. Builds the
   stem-to-descriptor index on every call; for hot-path use,
   build the index once with descriptor-by-stem-index and call
   canonicalize-with-index instead."
  [word canonical-descriptor-map]
  (canonicalize-with-index word
                           (descriptor-by-stem-index canonical-descriptor-map)))

(defn- consonant?
  [c]
  (not (#{\a \e \i \o \u \y} c)))

(defn- maybe-e-variants
  "Stems ending in a consonant get a -e variant added. Models the
   silent-e drop before vowel-initial suffixes: love + -able = lovable
   strips back to lov, and the reconstruction adds love."
  [stem]
  (let [s (if (keyword? stem) (name stem) stem)]
    (cond-> #{s}
      (and (>= (count s) 2)
           (consonant? (last s)))
      (conj (str s "e")))))

(defn- maybe-y-variants
  "Stems ending in consonant + i get a variant with -i replaced by -y.
   Models the y -> i rule: happy + -ness = happiness strips back to
   happi, and the reconstruction adds happy."
  [stem]
  (let [s (if (keyword? stem) (name stem) stem)
        n (count s)]
    (cond-> #{s}
      (and (>= n 2)
           (= \i (last s))
           (consonant? (nth s (- n 2))))
      (conj (str (subs s 0 (dec n)) "y")))))

(defn strips-to
  "Recursively strip recognized suffixes from word, collecting every
   intermediate form (including the original word). Returns a set of
   all reachable forms via legitimate suffix removal. The 3-char floor
   prevents pathological stripping; the suffix whitelist enforces the
   principled constraint."
  [word]
  (loop [reached #{word}
         frontier #{word}]
    (let [next-frontier
          (for [w frontier
                suf descriptor-suffixes
                :when (and (str/ends-with? w suf)
                           (>= (- (count w) (count suf)) 3))
                :let [stripped (subs w 0 (- (count w) (count suf)))]
                :when (not (contains? reached stripped))]
            stripped)]
      (if (seq next-frontier)
        (recur (into reached next-frontier)
               (set next-frontier))
        reached))))

(defn strips-to-extended
  "Strips augmented by English orthographic rules: silent-e drop
   before vowel-initial suffixes, and consonant+y -> consonant+i
   before -ness, -ly, etc. Applies BOTH rules to every form reached
   by the bare strips-to walk."
  [word]
  (let [raw (strips-to (name word))]
    (into raw
          (mapcat (fn [stem]
                    (clojure.set/union (maybe-e-variants stem)
                                       (maybe-y-variants stem))))
          raw)))

(defn- derivable?
  "True if a and b share any reachable form via legitimate suffix
   stripping, allowing for silent-e and y->i orthographic rules."
  [a b]
  (let [paths-a (strips-to-extended a)
        paths-b (strips-to-extended b)]
    (boolean (some paths-a paths-b))))

(defn derivationally-related?
  "True if a and b are morphological variants of the same root.
   Tests (in order): exact equality, blacklist exclusion, irregular
   mapping, and finally the principled derivable? check that requires
   a recognized suffix path between the words."
  [a b]
  (cond
    (= a b)                                         true
    (contains? false-positive-pairs #{a b})         false
    (= (get irregular-canonical a a)
       (get irregular-canonical b b))               true
    :else                                           (derivable? a b)))

(def meaning-shifting-pairs
  "Pairs that are derivationally related but represent a meaning
   shift from descriptor to meta-relation (X to 'relating-to-X').
   Excluded from descriptor-equivalent?. Grows by curation as you
   encounter cases in the tag corpus.

   This is the inverse of false-positive-pairs: false-positive-pairs
   are 'these look related but aren't derivationally connected',
   while meaning-shifting-pairs are 'these ARE derivationally
   connected but shouldn't collapse semantically'."
  #{#{:nature :natural}
    #{:culture :cultural}
    #{:history :historic}
    #{:history :historical}
    #{:child :childhood}
    #{:friend :friendly}
    #{:period :periodic}
    #{:critic :critical}
    #{:real :reality}
    #{:actual :actuality}
    #{:base :basement}
    #{:host :hostile}
    #{:business :busy}
    #{:wild :wilderness}
    #{:beauty :beautiful}
    #{:wonder :wonderful}
    #{:color :colorful}
    #{:skill :skillful}
    #{:power :powerful}})

(defn descriptor-equivalent?
  "True if a and b are morphological variants AND their descriptor
   senses are interchangeable as palette tags. The strict relation:
   collapse only when the derivation preserves the descriptor meaning.

   Excludes pairs where the derivative shifts to a meta-relation
   (nature/natural is not descriptor-equivalent because natural means
   'relating-to-nature' rather than 'being nature itself').

   This is the relation the canonicalization pipeline should use; the
   broader derivationally-related? is for diagnostics and authoring
   review where you want to see all the connections, including the
   ones that should NOT collapse."
  [a b]
  (and (derivationally-related? a b)
       (not (contains? meaning-shifting-pairs #{a b}))))

(defn validate-synonym-map
  "Walk every (canonical, synonym) pair in the synonym-map and flag
   any synonym that's morphologically related to its canonical. Such
   entries should be removed (the canonicalization step already
   handles them) rather than expanded as separate tags. Returns a
   sequence of issue maps; empty sequence means the map is clean."
  [synonym-map]
  (for [[canonical synonyms] synonym-map
        synonym (keys synonyms)
        :when (derivationally-related? canonical synonym)]
    {:canonical canonical
     :synonym   synonym
     :issue     :morphological-variant
     :hint      (str "Remove " synonym " from " canonical
                     "'s synonyms; canonicalization handles it")}))