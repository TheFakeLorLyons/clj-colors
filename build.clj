(ns build
  (:require
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as dd]))

;;; See also https://clojure.org/guides/tools_build
;;; To install jar:  clj -T:build jar

(def lib 'com.github.thefakelorlyons/clj-colors)
(def version "0.2.4")

(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar"
                      (name lib)
                      version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)

  (b/copy-dir
   {:src-dirs ["src" "resources"]
    :target-dir class-dir})

  (b/write-pom
   {:class-dir class-dir
    :lib lib
    :version version
    :basis basis
    :src-dirs ["src"]

    :scm {:url "https://github.com/thefakelorlyons/clj-colors"}

    :pom-data
    [[:licenses
      [:license
       [:name "MIT"]
       [:url "https://opensource.org/licenses/MIT"]]]]})

  (b/jar
   {:class-dir class-dir
    :jar-file jar-file}))

(defn install [_]
  (jar nil)

  (b/install
   {:basis basis
    :lib lib
    :version version
    :jar-file jar-file
    :class-dir class-dir}))

(defn deploy [_]
  (jar nil)

  (dd/deploy
   {:installer :remote
    :artifact jar-file
    :pom-file
    (format "%s/META-INF/maven/%s/%s/pom.xml"
            class-dir
            (namespace lib)
            (name lib))}))