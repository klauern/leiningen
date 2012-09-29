(ns leiningen.core.test.project
  (:refer-clojure :exclude [read])
  (:use [clojure.test]
        [leiningen.core.project])
  (:require [leiningen.core.user :as user]
            [leiningen.core.classpath :as classpath]
            [clojure.java.io :as io]))

(use-fixtures :once
              (fn [f]
                ;; Can't have user-level profiles interfering!
                (with-redefs [user/profiles (constantly {})
                              user/credentials (constantly nil)]
                  (f))))

(def paths {:source-paths ["src"],
            :test-paths ["test"],
            :resource-paths ["resources" "dev-resources"],
            :compile-path "target/classes",
            :native-path "target/native",
            :target-path "target"})

(def expected {:name "leiningen", :group "leiningen",
               :version "2.0.0-SNAPSHOT",
               :url "https://github.com/technomancy/leiningen"
               :disable-implicit-clean true,
               :eval-in :leiningen,
               :license {:name "Eclipse Public License"}
               :dependencies '[[["leiningen-core" "leiningen-core" nil nil]
                                 {:version "2.0.0-SNAPSHOT"
                                  :group-id "leiningen-core"
                                  :artifact-id "leiningen-core"}]
                               [["clucy" "clucy" nil nil]
                                {:group-id "clucy"
                                 :exclusions [org.clojure/clojure]
                                 :version "0.2.2"
                                 :artifact-id "clucy"}]
                               [["lancet" "lancet" nil nil]
                                {:version "1.0.1"
                                 :group-id "lancet"
                                 :artifact-id "lancet"}]
                               [["hooke" "robert" nil nil]
                                {:version "1.1.2"
                                 :group-id "robert"
                                 :artifact-id "hooke"}]
                               [["stencil" "stencil" nil nil]
                                {:version "0.2.0"
                                 :group-id "stencil"
                                 :artifact-id "stencil"}]]
               :twelve 12 ; testing unquote
               :repositories [["central" {:url "http://repo1.maven.org/maven2/"}]
                              ["clojars" {:url "https://clojars.org/repo/"}]]})

(deftest test-read-project
  (let [actual (read (.getFile (io/resource "p1.clj")))]
    (prn actual)
    (doseq [[k v] expected]
      (is (= v (k actual))))
    (doseq [[k path] paths
            :when (string? path)]
      (is (= (str (:root actual) "/" path)
             (k actual))))
    (doseq [[k path] paths
            :when (coll? path)]
      (is (= (for [p path]
               (str (:root actual) "/" p))
             (k actual))))))

;; TODO: test omit-default
;; TODO: test reading project that doesn't def project

(def test-profiles (atom {:qa {:resource-paths ["/etc/myapp"]}
                          :test {:resource-paths ["test/hi"]}
                          :repl {:dependencies '[[org.clojure/tools.nrepl
                                                  "0.2.0-beta6"
                                                  :exclusions
                                                  [org.clojure/clojure]]
                                                 [org.thnetos/cd-client "0.3.4"
                                                  :exclusions
                                                  [org.clojure/clojure]]]}
                          :tes :test
                          :dev {:test-paths ["test"]}}))

(deftest test-merge-profile-paths
  (with-redefs [default-profiles test-profiles]
    (is (= ["resources" "blue-resources" "test/hi" "/etc/myapp"]
           (-> {:resource-paths ["resources"]
                :profiles {:blue {:resource-paths ["blue-resources"]}}}
               (merge-profiles [:blue :tes :qa])
               :resource-paths)))
    (is (= ["blue-resources" "test/hi" "/etc/myapp"]
           (-> {:resource-paths ^:displace ["resources"]
                :profiles {:blue {:resource-paths ["blue-resources"]}}}
               (merge-profiles [:blue :tes :qa])
               :resource-paths)))
    (is (= ["replaced"]
           (-> {:resource-paths ["resources"]
                :profiles {:blue {:resource-paths ^:replace ["replaced"]}}}
               (merge-profiles [:tes :qa :blue])
               :resource-paths)))
    (is (= {:url "http://" :username "u" :password "p"}
           (-> {:repositories [["foo" {:url "http://" :creds :gpg}]]
                :profiles {:blue {:repositories {"foo"
                                                 ^:replace {:url "http://"
                                                            :username "u"
                                                            :password "p"}}}}}
               (merge-profiles [:tes :qa :blue])
               :repositories
               last last)))))

;; TODO
(deftest test-merge-profile-deps
  (with-redefs [default-profiles test-profiles]
    (let [project {:resource-paths ["resources"]
                   :profiles {:dev {:dependencies
                                    '[^:displace [org.thnetos/cd-client "0.3.0"]
                                      [org.clojure/tools.nrepl "0.2.0-beta2"]]}}}
          cp (-> (merge-profiles project [:dev :repl])
                 (classpath/get-classpath))]
      (is (some (partial re-find #"nrepl-0.2.0-beta2") cp))
      (is (some (partial re-find #"cd-client-0.3.4") cp)))))

(use 'useful.debug)

(deftest test-global-exclusions
  (is (= '[[org.clojure/clojure]
           [org.clojure/clojure pomegranate]
           [org.clojure/clojure]]
         (map (comp :exclusions second)
              (? (-> (normalize
                      {:dependencies
                       '[[lancet "1.0.1"]
                         [leiningen-core "2.0.0-SNAPSHOT" :exclusions [pomegranate]]
                         [clucy "0.2.2" :exclusions [org.clojure/clojure]]]
                       :exclusions '[org.clojure/clojure]})
                     (merge-profiles [:default])
                     :dependencies))))))

(defn add-seven [project]
  (assoc project :seven 7))

(deftest test-middleware
  (is (= 7 (:seven (init-project (read (.getFile (io/resource "p2.clj"))))))))

(deftest test-activate-middleware
  (is (= ""
         (with-out-str
           (binding [*err* *out*]
             (init-project (read (.getFile (io/resource "p3.clj")))))))))

(deftest test-plugin-vars
  (are [project hooks middleware] (= (list hooks middleware)
                                     (map #(plugin-vars (normalize project) %) 
                                          [:hooks :middleware]))
       {:plugins '[[lein-foo "1.2.3"]]}
       '(lein-foo.plugin/hooks) '(lein-foo.plugin/middleware)

       {:plugins '[[lein-foo "1.2.3" :hooks false]]}
       '() '(lein-foo.plugin/middleware)

       {:plugins '[[lein-foo "1.2.3" :middleware false]]}
       '(lein-foo.plugin/hooks) '()

       {:plugins '[[lein-foo "1.2.3" :hooks false :middleware false]]}
       '() '()))

(deftest test-add-profiles
  (let [expected-result {:dependencies [] :profiles {:a1 {:src-paths ["a1/"]}
                                                     :a2 {:src-paths ["a2/"]}}}]
    (is (= expected-result
           (-> {:dependencies []}
               (add-profiles {:a1 {:src-paths ["a1/"]}
                              :a2 {:src-paths ["a2/"]}}))))
    (is (= expected-result
           (-> {:dependencies []}
               (add-profiles {:a1 {:src-paths ["a1/"]}
                              :a2 {:src-paths ["a2/"]}})
               meta
               :without-profiles)))))

(deftest test-merge-anon-profiles
  (is (= {:A 1 :C 3}
         (-> {:profiles {:a {:A 1} :b {:B 2}}}
             (merge-profiles [:a {:C 3}])
             (dissoc :profiles)))))

(deftest test-composite-profiles
  (is (= {:A [1 3 2], :B 2, :C 3}
         (-> {:profiles {:a [:b :c]
                         :b [{:A [1] :B 1 :C 1} :d]
                         :c {:A [2] :B 2}
                         :d {:A [3] :C 3}}}
             (merge-profiles [:a])
             (dissoc :profiles)))))

(deftest test-override-default
  (is (= {:A 1, :B 2, :C 3}
         (-> {:profiles {:a {:A 1 :B 2}
                         :b {:B 2 :C 2}
                         :c {:C 3}
                         :default [:a :b :c]}}
             (merge-profiles [:default])
             (dissoc :profiles)))))

(deftest test-unmerge-profiles
  (is (= {:A 1 :C 3}
         (-> {:profiles {:a {:A 1}
                         :b {:B 2}
                         :c {:C 3}}}
             (merge-profiles [:a :b :c])
             (unmerge-profiles [:b])
             (dissoc :profiles))))
  (is (= {:A 1 :C 3}
         (-> {:profiles {:a {:A 1}
                         :b {:B 2}
                         :c {:C 3}}}
             (merge-profiles [:a :b :c {:D 4}])
             (unmerge-profiles [:b {:D 4}])
             (dissoc :profiles)))))

(deftest test-dedupe-deps
  (is (= '[[org.clojure/clojure "1.4.0"]
           [org.clojure/clojure "1.3.0" :classifier "sources"]]
         (->> {:dependencies '[[org.clojure/clojure "1.4.0"]
                               [org.clojure/clojure "1.3.0" :classifier "sources"]
                               [org.clojure/clojure "1.3.0"]]}
              (normalize-deps)
              (:dependencies)
              (map (comp classpath/dependency-coordinate second))))))

(defn- without-version [[id version & other]]
  (-> (apply hash-map other)
      (select-keys [:classifier :extension])
      (assoc :id id)))