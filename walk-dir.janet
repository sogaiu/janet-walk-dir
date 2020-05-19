(import path)

(defn is-dir?
  [path]
  (when-let [path path
             stat (os/lstat path)]
    (= :directory (stat :mode))))

(comment

 (is-dir? (os/getenv "HOME"))

 )

(defn is-file?
  [path]
  (when-let [path path
             stat (os/lstat path)]
    (= :file (stat :mode))))

(comment

 (is-file? (path/join (os/getenv "HOME") ".bashrc"))

 )

# path - filesystem path to start traversal
# acc - array to accumulate results to
(defn just-files
  [path acc &opt a-fn]
  (default a-fn identity)
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (cond
        (and (is-file? thing-path)
             (a-fn thing-path))
        (array/push acc thing-path)
        #
        (is-dir? thing-path)
        (just-files thing-path acc a-fn))))
  acc)

(comment

 (def acc @[])

 (just-files (string (os/getenv "HOME")
                     "/src/hpkgs/community")
             acc)

 (def acc @[])

 (just-files (string (os/getenv "HOME")
                     "/src/hpkgs/")
             acc
             |(= ".hpkg" (path/ext $)))

 )

(defn just-dirs
  [path acc &opt a-fn]
  (default a-fn identity)
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (when (is-dir? thing-path)
        (array/push acc thing-path)
        (just-dirs thing-path acc a-fn))))
  acc)

(comment

 (def acc @[])

 (just-dirs (path/join (os/getenv "HOME")
                       "src/hpkgs/")
             acc)

 )

(defn visit-files
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (cond
        (is-file? thing-path)
        (a-fn thing-path)
        #
        (is-dir? thing-path)
        (visit-files thing-path a-fn)))))

(comment

 (visit-files (path/join (os/getenv "HOME")
                         "src/hpkgs/")
              |(eprint $))

 )

(defn visit-dirs
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (when (is-dir? thing-path)
        (a-fn thing-path)
        (visit-dirs thing-path a-fn)))))

(comment

 (visit-dirs (path/join (os/getenv "HOME")
                         "src/hpkgs/")
             |(eprint $))

 )

(defn visit
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (when (or (is-file? thing-path)
                (is-dir? thing-path))
        (a-fn thing-path))
      (when (is-dir? thing-path)
        (visit thing-path a-fn)))))

(comment

 (visit (path/join (os/getenv "HOME")
                   "src/hermes/doc")
        |(eprint $))

 )
