(defn path-join
  [& parts]
  (string/join parts
               (dyn :path-fs-sep
                    (if (= :windows (os/which))
                      `\`
                      "/"))))

(comment

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (path-join "/tmp" "test.txt")))
  # =>
  "/tmp/test.txt"

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (path-join "/tmp" "foo" "test.txt")))
  # =>
  "/tmp/foo/test.txt"

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep `\`)
      (path-join "C:" "windows" "system32")))
  # =>
  `C:\windows\system32`

  )

(defn path-ext
  [path]
  (let [results (string/find-all "." path)]
    (if-let [last-one (last results)]
      (string/slice path last-one)
      "")))

(comment

  (path-ext "hello.janet")
  # =>
  ".janet"

  (path-ext "bye")
  # =>
  ""

  )

(defn is-dir?
  ``
  Returns true if `path` is a directory.  Otherwise, returns false.

  If optional argument `symlink` is true, return true for symlinks
  that resolve to directories.  The default value for `symlink`
  is false.
  ``
  [path &opt symlink]
  (default symlink false)
  (when-let [path path
             stat (if symlink
                    (os/stat path)
                    (os/lstat path))]
    (= :directory (stat :mode))))

(comment

  (is-dir? (or (os/getenv "HOME")
               (os/getenv "USERPROFILE")))
  # =>
  true

 )

(defn is-file?
  ``
  Returns true if `path` is an ordinary file (e.g. not a directory).
  Otherwise, returns false.

  If optional argument `symlink` is true, return true for symlinks
  that resolve to files.  The default value for `symlink` is false.
  ``
  [path &opt symlink]
  (default symlink false)
  (truthy?
    (when-let [path path
               mode-stat (if symlink
                           (os/stat path)
                           (os/lstat path :mode))]
      (= :file mode-stat))))

(comment

  (is-file? (or (os/getenv "HOME")
                (os/getenv "USERPROFILE")))
  # =>
  false

  (let [name (string (gensym))]
    (if (os/stat name)
      true
      (do
        (spit name "hello")
        (def res
          (is-file? name))
        (os/rm name)
        res)))
  # =>
  true

 )

(defn just-files
  ``
  Recursively visit directory tree starting at `path`, accumulating
  file (not directory) paths by default into array `acc`.

  If optional argument `a-fn` is specified, instead accumulate only
  file paths for which `a-fn` applied to the file path returns a
  truthy result.

  If optional argument `symlink` is truthy, treat symlinks to
  directories as directories.  That is, follow symlinks that point to
  directories and descend into them looking for files.
  ``
  [path acc &opt a-fn symlink]
  (default a-fn identity)
  (default symlink false)
  (when (is-dir? path symlink)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (cond
        (and (is-file? thing-path)
             (a-fn thing-path))
        (array/push acc thing-path)
        #
        (is-dir? thing-path symlink)
        (just-files thing-path acc a-fn symlink))))
  acc)

(comment

  (def acc @[])

  (just-files (path-join (os/getenv "HOME")
                         ".config")
              acc)

  )

(defn just-dirs
  ``
  Recursively visit directory tree starting at `path`, accumulating
  directory paths by default into array `acc`.

  If optional argument `a-fn` is specified, instead accumulate only
  directory paths for which `a-fn` applied to the directory path
  returns a truthy result.

  If optional argument `symlink` is truthy, treat symlinks to
  directories as directories.  That is, follow symlinks that point to
  directories and descend into them looking for directories.
  ``
  [path acc &opt a-fn symlink]
  (default a-fn identity)
  (default symlink false)
  (when (is-dir? path symlink)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (when (is-dir? thing-path symlink)
        (when (a-fn thing-path)
          (array/push acc thing-path))
        (just-dirs thing-path acc a-fn symlink))))
  acc)

(comment

  (def acc @[])

  (just-dirs (path-join (os/getenv "HOME")
                        ".config")
             acc)

 )

(defn visit-files
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered file (not directory) path.
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path (path-join path thing))
      (cond
        (is-file? thing-path)
        (a-fn thing-path)
        #
        (is-dir? thing-path)
        (visit-files thing-path a-fn)))))

(comment

  (visit-files (path-join (os/getenv "HOME")
                          ".config")
               |(eprint $))

 )

(defn visit-dirs
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered directory path.
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path (path-join path thing))
      (when (is-dir? thing-path)
        (a-fn thing-path)
        (visit-dirs thing-path a-fn)))))

(comment

  (visit-dirs (path-join (os/getenv "HOME")
                         ".config")
              |(eprint $))

 )

(defn visit
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered path (file and directory).
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path (path-join path thing))
      (when (or (is-file? thing-path)
                (is-dir? thing-path))
        (a-fn thing-path))
      (when (is-dir? thing-path)
        (visit thing-path a-fn)))))

(comment

  (visit (path-join (os/getenv "HOME")
                    ".config")
         |(eprint $))

 )

