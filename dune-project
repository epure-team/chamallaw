(lang dune 3.13)
(name chamallaw)
(generate_opam_files true)

(package
 (name chamallaw)
 (synopsis "Standalone law package scaffold for Chamallaw")
 (description
  "Standalone law-domain package scaffold with package-owned schema fragments, typed host adapters, and smoke verification for Chamallaw Epic 89.")
 (depends
  (ocaml (>= 5.3.0))
   (dune (>= 3.13))
   yojson
   ptime
   ppx_blob
   (caqti (= "2.2.4"))
   (caqti-eio (= "2.2.4"))
   (alcotest :with-test)
   (cabal :with-test)
   (caqti-driver-sqlite3 :with-test)
   (eio :with-test)
   (eio_posix :with-test)
   (uri :with-test))
  (license MIT)
  (authors "Epure Team")
  (maintainers "Epure Team")
  (homepage "https://github.com/epure-team/chamallaw")
  (source (github epure-team/chamallaw))
  (bug_reports "https://github.com/epure-team/chamallaw/issues")
  (tags ("law" "governance" "ocaml" "sqlite"))
  (documentation "https://epure-team.github.io/chamallaw/"))
